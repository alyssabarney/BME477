library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)

source("list.R")

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  #read saved files
  conditions <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  procedures <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath)
  })
  
  careplans <- reactive({
    req(input$file3)
    read.csv(input$file3$datapath)
  })
  
  #tab 1 -> make the table for all patients
  make_merge2_df <- reactive({
    #isolate patient & description columns
    # cond$PATIENT <- conditions()$PATIENT
    # cond$DESCRIPTION <- conditions()$DESCRIPTION
    cond <- conditions() %>% select("PATIENT", "DESCRIPTION")
    #sort patient column
    cond_order <- cond[order(cond$PATIENT),]
    #isolate patient, description, reason columns
    # proc$PATIENT <- procedures()$PATIENT
    # proc$DESCRIPTION <- procedures()$DESCRIPTION
    #proc$REASONDESCRIPTION <- procedures()$REASONDESCRIPTION
    proc <- procedures() %>% select("PATIENT", "DESCRIPTION", "REASONDESCRIPTION")
    #sort patient column
    proc_order <- proc[order(proc$PATIENT),]
    #isolate columns
    # care$PATIENT <- careplans()$PATIENT
    # care$DESCRIPTION <- careplans()$DESCRIPTION
    #care$REASONDESCRIPTION <- careplans()$REASONDESCRIPTION
    care <- careplans() %>% select("PATIENT", "DESCRIPTION", "REASONDESCRIPTION")
    #sort patient column
    care_order <- care[order(care$PATIENT),]
    #find overlapping patients
    
    
    same_patient_list <- vector("list")
    same_patient <- duplicated(cond_order$PATIENT)
    for (i in 1:length(same_patient)){
      if (same_patient[i] == FALSE){
        new_element <- rep(cond_order$PATIENT[i])
        same_patient_list[[length(same_patient_list) + 1]] <- new_element
      }
    }
    #make a table with Patient, condition(s), procedure(s), & careplans
    
    #df$Patient <- same_patient_list
    
    cond_df <- aggregate(DESCRIPTION~PATIENT, cond_order, paste, collapse = ",")
    
    proc_df <- aggregate(DESCRIPTION~PATIENT, proc_order, paste, collapse = ",")
    
    care_df <- aggregate(DESCRIPTION~PATIENT, care_order, paste, collapse = ",")
    
    merge1_df <- merge(cond_df, proc_df, by = "PATIENT")
    merge2_df <- merge(merge1_df, care_df, by = "PATIENT")
    colnames(merge2_df) <- c('Patient', 'Condition', 'Procedure', 'Careplan')
    
    return(merge2_df)
  })
  
  #display table for tab 1
  observeEvent(input$load, {
    output$table_all <- renderTable({
      merge2_df <- make_merge2_df()
      merge2_df
    })
  })
  
  #tab 2 -> make top 10 list
  make_cond_list2_df <- reactive({
    #separate into each condition
    condition_each <- split(conditions(), conditions()$DESCRIPTION)
    
    #make a list of the number of each condition & the position of each condition
    freq_list <- vector("list")
    
    for (i in condition_each){
      new_element <- rep(nrow(i))
      freq_list[[length(freq_list) + 1]] <- new_element
    }
    
    freq_df <- data.frame(matrix(unlist(freq_list), nrow = length(freq_list), byrow = TRUE))
    
    freq_df$order <- c(1:nrow(freq_df))
    
    colnames(freq_df) <- c("freq", "order")
    
    #find the top recurring conditions
    #sort data in descending order & list top 10 freq & positions
    freq_df1 <- freq_df %>%
      arrange(desc(freq)) %>%
      slice(1:10)
    #isolate freq column
    freq_df2 <- freq_df1 %>%
      select("freq")
    colnames(freq_df2) <- c("Number of Visits")
    #take top 10 positions & convert it to list of conditions
    cond_list <- vector("list")
    for (i in 1:nrow(freq_df1)){
      j <- freq_df1$order[i]
      new_element <- rep(condition_each[j])
      cond_list[[length(cond_list) + 1]] <- new_element
    }
    
    #isolate just the condition names
    colnames <- c("Start", "Stop", "Patient", "Encounter", "Code", "Description")
    top1 <- as.data.frame(cond_list[[1]])
    names(top1) <- colnames
    top2 <- as.data.frame(cond_list[[2]])
    names(top2) <- colnames
    top3 <- as.data.frame(cond_list[[3]])
    names(top3) <- colnames
    top4 <- as.data.frame(cond_list[[4]])
    names(top4) <- colnames
    top5 <- as.data.frame(cond_list[[5]])
    names(top5) <- colnames
    top6 <- as.data.frame(cond_list[[6]])
    names(top6) <- colnames
    top7 <- as.data.frame(cond_list[[7]])
    names(top7) <- colnames
    top8 <- as.data.frame(cond_list[[8]])
    names(top8) <- colnames
    top9 <- as.data.frame(cond_list[[9]])
    names(top9) <- colnames
    top10 <- as.data.frame(cond_list[[10]])
    names(top10) <- colnames
    
    cond_list2 <- vector("list")
    cond_list2 <- c(top1$Description[1], top2$Description[1], top3$Description[1], top4$Description[1], top5$Description[1], top6$Description[1], top7$Description[1], top8$Description[1], top9$Description[1], top10$Description[1])
    
    #converting into df
    cond_list2_df <- as.data.frame(cond_list2)
    colnames(cond_list2_df) <- c("Conditions")
    
    cond_list2_df$Frequency <- freq_df2$`Number of Visits`
    
    return(cond_list2_df)
  })
  
  #display plot of top 10 list
  observeEvent(input$load3, {
    output$table_top <- renderPlot({
      cond_list2_df <- make_cond_list2_df()
      
      cond_list2_df_mod <- data.frame(lapply(cond_list2_df, rep, cond_list2_df$Frequency))
      
      ggplot(data.frame(cond_list2_df_mod), aes(x = cond_list2_df_mod$Conditions)) +
        geom_bar() + 
        scale_x_discrete(guide = guide_axis(n.dodge = 3)) + 
        labs(title = "Top 10 Conditions with the Highest Number of Hospital Visits") + 
        xlab("Conditions") + 
        ylab("Frequency of Hospital Visits")
    })
  })
  
  #display picked condition
  output$select_choice <- renderText({
    #read the selected condition
    if (input$letter == 'A'){
      paste0(input$conditiona)
    } else if (input$letter == 'B'){
      paste0(input$conditionb)
    } else if (input$letter == 'C'){
      paste0(input$conditionc)
    } else if (input$letter == 'D'){
      paste0(input$conditiond)
    } else if (input$letter == 'E'){
      paste0(input$conditione)
    } else if (input$letter == 'F'){
      paste0(input$conditionf)
    } else if (input$letter == 'G'){
      paste0(input$conditiong)
    } else if (input$letter == 'H'){
      paste0(input$conditionh)
    } else if (input$letter == 'I'){
      paste0(input$conditioni)
    } else if (input$letter == 'L'){
      paste0(input$conditionl)
    } else if (input$letter == 'M'){
      paste0(input$conditionm)
    } else if (input$letter == 'N'){
      paste0(input$conditionn)
    } else if (input$letter == 'O'){
      paste0(input$conditiono)
    } else if (input$letter == 'P'){
      paste0(input$conditionp)
    } else if (input$letter == 'R'){
      paste0(input$conditionr)
    } else if (input$letter == 'S'){
      paste0(input$conditions)
    } else if (input$letter == 'T'){
      paste0(input$conditiont)
    } else if (input$letter == 'V'){
      paste0(input$conditionv)
    } else if (input$letter == 'W'){
      paste0(input$conditionw)
    }
  })
  
  #tab 3 -> make table for selected condition
  observeEvent(input$load2, {
    output$table_cond <- renderTable({
      merge2_df <- make_merge2_df()
      #make a table for the patients with the selected condition
      selected_df <- data.frame(matrix(ncol = 4))
      colnames(selected_df) <- c('Patient', 'Condition', 'Procedure', 'Careplan')
      if (input$letter == 'A'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditiona, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'B'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionb, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'C'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionc, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'D'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditiond, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'E'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditione, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'F'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionf, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'G'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditiong, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'H'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionh, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'I'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditioni, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'L'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionl, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'M'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionm, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'N'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionn, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'O'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditiono, patient_cond_list, fixed = TRUE) == TRUE){
            #selected_list[i] <- i
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'P'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionp, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'R'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionr, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'S'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditions, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'T'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditiont, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'V'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionv, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      } else if (input$letter == 'W'){
        for (i in 1:nrow(merge2_df)){
          patient_cond_list <- unlist(merge2_df$Condition[i])
          if (grepl(input$conditionw, patient_cond_list, fixed = TRUE) == TRUE){
            selected_df <- rbind(selected_df, merge2_df[i,])
          }
        }
      }
      selected_df
    })
  })
  
  #tab 4 -> read out conclusions
  output$conclusion <- renderText({
    cond_list2_df <- make_cond_list2_df()
    #determines if selected condition is in the top 10 conditions & will output conclusion based on that
    in_top10 <- FALSE
    if (input$letter == 'A'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditiona, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditiona, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditiona, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'B'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionb, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionb, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionb, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'C'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionc, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionc, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionc, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'D'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditiond, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditiond, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditiond, "  is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'E'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditione, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditione, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditione, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'F'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionf, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionf, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionf, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'G'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditiong, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditiong, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditiong, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'H'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionh, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionh, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionh, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'I'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditioni, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditioni, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditioni, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'L'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionl, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionl, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionl, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'M'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionm, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionm, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionm, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'N'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionn, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionn, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionn, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'O'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditiono, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditiono, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditiono, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'P'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionp, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionp, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionp, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'R'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionr, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionr, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionr, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'S'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditions, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditions, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditions, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'T'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditiont, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditiont, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditiont, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'V'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionv, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionv, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionv, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    } else if (input$letter == 'W'){
      for (i in 1:nrow(cond_list2_df)){
        if (grepl(input$conditionw, cond_list2_df$Conditions[i], fixed = TRUE) == TRUE){
          in_top10 <- TRUE
        }
      }
      if (in_top10 == TRUE){
        paste0(input$conditionw, " is in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should be prioritized. With a higher frequency patient visits, hospitals should allocate more resources or training towards the selected condition.")
      } else if (in_top10 == FALSE){
        paste0(input$conditionw, " is not in the top 10 conditions in terms of highest frequency of hospital visits. This suggests that this condition should not necessarily be prioritized over other conditions.")
      }
    }
  })
}