library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)

source("list.R")

ui <- fluidPage(
 
  #App title  
  titlePanel("Conditions Analysis"),
  
  #Sidebar layout with input & output definitions
  sidebarLayout(
    #Sidebar panel for inputs
    sidebarPanel(
      #text
      helpText("Upload files in the form \"conditions.csv\", \"procedures.csv\", and \"careplans.csv\". Choose a condition from list to get more information on."),
      
      #input: files
      fileInput(inputId = "file1", label = "Choose conditions CSV file", multiple = FALSE, accept = ".csv"), 
      fileInput(inputId = "file2", label = "Choose procedures CSV file", multiple = FALSE, accept = ".csv"), 
      fileInput(inputId = "file3", label = "Choose careplans CSV file", multiple = FALSE, accept = ".csv"), 
      
      #input: condition
      selectInput(inputId = "letter", label = "Pick the letter the condition starts with", choices = abc_list),
      conditionalPanel(
        condition = "input.letter == 'A'",
        selectInput(inputId = "conditiona", "Pick the condition", choices = a_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'B'",
        selectInput(inputId = "conditionb", "Pick the condition", choices = b_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'C'",
        selectInput(inputId = "conditionc", "Pick the condition", choices = c_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'D'",
        selectInput(inputId = "conditiond", "Pick the condition", choices = d_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'E'",
        selectInput(inputId = "conditione", "Pick the condition", choices = e_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'F'",
        selectInput(inputId = "conditionf", "Pick the condition", choices = f_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'G'",
        selectInput(inputId = "conditiong", "Pick the condition", choices = g_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'H'",
        selectInput(inputId = "conditionh", "Pick the condition", choices = h_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'I'",
        selectInput(inputId = "conditioni", "Pick the condition", choices = i_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'L'",
        selectInput(inputId = "conditionl", "Pick the condition", choices = l_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'M'",
        selectInput(inputId = "conditionm", "Pick the condition", choices = m_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'N'",
        selectInput(inputId = "conditionn", "Pick the condition", choices = n_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'O'",
        selectInput(inputId = "conditiono", "Pick the condition", choices = o_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'P'",
        selectInput(inputId = "conditionp", "Pick the condition", choices = p_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'R'",
        selectInput(inputId = "conditionr", "Pick the condition", choices = r_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'S'",
        selectInput(inputId = "conditions", "Pick the condition", choices = s_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'T'",
        selectInput(inputId = "conditiont", "Pick the condition", choices = t_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'V'",
        selectInput(inputId = "conditionv", "Pick the condition", choices = v_list)
      ),
      conditionalPanel(
        condition = "input.letter == 'W'",
        selectInput(inputId = "conditionw", "Pick the condition", choices = w_list)
      ),
      
      #action buttons
      #actionButton(inputId = "go", label = "Go"),
      
    ),
    
    #Main panel for displaying outputs
    mainPanel(
      #divide into tabs
      tabsetPanel(id = "tabset",
                  tabPanel("All Patient Information",
                           helpText("Click on Load Table after uploading all files."),
                           actionButton("load", "Load Table"),
                           tableOutput("table_all")),
                  tabPanel("Patients with Selected Condition",
                           helpText("You have selected:"),
                           textOutput("select_choice"),
                           helpText("Click Load Table to display all patients with the selected condition."),
                           actionButton("load2", "Load Table"),
                           tableOutput("table_cond")),
                  tabPanel("Top 10 Conditions",
                           helpText("Click Load Plot to display top 10 conditions with the highest frequency of hospital visits."),
                           actionButton("load3", "Load Plot"),
                           plotOutput("table_top")),
                  tabPanel("Selected Condition Conclusions",
                           helpText("You have selected:"),
                           textOutput("conclusion"))
      )
    ))
)