library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  h2("Correlation Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      selectInput("corr_x", "x Variable", numeric_vars[-1]),
      selectInput("corr_y", "y Variable", numeric_vars),
      h2("Choose a subset of the data:"),
      
      # Options to select for Household Language
      radioButtons("hhl_corr","Household Language", choiceNames=c("All","English only","Spanish", "Other"),
                                                   choiceValues=c("all","english","spanish","other")),
      
      # Options to select for SNAP Recipients
      radioButtons("fs_corr","SNAP Recipient", choiceNames=c("All", "Yes", "No"),
                                               choiceValues=c("all","yes","no")),
      
      # Options to select for Educational Attainment
      radioButtons("schl_corr","Educational attainment",
                   choiceNames=c("All","High School not Completed", "High School or GED", "College Degree"),
                   choiceValues=c("all", "no_hs", "hs", "college")),
      
      h2("Select a Sample Size"),
      sliderInput("corr_n", "", value=20, min=20, max=500),
      actionButton("corr_sample","Get a Sample!")
    ),
    mainPanel(
      plotOutput("plot"),
      textOutput("info"),
      conditionalPanel("input.corr_sample", #only show if a sample has been taken
                       h2("Guess the correlation!"),
                       column(6, numericInput("corr_guess", "", value = 0, min = -1, max = 1)),
                       column(6, actionButton("corr_submit", "Check Your Guess!"))
      ) 
    )
  )
)

my_sample <- readRDS("my_sample_temp.rds")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #This code makes sure the select boxes update so they can't select the same variable in both!
    observeEvent(input$corr_x, {
      corr_x <- input$corr_x
      corr_y <- input$corr_y
      choices <- numeric_vars
      if (corr_x != corr_y){
        choices <- choices[-which(choices == corr_x)]
        updateSelectizeInput(session, "corr_y", choices = choices, selected = corr_y)
      }
    })
    # now, update the 'x' selections available
    observeEvent(input$corr_y, {
      corr_x <- input$corr_x
      corr_y <- input$corr_y
      choices <- numeric_vars
      if (corr_x != corr_y){
        choices <- choices[-which(choices == corr_y)]
        updateSelectizeInput(session, "corr_x", choices = choices, selected = corr_x)
      }
    })
    
    #Create a reactiveValues() object called sample_corr
    sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
    
    # observeEvent() to look for the action button (corr_sample)
    observeEvent(input$corr_sample, {
      
      # Match options for Household Languages
      if(input$hhl_corr == "all"){ hhl_sub <- HHLvals } 
      else if(input$hhl_corr == "english"){ hhl_sub <- HHLvals["1"] } 
      else if(input$hhl_corr == "spanish"){ hhl_sub <- HHLvals["2"] } 
      else { hhl_sub <- HHLvals[c("0", "3", "4", "5")] }

      # Match options for SNAP Receipient
      if(input$fs_corr == "all"){ fs_sub <- FSvals } 
      else if(input$fs_corr == "yes"){ fs_sub <- FSvals["1"] }
      else { fs_sub <- FSvals["2"] }

      # Match options for Educational Attainment
      if(input$schl_corr == "all"){ schl_sub <- SCHLvals } 
      else if(input$schl_corr == "no_hs"){ schl_sub <- SCHLvals[sprintf("%02d", 0:15)] } 
      else if(input$schl_corr == "hs"){ schl_sub <- SCHLvals[as.character(16:19)] } 
      else { schl_sub <- SCHLvals[as.character(20:24)] }

      corr_vars <- c(input$corr_x, input$corr_y)

      subsetted_data <- my_sample |>
        filter(#cat vars first
          HHLfac %in% hhl_sub,
          FSfac %in% fs_sub,
          SCHLfac %in% schl_sub
        ) %>% # Make sure numeric variables are in appropriate range. Must use %>% here for {} to work
        {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
        {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
        {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
        {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
        {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
        {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
        {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
        {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
        {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}

      index <- sample(1:nrow(subsetted_data),
                      size = input$corr_n,
                      replace = TRUE,
                      prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
      
      # ***You now need to update the sample_corr reactive value object***
      #  the corr_data argument should be updated to be the subsetted_data[index,]
      #  the corr_truth argument should be updated to be the correlation between
      #  the two variables selected. This can be found with this code:
      #  cor(sample_corr$corr_data |> select(corr_vars))[1,2]

      sample_corr$corr_data <- subsetted_data[index,]
      sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(corr_vars))[1,2]
    })
    output$plot <- renderPlot({
      validate(
        need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
      )
      ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
        geom_point()
    })
    
    #This code does the correlation guessing game! Nothing to change here
    observeEvent(input$corr_submit, {
      close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
      if(close){
        shinyalert(title = "Nicely done!",
                   paste0("The sample correlation is ", 
                          round(sample_corr$corr_truth, 4), 
                          "."),
                   type = "success"
        )
      } else {
        if(input$corr_guess > sample_corr$corr_truth){
          shinyalert(title = "Try again!",
                     "Try guessing a lower value.")
        } else {
          shinyalert(title = "Try again!",
                     "Try guessing a higher value.")
        }
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
