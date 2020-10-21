library(shiny)
library(valueEQ5D)

#The EQ-5D levels, countries and corresponding methods pre-loaded as a table for later use
score_data <- read.table(
  text = " Score	Country	Method
  EQ-5D-3L	Argentina	VAS
  EQ-5D-3L	Belgium	VAS
  EQ-5D-3L	Denmark 	VAS
  EQ-5D-3L	Europe	VAS
  EQ-5D-3L	Finland	VAS
  EQ-5D-3L	Germany	VAS
  EQ-5D-3L	Malaysia	VAS
  EQ-5D-3L	NewZealand	VAS
  EQ-5D-3L	Slovenia	VAS
  EQ-5D-3L	Spain	VAS
  EQ-5D-3L	UK	VAS
  EQ-5D-3L	Argentina	TTO
  EQ-5D-3L	Australia	TTO
  EQ-5D-3L	Brazil	TTO
  EQ-5D-3L	Canada	TTO
  EQ-5D-3L	Chile	TTO
  EQ-5D-3L	China	TTO
  EQ-5D-3L	Denmark 	TTO
  EQ-5D-3L	France	TTO
  EQ-5D-3L	Germany	TTO
  EQ-5D-3L	Iran	TTO
  EQ-5D-3L	Italy	TTO
  EQ-5D-3L	Japan	TTO
  EQ-5D-3L	Korea	TTO
  EQ-5D-3L	Malaysia	TTO
  EQ-5D-3L	Netherlands	TTO
  EQ-5D-3L	Poland	TTO
  EQ-5D-3L	Portugal	TTO
  EQ-5D-3L	Singapore	TTO
  EQ-5D-3L	Spain	TTO
  EQ-5D-3L	SriLanka	TTO
  EQ-5D-3L	Sweden	TTO
  EQ-5D-3L	Taiwan	TTO
  EQ-5D-3L	Thailand	TTO
  EQ-5D-3L	Trinidad_and_Tobago	TTO
  EQ-5D-3L	UK	TTO
  EQ-5D-3L	USA	TTO
  EQ-5D-3L	Zimbabwe	TTO
  EQ-5D-5L	Canada	VT
  EQ-5D-5L	China	VT
  EQ-5D-5L	England 	VT
  EQ-5D-5L	Ethopia 	VT
  EQ-5D-5L	France 	VT
  EQ-5D-5L	Germany	VT
  EQ-5D-5L	HongKong	VT
  EQ-5D-5L	Indonesia	VT
  EQ-5D-5L	Ireland	VT
  EQ-5D-5L	Japan	VT
  EQ-5D-5L	Korea	VT
  EQ-5D-5L	Malaysia	VT
  EQ-5D-5L	Netherlands	VT
  EQ-5D-5L	Poland 	VT
  EQ-5D-5L	Portugal 	VT
  EQ-5D-5L	Spain	VT
  EQ-5D-5L	Taiwan	VT
  EQ-5D-5L	Thailand	VT
  EQ-5D-5L	Uruguay	VT
  EQ-5D-5L	USA	VT
  EQ-5D-5L	Vietnam	VT
  EQ-5D-5L	Denmark	CW
  EQ-5D-5L	France	CW
  EQ-5D-5L	Germany	CW
  EQ-5D-5L	Japan	CW
  EQ-5D-5L	Netherlands	CW
  EQ-5D-5L	Spain	CW
  EQ-5D-5L	Thailand	CW
  EQ-5D-5L	UK	CW
  EQ-5D-5L	USA	CW
  EQ-5D-5L	Zimbabwe	CW",
  header = TRUE, stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("EQ-5D scoring"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("score_selector"),
      uiOutput("type_selector"),
      uiOutput("input_selector"),
      htmlOutput("text_selector"),
      uiOutput("dim_selector"),
      htmlOutput("country_selector"),
      htmlOutput("method_selector"),
      htmlOutput("cw_selector")
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.type == 'Multiple'",
        uiOutput("tab_selector_multiple")
      ),
      conditionalPanel(
        condition = "input.type == 'Single'",
        uiOutput("tab_selector_single")
      )
    )
  ),
  actionButton("ShowDataButton", "Show the data !"),
  actionButton("goButton", "Calculate !"),
  br(),
  uiOutput("download_selector")

)
server <- function(input, output, session) {
  #choose the score that you want to calculate
  output$score_selector <- renderUI({
    selectInput(inputId = "Score", #name of input
                label = h3("Score:"), #label displayed in ui
                choices = as.character(unique(score_data$Score)),
                # calls unique values from the score column in the previously created table
                selected = unique(score_data$Score)[2]) #default choice (not required)
  })
  #choose the type of calculation
  output$type_selector <- renderUI({
    radioButtons("type", label = h4("Mutiple or single score:"),
                 choices = list("Multiple" = "Multiple", "Single" = "Single"), selected = "Multiple")
  })
  #depending on the type of calculation number of tabs are chosen
  output$tab_selector_multiple <- renderUI({ 
    tabsetPanel(type = "tabs", id = "multiple",
                tabPanel("Data", tableOutput("data")),
                tabPanel("Info", verbatimTextOutput("info")),
                tabPanel("Summary", tableOutput("table")),
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Index values", tableOutput("result.data")))
  })
  output$tab_selector_single <- renderUI({ 
    tabsetPanel(type = "tabs", id = "single",
                tabPanel("Input score", verbatimTextOutput("input.score")),
                tabPanel("Result", verbatimTextOutput("summary")))
  })
  output$country_selector <- renderUI({#creates country select box object called in ui
    data_available1 <- score_data[score_data$Score == input$Score, "Country"]
    #creates a reactive list of available countries
    selectInput(inputId = "Country", #name of input
                label = "Country:", #label displayed in ui
                choices = unique(data_available1), #calls list of available countries
                selected = unique(data_available1)[1])
  })
  output$method_selector <- renderUI({#creates method select box object called in ui
    data_available1 <- score_data[score_data$Score == input$Score, ]
    data_available <- data_available1[data_available1$Country == input$Country, "Method"]
    #creates a reactive list of available methods based on the country selection made
    selectInput(inputId = "Method", #name of input
                label = "Method:", #label displayed in ui
                choices = unique(data_available),
                selected = unique(data_available)[2])
  })
  output$text_selector <- renderText({
    "Please provide column names of responses from the data file provided. Or if the score is calculated using responses from single individual,
    please provide values of the responses below"
  })
  output$input_selector <- renderUI({
    conditionalPanel(
      condition = "input.type == 'Multiple'",
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      checkboxInput("header", "Header", TRUE),
      checkboxInput("gendercriteria", "Gender Criteria Inclusion", FALSE),
      conditionalPanel(
        condition = "input.gendercriteria == true",
        radioButtons("gender", label = h3("Choose the gender"),
                     choices = list("NA" = "NA", "Male" = "Male", "Female" = "Female"))
      ),
      checkboxInput("agecriteria", "Age Criteria Inclusion"),
      conditionalPanel(
        condition = "input.agecriteria == true",
        # Input: Specification of range within
        sliderInput("agerange", "Age range:",
                    min = 0, max = 120,
                    value = c(0, 120))
      )
    )
  })
  output$dim_selector <- renderUI({
    fluidRow(
      column(12,
             textInput("col1", "Response to question 1 (Mobility)"),
             textInput("col2", "Response to question 2 (Self care)"),
             textInput("col3", "Response to question 3 (Usual activity)"),
             textInput("col4", "Response to question 4 (Pain)"),
             textInput("col5", "Response to question 5 (Anxiety/Depression)")
      )
    )
  })
  output$cw_selector <- renderText({
    "For EQ-5D-5L scores, please select CW if you want to use crosswalk calculator"
  })
  output$download_selector <- renderUI({
    conditionalPanel(
      condition <- "input.type == 'Multiple'",
      downloadButton("downloadData", "Download modified data")
    )
  })

  datasetInput <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    validate(
      need(inFile != "", "Please select a data set")
    )
    eq5d_data <- read.csv(inFile$datapath, header = input$header)
  })

  calculate <- reactive({
    dataset <- datasetInput()
    dims <- c(dataset[[input$col1]], dataset[[input$col2]], dataset[[input$col3]],
              dataset[[input$col4]], dataset[[input$col5]])
    if (is.null(dataset[[input$col1]]) || is.null(dataset[[input$col2]]) 
        || is.null(dataset[[input$col3]]) || is.null(dataset[[input$col4]])
        || is.null(dataset[[input$col5]])) {
      stop("EQ-5D column names many be empty or invalid")
    }
    if (input$Score == "EQ-5D-5L") {
      if (any(dims < 0) || any(dims > 5)) stop("Invalid EQ-5D-5L dimensions")
    }
    if (input$Score == "EQ-5D-3L") {
      if (any(dims < 0) || any(dims > 3)) stop("Invalid EQ-5D-3L dimensions")
    }
    if (input$Score == "EQ-5D-5L") {
      if (input$Method == "CW") {
        ans <- map_5Lto3L(dataset, input$col1, input$col2, input$col3, 
                         input$col4, input$col5, input$Country, input$Method,
                         input$gender, c(input$agerange[1], input$agerange[2]))
      }else{
        ans <- value_5L(dataset, input$col1, input$col2, input$col3, 
                       input$col4, input$col5, input$Country, input$gender, 
                       c(input$agerange[1], input$agerange[2]))
      }
    }else{
      ans <- value_3L(dataset, input$col1, input$col2, input$col3, input$col4,
                     input$col5, input$Country, input$Method, input$gender, 
                     c(input$agerange[1], input$agerange[2]))
    }
    return(ans)
  })
  calculate_single <- reactive({ 
    dims <- c( input$col1, input$col2, input$col3, input$col4, input$col5)
    if (is.null(input$col1) || is.null(input$col2) || is.null(input$col3) 
        || is.null(input$col4) || is.null(input$col5)) {
      stop("EQ-5D columns many be empty or invalid")
    }
    if (input$Score == "EQ-5D-5L") {
      if (any(dims < 0) || any(dims > 5)) stop("Invalid EQ-5D-5L dimensions")
    }
    if (input$Score == "EQ-5D-3L") {
      if (any(dims < 0) || any(dims > 3)) stop("Invalid EQ-5D-3L dimensions")
    }
    if (input$Score == "EQ-5D-5L") {
      if (input$Method == "CW") {
        ans <- map_5Lto3L_Ind(input$Country, input$Method, input$col1, 
                              input$col2, input$col3, input$col4, input$col5)
      } else {
        ans <- value_5L_Ind(input$Country, input$col1, input$col2, input$col3, 
                            input$col4, input$col5)
      }
    }else{
      ans <- value_3L_Ind(input$Country, input$Method, input$col1, input$col2,
                          input$col3, input$col4, input$col5)
    }
  })

  observeEvent(input$ShowDataButton, {
    output$data <- renderTable({
      validate(
        need(input$file1 != "", "No data file found")
      )
      dataset <- datasetInput()
    })
    output$input.score <- renderText({
      paste0("You have selected : Score as ", input$col1, input$col2, input$col3, 
             input$col4, input$col5, " for ", input$Country)

    })
  })

  observeEvent(input$goButton, {
    output$info <- renderText({
      if (is.na(input$gender) | is.null(input$gender) | input$gender == "NA") {
        paste0("You have selected :", input$Score, " for ", input$Country, 
               " with ages between ", input$agerange[1], " and ", input$agerange[2])
      }else{
        paste0("You have selected :", input$Score, " for ", input$Country, " for ",
               input$gender, " with ages between ", input$agerange[1], " and ", 
               input$agerange[2])
      }

    })
    output$summary <- renderText({
      paste0("The index value is ", calculate_single())
    })
    output$table <- renderTable({
      answer <- calculate()$stats
    })
    output$result.data <- renderTable({
      answer <- calculate()$modifiedData
    })

    output$plot <- renderPlot({
      answer <- plot(calculate()$histogram)
    })
    output$downloadData <- downloadHandler(
      filename = function() { paste(input$Score, "_", input$Country, "_",
                                    input$Method, ".csv", sep = "")},
      content = function(file) {
        write.csv(calculate()$modifiedData, file, row.names = F)
      }
    )
  })
}

shinyApp(ui, server)

