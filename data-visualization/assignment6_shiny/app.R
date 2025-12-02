# Assignment 6 — Full Shiny Dashboard (Part A, B, C, D & E)

library(shiny)
library(readr)
library(datasets)

# ---- Load Data ----
hkid <- read_csv("https://raw.githubusercontent.com/datageneration/datavisualization/master/data/hkid.csv",
                 show_col_types = FALSE)

CO2  <- read_csv("https://raw.githubusercontent.com/datageneration/datavisualization/master/data/CO2.csv",
                 show_col_types = FALSE)

hpi2016 <- read_csv("https://raw.githubusercontent.com/datageneration/datavisualization/master/data/hpi2016.csv",
                    show_col_types = FALSE)

num_cols <- names(hkid)[sapply(hkid, is.numeric)]

# ------------------- UI ---------------------
ui <- navbarPage(

  title = "Assignment 6 — Interactive Shiny Dashboard",

  # ----------- TAB 1: Part A Histogram -----------
  tabPanel("Interactive Histogram (Part A)",

           sidebarLayout(
             sidebarPanel(
               h3("Controls"),
               selectInput("varA", "Choose numeric variable:",
                           choices = num_cols,
                           selected = "Hongkonger"),
               sliderInput("binsA", "Number of bins:", min = 1, max = 40, value = 10)
             ),

             mainPanel(
               h2("Distribution of Selected HKID Variable"),
               plotOutput("histPlot"),
               tags$p(em("Tip: Change variable or bins to see reactive updates."))
             )
           )
  ),

  # ----------- TAB 2: Dataset Viewer (Part B & D) -----------

  tabPanel("Dataset Viewer (Part B & D)",

           sidebarLayout(
             sidebarPanel(
               selectInput("datasetBD", "Choose dataset:",
                           choices = c("Hong Kong Identity", "HPI 2016", "CO2 emissions",
                                       "mtcars", "USArrests", "uspop")),
               numericInput("rowsBD", "Number of rows to view:",
                            value = 10, min = 1, max = 200)
             ),

             mainPanel(
               h3(textOutput("captionBD")),
               verbatimTextOutput("summaryBD"),
               tableOutput("viewBD"),
               plotOutput("plotBD")
             )
           )
  ),

  # ----------- TAB 3: Styled App (Part C & E) -----------

  tabPanel("Styled App (Part C & E)",

           fluidPage(
             tags$head(
               tags$style(HTML("
                 body { font-family: 'Palatino', serif; background: #ffffff; }
                 h2, h3 { font-family: 'Palatino'; }
                 .well { background:#f7f7f7; }
               "))
             ),

             sidebarLayout(
               sidebarPanel(
                 h3("Choose variable:"),
                 selectInput("varC", "HKID variable:", choices = num_cols),
                 sliderInput("binsC", "Bins:", 1, 30, 10)
               ),

               mainPanel(
                 h2("Styled Histogram (HKID Dataset)"),
                 plotOutput("styledPlot"),
                 tags$p(em("This version adds custom fonts and UI styling to match Part C & E."))
               )
             )
           )
  )
)

# ------------------- SERVER ---------------------
server <- function(input, output, session) {

  # ------------------ Part A Histogram -------------------
  output$histPlot <- renderPlot({
    x <- hkid[[input$varA]]
    x <- x[is.finite(x)]
    bins <- seq(min(x), max(x), length.out = input$binsA + 1)

    hist(x,
         breaks = bins,
         col = "#4C78A8",
         border = "white",
         main = paste("Distribution of", input$varA),
         xlab = input$varA)
  })

  # ------------------ Dataset Viewer (Part B & D) -------------------

  datasetBD <- reactive({
    switch(input$datasetBD,
           "Hong Kong Identity" = hkid,
           "HPI 2016" = hpi2016,
           "CO2 emissions" = CO2,
           "mtcars" = mtcars,
           "USArrests" = USArrests,
           "uspop" = data.frame(
             Year = time(uspop),
             Population = as.numeric(uspop)
           )
    )
  })

  output$captionBD <- renderText({
    paste("Dataset:", input$datasetBD)
  })

  output$summaryBD <- renderPrint({
    summary(datasetBD())
  })

  output$viewBD <- renderTable({
    head(datasetBD(), input$rowsBD)
  })

  output$plotBD <- renderPlot({
    data <- datasetBD()

    if (input$datasetBD == "mtcars") {
      plot(data$wt, data$mpg, pch = 19, col = "#4C78A8",
           main = "MPG vs Weight", xlab = "Weight", ylab = "MPG")
    }
    else if (input$datasetBD == "USArrests") {
      barplot(data$Murder, names.arg = rownames(data), las = 2,
              col = "#75AADB", main = "US Murder Rate by State")
    }
    else if (input$datasetBD == "uspop") {
      plot(data$Year, data$Population, type = "l", col = "#E76F51",
           main = "U.S. Population Growth", ylab = "Population")
    }
  })

  # ------------------ Styled Histogram (Part C & E) -------------------
  output$styledPlot <- renderPlot({
    x <- hkid[[input$varC]]
    x <- x[is.finite(x)]

    hist(x,
         breaks = input$binsC,
         col = "#547AA5",
         border = "white",
         main = paste("Styled Histogram of", input$varC),
         xlab = input$varC,
         family = "Palatino")
  })
}

# ---- Run App ----
shinyApp(ui, server)


