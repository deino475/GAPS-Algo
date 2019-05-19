library(shiny)
source("gaps.R")

ui <- fluidPage(
  navbarPage(title = "GAPS",
    tabPanel("Description",
      h1("Genetic Algorithm for Poll Selection (GAPS)"),
      h2("What is GAPS?"),
      p("GAPS is a machine learning algorithm developed to help organizations select the best early voting polling places based on the distribution of the population."),
      h2("How Does It Work?"),
      p("The algorithm uses a combination of a modified K-Means clustering with a genetic component in order to insure the best polling locations are selected."),
      p("The algorithm creates X number of possible polling place scenarios and then optimizes them. Then it finds the one with the highest fitness score. That moves into the training stage."),
      p("During the training stage, the algorithm applies X random mutations to the fittest clustering scenario to possibly find an even better allocation of polling places. If so, it replaces the previous example."),
      h2("Why Do We Need GAPS?"),
      p("Polling places are becoming the new line of attack during many elections in 2018. For example, Waller County, TX is being sued by Prairie View A&M University students because the unfair allocation of early voting polling places"),
      p("Algorithmic political decision-making has been very helpful in making strides in political redistricting, but these techniques have not ever been applied to the selection of polling places.")
    ),
    tabPanel("Case Study: Waller County, TX",
      sidebarLayout(
        sidebarPanel(
         p("This is a case study analyzing the results of our algorithm in comparison to the early voting polling locations selected in 2015 by Waller County."),
         p("You can try it yourself. Experiment with the parameters below in order to see if you can have a better selection of early voting locations than Waller County."),
         sliderInput("testNumberPlaces","Select the Number of Polling Places You Want",1,6,2),
         sliderInput("testNumberGens","Select the Number of Generations You Want to Train",1,50,20),
         selectInput("testInitMethod","Select an Initialization Method",choices = c("Forgy Method" = "forgy","Partition Method" = "partition")),
         actionButton("testGaps","Run Case Study"),
         downloadButton("downloadData", "Download")
        ),
        mainPanel(
          h2("Analysis of Polling Places"),
          fluidRow(
            column(6,
              textOutput("testGapsMean"),
              plotOutput("testGapsOutput")       
            ),
            column(6,
              textOutput("testCurrentMean"),
              plotOutput("testCurrentOutput")       
            )
          ),
          hr(),
          h2("Recommended Polling Places"),
          fluidRow(
            column(12,
              tableOutput("caseStudy")
            )
          )
        )
      )
    ),
    tabPanel("Try It Yourself",
      sidebarLayout(
        sidebarPanel(
          p("If you liked what you saw, you can use it too. Upload a CSV file with "),
          fileInput("file1", "Choose Population File (CSV)",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          fileInput("file2", "Choose Locations File (CSV)",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          sliderInput("numberPlaces","Select the Number of Polling Places You Want",1,6,3),
          sliderInput("numberGens","Select the Number of Generations You Want to Train",1,50,20),
          selectInput("initMethod","Select an Initialization Method",choices = c("Forgy Method" = "forgy","Partition Method" = "partition")),
          actionButton("yourGaps","Run Your Simulation"),
          downloadButton("downloadYourSim", "Download")
        ),
        mainPanel(
          tableOutput("yourStudy")
        )
      )
    )
  )
)

server <- function(input, output) {
  data.points <- read.csv("population.csv")
  potential.centers <- read.csv("locations.csv")
  current.centers <- read.csv("old_locations.csv")
  
  data.to.export <- reactive({
    number.gens <- input$testNumberGens
    number.polls <- input$testNumberPlaces
    return(gaps(data.points, potential.centers, k = number.polls, generations = number.gens))
  })
  
  your.example <- reactive({
    population.data <- input$file1
    locations.data <- input$file2
    if (is.null(population.data)) {
      return(NULL)
    }
    if (is.null(locations.data)) {
      return(NULL)
    }
    population.df <- read.csv(population.data$datapath)
    locations.df <- read.csv(locations.data$datapath)
    num.places <- input$numberPlaces
    num.gens <- input$numberGens
    return(gaps(population.df, locations.df, k = num.places, generations = num.gens))
  })
  
  observeEvent(input$testGaps, {
    number.gens <- input$testNumberGens
    number.polls <- input$testNumberPlaces
    
    #run gaps analysis
    new.centers <- data.to.export()
    output$caseStudy <-renderTable({
      return(new.centers)
    })
    
    #calc average distances
    new.centers.dist <- gaps.inert(data.points, new.centers)
    current.centers.dist <- gaps.inert(data.points, current.centers)
    
    output$testGapsOutput <- renderPlot({
      hist(new.centers.dist, main = "GAPS Algorithm", sub = "Waller County, TX", xlab = "Distance (miles)")
    })
    output$testCurrentOutput <- renderPlot({
      hist(current.centers.dist, main = "Current Method", sub = "Waller County, TX", xlab = "Distance (miles)")
    })
    output$testGapsMean <- renderText({
      distances <- gaps.inert(data.points, new.centers)
      mean.gaps <- mean(distances)
      sd.gaps <- sd(distances)
      return(paste("Mean: ", round(mean.gaps, digits = 3), ", Standard Deviation: ", round(sd.gaps, digits = 3)))
    })
    output$testCurrentMean <- renderText({
      distances <- gaps.inert(data.points, current.centers)
      mean.current <- mean(distances)
      sd.current <- sd(distances)
      return(paste("Mean: ", round(mean.current, digits = 3), ", Standard Deviation: ", round(sd.current, digits = 3)))
    })
  })
  
  observeEvent(input$yourGaps, {
    your.centers <- your.example()
    output$yourStudy <- renderTable({
      return(your.centers)
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("test_polling_places.csv", sep = "")
    },
    content = function(file) {
      write.csv(data.to.export(), file, row.names = FALSE)
    }
  )
  
  output$downloadYourSim <- downloadHandler(
    filename = function() {
      paste("polling_places.csv", sep = "")
    },
    content = function(file) {
      write.csv(your.example(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)