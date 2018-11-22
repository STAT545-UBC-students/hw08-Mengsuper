library(shiny)
library(ggplot2)
library(dplyr)
library(colourpicker)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      uiOutput("countryOutput")
    ),
    
    mainPanel(
      # BC Liqor Store image
      img(src = "BCLiqourStores.jpg"),
      
      tabsetPanel(
      	# histogram plot 
      	tabPanel("Plot", 
      			 colourInput("color", "Select histogram bar colour", "red"), # add color choices
      			 plotOutput("coolplot")
      	),
      
      	tabPanel("Table",
      		# download button
      		downloadButton('downloadData', 'Download'), 
   			# sort by price
     		checkboxInput("sortPrice", label = "Sort by Price", value = FALSE), 
  			conditionalPanel(
  				condition = "input.sortPrice == true",
  			 	radioButtons("sortPriceOrder", " ", choices = c("From low to high", "From high to low"))
  			),
      	DT::dataTableOutput("results")
      	)
      )
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })

  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }

    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
    
    # sort price
    if (input$sortPrice) {
        if (input$sortPriceOrder == "From low to high") 
        	return (arrange(bcl, Price))
    	else 
    		return (arrange(bcl, desc(Price)))
    } else {
        return (bcl)
    }
  })

  # plot
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram(fill = input$color)
  })

  output$results <- DT::renderDataTable({
    filtered()
  })
  
  output$downloadData <- downloadHandler(
  	filename = function() {
 		paste('data-', Sys.Date(), '.csv', sep='')
  	},
  	content = function(con) {
  		write.csv(bcl, con)
  	}
  )
}

shinyApp(ui = ui, server = server)
