
library(shiny)

ui <- fluidPage(
  column(width = 4,

         numericInput("rwA", "Alpha", NULL, min = -Inf , max = Inf )
  ),


  mainPanel( ## Tab 2 output
    # tableOutput("contents"),

    textOutput("result")

  ) ## mainPanel end --
)

server <- function(input, output) {

  output$result <- renderText( as.character(input$rwA  )  )

}

shinyApp(ui, server)


Fparam[  Fparam$parameter %!in% NewParam$parameter,  ]
