library(shiny)
library(dplyr)

ui <- fluidPage(
    titlePanel("Plot a function and its derivative"),
    sidebarLayout(
        sidebarPanel(
            textInput(
              inputId = "expr",
              label = "Expression",
              value = "x^2"
            ),
            fluidRow(
              column(6, numericInput(
                inputId = "x",
                label = "X",
                value = 0, 
                step = 0.1
              )),
              column(6, numericInput(
                inputId = "n_pnts",
                label = "N Points",
                value = 200,
                step = 1
              ))
            ),
            fluidRow(
              column(6, numericInput(
                inputId = "x_range_start",
                label = "X range start",
                value = -5,
                step = 1
              )),
              column(6, numericInput(
                inputId = "x_range_end",
                label = "X range end",
                value = 5,
                step = 1
              ))
            ),
            fluidRow(
              column(6, numericInput(
                inputId = "xlim_start",
                label = "X limit lower bound",
                value = -10,
                step = 1
              )),
              column(6, numericInput(
                inputId = "xlim_end",
                label = "X limit upper bound",
                value = 10,
                step = 1
              ))
            ),
            fluidRow(
              column(6, numericInput(
                inputId = "ylim_start",
                label = "Y limit lower bound",
                value = -5,
                step = 1
              )),
              column(6, numericInput(
                inputId = "ylim_end",
                label = "Y limit upper bound",
                value = 50,
                step = 1
              ))
            )
        ),
        mainPanel(
           fluidRow(
             column(6, plotOutput("deriv_plot")),
             column(6, plotOutput("slope_plot"))
           ),
           verbatimTextOutput("deriv_text")
        )
    )
)

server <- function(input, output) {

  d <- reactive({
    deriv(input$expr, input$x)
  })
  
  output$deriv_text <- renderPrint({
      d()
  })
  
  output$deriv_plot <- renderPlot({
    plot_deriv(
      d(), 
      x_range = c(input$x_range_start, input$x_range_end),
      n = input$n_pnts,
      xlim = c(input$xlim_start, input$xlim_end),
      ylim = c(input$ylim_start, input$ylim_end),
      main = paste0("Function: ", input$expr)
    )
  })
  
  output$slope_plot <- renderPlot({
    xs <- seq(input$x_range_start, input$x_range_end, length.out = input$n_pnts)
    
    slopes <- purrr::map(xs, function(x) deriv(input$expr, x)) %>% 
      purrr::map(~ .x[["m"]]) %>% 
      as.numeric()
    
    plot(xs, slopes, main = paste0("Derivative: ", input$expr))
  })
  
}

shinyApp(ui = ui, server = server)
