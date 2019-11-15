# Three views on functions of two variables
# (c) 2017, Jan Kroon

library(shiny)

ui1 <- fluidPage(
    titlePanel("EduCode 'Functions of two variables'"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'chosen.function', label = 'Function description: ', choices = c('f(x,y) = x + y', 'f(x,y) = x * y', 'f(x,y) = x^2 + y^2', 'f(x,y) = 100*sin(x + y)/sqrt(x^2 + y^2)')),
        sliderInput(inputId = 'angle', label = '3D view angle: ', min=0, max=360, value=90)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("3D Plot", plotOutput("Three.D.plot")),
          tabPanel("Contour Graph", plotOutput("Contour.graph")),
          tabPanel("Heat Map", plotOutput("Heat.map")),
          tabPanel("Raw data", tableOutput("Raw.data"))
        )
      )
    )
  )

server1 <- function(input, output) {

  test1 <- reactive({"Test 1, 2, 3"})
  test2 <- reactive({test2 <- c("Amsterdam", "Berlijn", "Copenhagen")})
  test3 <- reactive({test3 <- matrix(c(1,1,1,2,2,2,3,3,3), nrow=3, ncol=3)})
  
  z.s <- reactive({
    # Define 2D grid
    x.s <- seq(from=0, to=10, by=0.1)
    y.s <- seq(from=0, to=10, by=0.1)
  
    # Process function selection
    if (input$chosen.function == 'f(x,y) = x + y') {
      f <- function(x,y){return(x+y)}
    }
    else if (input$chosen.function == 'f(x,y) = x * y') {
      f <- function(x,y){return(x*y)}
    }
    else if (input$chosen.function == 'f(x,y) = x^2 + y^2') {
      f <- function(x,y){return(x^2 + y^2)}
    }
    else if (input$chosen.function == 'f(x,y) = 100*sin(x + y)/sqrt(x^2 + y^2)') {
      f <- function(x,y){return(100*sin(x + y)/sqrt(x^2 + y^2))}
    }
  
    # Calculate z values
    z.values = matrix(nrow=length(x.s), ncol=length(y.s))
    z.values[is.na(z.values)] <- 0
    for (i in 1:length(x.s)) {
      for (j in 1:length(y.s)){
        z.values[i,j] <- f(x.s[i], y.s[j])
      }
    }
    z.s <- z.values
  })
  
  # force aspect ratio of 1, i.e. square canvas if axis have same length
  par(pty="s")
  
  output$Three.D.plot <- renderPlot(
    #persp(volcano, theta=input$angle)
    persp(z.s(), theta=input$angle)
  )
  output$Contour.graph <- renderPlot(
    #contour(volcano)
    contour(z.s())
  )
  output$Heat.map <- renderPlot(
    #image(volcano)
    image(z.s())
  )
  output$Raw.data <- renderText(
    #"Test, 1, 2, 3"
    #volcano
    z.s()
  )
    
}

shinyApp(ui = ui1, server = server1)