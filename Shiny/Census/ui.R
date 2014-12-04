#setwd("C:/Users/chris_000/Documents/school/Statistical-Graphics/Shiny/App-1")

# ui.R

shinyUI(fluidPage(
  titlePanel("Census Visualization"),
  sidebarLayout(
    sidebarPanel(
      helpText("Census demographic maps with 
               information from the 2010 US 
               Census"),
      
      selectInput("var", 
                  label = "Choose a variale to display",
                  choices = list("Percent White", "Percent Black", 
                                 "Percent Hispanic", "Percent Asian"), 
                  selected = "Percent White"),
      
      sliderInput("range", label = "Range of interest",
                  min=0, max=100, value=c(50,100))
    ),
    mainPanel(
      plotOutput("map")
    )
  )
))