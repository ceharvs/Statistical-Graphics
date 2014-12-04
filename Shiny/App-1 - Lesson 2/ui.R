#setwd("C:/Users/chris_000/Documents/school/Statistical-Graphics/Shiny/App-1")

shinyUI(fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      h2("Installation"),
      p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
      code("install.packages(\"shiny\")")),
    mainPanel(
      h1("Introducing Shiny"),
      p("Shiny is a new package from Rstudio that makes it"), 
      em("incredibly"),
      p("easy to build interactive web applications with R.")
    
  )
  )
))