source("helpers.R")
source("myhelpers.R")
library(maps)
library(mapproj)
library(ggplot2)
library(scales)

dataLine = read.csv("data/Line3.csv")
dataLine$Organ <- factor(dataLine$Organ,levels = c("All","Kidney", "Pancreas", "Lungs", "Liver","Heart","Intestine"))
dataLine$Type <- factor(dataLine$Type)
dataLine$OrganType <- factor(dataLine$OrganType, levels = c("All Recovered","All Transplanted","Kidney Recovered","Kidney Transplanted","Pancreas Recovered","Pancreas Transplanted","Lungs Recovered","Lungs Transplanted","Liver Recovered"       ,"Liver Transplanted","Heart Recovered","Heart Transplanted","Intestine Recovered","Intestine Transplanted"))
counties <- readRDS("data/counties.rds")
state = read.csv("data/Data.csv")
state$State <- factor(state$State, levels = rev(levels(state$State)))
state$Gender <- factor(state$Gender)
state$Region <- factor(state$Region)
state$LeadingDeathMechanism <- factor(state$LeadingDeathMechanism)

shinyServer(
  
  function(input, output) {
    
    output$map <- renderPlot({
      data <- switch(input$var, 
                     "Percent White" = counties$white, 
                     "Percent Black" = counties$black,
                     "Percent Hispanic" = counties$hispanic,
                     "Percent Asian" = counties$asian)
      percent_map(var = data, color = "darkgreen", legend.title = input$var, max = input$range[2], min=input$range[1])
    })
    
    output$mymap <- renderPlot({
      data <- state$AllRecovered
      my_percent_map(var = data, color = "darkblue", legend.title = input$var, max = input$range[2], min=input$range[1])
    })
    
    output$line <- renderPlot({
      newdata <- dataLine[ which((dataLine$Organ !='All')), ]
      p <- ggplot(newdata, aes(Year, Value, color=OrganType, shape=Type))+geom_point(size=3)+geom_line()+scale_x_continuous(breaks=seq(1997, 2014, 1))
      p+labs(y = "Number of Organs", x="Year")+ 
        theme(title = element_text(size=18, face="bold"),
              strip.text.x = element_text(size=16, face="bold"), 
              strip.text.y = element_text(size=14), 
              axis.title.x = element_text(face="bold", size=16), 
              axis.text.x  = element_text(vjust=0.5, size=14, angle=45), 
              axis.title.y = element_text(face="bold", size=16), 
              axis.text.y  = element_text(size=16), 
              legend.text = element_text( size=16), 
              legend.title = element_text( size=16)) + 
        scale_color_brewer(name="Organs Per Donor",palette="Paired")
    })
    
  }
)