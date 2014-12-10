source("helpers.R")
source("myhelpers.R")
library(maps)
library(mapproj)
library(ggplot2)
library(scales)

library(micromapST) # has state boundary files
df<- read.csv("data/mmdatav2.csv")
source("panelFunctions.r")
source("confidentClass3WayMap.r")

pullRows <- function(df, gender, organ="All", type){
  if (type == "Total Donors" | type == "Population") {
    newdata <- df[ which(df$Gender==gender 
                         & df$Type == type), c(1,5)]
    mydata <- data.frame(newdata[,2],row.names=newdata[,1])
    
  } else if ( type == "Transplanted Per Donor") {
    top <- df[ which(df$Gender==gender 
                         & df$Organ == organ
                         & df$Type == "Total Transplanted"), c(1,5)]
    bottom <- df[ which(df$Gender==gender 
                        & df$Type == "Total Donors"), c(1,5)]
    mydata <- data.frame(top[,2]/bottom[,2],row.names=top[,1])
    
  } else if ( type == "Recovered Per Donor") {
    top <- df[ which(df$Gender==gender 
                     & df$Organ == organ
                     & df$Type == "Total Recovered"), c(1,5)]
    bottom <- df[ which(df$Gender==gender 
                        & df$Type == "Total Donors"), c(1,5)]
    mydata <- data.frame(top[,2]/bottom[,2],row.names=top[,1])
    
  } else if ( type == "Transplanted Per Recovered") {
    top <- df[ which(df$Gender==gender 
                     & df$Organ == organ
                     & df$Type == "Total Transplanted"), c(1,5)]
    bottom <- df[ which(df$Gender==gender 
                        & df$Organ == organ
                        & df$Type == "Total Recovered"), c(1,5)]
    mydata <- data.frame(top[,2]/bottom[,2],row.names=top[,1])
    
  } else if ( type == "Donors Per Population") {
    top <- df[ which(df$Gender==gender 
                     & df$Type == "Total Donors"), c(1,5)]
    bottom <- df[ which(df$Gender==gender 
                        & df$Type == "Population"), c(1,5)]
    mydata <- data.frame(top[,2]/bottom[,2],row.names=top[,1])
  }else {
    newdata <- df[ which(df$Gender==gender 
                         & df$Organ == organ
                         & df$Type == type), c(1,5)]
    mydata <- data.frame(newdata[,2],row.names=newdata[,1])
    
  }
  
  #Convert to 1:3
  mydata[,1] <- cut(mydata[,1], quantile(mydata[,1], probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
  return(mydata)
  
}

data = read.csv("data/Line3.csv")
data$Organ <- factor(data$Organ,levels = c("All","Kidney", "Pancreas", "Lungs", "Liver","Heart","Intestine"))
data$Type <- factor(data$Type)
data$OrganType <- factor(data$OrganType, levels = c("All Recovered","All Transplanted","Kidney Recovered","Kidney Transplanted","Pancreas Recovered","Pancreas Transplanted","Lungs Recovered","Lungs Transplanted","Liver Recovered"       ,"Liver Transplanted","Heart Recovered","Heart Transplanted","Intestine Recovered","Intestine Transplanted"))

shinyServer(
  
  function(input, output) {
    
    
    output$line1 <- renderPlot({
      cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#FF99FF", "#0072B2", "#D55E00", "#CC79A7")
      newdata <- data[ which((data$Organ !='All')), ]
      p <- ggplot(newdata, aes(Year, Value, color=Organ))+
        geom_point(size=4, aes(shape=Type))+
        geom_line(aes(linetype=Type))+
        scale_x_continuous(breaks=seq(1997, 2014, 1))
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
        scale_color_brewer(name="Organs Per Donor",palette="Dark2")+
        #scale_colour_manual(values=cbPalette) +
        scale_shape_manual(values=c(1,16)) +
        scale_linetype_manual(values=c("dashed", "solid"))
      
    })
    
    output$line2 <- renderPlot({
      p <- ggplot(data, aes(Year, Value, color=Organ))+geom_point(size=3)+geom_line()
      p+labs(y = "Number of Organs", x="Year")+facet_grid(Organ ~ Type, scales = "free")+ theme(title = element_text(size=18, face="bold"),strip.text.x = element_text(size=16, face="bold"), strip.text.y = element_text(size=14), axis.title.x = element_text(face="bold", size=16), axis.text.x  = element_text(vjust=0.5, size=14), axis.title.y = element_text(face="bold", size=16), axis.text.y  = element_text(size=16), legend.text = element_text( size=16), legend.title = element_text( size=16))
    })
    
    output$line3 <- renderPlot({
      p <- ggplot(data, aes(Year, Value, color=Type))+geom_point(size=3)+geom_line()
      p+labs(y = "Number of Organs", x="Year")+facet_grid(Organ ~ ., scales = "free")+ theme(title = element_text(size=18, face="bold"),strip.text.x = element_text(size=16, face="bold"), strip.text.y = element_text(size=14), axis.title.x = element_text(face="bold", size=16), axis.text.x  = element_text(vjust=0.5, size=14), axis.title.y = element_text(face="bold", size=16), axis.text.y  = element_text(size=16), legend.text = element_text( size=16), legend.title = element_text( size=16))
    })
    
    output$regional <- renderPlot({
      data = read.csv("data/Data3.csv")
      data$Region <- factor(data$Region, levels = rev(c("Pacific","Midwest", "Southeast", "Rocky Mountains", "Southwest","Northeast")))
      data$Gender <- factor(data$Gender)
      
      data$Organ <- factor(data$Organ)
      data$Type <- factor(data$Type)
      
      newdata <- data[ which((data$Type=='Recovered Per Donor' | data$Type=='Transplanted Per Donor') & (data$Organ =='All') & (data$Region!='Hawaii')& (data$Region!='Puerto Rico')), ]
      p <- ggplot(newdata, aes(Count, Region, color=Gender))+geom_point(size=4)
      p+labs(y = "Region", x="Number Organs Per Donor in 2012")+facet_grid(. ~ Type, scales = "free")+ scale_color_manual(values=c("#009900", "#FF0033", "#0000CC"))+ theme(title = element_text(size=18, face="bold"),strip.text.x = element_text(size=16, face="bold"), strip.text.y = element_text(size=14), axis.title.x = element_text(face="bold", size=16), axis.text.x  = element_text(vjust=0.5, size=14), axis.title.y = element_text(face="bold", size=16), axis.text.y  = element_text(size=16), legend.text = element_text( size=16), legend.title = element_text( size=16))
      
      
    })
    
    output$micro <- renderPlot({
      
  
      # ____________Design Rows_______________________
      if (input$horiz_type == "Donors" | input$horiz_type == "Population") {
        colLab = paste(input$horiz_type," - ", input$horiz_gender)
        newData <- pullRows(df, gender=input$horiz_gender, type=input$horiz_type)
        
      } else {
        colLab=paste(input$horiz_type, " - ", input$horiz_gender,  " - ", input$horiz_organ)
        newData <- pullRows(df, gender=input$horiz_gender, organ=input$horiz_organ, type=input$horiz_type)
        
      }
      
      # ____________Design Columns_______________________
      if (input$vert_type == "Donors" | input$vert_type == "Population") {
        rowLab = paste(input$vert_type, " - ", input$vert_gender)
        newData[,2] <- pullRows(df, gender=input$vert_gender, type=input$vert_type)
        
      } else {
        rowLab=paste(input$vert_type, " - ", input$vert_gender, " - ", input$vert_organ)
        newData[,2] <- pullRows(df, gender=input$vert_gender, organ=input$vert_organ, type=input$vert_type)[,1]
        
      }
      
      
      # ____________Design Color_______________________
      if (input$color_type == "Donors" | input$color_type == "Population") {
        colorLab = paste(input$color_type, " - ", input$color_gender)
        newData[,3] <- pullRows(df, gender=input$color_gender, type=input$color_type)
        
      } else {
        colorLab=paste(input$color_type, " - ", input$color_gender, " - ", input$color_organ)
        newData[,3] <- pullRows(df, gender=input$color_gender, organ=input$color_organ, type=input$color_type)[,1]
      }
      
      
      
      
      #___________Create three way map_______________
      confidentClass3WayMap(newData,refRegion=FALSE,
                            rowVar=1,colVar=2,colorVar=3,
                            rowLab=rowLab,
                            colLab=colLab,
                            colorLab=colorLab,
                            title="Micromap of the Transplant System")
    })
    
  }
)

