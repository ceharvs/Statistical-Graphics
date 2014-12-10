source("helpers.R")
source("myhelpers.R")
library(maps)
library(mapproj)
library(ggplot2)
library(scales)

library(micromapST) # has state boundary files
df<- read.csv("data/mmdata.csv", row.names=1)
source("panelFunctions.r")
source("confidentClass3WayMap.r")

dataLine = read.csv("data/Line3.csv")
dataLine$Organ <- factor(dataLine$Organ,levels = c("All","Kidney", "Pancreas", "Lungs", "Liver","Heart","Intestine"))
dataLine$Type <- factor(dataLine$Type)
dataLine$OrganType <- factor(dataLine$OrganType, levels = c("All Recovered","All Transplanted","Kidney Recovered","Kidney Transplanted","Pancreas Recovered","Pancreas Transplanted","Lungs Recovered","Lungs Transplanted","Liver Recovered"       ,"Liver Transplanted","Heart Recovered","Heart Transplanted","Intestine Recovered","Intestine Transplanted"))

# Format data for Micromaps
df$AllGender_LeadingDeathMechanism <- factor(df$AllGender_LeadingDeathMechanism)
df$AllGenders_Donors <- cut(df$AllGenders_Donors, quantile(df$AllGenders_Donors, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Recovered_All <- cut(df$AllGenders_Recovered_All, quantile(df$AllGenders_Recovered_All, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Recovered_Kidney <- cut(df$AllGenders_Recovered_Kidney, quantile(df$AllGenders_Recovered_Kidney, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Recovered_Pancreas <- cut(df$AllGenders_Recovered_Pancreas, quantile(df$AllGenders_Recovered_Pancreas, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Recovered_Liver <- cut(df$AllGenders_Recovered_Liver, quantile(df$AllGenders_Recovered_Liver, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Recovered_Intestine <- cut(df$AllGenders_Recovered_Intestine, quantile(df$AllGenders_Recovered_Intestine, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Recovered_Heart <- cut(df$AllGenders_Recovered_Heart, quantile(df$AllGenders_Recovered_Heart, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Recovered_Lung <- cut(df$AllGenders_Recovered_Lung, quantile(df$AllGenders_Recovered_Lung, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Transplanted_All <- cut(df$AllGenders_Transplanted_All, quantile(df$AllGenders_Transplanted_All, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Transplanted_Kidney <- cut(df$AllGenders_Transplanted_Kidney, quantile(df$AllGenders_Transplanted_Kidney, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Transplanted_Pancreas <- cut(df$AllGenders_Transplanted_Pancreas, quantile(df$AllGenders_Transplanted_Pancreas, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Transplanted_Liver <- cut(df$AllGenders_Transplanted_Liver, quantile(df$AllGenders_Transplanted_Liver, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Transplanted_Intestine <- cut(df$AllGenders_Transplanted_Intestine, quantile(df$AllGenders_Transplanted_Intestine, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Transplanted_Heart <- cut(df$AllGenders_Transplanted_Heart, quantile(df$AllGenders_Transplanted_Heart, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)
df$AllGenders_Transplanted_Lung <- cut(df$AllGenders_Transplanted_Lung, quantile(df$AllGenders_Transplanted_Lung, probs=seq(0,1,1/3)), labels = 1:3, include.lowest = TRUE)

#Micromaps Layout
# Columns 17-31: All Genders
# Columns 18-25: Recovered
# Columns 26-31: Transplanted
# All- Kidney - Pancreas - Liver - Intestine - Heart - Lung

shinyServer(
  
  function(input, output) {
    
    
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
    
    output$micro <- renderPlot({
      
      
      
      # ____________Design Rows_______________________
      if (input$horiz_type == "Donors") {
        if (input$horiz_gender == "All Genders") {
          colVar = 2
        }
        colLab = paste(input$horiz_type, input$horiz_gender)
      }
      else {
        if (input$horiz_organ == "Kidney") {add = 1}
        else if (input$horiz_organ == "Pancreas") {add = 2}
        else if (input$horiz_organ == "Liver") {add = 3}
        else if (input$horiz_organ == "Intestine") {add = 4}
        else if (input$horiz_organ == "Heart") {add = 5}
        else if (input$horiz_organ == "Lung") {add = 6}
        else {add = 0}
        if (input$horiz_type == "Recovered") {
          if (input$horiz_gender == "All Genders") {
            base = 3
          }
        }
        if (input$horiz_type == "Transplanted") {
          if (input$horiz_gender == "All Genders") {
            base = 10
          }
        }
        colVar = add + base
        colLab=paste(input$horiz_type, input$horiz_gender, input$horiz_organ)
      }
      
      # ____________Design Columns_______________________
      if (input$vert_type == "Donors") {
        if (input$vert_gender == "All Genders") {
          rowVar = 2
        }
        rowLab = paste(input$vert_type, input$vert_gender)
      }
      else {
        if (input$vert_organ == "Kidney") {add = 1}
        else if (input$vert_organ == "Pancreas") {add = 2}
        else if (input$vert_organ == "Liver") {add = 3}
        else if (input$vert_organ == "Intestine") {add = 4}
        else if (input$vert_organ == "Heart") {add = 5}
        else if (input$vert_organ == "Lung") {add = 6}
        else {add = 0}
        if (input$vert_type == "Recovered") {
          if (input$vert_gender == "All Genders") {
            base = 3
          }
        }
        if (input$vert_type == "Transplanted") {
          if (input$vert_gender == "All Genders") {
            base = 10
          }
        }
        rowVar = add + base
        rowLab=paste(input$vert_type, input$vert_gender, input$vert_organ)
      }
      
      # ____________Design Color_______________________
      if (input$color_type == "Donors") {
        if (input$color_gender == "All Genders") {
          colorVar = 2
        }
        colorLab = paste(input$color_type, input$color_gender)
      }
      else {
        if (input$color_organ == "Kidney") {add = 1}
        else if (input$color_organ == "Pancreas") {add = 2}
        else if (input$color_organ == "Liver") {add = 3}
        else if (input$color_organ == "Intestine") {add = 4}
        else if (input$color_organ == "Heart") {add = 5}
        else if (input$color_organ == "Lung") {add = 6}
        else {add = 0}
        if (input$color_type == "Recovered") {
          if (input$color_gender == "All Genders") {
            base = 3
          }
        }
        if (input$color_type == "Transplanted") {
          if (input$color_gender == "All Genders") {
            base = 10
          }
        }
        colorVar = add + base
        colorLab=paste(input$color_type, input$color_gender, input$color_organ)
      }
      
      
      #___________Create three way map_______________
      confidentClass3WayMap(df,refRegion=FALSE,
                            rowVar=rowVar,colVar=colVar,colorVar=colorVar,
                            rowLab=rowLab,
                            colLab=colLab,
                            colorLab=colorLab,
                            title="Micromap of the Transplant System")
    })
    
  }
)