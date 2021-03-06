#setwd("C:/Users/ceharvey/Documents/Personal/school/Statistical-Graphics/Redesign")

# ui.R

shinyUI(navbarPage("Organ Transplant",
                   tabPanel("Micromaps",
                            titlePanel("Organ Donation Visualization"),
                            helpText("Demographic Information on the recovery and transplantation of organs in the United States."),
                            fluidRow(
                              column(12,"",
                                     fluidRow(
                                       column(2, 
                                              # _____________ Design the Vertical Axis _________________
                                              helpText(h4("Vertical Axis")),
                                              radioButtons("vert_gender", label = h5("Gender"),
                                                           choices = list("All Genders", "Male",
                                                                          "Female"),selected = "All Genders"),
                                              radioButtons("vert_type", label = h5("Data Type"),
                                                           choices = list("Donors", "Recovered" , "Transplanted"),
                                                           selected = "Recovered"),
                                              conditionalPanel(
                                                condition = "input.vert_type != 'Donors'",
                                                radioButtons("vert_organ", label = h5("Organ"),
                                                             choices = list("All", "Kidney",
                                                                            "Pancreas", "Liver", 
                                                                            "Intestine", "Heart",
                                                                            "Lung"),selected = "All")
                                              )
                                       ),
                                       column(10, 
                                              plotOutput("micro"),
                                              fluidRow(
                                                
                                                column(6,helpText(h4("Horizontal Axis"))),
                                                
                                                column(6,helpText(h4("Colors")))),
                                              
                                              fluidRow(
                                                
                                                # _____________ Design the Horizonal Axis _________________                   
                                                
                                                column(2,radioButtons("horiz_gender", label = h5("Gender"),
                                                                      choices = list("All Genders", "Male",
                                                                                     "Female"),selected = "All Genders")),
                                                column(2,radioButtons("horiz_type", label = h5("Data Type"),
                                                                      choices = list("Donors", "Recovered" , "Transplanted"),
                                                                      selected = "Transplanted")),
                                                
                                                column(2,conditionalPanel(
                                                  condition = "input.horiz_type != 'Donors'",
                                                  radioButtons("horiz_organ", label = h5("Organ"),
                                                               choices = list("All", "Kidney",
                                                                              "Pancreas", "Liver", 
                                                                              "Intestine", "Heart",
                                                                              "Lung"),selected = "All")
                                                )),
                                                
                                                # _____________ Design the Colors _________________
                                                
                                                column(2, radioButtons("color_gender", label = h5("Gender"),
                                                                       choices = list("All Genders", "Male",
                                                                                      "Female"),selected = "All Genders")),
                                                column(2, radioButtons("color_type", label = h5("Data Type"),
                                                                       choices = list("Donors", "Recovered" , "Transplanted"),
                                                                       selected = "Donors")),
                                                
                                                column(2, conditionalPanel(
                                                  condition = "input.color_type != 'Donors'",
                                                  radioButtons("color_organ", label = h5("Organ"),
                                                               choices = list("All", "Kidney",
                                                                              "Pancreas", "Liver", 
                                                                              "Intestine", "Heart",
                                                                              "Lung"),selected = "All")
                                                ))
                                              ))
                                     )))
                            ),
                   tabPanel("Line Graph", plotOutput("line"))

))
 
                 
    
