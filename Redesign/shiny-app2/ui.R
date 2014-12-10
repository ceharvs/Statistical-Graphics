#setwd("C:/Users/ceharvey/Documents/Personal/school/Statistical-Graphics/Redesign")

# ui.R

shinyUI(navbarPage("Organ Transplant",
                   tabPanel("Micromaps",
                            titlePanel("Organ Donation Visualization for 2012"),
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
                                                           choices = list("Total Donors", "Total Recovered" , "Total Transplanted",
                                                                          "Population", "Recovered Per Donor", 
                                                                          "Transplanted Per Donor", "Transplanted Per Recovered", 
                                                                          "Donors Per Population"),
                                                           selected = "Total Recovered"),
                                              conditionalPanel(
                                                condition = "input.vert_type != 'Total Donors' & input.vert_type != 'Population'",
                                                radioButtons("vert_organ", label = h5("Organ"),
                                                             choices = list("All Organs", "Kidney",
                                                                            "Pancreas", "Liver", 
                                                                            "Intestine", "Heart",
                                                                            "Lung"),selected = "All Organs")
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
                                                                      choices = list("Total Donors", "Total Recovered" , "Total Transplanted",
                                                                                     "Population", "Recovered Per Donor", 
                                                                                     "Transplanted Per Donor", "Transplanted Per Recovered", 
                                                                                     "Donors Per Population"),
                                                                      selected = "Total Transplanted")),
                                                
                                                column(2,conditionalPanel(
                                                  condition = "input.horiz_type != 'Total Donors' & input.horiz_type != 'Population'",
                                                  radioButtons("horiz_organ", label = h5("Organ"),
                                                               choices = list("All Organs", "Kidney",
                                                                              "Pancreas", "Liver", 
                                                                              "Intestine", "Heart",
                                                                              "Lung"),selected = "All Organs")
                                                )),
                                                
                                                # _____________ Design the Colors _________________
                                                
                                                column(2, radioButtons("color_gender", label = h5("Gender"),
                                                                       choices = list("All Genders", "Male",
                                                                                      "Female"),selected = "All Genders")),
                                                column(2, radioButtons("color_type", label = h5("Data Type"),
                                                                       choices = list("Total Donors", "Total Recovered" , "Total Transplanted",
                                                                                      "Population", "Recovered Per Donor", 
                                                                                      "Transplanted Per Donor", "Transplanted Per Recovered", 
                                                                                      "Donors Per Population"),
                                                                       selected = "Total Donors")),
                                                
                                                column(2, conditionalPanel(
                                                  condition = "input.color_type != 'Total Donors' & input.color_type != 'Population'",
                                                  radioButtons("color_organ", label = h5("Organ"),
                                                               choices = list("All Organs", "Kidney",
                                                                              "Pancreas", "Liver", 
                                                                              "Intestine", "Heart",
                                                                              "Lung"),selected = "All Organs")
                                                ))
                                              ))
                                     )))
                            ),
                   tabPanel("Line Graphs", 
                            plotOutput("line1"),
                            plotOutput("line2"),
                            plotOutput("line3")), 
                   tabPanel("Regional", 
                            plotOutput("regional"))

))
 
                 
    
