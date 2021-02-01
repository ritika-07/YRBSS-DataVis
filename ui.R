library(shinydashboard)
library(shiny)

shinyUI(dashboardPage(
    dashboardHeader(title = "YRBSS Data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Research Summary", tabName = "rs", icon = icon("book-reader")),
            menuItem("Survey Demographics", tabName = "demo", icon = icon("users")),
            menuItem("Youth Violence", tabName = "vio", icon = icon("ambulance")),
            menuItem("Youth Bullying", tabName = "bully" , icon = icon("sad-tear")),
            menuItem("Youth Drug Use", tabName = "drug", icon = icon("pills")),
            menuItem("Variable Codebook", tabName =  "var", icon = icon("book"))
        )),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "rs",
                    fluidRow(
                        valueBoxOutput("PerSchool", width = 4),
                        valueBoxOutput("PerSubmit",width = 4),
                        valueBoxOutput("OverallPer",width = 4)
                    ),
                    fluidRow(
                        box(width = 12,
                            
                            tags$h1(strong("Youth Risk Behavior Survey")),
                            tags$h3(strong('Data Visualisation J-Component')),
                            tags$h3(strong('Group Members: Aditi Tarigoppula 18BCE0728, Ritika Kayal- 18BCE2518')),
                            tags$h2(strong('Abstract'),
                                    tags$h4(
                                        p('In modern times awareness on mental health issues have become increasingly common. 1 out of 10 children are affected by mental health problems like depression, anxiety, sedentary lifestyle and so on. Emotional wellbeing of children is as important as their physical health. Good mental health allows youth to inculcate resilience that helps them cope with life and promotes all round development by increasing the standard of life. The Youth Risk Behaviour Surveillance System can be used to monitor five categories of health behaviours that are considered to be a priority among the youth. The categories are as follows:'),
                                        p('1. Behaviours that contribute to unintentional injuries and violence'),
                                        p('2. Tobacco use'),
                                        p('3. Alcohol and other drug use'),
                                        p('4. Unhealthy dietary behaviour'),
                                        p('5. Physical inactivity'))),
                            tags$h2(strong('Objectives'),
                                    tags$h4(
                                        p('The main objective of this project is to analyse the statistics collected on the given categories so we can predict living patterns among the youth to determine risk factors and help them overcome it. The mental health problems that commonly occur are depression, self-harm, anxiety disorders, post traumatic stress disorders and eating disorders. We aim to use the information from the dataset to make prediction like:'),
                                        p('- Analysis of response rate between attributes like race, age, height and weight'),
                                        p('- Deduce the trend of students carrying weapons with respect to attributes such as age and ethnicity'),
                                        p('- Visualise the trend between bullying and cyber bullying'))),
                            tags$h2(strong('Data Sample Collection'),
                                    tags$h4(
                                        p('The YRBS data is gathered using a two-stage cluster sample design. In the first stage, schools are selected based on enrollment size. It is directly proportional. In the second stage of sampling, a random classroom is selected from each chosen school. All students in the selected classrooms are eligible to participate.
                                          Data can be collected in one of two ways: paper/pencil or electronically. Paper and pencil is the most common method that has been used from the past. These are then scanned onto computers and verified using Teleform software.'))),
                            tags$h2(strong('Use of Shiny App for Data Visualisation'),
                                    tags$h4(
                                        p('Shiny app enables us to visualise iteractive idioms of the YRBS that relate to violence, bullying, drug use, and 
                                          suicide. It also provides analysis of trends in youth risk behaviour throughout the sample youth population')))))),
        tabItem(
            tabName = "demo",
            fluidRow(
                valueBoxOutput("students", width = 3),
                valueBoxOutput("girls",width = 2),
                valueBoxOutput("boys",width = 2),
                valueBoxOutput("mgrade",width = 3),
                valueBoxOutput("mage",width = 2)
            ),
                    fluidRow(
                        box(plotlyOutput("gen_race"),width = 6, title = HTML("Survey Race")),
                        box(plotlyOutput("gen_age"),width = 6, title = HTML("Survey Age")),
                        box(plotlyOutput("kde_h"), width = 6,title = HTML("Survey Height")),
                        box(plotlyOutput("kde_w"),width = 6,title = HTML("Survey Weight"))
                        )),
            tabItem(
                tabName = "vio",
                    fluidRow(valueBoxOutput("All", width = 3),
                             valueBoxOutput("Gun", width = 3),
                             valueBoxOutput("To_sch", width = 3),
                             valueBoxOutput("Injured", width = 3)),
                
                    fluidRow(
                        box(plotlyOutput("weapons_all"), width = 9, title = HTML("Carried any Weapon in the Past 30 Days"),
                            selectInput(inputId = "var_select10",label = "Select a Variable:",
                                        choices = fill_options)),
                        
                        box(plotlyOutput("weapons_guns"), width = 9, title = HTML("Carried a Gun in the Past 12 Months"),
                            selectInput(inputId = "var_select11",label = "Select a Variable:",
                                        choices = fill_options))),
                        
                    fluidRow(
                        box(plotlyOutput("weapon_sch"), width = 9, title = HTML("Carried any Weapon to School in the Past 30 Days"),
                            selectInput(inputId = "var_select12",label = "Select a Variable:",
                                        choices = fill_options)), 
                        
                        box(plotlyOutput("weapons_inj"), width = 9, title = HTML("Injured with any Weapon on School Property"),
                            selectInput(inputId = "var_select13",label = "Select a Variable:",
                                        choices = fill_options)))
                        ),
            
            

            tabItem(
                tabName =  "bully",
                    fluidRow(
                             valueBoxOutput("Electronic", width = 3),
                             valueBoxOutput("Elec_resp", width = 3),
                             valueBoxOutput("School", width = 3),
                             valueBoxOutput("School_resp", width = 3)
                             ),
                
                    fluidRow(

                        box(plotlyOutput("bully_elec"), width = 6, title = HTML("Been Electronically Bullied in the Past 12 months"),
                            selectInput(inputId = "var_select5",label = "Select a Variable:",
                                        choices = fill_options)), 
                            
                        box(plotlyOutput("bully_sch"), width = 6, title = HTML("Been Bullied on School Property in the Past 12 months"),
                            selectInput(inputId = "var_select6",label = "Select a Variable:",
                                        choices = fill_options)
                            ))),
                    


            tabItem(
                tabName = "drug",
                    fluidRow(
                        valueBoxOutput("heroin", width = 2),
                        valueBoxOutput("Inhalants", width = 2),
                        valueBoxOutput("Methamphetamines", width = 3),
                        valueBoxOutput("Opioids", width = 2),
                        valueBoxOutput("MDMA", width = 3)),
                    
                    fluidRow(
                        box(plotlyOutput("gen_drug_use"),width = 12,title = paste("Drug Used by Gender"))
                        ),
                    fluidRow(
                        box(plotOutput("lf_use"),width = 12,title = paste("Times Each Drug Used"))
                        ),
                    fluidRow(
                        box(plotOutput("drug_gen"),width = 6,title = paste("Overall Use by Gender")),
                        box(plotOutput("drug_grade"),width = 6,title = paste("Overall Use by Grade "))
                        ),
                    fluidRow(
                        box(plotOutput("drug_age"),width = 6,title = paste("Overall Use by Age")),
                        box(plotOutput("drug_eth"),width = 6,title = paste("Overall Use by Ethnicity"))
                        ),
                    fluidRow(
                        box(plotOutput("drug_wt"),width = 12,title = paste("OVerall Use by Weight")))
                ),
        tabItem(
            tabName = "var",
            fluidRow(
                box(DT::dataTableOutput("mytable"), width = 12, title = HTML("YRBSS Codebook"))),
            fluidRow(
                box(plotlyOutput("all_var"), width = 12, title = HTML("Choose any two Variables"),
                    selectInput(inputId = "var_select19",label = "Select a Variable:",
                                choices = fill_options, selected = "raceeth"),
                    selectInput(inputId = "var_select20", label = "Select a Variable:",
                                choices = fill_options, selected = "Age")))
            )
        )
     )
   )
)


