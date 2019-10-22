library(shiny)
library(shinydashboard)
dashboardPage(
  dashboardHeader(title = "Digital Learning Dashboard", titleWidth = 400),
  dashboardSidebar(
    width = 160,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Course Enrollments", tabName = "course-enroll", icon = icon("book")),
      menuItem("Popularity", tabName = "Popularity", icon = icon("bullseye"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
  .skin-blue .main-header .logo {
                              font-family: "Times New Roman";
                              font-weight: bold;
                              font-size: 24px;
                              background-color: #003D76;
                              }
                              .skin-blue .main-header .navbar {
                              background-color: #0082D1;
                              }
                              
                              '))),
    tags$head(tags$style(HTML('
.skin-blue .main-sidebar {
                              font-family: "Times New Roman";
                              font-size:14px;
                              line-height:1.42857143;
                              color:#ebebeb;
                              }
'))),
    tabItems(
      #Overview Tab
      tabItem(tabName = "overview",
              fluidPage(
              #First row
              frow01<-fluidRow(
                #First chart on first row
                #Pie chart of Associate distribution by Location in overview tab
                box(
                  title = "Associate distribution by Location"
                  ,status = "primary"
                  ,solidHeader = TRUE
                  ,collapsible = TRUE
                  ,plotlyOutput("LocationDistribution", height ="400px")
                ),
                box(
                  title = "Associate distribution by Program"
                  ,status = "primary"
                  ,solidHeader = TRUE
                  ,collapsible = TRUE
                  ,plotOutput("Associateprojdist")
                )),
              frow02<-fluidRow(
                box(
                  title = "Depth of Learning",
                  status ="primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("DepthOfLearning")
                ),
                box(
                  title = "Breadth of Learning",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("overallCompletions",height = "400px")
                )))),
              #Course Enrollments tab
              tabItem(tabName = "course-enroll",
                      fluidPage(
                      #First row
                      frow1<-fluidRow(
                        #First chart on first row
                        #Bar chart of Number of associates in each course technology showing completion 
                        #Title - Course In-Progress Vs Completed 
                        box(
                          title = "Course enrollment and completions"
                          ,status = "primary"
                          ,solidHeader = TRUE 
                          ,collapsible = TRUE 
                          ,width = 12
                          ,plotOutput("enrolVsComp", height = "300px")
                          ,selectInput("competencyname", "Competency Name (Select Up to Four)",
                                       choices = as.character(unique(CompetencyHeldEnrolled$`Competency name`)),
                                       selected = as.character("Digital : BigData and Hadoop Ecosystems"),
                                       multiple =TRUE)
                        )),
                      
                      #Second chart on Second Row
                      #Number of associates enrolled in each Course Technology per Project Model with radiobutton inputs to select the project
                      frow2<-fluidRow(  
                        box(
                          title = "Certifications by Program"
                          ,width = 12
                          ,status = "primary"
                          ,solidHeader = TRUE
                          ,collapsible = TRUE
                          ,plotOutput("TechProj")
                          ,radioButtons("radio" 
                                        ,"Program Name" 
                                        ,choices =as.character(unique(competencyNameProject$`Program Name`)),
                                        inline = TRUE)
                        ,radioButtons("radio1"
                                      ,"Held/Enrolled"
                                      ,choices =as.character((unique(competencyNameProject$Held.Enrolled)))
                                      ,inline = TRUE))
                      ),
                      frow3<-fluidRow(
                      #Third chart on third row
                      #Dodge bar chart of Level of Progression per Location Model 
                      box(
                        title = "Level of Progression by Location"
                        ,status = "primary"
                        ,solidHeader = TRUE
                        ,collapsible = TRUE
                        ,width = 12
                        ,plotOutput("LProgCourse", height = "300px")
                        #Dropdown input for location with multiple/all inputs selection enabled
                        ,selectInput("location", "Work Location (Select One or More)",
                                     choices = as.character(unique(LocationHeldEnrolledSubset$`Work Location`)), multiple =TRUE,
                                     selected = as.character(unique(LocationHeldEnrolledSubset$`Work Location`)))
                        #Checkbox input of level of progression with multiple/all input selection enabled
                        ,checkboxGroupInput("levelOfProgression" 
                                            ,"Levels Held/Enrolled (Select One or More)" 
                                            ,choices =as.character(unique(LocationHeldEnrolledSubset$`Held/Enrolled`)),inline = TRUE,
                                            selected = as.character(unique(LocationHeldEnrolledSubset$`Held/Enrolled`))))
              ),
              frow4<-fluidRow(
                tabBox(
                  title = "Enrollment Distribution by Program"
                  , height = "450px",
                  tabPanel("E1", 
                           plotOutput("AssociateLoPE1Enrolledweb", height = "400px")),
                  tabPanel("E2", 
                           plotOutput("AssociateLoPE2Enrolledweb",  height = "400px"))
                 
                ),
                tabBox(
                  title = "Completion Distribution by Program"
                  , height = "450px",
                  tabPanel("E0", 
                           plotOutput("AssociateLoPE0Completedweb", height = "400px")),
                  tabPanel("E1", 
                           plotOutput("AssociateLoPE1Completedweb",  height = "400px")),
                  tabPanel("E2", 
                           plotOutput("AssociateLoPE2Completedweb",  height = "400px")),
                  tabPanel("E3", 
                           plotOutput("AssociateLoPE3Completedweb",  height = "400px"))
                  
                )),
              frow5<-fluidRow(
                #ZeroCompletions by location
                box(
                  title = "Digital Learning Defaulters by Location"
                  ,status = "primary"
                  ,solidHeader = TRUE
                  ,collapsible = TRUE
                  ,plotOutput("zeroCompletionslocationbar",height = "300px")
                ),
                box(
                  #ZeroCompletions by project
                  title = "Digital Learning Defaulters by Program"
                  ,status = "primary"
                  ,solidHeader = TRUE
                  ,collapsible = TRUE
                  ,plotOutput("zeroCompletionsprojectbar",height = "300px")
                ))
              )),
              #Popularity Tab
              tabItem(tabName = "Popularity",
                      fluidPage(
                        #Donut charts listing courses by popularity and by enrollments
                        frow001<-fluidRow(
                          box(
                            title = "Top 5 Courses by enrollments"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE
                            ,width = 12
                            ,plotlyOutput("DonutChart")
                          ),
                          box(
                            title = "Top 5 Courses by Completion"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE
                            ,width =12
                            ,plotlyOutput("DonutChart1")
                          )
                        ),
                        
                        #Second row on Popularity tab
                        #Data viewer listing courses by popularity
                        frow002<-fluidRow(
                          box(
                            title = "Bottom 5 Courses by Completion"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE
                            ,width =12
                            ,plotlyOutput("DonutChart2")
                          ),
                          box(
                            title = "Bottom 5 Courses by Enrollments"
                            ,status = "primary"
                            ,solidHeader = TRUE
                            ,collapsible = TRUE
                            ,width =12
                            ,plotlyOutput("DonutChart3")
                          )
                        )
                      )
      )
    )
  )
)