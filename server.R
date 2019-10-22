library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fmsb)
library(xlsx)
library(plotly)


#Start of Server side code
shinyServer(
  function(input, output, session) {
    
    #Pie chart of Associate distribution by Location in overview tab
        output$LocationDistribution<-renderPlotly({
                   plot_ly(WorkLocation, labels = ~Location, values = ~Number
                       ,type = 'pie',
                       textposition = 'outside',
                       textinfo = 'label+value+percent',
                       hoverinfo = 'label+value+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       marker = list(
                         line = list(color = '#FFFFFF', width = 1)),
                       #The 'pull' attribute can also be used to create space between the sectors
                       showlegend = FALSE) %>%
            layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   annotations = list())
   })
        
        #PieChart of Number of Overall Completions by Associates
        output$overallCompletions<-renderPlotly({
                   plot_ly(OverallComp, labels = ~`Number of Completions`, values = ~`Number of Associates`
                       ,type = 'pie',
                       textposition = 'inside',
                       text = 'completions',
                       textinfo = 'label+text+value+percent',
                      hoverinfo = 'label+text+value+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       marker = list(
                       line = list(color = '#FFFFFF', width = 1)),
                       #The 'pull' attribute can also be used to create space between the sectors
                       showlegend = FALSE) %>%
            layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   annotations = list())
          
        })
    
    #Dodge bar chart of Level of Progression per Location Model with dropdown and checkox input
   
    data <- reactive({ # reactive part = this code is repeated when user input changes
      
      validate( 
        # define error messages if user doesn't choose anything
        need(input$levelOfProgression != "", 
             "Please select at least one level of progression"),
        need(input$location != "", 
             "Please select at least one Location")
      )
      
      # filter data according to user input
      plotdata <- LocationHeldEnrolledSubset %>%
        as.data.frame() %>%
        filter(
          `Held/Enrolled` %in% input$levelOfProgression 
          &`Work Location` %in% input$location)
      scalecalc <- plotdata %>%
        group_by(`Competency name`) %>%
        summarize(value = sum(Number))
      
      
      scalemax <- max(scalecalc$value)
      scalesteps <- round(scalemax/5, digits = -1)
      
      list(plotdata = plotdata,
           scalemax = scalemax,
           scalesteps = scalesteps
      )
    })
    
    #rendering the dodge bar chart plot
    output$LProgCourse <- renderPlot({
      g <- ggplot(data = data()$plotdata, 
                  aes(`Work Location`, y = Number, 
                      fill = factor(`Held/Enrolled`), 
                      order = `Held/Enrolled`)) + 
        geom_bar(stat = "identity",position="dodge") + 
        
        xlab("Work Location") + 
        ylab("Certifications") +
        theme_minimal()+
        guides(fill=guide_legend(title="Level Of Progression", 
                                 reverse = T))
      print(g)              
    })
    
    #Bar chart of Number of associates in each course technology showing completion 
    #First chart in first row of Course Enrollments tab
    
    # reactive part = this code is repeated when user input changes
        dataforcompvenroll <- reactive({
      
      # define error messages if user doesn't choose anything
      validate( 
        need(input$competencyname != "", 
             "Please select at least one competency name")
              )
      
      # filter data according to user input
      plotdata <- coursenrolledvsccompleted %>%
        as.data.frame() %>%
        filter(
          `Competency name` %in% input$competencyname 
          )
      scalecalc <- plotdata %>%
        group_by(`Held/Enrolled`) %>%
        summarize(value = sum(Number))
      
      
      scalemax <- max(scalecalc$value)
      scalesteps <- round(scalemax/5, digits = -1)
      
      list(plotdata = plotdata,
           scalemax = scalemax,
           scalesteps = scalesteps
      )
    })
    
    
    output$enrolVsComp <- renderPlot({
      ggplot(data = dataforcompvenroll()$plotdata, 
             aes(x=`Competency name`, y=as.factor(Number),
                 fill=factor(`Held/Enrolled`,
                 levels = c("E0 Completed","E1 Completed","E1 Enrolled","E2 Completed","E2 Enrolled",
                            "E3 Completed")))) + 
        geom_bar(position = "dodge", stat = "identity") + ylab("Number of Enrollments/Completions") + 
        xlab("Course") + theme(legend.position="bottom" 
                                   ,plot.title = element_text(size=15, face="bold")) + 
        labs(fill = "Status")
    })
    
    #Number of certifications in each Competency per Project Model with radiobutton inputs to select the project
    dataforTechProj <- reactive({
      competencyNameProject<-competencyNameProject[!(competencyNameProject$`Number`==0),]
      # filter data according to user input
      plotdata <- competencyNameProject %>%
        as.data.frame() %>%
        filter(
          `Program Name` %in% input$radio &
          Held.Enrolled %in% input$radio1
        )
      scalecalc <- plotdata %>%
        group_by(`Program Name`) %>%
        summarize(value = sum(Number))
      
      
      scalemax <- max(scalecalc$value)
      scalesteps <- round(scalemax/5, digits = -1)
      
      list(plotdata = plotdata,
           scalemax = scalemax,
           scalesteps = scalesteps
      )
    })
    
    output$TechProj<-renderPlot({
      integer_breaks <- function(x)
        seq(floor(min(x)), ceiling(max(x)))
     ggplot(data=dataforTechProj()$plotdata, 
             aes(x=factor(`Competency name`), y=Number, 
                 fill = factor(`Competency name`))) +
        geom_bar(stat = "identity") + xlab("Course name") +
          ylab("Number of Enrollments/Completions") +
        theme(legend.position="bottom",axis.ticks = element_blank()
              ,axis.text.x = element_blank(),
              plot.title = element_text(size=15, face="bold")) + 
        labs(fill = "Competency")+guides(fill=guide_legend(nrow=5,byrow=TRUE))+
        scale_y_continuous(breaks=integer_breaks) +
        scale_x_discrete(drop = FALSE)
    })
    
 #Webchart of number of associates in each level of progression by project   
    output$AssociateLoPE1Enrolledweb<-renderPlot({
      radarchart( as.data.frame(levelOfe1ProgressionByProgram)  , axistype=3 , 
                             #custom polygon
                             pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=NA , plwd=4 , 
                            
                             #custom the grid
                             cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
                               
                              #custom labels
                               vlcex=0.8 
                   )
    })
    output$AssociateLoPE2Enrolledweb<-renderPlot({
      radarchart( as.data.frame(levelOfe2ProgressionByProgram)  , axistype=3 , 
                  #custom polygon
                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=NA , plwd=4 , 
                  
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
                  
                  #custom labels
                  vlcex=0.8 
      )
    })
    output$AssociateLoPE0Completedweb<-renderPlot({
      radarchart( as.data.frame(levelOfe0complProgressionByProgram)  , axistype=3 , 
                  #custom polygon
                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=NA , plwd=4 , 
                  
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
                  
                  #custom labels
                  vlcex=0.8 
      )
    })
    
    output$AssociateLoPE1Completedweb<-renderPlot({
      radarchart( as.data.frame(levelOfe1complProgressionByProgram)  , axistype=3 , 
                   #custom polygon
                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=NA , plwd=4 , 
                  
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
                  
                  #custom labels
                  vlcex=0.8 
      )
    })
    output$AssociateLoPE2Completedweb<-renderPlot({
        radarchart( as.data.frame(levelOfe2ComplProgressionByProgram)  , axistype=3 , 
                  #custom polygon
                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=NA , plwd=4 , 
                  
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
                  
                  #custom labels
                  vlcex=0.8 
      )
    })
    output$AssociateLoPE3Completedweb<-renderPlot({
      radarchart( as.data.frame(levelOfe3ComplProgressionByProgram)  , axistype=3 , 
                  #custom polygon
                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=NA , plwd=4 , 
                  
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
                  
                  #custom labels
                  vlcex=0.8 
      )
    })
    
    # bar chart of associate distribution in projects
    output$Associateprojdist<-renderPlot({
      ggplot(data=prognameassociatedist, 
                                    aes(x=factor(prognameassociatedist$Category), 
                                        y=as.factor(prognameassociatedist$x), 
                                        fill = prognameassociatedist$Category)) +
        geom_bar(stat = "identity") + xlab("Program") +
        ylab("Associate Count")+ guides(fill = FALSE)
      
    })
    
    #Bar chart of Number of Associates with Zero Completions and project
    output$zeroCompletionsprojectbar<-renderPlot({
      ggplot(data = finalzerocompproj,
                              aes(x=factor(finalzerocompproj$`Program Name`), 
                                 y=factor(finalzerocompproj$`Number of Associates`), 
                                 fill =`Program Name`)) +
        geom_bar(stat = "identity") + xlab("Program") +
        ylab("Associate Count") + guides(fill = FALSE)
    })
    #Bar chart of Number of Associates with Zero Completions and location
    output$zeroCompletionslocationbar<-renderPlot({
      finalzerocomploc<-finalzerocomploc[!(finalzerocomploc$`Number of Associates`==0),]
      ggplot(data = finalzerocomploc,
                              aes(x=factor(finalzerocomploc$`Work Location`), 
                                  y=factor(finalzerocomploc$`Number of Associates`), 
                                  fill =`Work Location`)) +
        geom_bar(stat = "identity") + xlab("Work Location") +
        ylab("Associate Count") + guides(fill = FALSE)
    })
    
    #Donut chart of popularity of courses by enrollments
    output$DonutChart<-renderPlotly({
        plot_ly(topfiveenrolledcompetencies, labels = ~Course, values = ~Number,
                textposition = 'outside',
                textinfo = 'label+value+percent',
                hoverinfo = 'label+value+percent',
                insidetextfont = list(color = '#FFFFFF'),
                marker = list(
                  line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE) %>%
        add_pie(hole = 0.5)%>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               annotations = list())
        
    })
    
    #Donut chart of popularity of courses by completions
    output$DonutChart1<-renderPlotly({
      plot_ly(topfivecompletedcompetencies, labels = ~Course, values = ~Number,
              textposition = 'outside',
              textinfo = 'label+value+percent',
              hoverinfo = 'label+value+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(
                line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE) %>%
        add_pie(hole = 0.5)%>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               annotations = list())
        
    
    })
    
    #Donut chart of bottom 5 courses by completions
    output$DonutChart2<-renderPlotly({
      validate(
        need(sum(bottomfivecompletedcompetencies$Number)!= 0, "The bottom five courses all have zero completions")
      )
      plot_ly(bottomfivecompletedcompetencies, labels = ~Course, values = ~Number,
              textposition = 'outside',
              textinfo = 'label+value+percent',
              hoverinfo = 'label+value+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(
                line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE) %>%
        add_pie(hole = 0.5)%>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               annotations = list())
      
      
    })
    
    #Donut chart of popularity of courses by completions
    output$DonutChart3<-renderPlotly({
      validate(
        need(sum(bottomfiveenrolledcompetencies$Number)!= 0, "The bottom five courses all have zero enrollments")
      )
      plot_ly(bottomfiveenrolledcompetencies, labels = ~Course, values = ~Number,
              textposition = 'outside',
              textinfo = 'label+value+percent',
              hoverinfo = 'label+value+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(
                line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE) %>%
        add_pie(hole = 0.5)%>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               annotations = list())
      
      
    })
    output$DepthOfLearning<-renderPlot({
      depthofLearning<-depthofLearning[!(depthofLearning$depthNumber==0),]
      ggplot(data=depthofLearning, 
             aes(x=factor(depthofLearning$depthgroupings), 
                 y=as.factor(depthofLearning$depthNumber), 
                 fill = depthofLearning$depthgroupings)) +
        geom_bar(stat = "identity") + ylab("Number of Completions/Associates")+
        xlab("Depth")+
        theme(legend.position="bottom",axis.ticks = element_blank()
              ,axis.text.x = element_blank(),
              plot.title = element_text(size=15, face="bold"))  + 
        labs(fill = "Status")+guides(fill=guide_legend(nrow=5,byrow=TRUE))
    })
  })


