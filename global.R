#Code to check if required packages are installed,
#if one or more packages are not installed, they are automatically installed
list.of.packages <- c("ggplot2", "shiny","shinydashboard","dplyr",
                      "tidyr","plotly","fmsb","xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(fmsb)
library(xlsx) 

#sd stands for sample data
#Loading the sample data set as a Dataframe
filename <- file.path("data", "Traning updates template.xlsx")
if (file.exists(filename)) {
  sd0<- read.xlsx(filename, 1, stringsAsFactors = TRUE)
}
filename <- file.path("data", "Headcount template.xlsx")
if (file.exists(filename)) {
  sd1 <- read.xlsx(filename, 3, stringsAsFactors = TRUE)
}
filename <- file.path("data", "Headcount template.xlsx")
if (file.exists(filename)) {
 sd2 <- read.xlsx(filename, 4, stringsAsFactors = TRUE)
}
filename <- file.path("data", "ProgProj.xlsx")
if (file.exists(filename)) {
  sd4 <- read.xlsx2(filename, 1, stringsAsFactors = TRUE)
}

#Renaming the columnnames of unallocated dataframe
names(sd2)[names(sd2)=="Employee.Number"] <- "ID"
names(sd2)[names(sd2)=="Last.Project.Number"] <- "Project.."
names(sd2)[names(sd2)=="Child.IOU.Name"] <- "Parent.IOU.Name"
sd2$Person.Type<-NA
sd2$Work.Location<-NA
sd3<-rbind(sd1,sd2)

#Preprocessiong and subsetting data and building dataframes 

#Subsetting the headcount dataframe to include only IS Retail and Person Type Employee/Trainee
ISRetailParentSubset <- subset(sd3, Parent.IOU.Name=='IS-Retail -Parent')
EmployeeSubsetHeadcountIsRetail<-subset(ISRetailParentSubset,Person.Type!='Business Associate')
colnames(EmployeeSubsetHeadcountIsRetail)<-c("Work Location","Project #","Project Name"
                                             ,"ID","Employee Name","Person Type","Parent IOU Name")

#Creating a dataframe of number of associates enrolled in each course and ranking it in the order of popularity by enrollments
sd0.subEnroll<-sd0[which(sd0$Held.Enrolled=='E1 Enrolled'|sd0$Held.Enrolled=='E2 Enrolled'),]
tablepopularity<-(table(sd0.subEnroll$Competency.name))
dataFrameCompetencyName<-as.data.frame(tablepopularity)
colnames(dataFrameCompetencyName)<-c("Course","Number")
#most popular to least popular courses
mostpopulardataFrameCompetencyName<-dataFrameCompetencyName[order(-dataFrameCompetencyName$Number),]
#Least popular to most popular course
topfiveenrolledcompetencies<-head(mostpopulardataFrameCompetencyName,n=5)
leastpopulardataFrameCompetencyName<-dataFrameCompetencyName[order(dataFrameCompetencyName$Number),]
leastpopulardataFrameCompetencyName<-leastpopulardataFrameCompetencyName[!(leastpopulardataFrameCompetencyName$`Number`==0),]

bottomfiveenrolledcompetencies<-head(leastpopulardataFrameCompetencyName,n=5)

#Creating a dataframe of number of associates who have Completed each course and ranking the courses by order of popularity
sd0.subComp <- sd0[which(sd0$Held.Enrolled=='E0 Completed'|
                         sd0$Held.Enrolled=='E3 Completed'|
                         sd0$Held.Enrolled=='E1 Completed'|
                         sd0$Held.Enrolled=='E2 Completed'),]
tablecompletionpopularity<-(table(sd0.subComp$Competency.name))
completeddataFrameCompetencyName<-as.data.frame(tablecompletionpopularity)
colnames(completeddataFrameCompetencyName)<-c("Course","Number")
#Most to least popular courses by completion
completedmostpopulardataFrameCompetencyName<-completeddataFrameCompetencyName[order(-completeddataFrameCompetencyName$Number),]
topfivecompletedcompetencies<-head(completedmostpopulardataFrameCompetencyName,n=5)
#least to most popular courses by completion
completedleastpopulardataFrameCompetencyName<-completeddataFrameCompetencyName[order(completeddataFrameCompetencyName$Number),] 
completedleastpopulardataFrameCompetencyName<-completedleastpopulardataFrameCompetencyName[!(completedleastpopulardataFrameCompetencyName$`Number`==0),]

bottomfivecompletedcompetencies<-head(completedleastpopulardataFrameCompetencyName,n=5)

#Create a dataframe of Number of Certifications of Competencies dataframe
tableofcertifications<-(table(sd0$Competency.name))
#Course-Technology Dataframe
competencyName<-as.data.frame(tableofcertifications) 
colnames(competencyName)<-c("Competency name","Total")

#Creating a dataframe of Number of Associates in each Location
tableofWorkLocation<-(table(EmployeeSubsetHeadcountIsRetail$`Work Location`))
WorkLocation<-as.data.frame(tableofWorkLocation)
colnames(WorkLocation)<-c("Location","Number")


#Creating a dataframe of Number of overall Competions of associates
tableofoverallcomp<-(table(as.factor(sd0$Overall.Completions)))
OverallComp<-as.data.frame(tableofoverallcomp)
colnames(OverallComp)<-c("Number of Completions","Number of Associates")

#Creating a Dataframe of number of associates who completed the course and those who are yet to complete
tableofCompletedVEnrolled<-(table(sd0$Competency.name, sd0$Held.Enrolled))
#Course enrollment vs completion dataframe
coursenrolledvsccompleted<-as.data.frame(tableofCompletedVEnrolled)
colnames(coursenrolledvsccompleted)<-c("Competency name","Held/Enrolled","Number")
coursenrolledvsccompleted$Number<-as.numeric(coursenrolledvsccompleted$Number)

#Creating a table of Number of Certifications in each program with E1 enrolled
e1enrolled.sub<-subset(sd0,Held.Enrolled=="E1 Enrolled")
e1enrolled.sub$`Program Name`<-sd4[match(e1enrolled.sub$Project.Name, sd4$Project.Name),2]
tableNumCerte1Enrolled<-table(as.factor(e1enrolled.sub$`Program Name`))
levelOfe1ProgressionByProgram<-t(tableNumCerte1Enrolled) 
Me1<-max(levelOfe1ProgressionByProgram)
me1<-min(levelOfe1ProgressionByProgram)
levelOfe1ProgressionByProgram=rbind(Me1,me1,levelOfe1ProgressionByProgram)

#Creating a table of Number of Certifications in each project with E2 enrolled
e2enrolled.sub<-subset(sd0,Held.Enrolled=="E2 Enrolled")
e2enrolled.sub$`Program Name`<-sd4[match(e2enrolled.sub$Project.Name, sd4$Project.Name),2]
tableNumCerte2Enrolled<-table(as.factor(e2enrolled.sub$`Program Name`))
levelOfe2ProgressionByProgram<-t(tableNumCerte2Enrolled) 
Me2<-max(levelOfe2ProgressionByProgram)
me2<-min(levelOfe2ProgressionByProgram)
levelOfe2ProgressionByProgram=rbind(Me2,me2,levelOfe2ProgressionByProgram)

#Creating a table of Number of Certifications in each program with E0 completed
e0Completed.sub<-subset(sd0,Held.Enrolled=="E0 Completed")
e0Completed.sub$`Program Name`<-sd4[match(e0Completed.sub$Project.Name, sd4$Project.Name),2]
tableNumCerte0Completed<-table(as.factor(e0Completed.sub$`Program Name`))
levelOfe0ComplProgressionByProgram<-t(tableNumCerte0Completed) 
Mc0<-max(levelOfe0ComplProgressionByProgram)
mc0<-min(levelOfe0ComplProgressionByProgram)
levelOfe0complProgressionByProgram=rbind(Mc0,mc0,levelOfe0ComplProgressionByProgram)

#Creating a table of Number of Certifications in each program with E1 completed
e1Completed.sub<-subset(sd0,Held.Enrolled=="E1 Completed")
e1Completed.sub$`Program Name`<-sd4[match(e1Completed.sub$Project.Name, sd4$Project.Name),2]
tableNumCerte1Completed<-table(as.factor(e1Completed.sub$`Program Name`))
levelOfe1ComplProgressionByProgram<-t(tableNumCerte1Completed) 
Mc1<-max(levelOfe1ComplProgressionByProgram)
mc1<-min(levelOfe1ComplProgressionByProgram)
levelOfe1complProgressionByProgram=rbind(Mc1,mc1,levelOfe1ComplProgressionByProgram)

#Creating a table of Number of Certifications in each program with E2 completed
e2Completed.sub<-subset(sd0,Held.Enrolled=="E2 Completed")
e2Completed.sub$`Program Name`<-sd4[match(e2Completed.sub$Project.Name, sd4$Project.Name),2]
tableNumCerte2Completed<-table(as.factor(e2Completed.sub$`Program Name`))
levelOfe2ComplProgressionByProgram<-t(tableNumCerte2Completed) 
Mc2<-max(levelOfe2ComplProgressionByProgram)
mc2<-min(levelOfe2ComplProgressionByProgram)
levelOfe2ComplProgressionByProgram=rbind(Mc2,mc2,levelOfe2ComplProgressionByProgram)

#Creating a table of Number of Certifications in each program with E3 completed
e3Completed.sub<-subset(sd0,Held.Enrolled=="E3 Completed")
e3Completed.sub$`Program Name`<-sd4[match(e3Completed.sub$Project.Name, sd4$Project.Name),2]
tableNumCerte3Completed<-table(as.factor(e3Completed.sub$`Program Name`))
levelOfe3ComplProgressionByProgram<-t(tableNumCerte3Completed) 
Mc3<-max(levelOfe3ComplProgressionByProgram)
mc3<-min(levelOfe3ComplProgressionByProgram)
levelOfe3ComplProgressionByProgram=rbind(Mc3,mc3,levelOfe3ComplProgressionByProgram)


#Creating a Dataframe of Number of Associates in each project enrolled/completed in each course
tableNumAssociatesperProject<-(table(sd0$Project.Name,sd0$Competency.name,sd0$Held.Enrolled))
competencyNameProject<-as.data.frame(tableNumAssociatesperProject) 
colnames(competencyNameProject)<-c("Project Name","Competency name","Held.Enrolled","Number")
competencyNameProject$Held.Enrolled<-substr(competencyNameProject$Held.Enrolled,4,13)
competencyNameProject$`Program Name`<-sd4[match(competencyNameProject$`Project Name`, sd4$Project.Name),2]

#Creating a Dataframe of Number of Associates at each level of progression in each course by enrollments
tableNumAssociatesperLevelofProgression<-(table(sd0$Held.Enrolled,sd0$Competency.name))
#Level of Course Progression and Technology dataframe
CompetencyHeldEnrolled<-as.data.frame(tableNumAssociatesperLevelofProgression) 
colnames(CompetencyHeldEnrolled)<-c("Held/Enrolled","Competency name","Number")

#Creating a dataframe of Number of associates who completed each course according to level of progression and location
tablenumassociatespercompetencyperLevel<-(table(sd0$Competency.name,sd0$Work.Location,sd0$Held.Enrolled))
LocationHeldEnrolledSubset<-as.data.frame(tablenumassociatespercompetencyperLevel)
colnames(LocationHeldEnrolledSubset)<-c("Competency name","Work Location","Held/Enrolled","Number")
LocationHeldEnrolledSubset[complete.cases(LocationHeldEnrolledSubset),]

#Preprocessing data for project-wise associate distribution
tableofprojnameAssociatedist<-(table(EmployeeSubsetHeadcountIsRetail$`Project Name`))
projnameassociatedist<-as.data.frame(tableofprojnameAssociatedist)
colnames(projnameassociatedist)<-c("Project","Number of Associates")
projnameassociatedist$`Program Name`<-sd4[match(projnameassociatedist$`Project`, sd4$Project.Name),2]
prognameassociatedist<-aggregate(projnameassociatedist$`Number of Associates`, by=list(Category=projnameassociatedist$`Program Name`), FUN=sum)
#w1=rbind(rep(30,36),rep(0,36),w1)

#Creating a dataframe of Number of Asssociates, location and project based on overall completions zero
sd3zerocomp<-sd3[!(sd3$ID %in% sd0$Employee.Number),]
sd3zerocomplocation<-as.data.frame(table(sd3zerocomp$Work.Location))
colnames(sd3zerocomplocation)<-c("Work Location","Number of Associates")
ZeroCompletionsSubset <- subset(sd0, Overall.Completions==0)
zerocompllocation<-as.data.frame(table(ZeroCompletionsSubset$Work.Location))
colnames(zerocompllocation)<-c("Work Location","Number of Associates")
finalzerocomploc<-aggregate(.~`Work Location`,rbind(zerocompllocation,setNames(sd3zerocomplocation,names(zerocompllocation))),sum)
zerocompproj<-as.data.frame(table(ZeroCompletionsSubset$Project.Name))
colnames(zerocompproj)<-c("Project Name","Number of Associates")
sd3zerocompproj<-as.data.frame(table(sd3zerocomp$Project.Name))
colnames(sd3zerocompproj)<-c("Project Name","Number of Associates")
sd3zerocompproj$`Program Name`<-sd4[match(sd3zerocompproj$`Project Name`, sd4$Project.Name),2]
sd3zerocompproj$`Project Name`<-NULL
zerocompproj$`Program Name`<-sd4[match(zerocompproj$`Project Name`, sd4$Project.Name),2]
zerocompproj$`Project Name`<-NULL
finalzerocompproj<-aggregate(.~`Program Name`,rbind(zerocompproj,setNames(sd3zerocompproj,names(zerocompproj))),sum)



#Creating dataframe for Depth of learning box
E0Comp<-sum(sd0$E0.Completed,na.rm = TRUE)
E1Comp<-sum(sd0$E1.Completed,na.rm = TRUE)
E2Comp<-sum(sd0$E2.Completed,na.rm = TRUE)
E3Comp<-sum(sd0$E3.Completed,na.rm = TRUE)
OverallCompsum<-sum(sd0$Overall.Completions,na.rm = TRUE)
atleastonecompletionsubset<-subset(sd0, Overall.Completions!=0)
Atleastonecomp<-nrow(atleastonecompletionsubset)
depthgroupings<-c("E0 Completed Certifications","E1 Completed Certifications",
                  "E2 Completed Certifications","E3 Completed Certifications",
                  "Total Completed Certifications",
                  "Number of Associates with at least one certification")
depthNumber<-c(E0Comp,E1Comp,E2Comp,E3Comp,OverallCompsum,Atleastonecomp)
depthofLearning<-data.frame(depthgroupings,depthNumber)