#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(readxl)
library(RCurl)

# setwd("C:/2020-2021-Fall-semester/BZAN 552_579/Final Project/datasets")
race_path = './Public-Dataset-RaceEthSex.xlsx'
race_eth_sex = read_excel(race_path,sheet = "ALL_RACEETHSEX_FINAL")
race_eth_sex$Date = format(race_eth_sex$Date,"%Y-%m-%d")
# race_eth_sex$Date = as.factor(race_eth_sex$Date)
race = subset(race_eth_sex, Category== "RACE")
ethnicity = subset(race_eth_sex,Category == "ETHNICITY")
sex = subset(race_eth_sex, Category == "SEX")
TN_county_age_path = './Public-Dataset-Daily-County-Age-Group.xlsx'
TN_county_age = read_excel(TN_county_age_path)
TN_county_age$DATE = as.Date(TN_county_age$DATE)
TN_county_age$AGE_GROUP = as.factor(TN_county_age$AGE_GROUP)

Group_Age = TN_county_age %>%
    group_by(DATE, AGE_GROUP) %>%
    summarize(daily_sum = sum(CASE_COUNT))
age_group = unique(Group_Age$AGE_GROUP)


# tidyverse spread function
TN_T_Nov = subset(TN_county_age, DATE == as.Date("2020-11-26"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("BZAN 579 Project - COVID-19 Analysis for Tennessee"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
        dateInput("date", label = h3("Date input"), value = "2020-11-27",max = "2020-11-27"),
            
        selectInput(
            inputId = "Category",
            label = "Select a Category",
            choices = c("Race","Ethnicity","Sex")),
        h6("Choose the category first, and you will see the corresponding charts in Dashboard 1."),
      
        selectInput(
            inputId = "age", label = "Select Age Group",
                     choices = c("0-10 years",  "11-20 years", "21-30 years", "31-40 years",
                                 "41-50 years", "51-60 years", "61-70 years", "71-80 years",
                                 "81+ years"),
                     selected = "11-20 years", multiple = TRUE),
        h6("Add age group for comparison, you will see the corresponding lines in Dashboard 2. "),
        
        numericInput(inputId = "obs",
                     label = "Number of top counties to view:",
                     value = 10,max = 98,min = 0),
        h3(textOutput("topcases",container = span)),
        tableOutput("toptable"),
        tags$a(href = "https://www.tn.gov/health/cedep/ncov/data/downloadable-datasets.html",
               "Source: Tennessee covid-19 datasets", target = "_blank")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("tilldate",container = span)),
            tableOutput("view"),
            h3("Dashboard 1. Confirmed cases by Category"),
            plotOutput("CumPlot"),
            h3("-----------------------------------------------------------------------------------"),
            h3("Dashboard 2. Confirmed cases by Age Group"),
            plotOutput("ageview"),
            
           
           
        )
    )
)

server <- function(input, output) {
    
    
    datasetInput <- reactive({
        switch(input$Category,
               "Race" = subset(race_eth_sex, Category== "RACE") ,
               "Ethnicity" = subset(race_eth_sex,Category == "ETHNICITY"),
               "Sex" = subset(race_eth_sex, Category == "SEX"))
         })
    
    dataageInput <- reactive({
        
        dataset = subset(Group_Age, Group_Age$AGE_GROUP %in% input$age)
        
    })
    datatopInput <- reactive({
        
        Top_n_county <- TN_T_Nov %>%
            group_by(COUNTY) %>%
            summarise(total_confirmed_cases = sum(CASE_COUNT)) %>%
            top_n(input$obs)
        
    })
    
    
        output$tilldate <- renderText({
            paste("Confirmed cases by ",input$Category, " till ", format(input$date, "%Y-%m-%d")," in Tennessee")
            })
    
        output$CumPlot <- renderPlot({
            
            dataset <- datasetInput()
            dataset$Date = as.Date(dataset$Date)

            ggplot(dataset, aes(fill=CAT_DETAIL, y=Cat_CaseCount, x=Date)) + 
                geom_bar(position="stack", stat="identity") + ylab("Tennessee Confrimed Cases by Race \n from 2020/04/8 to 2020/12/26")+
                theme(text = element_text(size= 15))+theme(legend.position=c(.2, .7))+
                aes(xmin = Date[1], 
                    xmax = tail(Date[1])) + 
                scale_x_date(breaks = '1 month')+ theme(axis.text.x = element_text(family = "serif",angle = 45, vjust = 0.5,face = "bold"))

        })    
    
        output$view <- renderTable({
            dataset = datasetInput()
            dataset_show = subset(dataset, dataset$Date == as.Date(input$date)) 
        })
        
        output$ageview <- renderPlot(
            {
            dataset = dataageInput()
            ggplot(dataset, aes(x=DATE, y=daily_sum,
            group= AGE_GROUP, color = AGE_GROUP, shape = AGE_GROUP,size = 4)) + scale_shape_manual(values=1:10)+ theme(legend.position=c(.2, .7))+
            geom_point(size = 2) + geom_line(size = 0.5) + ylab("Cumulative Confirmed cases since June 12, 2020") + 
            aes(xmin = DATE[1], 
                xmax = tail(DATE[1])) + 
                scale_x_date(breaks = '1 month')+ theme(axis.text.x = element_text(family = "serif",angle = 45, vjust = 0.5,face = "bold"))
            }
        )
        output$topcases <- renderText({
            paste("Top ", input$obs, " Counties with highest confirmed cases")
        })
        output$toptable <- renderTable({
            datatop = datatopInput()
            datatop = datatop[order(datatop$total_confirmed_cases,decreasing = TRUE),]
        })
        
        
        
        
}

shinyApp(ui, server)
