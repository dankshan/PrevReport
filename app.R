# Sys.setlocale("LC_CTYPE", "Greek_Greece.1257")

library(shiny)
library(tidyverse)
library(srvyr)
library(readxl)
library(gt)
library(cyphr)


source("./functions/loadData.R")
source("./functions/single_year.R")
source("./functions/multi_year.R")
# source("./functions/prevGraph1.R")
source("./functions/demographics.R")

ui <-
    
    navbarPage(
        
        theme = "style.css",
        
        title = div(img(src="youth19.png", height = 30)),
        
        tabPanel("Password",
                 
                 textInput("Password", label = "Enter Password")
                 
                 ),
        
        tabPanel("Prevalence",
                 
                 # Sidebar with a slider input for number of bins
                 sidebarLayout(
                     sidebarPanel(
                         
                         width = 2,
                         
                         
                         
                         tags$hr(style="border-color: black;"),
                         
                         tags$h2("Standard tables"),
                         
                             uiOutput("topicChoices"),
                             
                             uiOutput("tableChoices"),
                         
                         tags$hr(style="border-color: black;"),
                         
                         tags$h2("Custom table"),
                         
                             downloadButton("downloadTemplate", "Download xlsx template"),
                             
                             fileInput("uploadVariables", "Upload xlsx template",
                                       accept = c(".xlsx")),
                             
                             actionButton('reset', 'Reset to standard tables'),
                         
                         tags$hr(style="border-color: black;"),
                         
                         tags$h2("Configuration"),
                         
                             # uiOutput("ethnicFilter"),
                         
                            selectInput("ethnicity",
                                        h3("Filter by ethnicity"),
                                        choices = list("All",
                                                       "Maori",
                                                       "Pacific",
                                                       "Asian",
                                                       "Other",
                                                       "European"),
                                        selected = "All"),
                         
                             checkboxGroupInput("groups",
                                                h3("Select groups"),
                                                choices = list("Total",
                                                               "Age",
                                                               "Sex",
                                                               "Ethnicity",
                                                               "School_Year",
                                                               "School_Decile",
                                                               "Neighbourhood_Deprivation",
                                                               "Urban_Rural",
                                                               "Region"
                                                ), selected = list("Total",
                                                                   "Age",
                                                                   "Sex")),
                             
                             # uiOutput("groupsSelection"),
                         
                         tags$hr(style="border-color: black;"),
                         
                         tags$h2("Download data"),
                         
                            downloadButton("downloadData", "Download csv")
                         
                     ),
                     
                     mainPanel(
                         
                         gt_output(outputId = "prevalenceTable")
                     )
                 )),
                 
        tabPanel("Time Series",

                 sidebarLayout(
                     sidebarPanel(

                         width = 2,

                         uiOutput("time_series_topic_choices"),

                         uiOutput("time_series_table_choices"),

                         uiOutput("time_series_ethnic_filter"),

                         checkboxGroupInput("time_series_group_selection",
                                     h3("Select groups"),
                                     choices = list("Total",
                                                    "Age",
                                                    "Sex",
                                                    # "School_Year",
                                                    "Ethnicity"
                                                    # "Decile",
                                                    # "Deprivation",
                                                    # "Urban_Rural"
                                     ),
                                     selected = list("Total", "Age", "Sex", "Ethnicity")),
                                     
                          tags$hr(style="border-color: black;"),
                         
                         tags$h2("Download data"),
                         
                            downloadButton("time_series_download_data", "Download csv")

                     ),

                     mainPanel(

                         width = 10,

                         gt_output(outputId = "timeSeries")

                         # plotOutput(outputId = "timeSeriesPlot")
                     )
                 )
        ),
        
        tabPanel("Demographics",
                 
                 sidebarLayout(
                     sidebarPanel(
                         
                         width = 2,
                         
                         selectInput("demographicsType",
                                     h3("Select table"),
                                     choices = list("Schools",
                                                    "Students"),
                                     selected = "Schools"),
                         
                         selectInput("demographicsFilterGroup",
                                     h3("Filter by group"),
                                     choices = list("None",
                                                    "EducationRegion",
                                                    "Type",
                                                    "Authority",
                                                    "SchoolSize",
                                                    "Decile",
                                                    "DecileBand"),
                                     selected = "None"),
                         
                         
                         uiOutput("demographicsFilterVals"),
                         
                         # selectInput("demographicsFilterVal",
                         #             h3("Filter group value"),
                         #             choices = list("1",
                         #                            "2",
                         #                            "3",
                         #                            "4",
                         #                            "5",
                         #                            "6"),
                         #             selected = "1")
                         
                         
                     ),
                     
                     mainPanel(
                         
                         width = 10,
                         
                         gt_output(outputId = "demographics")
                     )
                     
                 )
        )
    )


server <- function(input, output) {
    
    #workaround to reset view after uploading custom table
    reset_flag <- reactiveValues(
        clear = TRUE
    )
    
    observeEvent(input$uploadVariables, {
        reset_flag$clear <- FALSE
    }, priority = 1000)
    
    observeEvent(input$reset, {
        reset_flag$clear <- TRUE
    }, priority = 1000)
    
    
    
    #download template file
    output$downloadTemplate <-
        downloadHandler(
            filename = "template.xlsx",
            
            content = function(file) {
                file.copy("www/template.xlsx", file)
            })
    
    #2019 default table inputs
    output$topicChoices <- renderUI({
        
        topic <- variables %>% distinct(mainSection)
        
        selectInput("topics", "Select a topic", topic)
    })
    
    output$tableChoices <- renderUI({
        
        req(input$topics)
        
        table <- variables %>%
            filter(mainSection == input$topics) %>%
            distinct(subSection)
        
        selectInput("tables", "Select a table", table)
        
    })
    
    # output$ethnicFilter <- renderUI({
    #
    #     # ethnicities <- tibble(ethnicities = c("All", unique(as.character(svy_2019_kura$variables$Ethnicity))))
    #
    #     ethnicities <- tibble(ethnicities = c("All", "Maori", "Asian"))
    #
    #     selectInput("ethnicity", "Filter by ethnicity", ethnicities)
    # })
    
    # output$groupsSelection <- renderUI({
    #
    #     groups <-
    #         if(input$ethnicity == "All"){
    #             list("Total",
    #                  "Sex",
    #                  "Age",
    #                  "Deprivation",
    #                  "School_Year",
    #                  "Decile",
    #                  "Urban_Rural",
    #                  "Region"
    #                  # "Disability"
    #                  )
    #         } else {
    #             list("Total",
    #                  "Sex",
    #                  "Deprivation")
    #         }
    #
    #     checkboxGroupInput("groups", h3("Select groups"), groups, selected = c("Total", "Age", "Sex"))
    # })
    
    output$time_series_topic_choices<- renderUI({

        time_series_topic <- timeSeries %>% distinct(mainSection)

        selectInput("time_series_topics", "Select a topic", time_series_topic)
    })

    output$time_series_table_choices<- renderUI({

        timeSeriestable <- timeSeries %>%
            filter(mainSection == input$time_series_topics) %>%
            distinct(subSection)

        selectInput("timeSeriesTables", "Select a table", timeSeriestable)
    })

    output$time_series_ethnic_filter <- renderUI({

        time_series_ethnicities <- tibble(ethnicities = c("All", unique(as.character(svy_2019$variables$Ethnicity))))

        selectInput("time_series_ethnicity", "Filter by ethnicity", time_series_ethnicities)
    })
    
    output$demographicsFilterVals <- renderUI({
        
        values <- demographics2 %>%
            filter(Eligible == 1) %>%
            select(input$demographicsFilterGroup) %>%
            distinct()
        
        selectInput("demographicsFilterVal", "Select group value", values)
    })
    
    ###
    ##generate the data, table, and download
    ###
    
    variable_table <-
        reactive({
            
            req(input$topics, input$tables, cancelOutput = TRUE)
            
            if (!is.null(input$uploadVariables) & !reset_flag$clear){
                return(read_excel(input$uploadVariables$datapath) %>% select(var, val, varname))
            } else {
                return(variables %>% filter(mainSection == input$topics, subSection == input$tables) %>% select(var, val, varname))
            }
        })
    
    variable_title <-
        reactive({
            
            req(input$topics, input$tables, cancelOutput = TRUE)
            
            if (!is.null(input$uploadVariables) & !reset_flag$clear){
                return(read_excel(input$uploadVariables$datapath) %>% select(title) %>% distinct())
            } else {
                return(variables %>% filter(mainSection == input$topics, subSection == input$tables) %>% select(title) %>% distinct())
            }
        })
        
    
    time_series_variable_table <-
        reactive({
            
            #req(input$topics, input$tables, cancelOutput = TRUE)
            
          
                return(timeSeries %>%
                                  filter(mainSection == input$time_series_topics, subSection == input$timeSeriesTables) %>%
                                  select(var, val, varname))
            
        })
    
    time_series_variable_title <-
        reactive({
            
            #req(input$topics, input$tables, cancelOutput = TRUE)
            
                return(timeSeries %>%
                                  filter(mainSection == input$time_series_topics, subSection == input$timeSeriesTables) %>%
                                  select(title) %>%
                                  distinct())
            
        })
    
    
    output$prevalenceTable <-
        render_gt({
            
            single_year(df = svy_2019_kura,
                        variable_table = variable_table(),
                        groups_table = input$groups,
                        title = variable_title(),
                        filterGroup = if(input$ethnicity == "All"){NA} else {"Ethnicity"},
                        filterVal = if(input$ethnicity == "All"){NA} else {input$ethnicity},
                        password = input$Password)
        })
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(variable_title(),"_ethnicity_",input$ethnicity,".csv")
        },
        content = function(file) {
            write.csv(single_year(df = svy_2019_kura,
                                  variable_table = variable_table(),
                                  groups_table = input$groups,
                                  title = variable_title(),
                                  filterGroup = if(input$ethnicity == "All"){NA} else {"Ethnicity"},
                                  filterVal = if(input$ethnicity == "All"){NA} else {input$ethnicity},
                                  password = input$Password,
                                  html_output = FALSE),
                      file, row.names = FALSE)
        }
    )
    

    output$timeSeries <-
        render_gt(
            expr = multi_year(df_table = c("svy_2001","svy_2007", "svy_2012", "svy_2019"),
                              variable_table = time_series_variable_table(),
                              groups_table = input$time_series_group_selection,
                              title = time_series_variable_title(),
                              filterGroup = if(input$time_series_ethnicity == "All"){NA} else {"Ethnicity"},
                              filterVal = if(input$time_series_ethnicity == "All"){NA} else {input$time_series_ethnicity},
                              password = input$Password)
        )
        
    output$time_series_download_data <- downloadHandler(
        filename = function() {
            paste0("timeSeries_",variable_title(),"_ethnicity_",input$ethnicity,".csv")
        },
        content = function(file) {
            write.csv(multi_year(df_table = c("svy_2001","svy_2007", "svy_2012", "svy_2019"),
                              variable_table = time_series_variable_table(),
                              groups_table = input$time_series_group_selection,
                              title = time_series_variable_title(),
                              filterGroup = if(input$time_series_ethnicity == "All"){NA} else {"Ethnicity"},
                              filterVal = if(input$time_series_ethnicity == "All"){NA} else {input$time_series_ethnicity},
                              password = input$Password,
                              html_output = FALSE),
                      file, row.names = FALSE)
        }
    )
    
    # output$timeSeriesPlot <-
    #     renderPlot(
    #         prevGraph1(svyyears,
    #                    variable_table = timeSeries %>%
    #                        filter(mainSection == input$time_series_topics, subSection == input$timeSeriesTables) %>%
    #                        select(variable, value, label, title),
    #                    group2 = get(!! input$timeSeriesGroup),
    #                    filter = input$time_series_ethnicity,
    #                    password = input$Password2)
    #     )
    
    output$demographics <-
        render_gt(
            expr = demTable(type = input$demographicsType,
                               filterGroup = if(input$demographicsFilterGroup == "None"){
                                   NULL
                               } else {
                                   get(!!input$demographicsFilterGroup)
                                       },
                               filterVal = input$demographicsFilterVal,
                            password = input$Password)
        )

    
}

# Run the application
shinyApp(ui = ui, server = server)

