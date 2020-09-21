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
                         
                         selectInput("data_type",
                                     h3("Select data"),
                                     choices = list("National calibration",
                                                    "Regional weighting"),
                                     selected = "National calibration"),
                         
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
                         
                             selectInput("filter_type",
                                         h3("Filter by ethnicity"),
                                         choices = list("None",
                                                        "Prioritised",
                                                        "Total"),
                                         selected = "None"),
                         
                            selectInput("ethnicity",
                                        h3("Select ethnicity"),
                                        choices = list("Maori",
                                                       "Pacific",
                                                       "Asian",
                                                       "Other",
                                                       "European"),
                                        selected = "Maori"),
                         
                            # uiOutput("ethnicity_choices"),
                         
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
                                                               "Region",
                                                               "Attraction",
                                                               "Trans",
                                                               "Sex44"
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
                         
                         # selectInput("time_series_data_type",
                         #             h3("Select data"),
                         #             choices = list("National calibration",
                         #                            "Regional weighting"),
                         #             selected = "National calibration"),

                         uiOutput("time_series_topic_choices"),

                         uiOutput("time_series_table_choices"),

                         uiOutput("time_series_ethnic_filter"),

                         checkboxGroupInput("time_series_group_selection",
                                     h3("Select groups"),
                                     choices = list("Total",
                                                    "Age",
                                                    "Sex",
                                                    # "School_Year",
                                                    "Ethnicity",
                                                    "Attraction",
                                                    "Sex44"
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
                         
                         checkboxGroupInput("demographicsPilotWharekura",
                                     h3("Include:"),
                                     choices = list("Pilot",
                                                    "Wharekura"),
                                     selected = list("Pilot",
                                                     "Wharekura")),
                         
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
    
    df <-
        reactive({
            if(input$data_type == "National calibration"){
                svy_2019_kura
            } else if(input$data_type == "Regional weighting"){
                svy_2019_kura_uncalibrated
            }
        })
    
    
    #allow to choose between prioritised and total ethnicity
    filter_variable <-
        reactive({
            
            if(input$filter_type == "None"){
                return(NA)
            } else if (input$filter_type == "Prioritised"){
                return("ethnic_p5")
            } else if (input$filter_type == "Total"){
                return(case_when(input$ethnicity == "Maori" ~ "ethnic_maori",
                                 input$ethnicity == "Pacific" ~ "ethnic_pacific",
                                 input$ethnicity == "Asian" ~ "ethnic_asian",
                                 input$ethnicity == "Other" ~ "ethnic_other",
                                 input$ethnicity == "European" ~ "ethnic_european"))
            }
        })
    
    #allow to choose between prioritised and total ethnicity
    filter_value <-
        reactive({
            
            if(is.na(filter_variable())) {
                return(NA)
            } else if(filter_variable() == "ethnic_p5"){
                return(input$ethnicity)
            } else {
                return(1)
            }
        })
    
    #create the table of variables to be generated
    variable_table <-
        reactive({
            
            req(input$topics, input$tables, cancelOutput = TRUE)
            
            if (!is.null(input$uploadVariables) & !reset_flag$clear){
                return(read_excel(input$uploadVariables$datapath) %>% select(var, val, varname))
            } else {
                return(variables %>% filter(mainSection == input$topics, subSection == input$tables) %>% select(var, val, varname))
            }
        })
    
    #name of the table
    variable_title <-
        reactive({
            
            req(input$topics, input$tables, cancelOutput = TRUE)
            
            if (!is.null(input$uploadVariables) & !reset_flag$clear){
                return(read_excel(input$uploadVariables$datapath) %>% select(title) %>% distinct())
            } else {
                return(variables %>% filter(mainSection == input$topics, subSection == input$tables) %>% select(title) %>% distinct())
            }
        })
    
    #need to handle multple footnotes
    variable_footnote <-
        reactive({
            
            req(input$topics, input$tables, cancelOutput = TRUE)
            
            if (!is.null(input$uploadVariables) & !reset_flag$clear){
                return(read_excel(input$uploadVariables$datapath) %>% select(footnote))
            } else {
                return(variables %>% filter(mainSection == input$topics, subSection == input$tables) %>% select(footnote))
            }
        })
        
    # df_table <-
    #     reactive({
    #         if(input$time_series_data_type == "National calibration"){
    #             c("svy_2001","svy_2007", "svy_2012", "svy_2019")
    #         } else if(input$time_series_data_type == "Regional weighting"){
    #             c("svy_2001_uncalibrated","svy_2007_uncalibrated", "svy_2012_uncalibrated", "svy_2019_uncalibrated")
    #         }
    #     })
    
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
    
    
    time_series_variable_footnote <-
        reactive({

            #req(input$topics, input$tables, cancelOutput = TRUE)

            return(timeSeries %>%
                       filter(mainSection == input$time_series_topics, subSection == input$timeSeriesTables) %>%
                       select(footnote))

        })
    
    
    output$prevalenceTable <-
        render_gt({
            
            single_year(df = df(),
                        variable_table = variable_table(),
                        groups_table = input$groups,
                        title = variable_title(),
                        footnote = variable_footnote(),
                        filterGroup = filter_variable(),
                        filterVal = filter_value(),
                        password = input$Password)
        })
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(variable_title(),"_ethnicity_",
                   if(input$filter_type == "None"){
                       paste0("All")
                   } else if(input$filter_type == "Prioritised"){
                       paste("prioritised_", input$ethnicity)
                   } else if(input$filter_type == "Total"){
                       paste0("total_", input$ethnicity)
                   },
        ".csv")
        },
        content = function(file) {
            write.csv(single_year(df = df(),
                                  variable_table = variable_table(),
                                  groups_table = input$groups,
                                  title = variable_title(),
                                  footnote = variable_footnote(),
                                  filterGroup = filter_variable(),
                                  filterVal = filter_value(),
                                  password = input$Password,
                                  html_output = FALSE),
                      file, row.names = FALSE)
        }
    )
    

    output$timeSeries <-
        render_gt(
            expr = multi_year(df_table = c("svy_2001_uncalibrated","svy_2007_uncalibrated", "svy_2012_uncalibrated", "svy_2019_uncalibrated"),
                              variable_table = time_series_variable_table(),
                              groups_table = input$time_series_group_selection,
                              title = time_series_variable_title(),
                              footnote = time_series_variable_footnote(),
                              filterGroup = if(input$time_series_ethnicity == "All"){NA} else {"Ethnicity"},
                              filterVal = if(input$time_series_ethnicity == "All"){NA} else {input$time_series_ethnicity},
                              password = input$Password)
        )
        
    output$time_series_download_data <- downloadHandler(
        filename = function() {
            paste0("timeSeries_",time_series_variable_title(),"_ethnicity_",input$time_series_ethnicity,".csv")
        },
        content = function(file) {
            write.csv(multi_year(df_table = df_table(),
                              variable_table = time_series_variable_table(),
                              groups_table = input$time_series_group_selection,
                              title = time_series_variable_title(),
                              footnote = time_series_variable_footnote(),
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
                            include_pilot = if_else("Pilot" %in% input$demographicsPilotWharekura, TRUE, FALSE),
                            include_wharekura = if_else("Wharekura" %in% input$demographicsPilotWharekura, TRUE, FALSE),
                            password = input$Password)
        )

    
}

# Run the application
shinyApp(ui = ui, server = server)

