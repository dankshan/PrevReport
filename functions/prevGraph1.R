prevGraph1 <- function(df, variable_table, group2, filter = "All", rounding = 2, password = NULL){
  
  if(password == "@$@#%Youth") {
    
    plotGraph <- function(df, variable, value, varname, group2, filter = filter) {
      
      var <- enquo(variable)
      val <- enquo(value)
      
      group1 <- "Year"
      

      df %>%
        filter(
          if(filter == "All"){
            !is.na(Total)
          }
          else {
            Ethnicity == filter
          }
        ) %>%
        group_by_(groupType1 = group1, groupType2 = group2) %>%
        summarise(var.pct = survey_ratio(!! var %in% !! val, !is.na(!! var), na.rm = TRUE, vartype = "ci", level = 0.95))
    }
    
    outdf <- tibble(groupType2 = NA_character_)
    
    for (k in 1:nrow(variable_table)) {
      
      outdf <-
        outdf %>%
        full_join(plotGraph(df,
                             variable = !! sym(as.character(variable_table[k,1])),
                             value = as.numeric(variable_table[k,2]),
                             varname = as.character(variable_table[k,3]),
                             group2 = group2,
                             filter = filter))
    }
    
    title <- as.character(variable_table[k,3])
    
    subtitle <- paste0(
      if(group2 != "Total"){paste0("Grouped by ", group2)} else {NULL},
      if(group2 != "Total" & filter != "All") {paste0(", ")} else {NULL},
      if(filter != "All"){paste0("Filtered for ", filter, " ethnicity")} else {NULL}
    )
    
    sourceNote <- 
      paste0(
        if(group2 == "Deprivation") {paste0("NZ Deprivation Index 2018, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
        if(group2 == "Decile") {paste0("School Decile, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
        if(group2 == "Ethnicity" | filter != "All") {paste0("Ethnicity is prioritised. \n")} else {NULL}
      )
    
    outdf %>%
      filter(!is.na(groupType2), groupType2 != "", !is.na(groupType1), groupType1 != "") %>%
      ggplot(aes(x = groupType1, y = var.pct, fill = groupType2)) +
      geom_col(position = "dodge") +
      geom_errorbar(aes(ymin = var.pct_low, ymax = var.pct_upp), position = "dodge") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = title,
           subtitle = subtitle,
           fill = group2,
           caption = sourceNote) + #this needs automating
      ylab("Prevalence") +
      xlab("Year") +
      scale_fill_manual(values = wes_palette("Zissou1")) +
      theme(plot.title = element_text(size = rel(2), face = "bold", margin = margin(10,0,10,0)),
            axis.title = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.caption = element_text(hjust = 0),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  }
}

# 
# prevGraph1(svyyears, outputvariablestest, group2 = "Ethnicity", filter = "European", password = "@$@#%Youth")
# # 
# outputvariablestest = tibble(var = "wellbeing", val = 1, label = "wellbeing", title = "wellbeing")
# # prevTable3(svyyears, outputvariablestest, "age")
# #
# singlePrev(svyyears, wellbeing, 1, "wellbeing", "age", rnd = 1) %>%
#   ggplot(aes(x = groupType1, y = pct)) +
#   geom_col()
# 
# filt <- NULL
# is.null(filt)
# 
# 
# test <- function(filter = NULL) {
#   
#   svydf %>%
#     filter(
#       if(is.null(filter)){
#         !is.na(ethnic_p5)
#       }
#       else {
#         ethnic_p5 == filter
#       }
#     ) %>%
#     summarise(varN = unweighted(sum(depressed28 == 1, na.rm = TRUE)),
#               varN = unweighted(sum(!is.na(depressed28))),
#               var.pct = survey_ratio(depressed28 == 1, !is.na(depressed28), na.rm = TRUE, vartype = "ci")) %>%
#     na.omit()
# }
# 
# test(filter = "Maori")
# 
# svydf %>%
#   filter(ethnic_p5 == "Maori") %>%
#   summarise(varN = unweighted(sum(depressed28 == 1, na.rm = TRUE)),
#             varN = unweighted(sum(!is.na(depressed28))),
#             var.pct = survey_ratio(depressed28 == 1, !is.na(depressed28), na.rm = TRUE, vartype = "ci"))
# 
# svyyears %>%
#   group_by(groupType1 = Year, groupType2 = Age) %>%
#   summarise(var.pct = survey_ratio(depressed28 == 1, !is.na(depressed28), na.rm = TRUE, vartype = "ci")) %>%
#   filter(groupType2 != "") %>%
#   ggplot(aes(x = groupType1, y = var.pct, fill = groupType2)) +
#   geom_col(position = "dodge") +
#   geom_errorbar(aes(ymin = var.pct_low, ymax = var.pct_upp), position = "dodge") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(title = "Significant depressive symptoms",
#        subtitle = "Grouped by Ethnicity",
#        fill = "Ethnicity",
#        caption = "Ethnicity is prioritised") + #this needs automating
#   ylab("Prevalence") +
#   xlab("Year") +
#   scale_fill_manual(values = wes_palette("Zissou1"))
#   
