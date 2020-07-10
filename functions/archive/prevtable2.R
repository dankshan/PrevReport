prevTable2 <- function(df, variable_table, group1, group2, filter = "All", rounding = 2, password = NULL){
  
  # if(password == "@$@#%Youth") {
    
    singlePrev <- function(df, variable, value, varname, group1, group2, filter = filter, rounding = rounding){
      
      var <- enquo(variable)
      val <- enquo(value)
      
      
      varnameN <- paste0(varname,"_N")
      varnamen <- paste0(varname,"_n")
      varnamepct <- paste0(varname,"_pct")
      
      
      varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
      varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
      
      
      df %>%
        # filter(
        #   if(filter == "All"){
        #     !is.na(Total)
        #   }
        #   else {
        #     Ethnicity == "European"
        #   }
        # ) %>%
        group_by_(groupType1 = group1, groupType2 = group2) %>%
        summarise(!! varnamen := unweighted(sum(!! var %in% !! val, na.rm = TRUE)),
                  !! varnameN := unweighted(sum(!is.na(!! var))),
                  !! varnamepct := survey_ratio(!! var %in% !! val, !is.na(!! var), na.rm = TRUE, vartype = "ci", level = 0.95),
        ) %>%
        na.omit() %>%
        mutate_at(vars(contains("pct")), function(x) formatC(round(x * 100, rounding), digits = if_else(rounding > 1, rounding, 1), format = "f")) %>%
        mutate_at(vars(contains("n")), function(x) formatC(x, big.mark=",")) %>%
        mutate(group1 = paste0(group1, ": ", groupType1),
               group2 = paste0(group2, ": ", groupType2)) %>%
        mutate(group1 = str_replace(group1, "Total: Total", "Total"),
               group2 = str_replace(group2, "Total: Total", "Total")) %>%
        mutate(!! varnameNoutput := paste0(.[[3]], "\n", "(", .[[4]], ")"),
               !! varnamepctoutput := paste0(.[[5]], "\n", "[", .[[6]], "-", .[[7]], "]")) %>%
        select(group1, group2, !! varnameNoutput, !! varnamepctoutput, -contains("_N"), -contains("_n"), -contains("_pct"))
    }
    
    outdf <- tibble(group1 = NA_character_,
                    group2 = NA_character_)
    
    for (k in 1:nrow(variable_table)) {
      
      outdf <-
        outdf %>%
        full_join(singlePrev(df,
                             variable = !! sym(as.character(variable_table[k,1])),
                             value = as.numeric(variable_table[k,2]),
                             varname = as.character(variable_table[k,3]),
                             group1 = group1,
                             group2 = group2,
                             filter = filter,
                             rounding = rounding),
                  by = c("group1", "group2"))
      
    }
    
    # return(outdf)
    
    ## create table title and subtitle
    tabTitle <- variable_table %>% distinct(title)
    
    tabSubtitle <- 
      paste0(
        if(group1 == "Total" & group2 != "Total"){paste0("Table grouped by ", group2, ". \n")} else {NULL},
        if(group1 != "Total" & group2 == "Total"){paste0("Table grouped by ", group1, ". \n")} else {NULL},
        if(group1 != "Total" & group2 != "Total"){paste0("Table grouped by ", group1, " and ", group2, ". \n")} else {NULL},
        if(filter != "All"){paste0("Filtered for ", filter, " ethnicity. \n")} else {NULL}
      )
    
    ## create table footer
    sourceNote <- 
      paste0(
        if(group1 == "Deprivation" | group2 == "Deprivation") {paste0("NZ Deprivation Index 2018, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
        if(group1 == "Decile" | group2 == "Decile") {paste0("School Decile, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
        if(group1 == "Disability" | group2 == "Disability") {paste0("Chronic condition determined by long term health problem, disability, or pain. \n")} else {NULL},
        if(group1 == "Ethnicity" | group2 == "Ethnicity" | filter != "All"){paste0("Ethnicity is prioritised. \n")} else {NULL}
      )
    

    
    
    outdf %>%
      filter(!is.na(group1), !is.na(group2)) %>%
      filter(group1 != "Another way", group2 != "Another way") %>%
      gt(groupname_col = "group1", rowname_col = "group2") %>%
      tab_spanner_delim(delim = "**") %>%
      cols_align(align = "center") %>%
      tab_header(
        title = tabTitle,
        subtitle = tabSubtitle
      ) %>%
      tab_source_note(source_note = sourceNote) %>%
      tab_style(
        style = list(cell_text(size = "large",
                               weight = "bold",
                               transform = "capitalize")),
        locations = cells_title()
      ) %>%
      tab_style(
        style = list(cell_text(weight = "bold",
                               transform = "capitalize"),
                     cell_fill(color = "gray90"),
                     cell_borders(sides = c("left", "right", "top", "bottom"))),
        locations = cells_row_groups(groups = everything())
      ) %>%
      tab_style(
        style = list(cell_text(weight = "bold",
                               size = "large"),
                     cell_fill(color = "gray90"),
                     cell_borders(sides = c("left", "right", "top", "bottom"))),
        locations = cells_column_spanners(spanners = everything())
      ) %>%
      tab_style(
        style = list(cell_text(weight = "bold",
                               size = "small"),
                     cell_fill(color = "gray90"),
                     cell_borders(sides = c("left", "right", "top", "bottom"))),
        locations = cells_column_labels(columns = everything())
      ) %>%
      tab_style(
        style = list(cell_text(size = "small"),
                     cell_borders(sides = c("left", "right", "top", "bottom"))),
        locations = cells_body(columns = everything(),
                               rows = everything())
      ) %>%
      tab_style(
        style = list(cell_text(size = "medium"),
                     cell_borders(sides = c("left", "right", "top", "bottom"))),
        locations = cells_stub()
      ) %>%
      opt_table_lines(extent = "none") %>%
      opt_align_table_header(align = "left") %>%
      opt_row_striping()
  # }
}

# outputvariablestest = tibble(var = c("wellbeing", "depressed28"), val = c(1,1), label = c("wellbeing", "depressed28"), title = c("wellbeing", "depressed28"))
# prevTable2(svydf, outputvariablestest, group1 = "Age", group2 = "Decile", filter = "Pacific", password = "@$@#%Youth")
# singlePrev(svydf, wellbeing, 1, "Wellbeing", group1 = "Age", group2 = "Decile", filter = "All", rounding = 2)

# 
# prevTable2(svydf,
#            variable = outputvariablestest,
#            group1 = "Total",
#            group2 = "Total",
#            filter = "Pacific")

# 
# svydf %>%
#   group_by(sex) %>%
#   summarise(everVapen = unweighted(sum(everVape == 1, na.rm = TRUE)),
#             everVapeN = unweighted(sum(!is.na(everVape))),
#             everVape = survey_ratio(everVape == 1, !is.na(everVape), na.rm = TRUE, vartype = "ci"))
# 
# svydfcal2 %>%
#   summarise(wellbeing = survey_total(wellbeing == 1, na.rm = TRUE),
#             wellbeing_pct = survey_ratio(wellbeing == 1, !is.na(wellbeing), na.rm = TRUE))
# 
# 
# svydf2 <- svydf %>% mutate(x = 1) %>% select(wellbeing, depressed28, x) %>% na.omit()
# 
# svydfcal2 <- as_survey(svydfcal) %>% mutate(Total = "Total")
# 
# svytotal(~wellbeing, svydfcal2, na.rm = TRUE)
# 
# 
# svyratio(~wellbeing == 1, ~!is.na(wellbeing), svydfcal3)

