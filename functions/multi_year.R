multi_year <- function(df_table, variable_table, groups_table, title, filterGroup = NA, filterVal = NA, password = NULL, html_output = TRUE){
  
  if(password == saved_password) {
  
  tabSubtitle <-
    paste0(
      variable_table %>% select(varname) %>% distinct(),
      if(!is.na(filterGroup)){paste0("Filtered for ", filterVal, " ethnicity. \n")} else {NULL}
    )
  
  ## create table footer
  sourceNote <<- 
    paste0(
      if("Neighbourhood_Deprivation" %in% groups_table) {paste0("NZ Deprivation Index 2018, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
      if("School_Decile" %in% groups_table) {paste0("School Decile, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
      if("Disability" %in% groups_table) {paste0("Chronic condition determined by long term health problem, disability, or pain. \n")} else {NULL},
      if("Urban_Rural" %in% groups_table) {paste0("Urban (population of 10,000 or more), Small towns (population between 1,000 and 9,999 people), Rural (population less than 1,000). \n")} else {NULL},
      if("Ethnicity" %in% groups_table | "Ethnicity" %in% filterGroup) {paste0("Ethnicity is prioritised. \n")} else {NULL}
    )
  
  output <-
    df_table %>%
    map_dfr(function(df) {
      
      groups_table %>%
        map_df(function(group) {
          
          variable_table  %>%
            pmap(function(var, val, varname, grp = group){
              
              varnamen <- paste0(varname, "_n")
              varnameN <- paste0(varname, "_N")
              varnamepct <- paste0(varname, "_pct")
              
              varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
              varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
              
              get(df) %>%
                filter(
                  if(!is.na(filterGroup)){
                    !! as.name(filterGroup) == filterVal
                  } else {
                    !is.na(Total)
                  }
                ) %>%
                group_by(group1 = Year, group2 = !!as.name(grp)) %>%
                summarise(!! varnamen := unweighted(sum(!!as.name(var) %in% !!val &
                                                          if(!is.na(!!filterGroup)){
                                                            !!as.name(filterGroup) == !!filterVal
                                                          } else {
                                                            !is.na(Total)
                                                          },
                                                        na.rm = TRUE)),
                          !! varnameN := unweighted(sum(!is.na(!!as.name(var)) &
                                                          if(!is.na(!!filterGroup)){
                                                            !!as.name(filterGroup) == !!filterVal
                                                          } else {
                                                            !is.na(Total)
                                                          },
                                                        na.rm = TRUE)),
                          !! varnamepct := survey_ratio(!!as.name(var) %in% !!val, !is.na(!!as.name(var)), na.rm = TRUE, vartype = "ci", level = 0.95)
                ) %>%
                mutate_at(vars(contains("pct")), function(x) formatC(round(x * 100, 1), digits = 1, format = "f")) %>%
                mutate_at(vars(contains("n")), function(x) formatC(x, big.mark=",")) %>%
                # mutate(!! varnameNoutput := paste0(.[[3]], "\n", "(", .[[4]], ")"),
                #        !! varnamepctoutput := paste0(.[[5]], "\n", "[", .[[6]], "-", .[[7]], "]")) %>%
                # select(-contains("_N"), -contains("_n"), -contains("_pct")) %>%
                mutate(groupType1 = "Year",
                       groupType2 = paste0(group))
            }) %>%
            reduce(left_join, by = c("groupType1", "group1", "groupType2", "group2")) %>%
            na.omit()
        }) %>%
        select(groupType1, group1, groupType2, group2, everything())
    }) %>%
    pivot_longer(cols = c(-groupType1, -group1, -groupType2, -group2), names_to = c("var", "type1", "type2"), names_sep = "_") %>%
    pivot_wider(names_from = c(group1, type1, type2), names_prefix = "Year", names_sep = "_") %>%
    mutate(`Year 2001**n (N)`= paste0(Year2001_n_NA, "\n(", Year2001_N_NA, ")"),
           `Year 2001**% [95% CI]` = paste0(Year2001_pct_NA, "\n[", Year2001_pct_low, "-", Year2001_pct_upp, "]"),
           `Year 2007**n (N)`= paste0(Year2007_n_NA, "\n(", Year2007_N_NA, ")"),
           `Year 2007**% [95% CI]` = paste0(Year2007_pct_NA, "\n[", Year2007_pct_low, "-", Year2007_pct_upp, "]"),
           `Year 2012**n (N)`= paste0(Year2012_n_NA, "\n(", Year2012_N_NA, ")"),
           `Year 2012**% [95% CI]` = paste0(Year2012_pct_NA, "\n[", Year2012_pct_low, "-", Year2012_pct_upp, "]"),
           `Year 2019**n (N)`= paste0(Year2019_n_NA, "\n(", Year2019_N_NA, ")"),
           `Year 2019**% [95% CI]` = paste0(Year2019_pct_NA, "\n[", Year2019_pct_low, "-", Year2019_pct_upp, "]")) %>%
    select(groupType2, group2, everything(), -groupType1, -contains("_pct"), -contains("_n"))
  
  if(html_output == TRUE){
    
    output %>%
      select(-var) %>%
      gt(groupname_col = c("groupType2"), rowname_col = "group2") %>%
      tab_spanner_delim(delim = "**") %>%
      cols_align(align = "center") %>%
      tab_header(
        title = title,
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
    
  } else if(html_output == FALSE) {
    output %>%
      select(var, groupType2, group2, everything())
  }
  }
}
