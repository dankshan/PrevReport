# prevTable3 <- function(df, variable_table, group2, filter = "All", rounding = 2, password = NULL){
#   
#   # if(password == "@$@#%Youth") {
#     
#     singlePrev <- function(df, variable, value, varname, group2, filter = filter, rnd){
#       
#       var <- enquo(variable)
#       val <- enquo(value)
#       
#       
#       varnamen <- paste0(varname, "_n")
#       varnameN <- paste0(varname, "_N")
#       varnamepct <- paste0(varname, "_pct")
#       
#       group1 <- "Year"
#       group2 <- enquo(group2)
#       
#       # grp <- enquo(group)
#       # grpname <- str_remove(str_remove_all(deparse(substitute(group)), "\""), "~")
#       
#       
#       df %>%
#         filter(
#           if(filter == "All"){
#             !is.na(Total)
#           }
#           else {
#             Ethnicity == filter
#           }
#         ) %>%
#         group_by(groupType1 = "Year", groupType2 = !! group2) %>%
#         summarise(!! varnamen := unweighted(sum(!! var %in% !! val, na.rm = TRUE)),
#                   !! varnameN := unweighted(sum(!is.na(!! var))),
#                   !! varnamepct := survey_ratio(!! var %in% !! val, !is.na(!! var), na.rm = TRUE, vartype = "ci", level = 0.95),
#         ) %>%
#         mutate_at(vars(contains("pct")), function(x) formatC(round(x * 100, rnd), digits = if_else(rnd > 1, rnd, 1), format = "f")) %>%
#         mutate_at(vars(contains("n")), function(x) formatC(x, big.mark=",")) %>%
#         mutate(group1 = paste0(group1),
#                group2 = paste0(group2)) %>%
#         pivot_longer(cols = c(-groupType1, -group1, -groupType2, -group2), names_to = c("var", "type1", "type2"), names_sep = "_") %>%
#         pivot_wider(names_from = c(groupType1, type1, type2), names_prefix = "Year", names_sep = "_") %>%
#         mutate(`Year 2001**n (N)`= paste0(Year2001_n_NA, "\n(", Year2001_N_NA, ")"),
#                `Year 2001**% [95% CI]` = paste0(Year2001_pct_NA, "\n[", Year2001_pct_low, "-", Year2001_pct_upp, "]"),
#                `Year 2007**n (N)`= paste0(Year2007_n_NA, "\n(", Year2007_N_NA, ")"),
#                `Year 2007**% [95% CI]` = paste0(Year2007_pct_NA, "\n[", Year2007_pct_low, "-", Year2007_pct_upp, "]"),
#                `Year 2012**n (N)`= paste0(Year2012_n_NA, "\n(", Year2012_N_NA, ")"),
#                `Year 2012**% [95% CI]` = paste0(Year2012_pct_NA, "\n[", Year2012_pct_low, "-", Year2012_pct_upp, "]"),
#                `Year 2019**n (N)`= paste0(Year2019_n_NA, "\n(", Year2019_N_NA, ")"),
#                `Year 2019**% [95% CI]` = paste0(Year2019_pct_NA, "\n[", Year2019_pct_low, "-", Year2019_pct_upp, "]")) %>%
#         mutate_all(list(~str_replace_all(., "  NaN\\n\\[  NaN-  NaN\\]", "NR"))) %>%
#         mutate_all(list(~str_replace_all(., "0\\n\\(0\\)", "NR"))) %>%
#         select(group2, groupType2, everything(), -contains("_pct"), -contains("_n"), -group1)
#     }
#     
#     
#     outdf <- tibble(groupType2 = NA_character_,
#                     group2 = NA_character_,
#                     var = NA_character_)
#     
#     for (k in 1:nrow(variable_table)) {
#       
#       outdf <-
#         outdf %>%
#         full_join(singlePrev(df,
#                              variable = !! sym(as.character(variable_table[k,1])),
#                              value = as.numeric(variable_table[k,2]),
#                              varname = as.character(variable_table[k,3]),
#                              group2 = group2,
#                              filter = filter,
#                              rnd = rounding))
#     }
#     
#     return(outdf)
#     
#     subtitle <- 
#       paste0(
#       if(group2 != "Total"){paste0("Grouped by ", group2, ".")} else {NULL},
#       if(filter != "All"){paste0("Filtered for ", filter, " ethnicity.")} else {NULL}
#     )
#     
#     sourceNote <- 
#       paste0(
#         if(group2 == "Deprivation") {paste0("NZ Deprivation Index 2018, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
#         if(group2 == "Decile") {paste0("School Decile, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
#         if(group2 == "Ethnicity" | filter != "All") {paste0("Ethnicity is prioritised. \n")} else {NULL}
#       )
#     
#     # if(group2 == "Deprivation" | group2 == "Deprivation") {
#     #   depNote <- "NZ Deprivation Index 2018, Low (1-3), Medium (4-7), High (8-10)."
#     # } else {
#     #   depNote <- NULL
#     # }
#     # 
#     # if(group2 == "Decile" | group2 == "Decile") {
#     #   decNote <- "School Decile, Low (1-3), Medium (4-7), High (8-10)."
#     # } else {
#     #   decNote <- NULL
#     # }
#     # 
#     # sourceNote <- paste0(depNote, "\n", decNote)
#     
#     
#     # outdf %>%
#     #   filter(!is.na(groupType2), !is.na(group2)) %>%
#     #   gt(groupname_col = c("var", "group2"), rowname_col = "groupType2") %>%
#     #   tab_spanner_delim(delim = "**") %>%
#     #   cols_align(align = "center") %>%
#     #   tab_header(
#     #     title = variable_table %>% distinct(title),
#     #     subtitle = subtitle
#     #   ) %>%
#     #   tab_source_note(source_note = sourceNote) %>%
#     #   tab_style(
#     #     style = list(cell_text(size = "large",
#     #                            weight = "bold",
#     #                            transform = "capitalize")),
#     #     locations = cells_title()
#     #   ) %>%
#     #   tab_style(
#     #     style = list(cell_text(weight = "bold",
#     #                            transform = "capitalize"),
#     #                  cell_fill(color = "gray90"),
#     #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
#     #     locations = cells_row_groups(groups = everything())
#     #   ) %>%
#     #   tab_style(
#     #     style = list(cell_text(weight = "bold",
#     #                            size = "large"),
#     #                  cell_fill(color = "gray90"),
#     #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
#     #     locations = cells_column_spanners(spanners = everything())
#     #   ) %>%
#     #   tab_style(
#     #     style = list(cell_text(weight = "bold",
#     #                            size = "small"),
#     #                  cell_fill(color = "gray90"),
#     #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
#     #     locations = cells_column_labels(columns = everything())
#     #   ) %>%
#     #   tab_style(
#     #     style = list(cell_text(size = "small"),
#     #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
#     #     locations = cells_body(columns = everything(),
#     #                            rows = everything())
#     #   ) %>%
#     #   tab_style(
#     #     style = list(cell_text(size = "medium"),
#     #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
#     #     locations = cells_stub()
#     #   ) %>%
#     #   opt_table_lines(extent = "none") %>%
#     #   opt_align_table_header(align = "left") %>%
#     #   opt_row_striping()
#   # }
# }




# multi_year <- function(df, variable_table, groups_table, title, filterGroup = NA, filterVal = NA, password = NULL){
# 
#   
#   tabSubtitle <-
#     paste0(
#       if(!is.na(filterGroup)){paste0("Filtered for ", filterVal, " ethnicity. \n")} else {NULL}
#     )
#   
#   ## create table footer
#   sourceNote <<- 
#     paste0(
#       if("Deprivation" %in% groups_table) {paste0("NZ Deprivation Index 2018, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
#       if("Decile" %in% groups_table) {paste0("School Decile, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
#       if("Disability" %in% groups_table) {paste0("Chronic condition determined by long term health problem, disability, or pain. \n")} else {NULL},
#       if("Ethnicity" %in% groups_table) {paste0("Ethnicity is prioritised. \n")} else {NULL}
#     )
#   
#   groups_table %>%
#     map_df(function(group) {
#       
#       variable_table  %>%
#         pmap(function(var, val, varname, grp = group){
#           
#           varnamen <- paste0(varname, "_n")
#           varnameN <- paste0(varname, "_N")
#           varnamepct <- paste0(varname, "_pct")
#           
#           df %>%
#             filter(
#               if(!is.na(filterGroup)){
#                 !! as.name(filterGroup) == filterVal
#               } else {
#                 !is.na(Total)
#               }
#             ) %>%
#             group_by(groupType1 = Year, groupType2 = !!as.name(grp)) %>%
#             summarise(!! varnamen := unweighted(sum(!!as.name(var) %in% !!val &
#                                                       if(!is.na(!!filterGroup)){
#                                                         !!as.name(filterGroup) == !!filterVal
#                                                       } else {
#                                                         !is.na(Total)
#                                                       },
#                                                     na.rm = TRUE)),
#                       !! varnameN := unweighted(sum(!is.na(!!as.name(var)) &
#                                                       if(!is.na(!!filterGroup)){
#                                                         !!as.name(filterGroup) == !!filterVal
#                                                       } else {
#                                                         !is.na(Total)
#                                                       },
#                                                     na.rm = TRUE)),
#                       !! varnamepct := survey_ratio(!! sym(as.character(var)) %in% !! val, !is.na(!! sym(as.character(var))), na.rm = TRUE, vartype = "ci", level = 0.95)
#             ) %>%
#             mutate_at(vars(contains("pct")), function(x) formatC(round(x * 100, 2), digits = 1, format = "f")) %>%
#             mutate_at(vars(contains("n")), function(x) formatC(x, big.mark=",")) %>%
#             mutate(group1 = "Year",
#                    group2 = paste0(group)) %>%
#             pivot_longer(cols = c(-groupType1, -group1, -groupType2, -group2), names_to = c("var", "type1", "type2"), names_sep = "_") %>%
#             pivot_wider(names_from = c(groupType1, type1, type2), names_prefix = "Year", names_sep = "_") %>%
#             mutate(`Year 2001**n (N)`= paste0(Year2001_n_NA, "\n(", Year2001_N_NA, ")"),
#                    `Year 2001**% [95% CI]` = paste0(Year2001_pct_NA, "\n[", Year2001_pct_low, "-", Year2001_pct_upp, "]"),
#                    `Year 2007**n (N)`= paste0(Year2007_n_NA, "\n(", Year2007_N_NA, ")"),
#                    `Year 2007**% [95% CI]` = paste0(Year2007_pct_NA, "\n[", Year2007_pct_low, "-", Year2007_pct_upp, "]"),
#                    `Year 2012**n (N)`= paste0(Year2012_n_NA, "\n(", Year2012_N_NA, ")"),
#                    `Year 2012**% [95% CI]` = paste0(Year2012_pct_NA, "\n[", Year2012_pct_low, "-", Year2012_pct_upp, "]"),
#                    `Year 2019**n (N)`= paste0(Year2019_n_NA, "\n(", Year2019_N_NA, ")"),
#                    `Year 2019**% [95% CI]` = paste0(Year2019_pct_NA, "\n[", Year2019_pct_low, "-", Year2019_pct_upp, "]")) %>%
#             mutate_all(list(~if_else(str_detect(., "NaN") | str_detect(., "NA") | str_detect(., "0\\n\\(0\\)"), "NR", as.character(.)))) %>%
#             select(group2, groupType2, everything(), -contains("_pct"), -contains("_n"), -group1, -var)
#         }) %>%
#         reduce(left_join, by = c("groupType1", "groupType2")) %>%
#         na.omit()
#     }) %>%
#     filter(!is.na(groupType2), !is.na(group2)) %>%
#     gt(groupname_col = c("group2"), rowname_col = "groupType2") %>%
#     tab_spanner_delim(delim = "**") %>%
#     cols_align(align = "center") %>%
#     tab_header(
#       title = title,
#       subtitle = tabSubtitle
#     ) %>%
#     tab_source_note(source_note = sourceNote) %>%
#     tab_style(
#       style = list(cell_text(size = "large",
#                              weight = "bold",
#                              transform = "capitalize")),
#       locations = cells_title()
#     ) %>%
#     tab_style(
#       style = list(cell_text(weight = "bold",
#                              transform = "capitalize"),
#                    cell_fill(color = "gray90"),
#                    cell_borders(sides = c("left", "right", "top", "bottom"))),
#       locations = cells_row_groups(groups = everything())
#     ) %>%
#     tab_style(
#       style = list(cell_text(weight = "bold",
#                              size = "large"),
#                    cell_fill(color = "gray90"),
#                    cell_borders(sides = c("left", "right", "top", "bottom"))),
#       locations = cells_column_spanners(spanners = everything())
#     ) %>%
#     tab_style(
#       style = list(cell_text(weight = "bold",
#                              size = "small"),
#                    cell_fill(color = "gray90"),
#                    cell_borders(sides = c("left", "right", "top", "bottom"))),
#       locations = cells_column_labels(columns = everything())
#     ) %>%
#     tab_style(
#       style = list(cell_text(size = "small"),
#                    cell_borders(sides = c("left", "right", "top", "bottom"))),
#       locations = cells_body(columns = everything(),
#                              rows = everything())
#     ) %>%
#     tab_style(
#       style = list(cell_text(size = "medium"),
#                    cell_borders(sides = c("left", "right", "top", "bottom"))),
#       locations = cells_stub()
#     ) %>%
#     opt_table_lines(extent = "none") %>%
#     opt_align_table_header(align = "left") %>%
#     opt_row_striping()
#   # }
# }

multi_year <- function(df_table, variable_table, groups_table, title, filterGroup = NA, filterVal = NA, password = NULL, html_output = TRUE){
  
  
  tabSubtitle <-
    paste0(
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


# multi_year(df_table, variable_table, groups_table, "test")

# 
# groups_table <- c("Total", "Age", "Sex", "Ethnicity")
# 
# variable_table = tibble(var = "attemptSuicide", val = 1, varname = "suicide")
# prevTable3(svyyears, variable_table, groups_table, title = "Suicide attempts", filterGroup = NA, filterVal = NA, password = "@$@#%Youth")
# # # #
# singlePrev(df = svyyears, variable = wellbeing, value = 1, varname = "wellbeing", group = "Age")
