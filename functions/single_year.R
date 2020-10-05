single_year <- function(df, variable_table, groups_table, title, footnote = NA, filterGroup = NA, filterVal = NA, password, html_output = TRUE){
  
  if(password == saved_password) {
    
    # tabSubtitle <- NULL
      
    
    tabSubtitle <-
      paste0(
        if(is.na(filterGroup)){
          NULL
        } else if(filterGroup == "ethnic_p5"){
          paste0("Filtered for ", filterVal, " prioritised ethnicity. \n")
        } else if(filterGroup %in% c("ethnic_maori", "ethnic_pacific", "ethnic_asian", "ethnic_other", "ethnic_european")){
          paste0("Filtered for ", str_remove(filterGroup, "ethnic_"), " total ethnicity. \n")
        }
      )
    
    ## create table footer
    sourceNote <<- 
      paste0(
        if(!is.na(footnote)) {paste0(footnote)} else {NULL},
        if("Neighbourhood_Deprivation" %in% groups_table) {paste0("NZ Deprivation Index 2018, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
        if("School_Decile" %in% groups_table) {paste0("School Decile, Low (1-3), Medium (4-7), High (8-10). \n")} else {NULL},
        if("Disability" %in% groups_table) {paste0("Chronic condition determined by long term health problem, disability, or pain. \n")} else {NULL},
        if("Urban_Rural" %in% groups_table) {paste0("Urban (population of 10,000 or more), Small towns (population between 1,000 and 9,999 people), Rural (population less than 1,000). \n")} else {NULL},
        if("Ethnicity" %in% groups_table | "Ethnicity" %in% filterGroup) {paste0("Ethnicity is prioritised. \n")} else {NULL}
      )
    
    output <-  
      groups_table %>%
      map_df(function(group) {
        
        variable_table  %>%
          pmap(function(var, val, varname, grp = group){
            
            varnamen <- paste0(varname,"_n")
            varnameN <- paste0(varname,"_N")
            varnamepct <- paste0(varname,"_pct")
            
            varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
            varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
            
            
            df %>%
              filter(if(!is.na(!!filterGroup)){
                !!as.name(filterGroup) == filterVal
              } else {
                !is.na(Total)
              }
              ) %>%
              filter(!is.na(!!as.name(grp))) %>%
              group_by(groupName = !! as.name(grp)) %>%
              summarise(!! varnamen := unweighted(sum(!!as.name(var) %in% c(as.numeric(str_split(!!val, pattern = ",", simplify = TRUE))), na.rm = TRUE)),
                        !! varnameN := unweighted(sum(!is.na(!!as.name(var)), na.rm = TRUE)),
                        !! varnamepct := survey_ratio(!!as.name(var) %in% c(as.numeric(str_split(!!val, pattern = ",", simplify = TRUE))), !is.na(!!as.name(var)), na.rm = TRUE, vartype = "ci", level = 0.95)
              ) %>%
              mutate_at(vars(contains("_pct")), function(x) formatC(round(x * 100, 1), digits = 1, format = "f")) %>%
              mutate_at(vars(contains("_n")), function(x) formatC(x, big.mark = ",")) %>%
              mutate(!! varnameNoutput := paste0(.[[2]], "\n", "(", .[[3]], ")"),
                     !! varnamepctoutput := paste0(.[[4]], "\n", "[", .[[5]], "-", .[[6]], "]")) %>%
              select(-contains("_N"), -contains("_n"), -contains("_pct"))
            
          }) %>%
          reduce(left_join, by = "groupName") %>%
          mutate(groupType = as.character(group)) %>%
          na.omit()
      }) %>%
      select(groupType, groupName, everything())
    
    if(html_output == TRUE) {
      
      output %>%
        gt(groupname_col = "groupType", rowname_col = "groupName") %>%
        tab_spanner_delim(delim = "**") %>%
        cols_align(align = "center") %>%
        tab_header(
          title = title,
          subtitle = tabSubtitle) %>%
        tab_source_note(source_note = sourceNote) %>%
        tab_style(
          style = list(cell_text(size = "medium",
                                 weight = "bold",
                                 transform = "capitalize")),
          locations = cells_title()
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold",
                                 transform = "capitalize",
                                 size = "small"),
                       cell_fill(color = "gray90"),
                       cell_borders(sides = c("left", "right", "top", "bottom"))),
          locations = cells_row_groups(groups = everything())
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold",
                                 size = "small"),
                       cell_fill(color = "gray90"),
                       cell_borders(sides = c("left", "right", "top", "bottom"))),
          locations = cells_column_spanners(spanners = everything())
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold",
                                 size = "x-small"),
                       cell_fill(color = "gray90"),
                       cell_borders(sides = c("left", "right", "top", "bottom"))),
          locations = cells_column_labels(columns = everything())
        ) %>%
        tab_style(
          style = list(cell_text(size = "x-small"),
                       cell_borders(sides = c("left", "right", "top", "bottom"))),
          locations = cells_body(columns = everything(),
                                 rows = everything())
        ) %>%
        tab_style(
          style = list(cell_text(size = "small"),
                       cell_borders(sides = c("left", "right", "top", "bottom"))),
          locations = cells_stub()
        ) %>%
        # cols_width(everything() ~ px(100)) %>%
        opt_table_lines(extent = "none") %>%
        opt_align_table_header(align = "left") %>%
        opt_row_striping()
      
    } else if(html_output == FALSE) {
      
      return(output)
      
    }
  }
}

