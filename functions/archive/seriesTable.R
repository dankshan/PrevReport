seriesTable <- function(df, variable_table, groups_table, rounding = 2, subheading = TRUE){
  
  singlePrev <- function(df, variable, value, varname, group, rnd){
    
    var <- enquo(variable)
    val <- enquo(value)
    grp <- enquo(group)
    
    grpname <- str_remove(deparse(substitute(grp)), "~")
    
    varnameN <- paste0("2001.", varname,".N")
    varnamen <- paste0("2001.", varname,".n")
    varnamepct <- paste0("2001.", varname,".pct")
    
    
    varnameNoutput <- paste0(varname,".n")
    varnamepctoutput <- paste0(varname, ".%")
    
    #build the table
    df %>%
      filter(!is.na(!! grp)) %>%
      group_by(Year, group = !! grp) %>%
      summarise(!! varnamen := unweighted(sum(!! var %in% !! val, na.rm = TRUE)),
                !! varnameN := unweighted(sum(!is.na(!! var))),
                !! varnamepct := survey_ratio(!! var %in% !! val, !is.na(!! var), na.rm = TRUE, vartype = "ci", level = 0.95),
      ) %>%
      mutate(groupType = grpname,
             group = as.character(group))
      # mutate_at(vars(contains("_pct")), function(x) formatC(round(x * 100, rnd), digits = if_else(rnd > 1, rnd, 1), format = "f")) %>%
      # mutate_at(vars(contains("_n")), function(x) formatC(x, )) %>%
      # mutate(!! varnameNoutput := paste0(.[[3]], "\n", "(", .[[4]], ")"),
      #        !! varnamepctoutput := paste0(.[[5]], "\n", "[", .[[6]], "-", .[[7]], "]")) %>%
      # select(Year, groupType, group, !! varnameNoutput, !! varnamepctoutput, -contains("_N"), -contains("_n"), -contains("_pct"))
  }
  
  outdf <- as_tibble()
  
  for (j in seq_along(groups_table)) {
    
    temptable <- tibble(Year = NA_character_,
                        groupType = NA_character_,
                        group = NA_character_)
    
    for (k in 1:nrow(variable_table)) {
      
      temptable <-
        temptable %>%
        full_join(singlePrev(df,
                             variable = !! sym(as.character(variable_table[k,1])),
                             value = as.numeric(variable_table[k,2]),
                             varname = as.character(variable_table[k,3]),
                             group = !! sym(groups_table[j]),
                             rnd = rounding),
                  by = c("Year", "groupType", "group")) %>%
        na.omit()
      
    }
    
    outdf <- bind_rows(outdf, temptable)
    
  }
  
  outdf <- 
    outdf %>%
    filter(group != "Another way")
  
  return(outdf)

  
  # rowname_col = "group", 
  
  # outdf %>%
  #   gt(groupname_col = "groupType") %>%
  #   tab_spanner_delim(delim = ".") %>%
  #   cols_align(align = "center") %>%
  #   tab_style(
  #     style = list(cell_text(weight = "bold",
  #                            transform = "capitalize"),
  #                  cell_fill(color = "gray90")),
  #     locations = cells_row_groups(groups = everything())
  #   ) %>%
  #   tab_style(
  #     style = list(cell_text(weight = "bold",
  #                            v_align = "bottom",
  #                            size = "large")),
  #     locations = cells_column_spanners(spanners = everything())
  #   ) %>%
  #   tab_style(
  #     style = cell_borders(sides = c("left", "right")),
  #     locations = cells_body(columns = everything(),
  #                            rows = everything()
  #     )
  #   ) %>%
  #   tab_options(
  #     data_row.padding = pct(1),
  #     container.width = pct(100),
  #     table.width = pct(80)
  #   ) %>%
  #   opt_row_striping(row_striping = TRUE)
}

