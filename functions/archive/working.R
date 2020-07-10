# singlePrev <- function(df, variable, value, varname, group, filterGroup = NULL, filterVal = NULL, rnd = 2){
#   
#   var <- enquo(variable)
#   val <- enquo(value)
#   grp <- enquo(group)
#   
#   fltGrp <- enquo(filterGroup)
#   
#   grpname <- str_remove(deparse(substitute(grp)), "~")
#   
#   varnameN <- paste0(varname,"_N")
#   varnamen <- paste0(varname,"_n")
#   varnamepct <- paste0(varname,"_pct")
#   
#   
#   varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
#   varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
#   
#   
#   df %>%
#     group_by(group = !! grp) %>%
#     summarise(!! varnamen := unweighted(sum(!! var %in% !! val, na.rm = TRUE)),
#               !! varnameN := unweighted(sum(!is.na(!! var))),
#               !! varnamepct := survey_ratio(!! var %in% !! val, !is.na(!! var), na.rm = TRUE, vartype = "ci", level = 0.95),
#     ) %>%
#     mutate_at(vars(contains("pct")), function(x) formatC(round(x * 100, rnd), digits = if_else(rnd > 1, rnd, 1), format = "f")) %>%
#     mutate_at(vars(contains("n")), function(x) formatC(x, )) %>%
#     mutate(groupType = grpname,
#            group = as.character(group)) %>%
#     mutate(!! varnameNoutput := paste0(.[[2]], "\n", "(", .[[3]], ")"),
#            !! varnamepctoutput := paste0(.[[4]], "\n", "[", .[[5]], "-", .[[6]], "]")) %>%
#     select(groupType, group, !! varnameNoutput, !! varnamepctoutput, -contains("_N"), -contains("_n"), -contains("_pct"))
# }
# 
# 
# singlePrev(svydf, wellbeing, 1, "wellbeing", "Total")            
#             
# groups <- c("Total", "Sex", "Age")
# 
# 
# groups %>%
#   lapply(function(group){
#     svydf %>%
#       group_by(group = !! as.name(group)) %>%
#       summarise(Male = unweighted(sum(sex == "Male", na.rm = TRUE)))
#   })
# 
# 
# svydf %>%
#   summarise(Male = unweighted(sum(sex == "Male", na.rm = TRUE)))
# 
# 
# outputvariablestest = tibble(var = c("wellbeing", "depressed28"), val = c(1,1), label = c("wellbeing", "depressed28"), title = c("wellbeing", "depressed28"))
# 
outputvariablestest2 = tibble(var = c("wellbeing", "depressed28"), val = c(1,1), varname = c("Good Wellbeing", "Depressive Symptoms"))
# 
# outputvariablestest2 = tibble(var = c("wellbeing"), val = c(1), varname = c("Good Wellbeing"))
# 
# test <- list(outputvariablestest2, groups)



# prevTable <- function(variable_table, groups_table){
  
groups %>%
  map_dfr(function(group) {
    
    varTable  %>%
      pmap(function(var, val, varname, grp = group){
        
        varnamen <- paste0(varname,"_n")
        varnameN <- paste0(varname,"_N")
        varnamepct <- paste0(varname,"_pct")
        
        varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
        varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
        
        
        svydf %>%
          filter(!is.na(!! as.name(grp))) %>%
          group_by(groupName = !! as.name(grp)) %>%
          summarise(!! varnamen := unweighted(sum(!! sym(as.character(var)) %in% !! val, na.rm = TRUE)),
                    !! varnameN := unweighted(sum(!is.na(!! sym(as.character(var))))),
                    !! varnamepct := survey_ratio(!! sym(as.character(var)) %in% !! val, !is.na(!! sym(as.character(var))), na.rm = TRUE, vartype = "ci", level = 0.95)
          ) %>%
          mutate_at(vars(contains("_pct")), function(x) formatC(round(x * 100, 2), digits = 2, format = "f")) %>%
          mutate_at(vars(contains("_n")), function(x) formatC(x, big.mark = ",")) %>%
          mutate(!! varnameNoutput := paste0(.[[2]], "\n", "(", .[[3]], ")"),
                 !! varnamepctoutput := paste0(.[[4]], "\n", "[", .[[5]], "-", .[[6]], "]")) %>%
          select(-contains("_N"), -contains("_n"), -contains("_pct")) %>%
          mutate(groupType = as.character(grp)) %>%
          select(groupType, everything())
        
      }) %>% 
      flatten_dfr()
  })
  
str(outdf)
  
  
  
  #   select(-matches("groupName\\d")) %>%
  #   gt(groupname_col = "GroupType", rowname_col = "groupName") %>%
  #   tab_spanner_delim(delim = "**") %>%
  #   cols_align(align = "center") %>%
  # # tab_header(
  # #   title = variable_table %>% distinct(title)) %>%
  #   tab_style(
  #     style = list(cell_text(size = "large",
  #                            weight = "bold",
  #                            transform = "capitalize")),
  #     locations = cells_title()
  #   ) %>%
  #   tab_style(
  #     style = list(cell_text(weight = "bold",
  #                            transform = "capitalize"),
  #                  cell_fill(color = "gray90"),
  #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
  #     locations = cells_row_groups(groups = everything())
  #   ) %>%
  #   tab_style(
  #     style = list(cell_text(weight = "bold",
  #                            size = "large"),
  #                  cell_fill(color = "gray90"),
  #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
  #     locations = cells_column_spanners(spanners = everything())
  #   ) %>%
  #   tab_style(
  #     style = list(cell_text(weight = "bold",
  #                            size = "small"),
  #                  cell_fill(color = "gray90"),
  #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
  #     locations = cells_column_labels(columns = everything())
  #   ) %>%
  #   tab_style(
  #     style = list(cell_text(size = "small"),
  #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
  #     locations = cells_body(columns = everything(),
  #                            rows = everything())
  #   ) %>%
  #   tab_style(
  #     style = list(cell_text(size = "medium"),
  #                  cell_borders(sides = c("left", "right", "top", "bottom"))),
  #     locations = cells_stub()
  #   ) %>%
  #   cols_width(everything() ~ px(100)) %>%
  #   opt_table_lines(extent = "none") %>%
  #   opt_align_table_header(align = "left") %>%
  #   opt_row_striping()
}

groups <- c("Total",
            "Age", 
            "Sex",
            "School_Year",
            "Decile",
            "Deprivation",
            "Urban_Rural")

varTable <- 
  variables %>%
  filter(mainSection == "Mental Health", subSection == "Emotional Wellbeing") %>%
  select(var, val, varname)

prevTable(variable_table = varTable, groups_table = groups)




outputvariablestest <-
  tibble(
    var = c("wellbeing", "hapLife", "depressed28", "thoughtSuicide", "attemptSuicide", "helpBadTime"),
    val = c(1, 1, 1, 1, 1, 1),
    varname = c("Good emotional wellbeing", "Happy or satisfied with life" , "Significant depressive symptoms", "Serious thoughts of suicide", "Attempted suicide", "Difficulty getting help for emotional concerns")
  )

groups_table = c("Total", "Sex", "Age", "Deprivation", "Decile", "Urban_Rural")

prevTable(df = svydf, outputvariablestest, groups_table = groups_table, title = "Emotional wellbeing indicators", password = "@$@#%Youth")





## ethnic

variable_table <-
  tibble(
    var = c("wellbeing", "depressed28", "attemptSuicide"),
    val = c(1, 1, 1),
    varname = c("Good emotional wellbeing", "Significant depressive symptoms", "Attempted suicide")
  )

groups_table = c("Total", "Sex", "Deprivation")

prevTable(df = svydf,
          variable_table = variable_table,
          groups_table = groups_table,
          title = "Emotional wellbeing indicators - European",
          filterGroup = "Ethnicity",
          filterVal = "European",
          password = "@$@#%Youth") %>%
  gtsave("./output.html")


svydf$variables %>% count(Intro5)

groups_table %>%
  map_dfr(function(group) {
    
      variable_table  %>%
      pmap(function(var, val, varname, grp = group){
        
        varnamen <- paste0(varname,"_n")
        varnameN <- paste0(varname,"_N")
        varnamepct <- paste0(varname,"_pct")
        
        varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
        varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
        
        
        svydf %>%
          # filter(!is.na(!! as.name(grp)),
          #        if(!is.na(filterGroup)){
          #          !! as.name(filterGroup) == filterVal
          #        } else {
          #          !is.na(SchoolID)
          #        }
          # ) %>%
          group_by(groupName = !! as.name(grp)) %>%
          # group_by(groupName = sex) %>%
          summarise(!! varnamen := unweighted(sum(!! sym(as.character(var)) %in% !! val, na.rm = TRUE)),
                    !! varnameN := unweighted(sum(!is.na(!! sym(as.character(var))))),
                    !! varnamepct := survey_ratio(!! sym(as.character(var)) %in% !! val, !is.na(!! sym(as.character(var))), na.rm = TRUE, vartype = "ci", level = 0.95)
          ) %>%
          mutate_at(vars(contains("_pct")), function(x) formatC(round(x * 100, 2), digits = 2, format = "f")) %>%
          mutate_at(vars(contains("_n")), function(x) formatC(x, big.mark = ",")) %>%
          mutate(!! varnameNoutput := paste0(.[[2]], "\n", "(", .[[3]], ")"),
                 !! varnamepctoutput := paste0(.[[4]], "\n", "[", .[[5]], "-", .[[6]], "]")) %>%
          select(-contains("_N"), -contains("_n"), -contains("_pct"))
        
      }) %>% 
      reduce(left_join, by = "groupName") %>%
      mutate(groupType = as.character(group)) %>%
      na.omit()
  })
  reduce(full_join, by = c("groupType", "groupName"))

svydf %>%
  # group_by(Ethnicity) %>%
  filter(Ethnicity == "Asian") %>%
  srvyr::summarise(wellbeing_n = unweighted(sum(wellbeing == 1 & Ethnicity == "Asian", na.rm = TRUE)),
                   wellbeing_N = unweighted(sum(!is.na(wellbeing)  & Ethnicity == "Asian", na.rm = TRUE)),
                   wellbeing_pct = survey_ratio(wellbeing == 1, !is.na(wellbeing), na.rm = TRUE))

svydf2 <-
  svydf %>%
  filter(Ethnicity == "Asian")

svydf %>%
  group_by(Ethnicity) %>%
  summarise(n = unweighted(n()))
 
