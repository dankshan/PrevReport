variable_table = tibble(var = "wellbeing", val = 1, varname = "wellbeing")
variable_table = tibble(var = c("transex", "transex", "transex"), val = c("Yes", "No", "Unsure"), varname = c("Yes", "No", "Unsure"))

groups_table = c("Total", "decile_band")

title = "test"

filterGroup = "ethnic_p5"
filterVal = "Asian"


single_year(df = svy_2019_kura,
            variable_table = variable_table,
            groups_table = groups_table,
            title = title,
            filterGroup = NA,
            filterVal = NA,
            password = "@$@#%Youth",
            html_output = FALSE)

svy_2019_kura %>%
  group_by(Neighbourhood_Deprivation) %>%
  summarise(test = survey_ratio(depressed28 == 1, !is.na(depressed28), na.rm = TRUE))


groups <- c("Total", "age", "sex", "Neighbourhood_Deprivation", "urban2")

groups %>%
  map_dfr(function(group){
    svy_2019_kura$variables %>%
      group_by(groupname = group, grouptype = as.factor(!! as.name(group))) %>%
      summarise(Maori = sum(ethnic_maori == 1, na.rm = TRUE),
                Pacific = sum(ethnic_pacific == 1, na.rm = TRUE),
                Asian = sum(ethnic_asian == 1, na.rm = TRUE),
                Other = sum(ethnic_other == 1 | ethnic_MELAA, na.rm = TRUE),
                European = sum(ethnic_european == 1, na.rm = TRUE))
  })
  write_csv("total_ethnicity.csv")

svy_2019_kura$variables %>%
  summarise(one_ethnicity = sum())

svy_2019_kura$variables$trans



groups_table %>%
  map_df(function(group) {
    
    variable_table  %>%
      pmap(function(var, val, varname, grp = group){
        
        varnamen <- paste0(varname,"_n")
        varnameN <- paste0(varname,"_N")
        varnamepct <- paste0(varname,"_pct")
        
        varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
        varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
        
        
        svy_2019_kura %>%
          filter(if(!is.na(!!filterGroup)){
                   !!as.name(filterGroup) == !!filterVal
                 } else {
                   !is.na(Total)
                 }
          ) %>%
          group_by(groupName = !! as.name(grp)) %>%
          summarise(!! varnamen := unweighted(sum(!!as.name(var) %in% !!val,
                                                  na.rm = TRUE)),
                    !! varnameN := unweighted(sum(!is.na(!!as.name(var)),
                                                  na.rm = TRUE)),
                    !! varnamepct := survey_ratio(!!as.name(var) %in% !! val, !is.na(!!as.name(var)), na.rm = TRUE, vartype = "ci", level = 0.95)
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
