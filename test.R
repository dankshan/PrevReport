variable_table = tibble(var = "Cook8", val = 2, varname = "Cook8")
variable_table = tibble(var = c("transex", "transex", "transex"), val = c("Yes", "No", "Unsure"), varname = c("Yes", "No", "Unsure"))

variable_table <-
  variables %>%
  filter(mainSection == "Aatest") %>%
  select(-title, -mainSection, -subSection) %>%
  mutate(val2 = as.numeric(str_split(val, pattern = ",", simplify = TRUE)),
         val3 = as.numeric)

str(paste("1", "2", "3", sep = ","))


groups_table = c("Total")

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


test2 <- as.numeric(str_split("1,2", pattern = ",", simplify = TRUE))


######-----

devtools::install_github("hadley/multidplyr")
library(parrallel)

##### ------

df_table <- c("svy_2001","svy_2007", "svy_2012", "svy_2019")

groups_table <- c("Total", "Age")

variable_table = tibble(var = "wellbeing", val = 1, varname = "wellbeing")

plan(multisession, workers = 4)

df_table %>%
  map_dfr(function(df) {
    
    groups_table %>%
      map_dfr(function(group) {
        
        variable_table  %>%
          pmap(function(var, val, varname, grp = group){
            
            varnamen <- paste0(varname, "_n")
            varnameN <- paste0(varname, "_N")
            varnamepct <- paste0(varname, "_pct")
            
            varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
            varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
            
            get(df) %>%
              filter(!is.na(!!as.name(grp))) %>%
              group_by(group1 = Year, group2 = !!as.name(grp)) %>%
              summarise(!! varnamen := unweighted(sum(!!as.name(var) %in% !!val, na.rm = TRUE)),
                        !! varnameN := unweighted(sum(!is.na(!!as.name(var)), na.rm = TRUE)),
                        !! varnamepct := survey_ratio(!!as.name(var) %in% !!val, !is.na(!!as.name(var)), na.rm = TRUE, vartype = "ci", level = 0.95)
              ) %>%
              mutate_at(vars(contains("pct")), function(x) formatC(round(x * 100, 1), digits = 1, format = "f")) %>%
              mutate_at(vars(contains("n")), function(x) formatC(x, big.mark=",")) %>%
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



variable_table  %>%
  future_pmap(function(var, val, varname, grp = "Ethnicity"){
    
    varnamen <- paste0(varname, "_n")
    varnameN <- paste0(varname, "_N")
    varnamepct <- paste0(varname, "_pct")
    
    varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
    varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
    
    svy_2019 %>%
      filter(
          Ethnicity == "Asian"
      ) %>%
      filter(!is.na(!!as.name(grp))) %>%
      group_by(group1 = Year, group2 = !!as.name(grp)) %>%
      summarise(!! varnamen := unweighted(sum(!!as.name(var) %in% !!val, na.rm = TRUE)),
                !! varnameN := unweighted(sum(!is.na(!!as.name(var)), na.rm = TRUE)),
                !! varnamepct := survey_ratio(!!as.name(var) %in% !!val, !is.na(!!as.name(var)), na.rm = TRUE, vartype = "ci", level = 0.95)
      ) %>%
      mutate_at(vars(contains("pct")), function(x) formatC(round(x * 100, 1), digits = 1, format = "f")) %>%
      mutate_at(vars(contains("n")), function(x) formatC(x, big.mark=",")) %>%
      mutate(groupType1 = "Year",
             groupType2 = paste0(grp))
  })

groups_table <- c("Total", "Sex", "School_Year")

variable_table = tibble(var = "depressed28", val = 1, varname = "depressed28")

groups_table %>%
  map_df(function(group) {
    
    variable_table  %>%
      pmap(function(var, val, varname){
        
        varnamen <- paste0(varname,"_n")
        varnameN <- paste0(varname,"_N")
        varnamepct <- paste0(varname,"_pct")
        
        varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
        varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
        
        
        svy_2019_kura %>%
          # filter(if(!is.na(!!filterGroup)){
          #   !!as.name(filterGroup) == filterVal
          # } else {
          #   !is.na(Total)
          # }
          # ) %>%
          # group_by(groupName = !!as.name(group)) %>%
          group_by(ethnic_p5) %>%
          summarise(!! varnamen := unweighted(sum(!!as.name(var) %in% c(as.numeric(str_split(!!val, pattern = ",", simplify = TRUE))), na.rm = TRUE)),
                    !! varnameN := unweighted(sum(!is.na(!!as.name(var)), na.rm = TRUE)),
                    !! varnamepct := survey_ratio(!!as.name(var) %in% c(as.numeric(str_split(!!val, pattern = ",", simplify = TRUE))), !is.na(!!as.name(var)), na.rm = TRUE, vartype = "ci", level = 0.95)
          )
          # mutate_at(vars(contains("_pct")), function(x) formatC(round(x * 100, 1), digits = 1, format = "f")) %>%
          # mutate_at(vars(contains("_n")), function(x) formatC(x, big.mark = ",")) %>%
          # mutate(!! varnameNoutput := paste0(.[[2]], "\n", "(", .[[3]], ")"),
          #        !! varnamepctoutput := paste0(.[[4]], "\n", "[", .[[5]], "-", .[[6]], "]")) %>%
          # select(-contains("_N"), -contains("_n"), -contains("_pct"))
        
      })
      # reduce(left_join, by = "groupName") %>%
      # mutate(groupType = as.character(group)) %>%
      # na.omit()
  })
  # select(groupType, groupName, everything())
  
groups_table %>%
  map(function(group) {
    svy_2019_kura %>%
      group_by(group = !!as.name(group)) %>%
      summarise(
        depressed_n = unweighted(sum(depressed28 == 2, na.rm = TRUE)),
        depressed_N = unweighted(sum(!is.na(depressed28), na.rm = TRUE)),
        depressed_pct = survey_ratio(depressed28 == 2, !is.na(depressed28), na.rm = TRUE)
      ) %>%
      mutate(groupName = "Test")
  })
  reduce(left_join, by = "groupName")


svy_2019_kura %>%
  group_by(ethnic_p6) %>%
  summarise(
    Cook8_pct = survey_ratio(Cook8 == 2, !is.na(Cook8), na.rm = TRUE)
  )

svy_2019_kura$variables %>%
  group_by(ethnic_p5) %>%
  summarise(
    Cook8_pct = sum(Cook8 == 2, na.rm = TRUE)
  )

?survey_ratio

groups_table %>%
  map(function(group) {
    
    variable_table  %>%
      pmap(function(var, val, varname){
        
        varnamen <- paste0(varname,"_n")
        varnameN <- paste0(varname,"_N")
        varnamepct <- paste0(varname,"_pct")
        
        varnameNoutput <- paste0(varname,"**n", "\n", "(N)")
        varnamepctoutput <- paste0(varname, "**%", "\n", "[95% CI]")
        
        
        svy_2019_kura %>%
          filter(ethnic_p5 == "Asian") %>%
          group_by(!!as.name(group)) %>%
          summarise(!! varnamen := unweighted(sum(!!as.name(var) %in% c(as.numeric(str_split(!!val, pattern = ",", simplify = TRUE))), na.rm = TRUE)),
                    !! varnameN := unweighted(sum(!is.na(!!as.name(var)), na.rm = TRUE)),
                    !! varnamepct := survey_ratio(!!as.name(var) %in% c(as.numeric(str_split(!!val, pattern = ",", simplify = TRUE))), !is.na(!!as.name(var)), na.rm = TRUE, vartype = "ci", level = 0.95)
          )
        
      })
  })

