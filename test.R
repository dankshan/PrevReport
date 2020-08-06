variable_table = tibble(var = "wellbeing", val = 1, varname = "wellbeing")
variable_table = tibble(var = c("transex", "transex", "transex"), val = c("Yes", "No", "Unsure"), varname = c("Yes", "No", "Unsure"))

groups_table = c("Total")

title = "test"

single_year(df = svy_2019_kura,
            variable_table = variable_table,
            groups_table = groups_table,
            title = title,
            filterGroup = NA,
            filterVal = NA,
            password = "@$@#%Youth")

str(variables)


groups <- c("Total", "age", "sex", "NZDep_band3", "urban2")

groups %>%
  map_dfr(function(group){
    svy_2019_kura$variables %>%
      group_by(groupname = group, grouptype = as.factor(!! as.name(group))) %>%
      summarise(Maori = sum(ethnic_maori == 1, na.rm = TRUE),
                Pacific = sum(ethnic_pacific == 1, na.rm = TRUE),
                Asian = sum(ethnic_asian == 1, na.rm = TRUE),
                Other = sum(ethnic_other == 1 | ethnic_MELAA, na.rm = TRUE),
                European = sum(ethnic_european == 1, na.rm = TRUE))
  }) %>%
  write_csv("total_ethnicity.csv")

svy_2019_kura$variables %>%
  summarise(one_ethnicity = sum())

svy_2019_kura$variables$trans

