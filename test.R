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

