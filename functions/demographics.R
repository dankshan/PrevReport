#this all needs to be re-worked to be more efficient

demTable <- function(type, filterGroup = NULL, filterVal = NULL, include_pilot = TRUE, include_wharekura = TRUE, password){
  
  if(password == "@$@#%Youth") {
  
  fltGrp <- enquo(filterGroup)
  
  if(type == "Schools") {
    
    df <-
      demographics %>%
      filter(
        if(is.null(!! fltGrp)){
          !is.na(ECSchoolID)
        } else {
          !! fltGrp %in% filterVal
        }
      ) %>%
      filter(
        if(include_pilot == TRUE){
          pilot %in% c(0,1)
        } else if(include_pilot == FALSE){
          pilot == 0
        }
      ) %>%
      filter(
        if(include_wharekura == TRUE){
          Wharekura %in% c(0,1)
        } else if(include_wharekura == FALSE){
          Wharekura == 0
        }
      )
    
    df %>%
      group_by(GroupName = "Total", Group = Total) %>%
      summarise(Nationally_Eligible.Number = sum(NationalEligible ==1 | WharekuraNationalEligible == 1, na.rm = TRUE),
                Eligible.Number = sum(Eligible ==1 | WharekuraEligible == 1, na.rm = TRUE),
                Invited.Number = sum(Invited == 1 | WharekuraInvited == 1, na.rm = TRUE),
                Participating.Number = sum(Participated == 1, na.rm = TRUE)) %>%
      bind_rows(
        df %>%
          group_by(GroupName = "Education Region", Group = EducationRegion) %>%
          summarise(Nationally_Eligible.Number = sum(NationalEligible ==1 | WharekuraNationalEligible == 1, na.rm = TRUE),
                    Eligible.Number = sum(Eligible ==1 | WharekuraEligible == 1, na.rm = TRUE),
                    Invited.Number = sum(Invited == 1 | WharekuraInvited == 1, na.rm = TRUE),
                    Participating.Number = sum(Participated == 1, na.rm = TRUE))
      ) %>%
      bind_rows(
        df %>%
          group_by(GroupName = "Type", Group = Type) %>%
          summarise(Nationally_Eligible.Number = sum(NationalEligible ==1 | WharekuraNationalEligible == 1, na.rm = TRUE),
                    Eligible.Number = sum(Eligible ==1 | WharekuraEligible == 1, na.rm = TRUE),
                    Invited.Number = sum(Invited == 1 | WharekuraInvited == 1, na.rm = TRUE),
                    Participating.Number = sum(Participated == 1, na.rm = TRUE))
      ) %>%
      bind_rows(
        df %>%
          group_by(GroupName = "Kura Kaupapa Māori", Group = Kura) %>%
          summarise(Nationally_Eligible.Number = sum(NationalEligible ==1 | WharekuraNationalEligible == 1, na.rm = TRUE),
                    Eligible.Number = sum(Eligible ==1 | WharekuraEligible == 1, na.rm = TRUE),
                    Invited.Number = sum(Invited == 1 | WharekuraInvited == 1, na.rm = TRUE),
                    Participating.Number = sum(Participated == 1, na.rm = TRUE))
      ) %>%
      bind_rows(
        df %>%
          group_by(GroupName = "School Size", Group = SchoolSize) %>%
          summarise(Nationally_Eligible.Number = sum(NationalEligible ==1 | WharekuraNationalEligible == 1, na.rm = TRUE),
                    Eligible.Number = sum(Eligible ==1 | WharekuraEligible == 1, na.rm = TRUE),
                    Invited.Number = sum(Invited == 1 | WharekuraInvited == 1, na.rm = TRUE),
                    Participating.Number = sum(Participated == 1, na.rm = TRUE))
      ) %>%
      bind_rows(
        df %>%
          group_by(GroupName = "Decile Band", Group = DecileBand) %>%
          summarise(Nationally_Eligible.Number = sum(NationalEligible ==1 | WharekuraNationalEligible == 1, na.rm = TRUE),
                    Eligible.Number = sum(Eligible ==1 | WharekuraEligible == 1, na.rm = TRUE),
                    Invited.Number = sum(Invited == 1 | WharekuraInvited == 1, na.rm = TRUE),
                    Participating.Number = sum(Participated == 1, na.rm = TRUE))
      ) %>%
      bind_rows(
        df %>%
          group_by(GroupName = "Decile", Group = Decile) %>%
          summarise(Nationally_Eligible.Number = sum(NationalEligible ==1 | WharekuraNationalEligible == 1, na.rm = TRUE),
                    Eligible.Number = sum(Eligible ==1 | WharekuraEligible == 1, na.rm = TRUE),
                    Invited.Number = sum(Invited == 1 | WharekuraInvited == 1, na.rm = TRUE),
                    Participating.Number = sum(Participated == 1, na.rm = TRUE))
      ) %>%
      mutate(Nationally_Eligible.Percent = case_when(GroupName == "Total" ~ Nationally_Eligible.Number/sum(Nationally_Eligible.Number),
                                          GroupName == "Education Region" ~ Nationally_Eligible.Number/sum(Nationally_Eligible.Number),
                                          GroupName == "Type" ~ Nationally_Eligible.Number/sum(Nationally_Eligible.Number),
                                          GroupName == "Kura Kaupapa Māori" ~ Nationally_Eligible.Number/sum(Nationally_Eligible.Number),
                                          GroupName == "School Size" ~ Nationally_Eligible.Number/sum(Nationally_Eligible.Number),
                                          GroupName == "Decile Band" ~ Nationally_Eligible.Number/sum(Nationally_Eligible.Number),
                                          GroupName == "Decile" ~ Nationally_Eligible.Number/sum(Nationally_Eligible.Number)),
             Eligible.Percent = case_when(GroupName == "Total" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Education Region" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Type" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Kura Kaupapa Māori" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "School Size" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Decile Band" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Decile" ~ Eligible.Number/sum(Eligible.Number)),
             Invited.Percent = case_when(GroupName == "Total" ~ Invited.Number/sum(Invited.Number),
                                         GroupName == "Education Region" ~ Invited.Number/sum(Invited.Number),
                                         GroupName == "Type" ~ Invited.Number/sum(Invited.Number),
                                         GroupName == "Kura Kaupapa Māori" ~ Invited.Number/sum(Invited.Number),
                                         GroupName == "School Size" ~ Invited.Number/sum(Invited.Number),
                                         GroupName == "Decile Band" ~ Invited.Number/sum(Invited.Number),
                                         GroupName == "Decile" ~ Invited.Number/sum(Invited.Number)),
             Participating.Percent = case_when(GroupName == "Total" ~ Participating.Number/sum(Participating.Number),
                                               GroupName == "Education Region" ~ Participating.Number/sum(Participating.Number),
                                               GroupName == "Type" ~ Participating.Number/sum(Participating.Number),
                                               GroupName == "Kura Kaupapa Māori" ~ Participating.Number/sum(Participating.Number),
                                               GroupName == "School Size" ~ Participating.Number/sum(Participating.Number),
                                               GroupName == "Decile Band" ~ Participating.Number/sum(Participating.Number),
                                               GroupName == "Decile" ~ Participating.Number/sum(Participating.Number))) %>%
      filter(Eligible.Number >= 1) %>%
      mutate_at(vars(contains("percent")), function(x) formatC(round(x * 100, 2), digits = 1, format = "f")) %>%
      mutate_at(vars(contains("number")), function(x) formatC(x, big.mark=",", format = "d")) %>%
      select(GroupName, Group, Eligible.Number, Eligible.Percent, Invited.Number, Invited.Percent, Participating.Number, Participating.Percent) %>%
      gt(groupname_col = "GroupName", rowname_col = "Group") %>%
      tab_spanner_delim(delim = ".") %>%
      cols_align(align = "center") %>%
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
    
  } else if(type == "Students") {
    
    df <-
      demographics2 %>%
      filter(
        if(is.null(!! fltGrp)){
          !is.na(ECSchoolID)
        } else {
          !! fltGrp %in% filterVal
        }
      ) %>%
      filter(
        if(include_pilot == TRUE){
          pilot %in% c(0,1)
        } else if(include_pilot == FALSE){
          pilot == 0
        }
      ) %>%
      filter(
        if(include_wharekura == TRUE){
          Wharekura %in% c(0,1)
        } else if(include_wharekura == FALSE){
          Wharekura == 0
        }
      )
    
    df %>%
      group_by(GroupName = Group1, Group = Group2) %>%
      summarise(Eligible.Number = sum(value[ECSurv == "EC" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
                Invited.number = sum(value[ECSurv == "EC" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
                Participating.number = sum(value[ECSurv == "EC" & Participated == 1], na.rm = TRUE),
                Surveyed.number = sum(value[ECSurv == "survey" & (Participated == 1)], na.rm = TRUE)) %>%
      bind_rows(
        df %>%
          group_by(GroupName = "Decile", Group = Decile) %>%
          summarise(Eligible.Number = sum(value[ECSurv == "EC" & Group1 == "Total" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
                    Invited.number = sum(value[ECSurv == "EC" & Group1 == "Total" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
                    Participating.number = sum(value[ECSurv == "EC" & Group1 == "Total" & Participated == 1], na.rm = TRUE),
                    Surveyed.number = sum(value[ECSurv == "survey" & Group1 == "Total" & Participated == 1], na.rm = TRUE))
      )%>%
      bind_rows(
        df %>%
          group_by(GroupName = "Education Region", Group = EducationRegion) %>%
          summarise(Eligible.Number = sum(value[ECSurv == "EC" & Group1 == "Total" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
                    Invited.number = sum(value[ECSurv == "EC" & Group1 == "Total" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
                    Participating.number = sum(value[ECSurv == "EC" & Group1 == "Total" & Participated == 1], na.rm = TRUE),
                    Surveyed.number = sum(value[ECSurv == "survey" & Group1 == "Total" & Participated == 1], na.rm = TRUE))
      ) %>%
      bind_rows(
        df %>%
          group_by(GroupName = "Kura Kaupapa Maori", Group = Kura) %>%
          summarise(Eligible.Number = sum(value[ECSurv == "EC" & Group1 == "Total" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
                    Invited.number = sum(value[ECSurv == "EC" & Group1 == "Total" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
                    Participating.number = sum(value[ECSurv == "EC" & Group1 == "Total" & Participated == 1], na.rm = TRUE),
                    Surveyed.number = sum(value[ECSurv == "survey" & Group1 == "Total" & Participated == 1], na.rm = TRUE))
      ) %>%
      mutate(Eligible.percent = case_when(GroupName == "Total" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Age" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Ethnicity" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Sex" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Year" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Decile" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Education Region" ~ Eligible.Number/sum(Eligible.Number),
                                          GroupName == "Kura Kaupapa Maori" ~ Eligible.Number/sum(Eligible.Number)),
             Invited.percent = case_when(GroupName == "Total" ~ Invited.number/sum(Invited.number),
                                         GroupName == "Age" ~ Invited.number/sum(Invited.number),
                                         GroupName == "Ethnicity" ~ Invited.number/sum(Invited.number),
                                         GroupName == "Sex" ~ Invited.number/sum(Invited.number),
                                         GroupName == "Year" ~ Invited.number/sum(Invited.number),
                                         GroupName == "Decile" ~ Invited.number/sum(Invited.number),
                                         GroupName == "Education Region" ~ Invited.number/sum(Invited.number),
                                         GroupName == "Kura Kaupapa Maori" ~ Invited.number/sum(Invited.number)),
             Participating.percent = case_when(GroupName == "Total" ~ Participating.number/sum(Participating.number),
                                               GroupName == "Age" ~ Participating.number/sum(Participating.number),
                                               GroupName == "Ethnicity" ~ Participating.number/sum(Participating.number),
                                               GroupName == "Sex" ~ Participating.number/sum(Participating.number),
                                               GroupName == "Year" ~ Participating.number/sum(Participating.number),
                                               GroupName == "Decile" ~ Participating.number/sum(Participating.number),
                                               GroupName == "Education Region" ~ Participating.number/sum(Participating.number),
                                               GroupName == "Kura Kaupapa Maori" ~ Participating.number/sum(Participating.number)),
             Surveyed.percent = case_when(GroupName == "Total" ~ Surveyed.number/sum(Surveyed.number),
                                          GroupName == "Age" ~ Surveyed.number/sum(Surveyed.number),
                                          GroupName == "Ethnicity" ~ Surveyed.number/sum(Surveyed.number),
                                          GroupName == "Sex" ~ Surveyed.number/sum(Surveyed.number),
                                          GroupName == "Year" ~ Surveyed.number/sum(Surveyed.number),
                                          GroupName == "Decile" ~ Surveyed.number/sum(Surveyed.number),
                                          GroupName == "Education Region" ~ Surveyed.number/sum(Surveyed.number),
                                          GroupName == "Kura Kaupapa Maori" ~ Surveyed.number/sum(Surveyed.number))) %>%
      mutate_at(vars(contains("percent")), function(x) formatC(round(x * 100, 2), digits = 1, format = "f")) %>%
      mutate_at(vars(contains("number")), function(x) formatC(x, big.mark=",", format = "d")) %>%
      filter(Eligible.Number >= 1) %>%
      select(GroupName, Group, Eligible.Number, Eligible.percent, Invited.number, Invited.percent, Participating.number, Participating.percent, Surveyed.number, Surveyed.percent) %>%
      rename("Students at Eligible Schools.Number" = Eligible.Number,
             "Students at Eligible Schools.Percent" = Eligible.percent,
             "Students at Invited Schools.Number" = Invited.number,
             "Students at Invited Schools.Percent" = Invited.percent,
             "Students at Participating Schools.Number" = Participating.number,
             "Students at Participating Schools.NPercent" =  Participating.percent,
             "Surveyed Students.Number" = Surveyed.number,
             "Surveyed Students.Percent" = Surveyed.percent) %>%
      gt(groupname_col = "GroupName", rowname_col = "Group") %>%
      tab_spanner_delim(delim = ".") %>%
      cols_align(align = "center") %>%
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
    
  }
  
  }
  
}

# demTable(type = "Students", filterGroup = Decile, filterVal = 1)
  





# studentsDem <- function(df, filterGroup = NULL, filterVal = NULL) {
#   
#   fltGrp <- enquo(filterGroup)
#   
#   df <-
#     df %>%
#     filter(
#       if(is.null(!! fltGrp)){
#         !is.na(ECSchoolID)
#       } else {
#         !! fltGrp %in% filterVal
#       }
#     )
#   
#   df %>%
#     group_by(GroupName, Group) %>%
#     summarise(Eligible.Number = sum(value[ECSurv == "EC" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
#               Invited.number = sum(value[ECSurv == "EC" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
#               Participating.number = sum(value[ECSurv == "EC" & Participated == 1], na.rm = TRUE),
#               Surveyed.number = sum(value[ECSurv == "survey" & (Participated == 1)], na.rm = TRUE)) %>%
#     bind_rows(
#       df %>%
#         group_by(GroupName = "Decile", Group = Decile) %>%
#         summarise(Eligible.Number = sum(value[ECSurv == "EC" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
#                   Invited.number = sum(value[ECSurv == "EC" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
#                   Participating.number = sum(value[ECSurv == "EC" & Participated == 1], na.rm = TRUE),
#                   Surveyed.number = sum(value[ECSurv == "survey" & (Participated == 1)], na.rm = TRUE))
#     )%>%
#     bind_rows(
#       df %>%
#         group_by(GroupName = "Education Region", Group = EducationRegion) %>%
#         summarise(Eligible.Number = sum(value[ECSurv == "EC" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
#                   Invited.number = sum(value[ECSurv == "EC" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
#                   Participating.number = sum(value[ECSurv == "EC" & Participated == 1], na.rm = TRUE),
#                   Surveyed.number = sum(value[ECSurv == "survey" & (Participated == 1)], na.rm = TRUE))
#     ) %>%
#     bind_rows(
#       df %>%
#         group_by(GroupName = "Kura Kaupapa Maori", Group = Kura) %>%
#         summarise(Eligible.Number = sum(value[ECSurv == "EC" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
#                   Invited.number = sum(value[ECSurv == "EC" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
#                   Participating.number = sum(value[ECSurv == "EC" & Participated == 1], na.rm = TRUE),
#                   Surveyed.number = sum(value[ECSurv == "survey" & (Participated == 1)], na.rm = TRUE))
#     ) %>%
#     mutate(Eligible.percent = case_when(GroupName == "Total" ~ Eligible.Number/sum(Eligible.Number),
#                                         GroupName == "Age" ~ Eligible.Number/sum(Eligible.Number),
#                                         GroupName == "Ethnicity" ~ Eligible.Number/sum(Eligible.Number),
#                                         GroupName == "Sex" ~ Eligible.Number/sum(Eligible.Number),
#                                         GroupName == "Year" ~ Eligible.Number/sum(Eligible.Number),
#                                         GroupName == "Decile" ~ Eligible.Number/sum(Eligible.Number),
#                                         GroupName == "Education Region" ~ Eligible.Number/sum(Eligible.Number),
#                                         GroupName == "Kura Kaupapa Maori" ~ Eligible.Number/sum(Eligible.Number)),
#            Invited.percent = case_when(GroupName == "Total" ~ Invited.number/sum(Invited.number),
#                                        GroupName == "Age" ~ Invited.number/sum(Invited.number),
#                                        GroupName == "Ethnicity" ~ Invited.number/sum(Invited.number),
#                                        GroupName == "Sex" ~ Invited.number/sum(Invited.number),
#                                        GroupName == "Year" ~ Invited.number/sum(Invited.number),
#                                        GroupName == "Decile" ~ Invited.number/sum(Invited.number),
#                                        GroupName == "Education Region" ~ Invited.number/sum(Invited.number),
#                                        GroupName == "Kura Kaupapa Maori" ~ Invited.number/sum(Invited.number)),
#            Participating.percent = case_when(GroupName == "Total" ~ Participating.number/sum(Participating.number),
#                                              GroupName == "Age" ~ Participating.number/sum(Participating.number),
#                                              GroupName == "Ethnicity" ~ Participating.number/sum(Participating.number),
#                                              GroupName == "Sex" ~ Participating.number/sum(Participating.number),
#                                              GroupName == "Year" ~ Participating.number/sum(Participating.number),
#                                              GroupName == "Decile" ~ Participating.number/sum(Participating.number),
#                                              GroupName == "Education Region" ~ Participating.number/sum(Participating.number),
#                                              GroupName == "Kura Kaupapa Maori" ~ Participating.number/sum(Participating.number)),
#            Surveyed.percent = case_when(GroupName == "Total" ~ Surveyed.number/sum(Surveyed.number),
#                                         GroupName == "Age" ~ Surveyed.number/sum(Surveyed.number),
#                                         GroupName == "Ethnicity" ~ Surveyed.number/sum(Surveyed.number),
#                                         GroupName == "Sex" ~ Surveyed.number/sum(Surveyed.number),
#                                         GroupName == "Year" ~ Surveyed.number/sum(Surveyed.number),
#                                         GroupName == "Decile" ~ Surveyed.number/sum(Surveyed.number),
#                                         GroupName == "Education Region" ~ Surveyed.number/sum(Surveyed.number),
#                                         GroupName == "Kura Kaupapa Maori" ~ Surveyed.number/sum(Surveyed.number))) %>%
#     mutate_at(vars(contains("percent")), function(x) formatC(round(x * 100, 2), digits = 1, format = "f")) %>%
#     mutate_at(vars(contains("number")), function(x) formatC(x, big.mark=",", format = "d")) %>%
#     filter(Eligible.Number >= 1) %>%
#     select(GroupName, Group, Eligible.Number, Eligible.percent, Invited.number, Invited.percent, Participating.number, Participating.percent, Surveyed.number, Surveyed.percent) %>%
#     gt(groupname_col = "GroupName", rowname_col = "Group") %>%
#     tab_spanner_delim(delim = ".") %>%
#     cols_align(align = "center") %>%
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
#   
# }
# 
# # studentsDem(demographics2, filterGroup = Decile, filterVal = 10)
# 
# # studentsDem(demographics2)
# 
# 
# # demographics2 %>%
# #   group_by(GroupName, Group) %>%
# #   summarise(Eligible.Number = sum(value[ECSurv == "EC" & (Eligible == 1 | WharekuraEligible == 1)], na.rm = TRUE),
# #             Invited.number = sum(value[ECSurv == "EC" & (Invited == 1 | WharekuraInvited == 1)], na.rm = TRUE),
# #             Participating.number = sum(value[ECSurv == "EC" & Participated == 1], na.rm = TRUE),
# #             Surveyed.number = sum(value[ECSurv == "EC" & (Participated == 1)], na.rm = TRUE)) %>%
# #   mutate(Eligible.percent = case_when(GroupName == "Age" ~ Eligible.Number/sum(Eligible.Number),
# #                                   GroupName == "Ethnicity" ~ Eligible.Number/sum(Eligible.Number),
# #                                   GroupName == "Sex" ~ Eligible.Number/sum(Eligible.Number),
# #                                   GroupName == "Year" ~ Eligible.Number/sum(Eligible.Number)),
# #          Invited.percent = case_when(GroupName == "Age" ~ Invited.number/sum(Invited.number),
# #                                  GroupName == "Ethnicity" ~ Invited.number/sum(Invited.number),
# #                                  GroupName == "Sex" ~ Invited.number/sum(Invited.number),
# #                                  GroupName == "Year" ~ Invited.number/sum(Invited.number)),
# #          Participating.percent = case_when(GroupName == "Age" ~ Participating.number/sum(Participating.number),
# #                                  GroupName == "Ethnicity" ~ Participating.number/sum(Participating.number),
# #                                  GroupName == "Sex" ~ Participating.number/sum(Participating.number),
# #                                  GroupName == "Year" ~ Participating.number/sum(Participating.number)),
# #          Surveyed.percent = case_when(GroupName == "Age" ~ Surveyed.number/sum(Surveyed.number),
# #                                        GroupName == "Ethnicity" ~ Surveyed.number/sum(Surveyed.number),
# #                                        GroupName == "Sex" ~ Surveyed.number/sum(Surveyed.number),
# #                                        GroupName == "Year" ~ Surveyed.number/sum(Surveyed.number))) %>%
# #   mutate_at(vars(contains("percent")), function(x) formatC(round(x * 100, 2), digits = 1, format = "f")) %>%
# #   mutate_at(vars(contains("number")), function(x) formatC(x, big.mark=",", format = "d")) %>%
# #   select(GroupName, Group, Eligible.Number, Eligible.percent, Invited.number, Invited.percent, Participating.number, Participating.percent, Surveyed.number, Surveyed.percent) %>%
# #   gt(groupname_col = "GroupName", rowname_col = "Group") %>%
# #   tab_spanner_delim(delim = ".")
# # 
# # ?formatC
# # 
# # 
