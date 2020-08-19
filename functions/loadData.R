data_path <- "./data/"

variables <- na.omit(read_excel(paste0(data_path, "outputVariables.xlsx"), sheet = "variables"))

timeSeries <- na.omit(read_excel(paste0(data_path, "outputVariables.xlsx"), sheet = "timeSeries"))

saved_password <- cyphr::decrypt(readRDS(paste0(data_path, "password.rds")), cyphr::data_key(data_path))

load_survey <- function(df){
  
  df %>%
    mutate(Ethnicity = fct_relevel(as.factor(case_when(ethnic_p5 == "Maori" ~ "Maori",
                                                       ethnic_p5 == "Pacific" ~ "Pacific",
                                                       ethnic_p5 == "Asian" ~ "Asian",
                                                       ethnic_p5 == "Other" | is.na(ethnic_p5) ~ "Other",
                                                       ethnic_p5 == "European" ~ "European",
                                                       TRUE ~ NA_character_)),
                                   "Maori", "Pacific", "Asian", "Other", "European"),
           
           School_Year = fct_relevel(as.factor(School_Year),
                                     "Year 9", "Year 10", "Year 11", "Year 12", "Year 13"),
           
           Sex = fct_relevel(as.factor(case_when(sex == "Male" ~ "Male",
                                                 sex == "Female" ~ "Female")),
                             "Male", "Female"),
           
           Neighbourhood_Deprivation = fct_relevel(as.factor(case_when(NZDep_band3 == 1 ~ "Low",
                                                         NZDep_band3 == 2 ~ "Medium",
                                                         NZDep_band3 == 3 ~ "High",
                                                         TRUE ~ NA_character_)),
                                     "Low", "Medium", "High"),
           
           Urban_Rural = fct_relevel(as.factor(case_when(urban2 == 1 ~ "Urban",
                                                         urban2 == 2 ~ "Small towns",
                                                         urban2 == 3 ~ "Rural",
                                                         TRUE ~ NA_character_)),
                                     "Urban", "Small towns", "Rural"),
           
           School_Decile = fct_relevel(as.factor(case_when(Decile %in%  c(1, 2, 3) ~ "Low", #Low
                                                    Decile %in% c(4, 5, 6, 7) ~ "Medium", #Medium
                                                    Decile %in% c(8, 9, 10) ~ "High", #High
                                                    TRUE ~ NA_character_)),
                                "Low", "Medium", "High"),
           
           Region = fct_relevel(as.factor(ECEducationRegion),
                                "Auckland", "Waikato", "Tai Tokerau"),
           
           Total = as.factor("Total"),
           
           Age = fct_relevel(as.factor(age), "13 and under", "14", "15", "16", "17 and over"),
           
           Attraction = fct_relevel(as.factor(case_when(attract_3Cat == 1 ~ "Opposite Sex",
                                                        attract_3Cat == 2 ~ "Same sex or both sexes",
                                                        attract_3Cat == 3 ~ "Not sure or neither",
                                                        TRUE ~ NA_character_)),
                                    "Opposite Sex", "Same sex or both sexes", "Not sure or neither"),
           
    )
  
}



svy_2019_kura_uncalibrated <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2019_regional_uncalibrated_incl_wharekura.rds")), cyphr::data_key(data_path)))

svy_2019_uncalibrated <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2019_regional_uncalibrated_no_wharekura.rds")), cyphr::data_key(data_path)))

svy_2012_uncalibrated <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2012_regional_uncalibrated_no_wharekura.rds")), cyphr::data_key(data_path)))

svy_2007_uncalibrated <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2007_regional_uncalibrated_no_wharekura.rds")), cyphr::data_key(data_path)))

svy_2001_uncalibrated <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2001_regional_uncalibrated_no_wharekura.rds")), cyphr::data_key(data_path)))



svy_2019_kura <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2019_national_calibrated_incl_wharekura.rds")), cyphr::data_key(data_path)))

svy_2019 <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2019_national_calibrated_no_wharekura.rds")), cyphr::data_key(data_path)))

svy_2012 <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2012_national_calibrated_no_wharekura.rds")), cyphr::data_key(data_path)))

svy_2007 <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2007_national_calibrated_no_wharekura.rds")), cyphr::data_key(data_path)))

svy_2001 <- load_survey(cyphr::decrypt(readRDS(paste0(data_path, "youth2001_national_calibrated_no_wharekura.rds")), cyphr::data_key(data_path)))



svy_2001 <-
  svy_2001 %>%
  mutate(
    wellbeing = 0,
    attemptSuicide = 0,
    familyMeals = 0,
    usedGP = 0,
    schClinic = 0,
    afterHours = 0,
    ythHltCen = 0,
    confident = 0,
    private = 0,
    unableHC = 0,
    
    Sex44 = "NA"
  )

svy_2001_uncalibrated <-
  svy_2001_uncalibrated %>%
  mutate(
    wellbeing = 0,
    attemptSuicide = 0,
    familyMeals = 0,
    usedGP = 0,
    schClinic = 0,
    afterHours = 0,
    ythHltCen = 0,
    confident = 0,
    private = 0,
    unableHC = 0,
    
    Sex44 = "NA"
  )

svy_2007 <-
  svy_2007 %>%
  mutate(
    
    Sex44 = "NA"
    )

svy_2007_uncalibrated <-
  svy_2007_uncalibrated %>%
  mutate(
    
    Sex44 = "NA"
  )

svy_2012 <-
  svy_2012 %>%
  mutate(
    
    Sex44 = fct_relevel(as.factor(case_when(Sex44 == 1 ~ "Yes",
                                            Sex44 == 2 ~ "No",
                                            Sex44 == 3 ~ "Unsure",
                                            Sex44 == 4 ~ "I don't understand",
                                            TRUE ~ NA_character_)),
                        "Yes", "No", "Unsure", "I don't understand")
  )

svy_2012_uncalibrated <-
  svy_2012_uncalibrated %>%
  mutate(
    
    Sex44 = fct_relevel(as.factor(case_when(Sex44 == 1 ~ "Yes",
                                            Sex44 == 2 ~ "No",
                                            Sex44 == 3 ~ "Unsure",
                                            Sex44 == 4 ~ "I don't understand",
                                            TRUE ~ NA_character_)),
                        "Yes", "No", "Unsure", "I don't understand")
  )

svy_2019 <-
  svy_2019 %>%
  mutate(
    DHB_name = DHB2015_name,
    
    Trans = fct_relevel(as.factor(case_when(trans == 1 ~ "Yes",
                                            trans == 0 ~ "No",
                                            trans == 2 ~ "Unsure",
                                            TRUE ~ NA_character_)),
                        "Yes", "No", "Unsure"),
    
    Sex44 = fct_relevel(as.factor(case_when(Sex44 == 1 ~ "Yes",
                                            Sex44 == 2 ~ "No",
                                            Sex44 == 3 ~ "Unsure",
                                            Sex44 == 4 ~ "I don't understand",
                                            TRUE ~ NA_character_)),
                        "Yes", "No", "Unsure", "I don't understand"))

svy_2019_uncalibrated <-
  svy_2019_uncalibrated %>%
  mutate(
    DHB_name = DHB2015_name,
    
    Trans = fct_relevel(as.factor(case_when(trans == 1 ~ "Yes",
                                            trans == 0 ~ "No",
                                            trans == 2 ~ "Unsure",
                                            TRUE ~ NA_character_)),
                        "Yes", "No", "Unsure"),
    
    Sex44 = fct_relevel(as.factor(case_when(Sex44 == 1 ~ "Yes",
                                            Sex44 == 2 ~ "No",
                                            Sex44 == 3 ~ "Unsure",
                                            Sex44 == 4 ~ "I don't understand",
                                            TRUE ~ NA_character_)),
                        "Yes", "No", "Unsure", "I don't understand"))

svy_2019_kura <-
  svy_2019_kura %>%
  mutate(DHB_name = DHB2015_name,
         
         Trans = fct_relevel(as.factor(case_when(trans == 1 ~ "Yes",
                                                      trans == 0 ~ "No",
                                                      trans == 2 ~ "Unsure",
                                                      TRUE ~ NA_character_)),
                                  "Yes", "No", "Unsure"),
         
         Sex44 = fct_relevel(as.factor(case_when(Sex44 == 1 ~ "Yes",
                                                 Sex44 == 2 ~ "No",
                                                 Sex44 == 3 ~ "Unsure",
                                                 Sex44 == 4 ~ "I don't understand",
                                                 TRUE ~ NA_character_)),
                             "Yes", "No", "Unsure", "I don't understand"))

svy_2019_kura_uncalibrated <-
  svy_2019_kura_uncalibrated %>%
  mutate(DHB_name = DHB2015_name,
         
         Trans = fct_relevel(as.factor(case_when(trans == 1 ~ "Yes",
                                                 trans == 0 ~ "No",
                                                 trans == 2 ~ "Unsure",
                                                 TRUE ~ NA_character_)),
                             "Yes", "No", "Unsure"),
         
         Sex44 = fct_relevel(as.factor(case_when(Sex44 == 1 ~ "Yes",
                                                 Sex44 == 2 ~ "No",
                                                 Sex44 == 3 ~ "Unsure",
                                                 Sex44 == 4 ~ "I don't understand",
                                                 TRUE ~ NA_character_)),
                             "Yes", "No", "Unsure", "I don't understand"))


demographics <-
  cyphr::decrypt(readRDS(paste0(data_path, "demographics.rds")), cyphr::data_key(data_path)) %>%
  mutate(Decile = fct_relevel(as.factor(Decile),
                              "1","2","3","4","5","6","7","8","9","10","99"),
         
         DecileBand = fct_relevel(as.factor(case_when(Decile %in% c(1,2,3) ~ "Low (1-3)",
                                                      Decile %in% c(4,5,6,7) ~ "Medium (4-7)",
                                                      Decile %in% c(8,9,10) ~ "High (8-10)",
                                                      Decile == 99 ~ "No Decile information")),
                                  "Low (1-3)", "Medium (4-7)", "High (8-10)", "No Decile information"),
         
         SchoolSize = fct_relevel(as.factor(case_when(ECRoll >= 350 ~ "Large",
                                                      ECRoll < 350 ~ "Small")),
                                  "Small", "Large"),
         
         Kura = fct_relevel(as.factor(case_when(Wharekura == 1 ~ "Kura Kaupapa Maori",
                                                Wharekura == 0 ~ "Non-Kura Kaupapa Maori")),
                            "Non-Kura Kaupapa Maori", "Kura Kaupapa Maori")
  ) %>%
  mutate_at(vars(ECMaori:surveyYear13), ~as.numeric(.))

demographics2 <-
  demographics %>%
  rename(EC_Total_Total = ECRoll,
         EC_Sex_Female = ECFemale,
         EC_Sex_Male = ECMale,
         EC_Ethnicity_Maori = ECMaori,
         EC_Ethnicity_Pacific = ECPacific,
         EC_Ethnicity_Asian = ECAsian,
         EC_Ethnicity_MELAA = ECMELAA,
         EC_Ethnicity_Other = ECOther,
         EC_Ethnicity_European = ECEuropean,
         EC_Year_9 = ECYear9,
         EC_Year_10 = ECYear10,
         EC_Year_11 = ECYear11,
         EC_Year_12 = ECYear12,
         EC_Year_13 = ECYear13,
         EC_Year_14 = ECYear14,
         EC_Year_15 = ECYear15,
         EC_Age_Under12 = ECAgeUnder12,
         EC_Age_12 = ECAge12,
         EC_Age_13 = ECAge13,
         EC_Age_14 = ECAge14,
         EC_Age_15 = ECAge15,
         EC_Age_16 = ECAge16,
         EC_Age_17 = ECAge17,
         EC_Age_18 = ECAge18,
         EC_Age_19 = ECAge19,
         EC_Age_Over19 = ECAgeOver19,
         survey_Total_Total = surveyParticipated,
         survey_Sex_Male = surveyMale,
         survey_Sex_Female = surveyFemale,
         survey_Ethnicity_Maori = surveyMaori,
         survey_Ethnicity_Pacific = surveyPacific,
         survey_Ethnicity_Asian = surveyAsian,
         survey_Ethnicity_MELAA = surveyMELAA,
         survey_Ethnicity_Other = surveyOther,
         survey_Ethnicity_European = `surveyEuropean/Pakeha`,
         survey_Age_Under12 = surveyAgeUnder12,
         survey_Age_12 = surveyAge12,
         survey_Age_13 = surveyAge13,
         survey_Age_14 = surveyAge14,
         survey_Age_15 = surveyAge15,
         survey_Age_16 = surveyAge16,
         survey_Age_17 = surveyAge17,
         survey_Age_18 = surveyAge18,
         survey_Age_19 = surveyAge19,
         survey_Age_Over19 = surveyAgeOver19,
         survey_Year_9 = surveyYear9,
         survey_Year_10 = surveyYear10,
         survey_Year_11 = surveyYear11,
         survey_Year_12 = surveyYear12,
         survey_Year_13 = surveyYear13) %>%
  mutate_at(vars(contains("_")), list(~as.numeric(.))) %>%
  select(ECSchoolID, Type, Authority, Eligible, Invited, pilot, Participated, Wharekura, WharekuraEligible, WharekuraInvited, Decile, DecileBand, EducationRegion, SchoolSize, Kura, contains("EC_"), contains("survey_")) %>%
  pivot_longer(-c(ECSchoolID, Type, Authority, Eligible, Invited, pilot, Participated, Wharekura, WharekuraEligible, WharekuraInvited, Decile, DecileBand, EducationRegion, SchoolSize, Kura), names_to = c("ECSurv", "Group1", "Group2"), names_sep = "_") %>%
  mutate(Group1 = fct_relevel(as.factor(Group1), "Total", "Sex", "Age", "Ethnicity", "Year"),
         Group2 = fct_relevel(as.factor(Group2), "Total", "Male", "Female", "9", "10", "11", "Under12", "12", "13", "14", "15", "16", "17", "18", "19", "Over19", "Maori", "Pacific", "Asian", "Other", "MELAA", "European"))


