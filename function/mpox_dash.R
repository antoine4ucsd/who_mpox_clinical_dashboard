

tmp<- whomapper::pull_who_logo('HQ')

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available", "")

date_start="2022-01-01"
date_end=Sys.Date()



as_of_date=Sys.Date()
today=as_of_date
base_size = 24

get_flag <- function(country_codes) { 
        sapply(country_codes, function(country_code) { 
                if (is.null(country_code) || is.na(country_code)) { 
                        return(NULL) 
                } 
                intToUtf8(127397 + strtoi(charToRaw(toupper(country_code)), 16L)) 
        }) %>% as.vector()
}



to_yn_char2num <- function(vec_in){
        vec_out <- case_when(
                vec_in %in% c("Yes","Positive","On ART","Present and tender","Present") ~ 1,
                vec_in %in% c("No","Negative","Not on ART") ~ 0,
                vec_in %in%  c("Unknown","ART status unknown") ~ NA,
                .default = NA
        ) 
        return(vec_out)
}


to_yn_num <- function(vec_in){
        vec_out <- case_when(
                vec_in %in% c("1") ~ 1,
                vec_in %in% c("2") ~ 0,
                vec_in == 3 ~ NA,
                .default = NA
        ) 
        return(vec_out)
}

to_yn_char <- function(vec_in){
        vec_out <- case_when(
                vec_in %in% c("1") ~ "Yes",
                vec_in %in% c("0") ~ "No",
                .default = NA
        ) 
        return(vec_out)
}

hiv_fun2<- function(data_in){
        data_out<- data_in|>
                dplyr::mutate(adm03_1=unlabel(adm03_1))|>
                dplyr::mutate(hiv_status=case_when(
                        adm40.factor=="Yes (on antiretroviral therapy/ART)"~"People with HIV",
                        adm40.factor=="Yes (not on antiretroviral therapy/ART)"~"People with HIV",
                        adm40.factor=="Yes (unknown if on antiretroviral therapy/ART)"~"People with HIV",
                        adm40.factor=="No"~"Peoples without HIV",
                        adm40.factor=="Unknown"~"Unknown HIV status",.default = NA),
                        HIV_ART = case_when(as.numeric(adm40) %in% c(1) ~ 1,
                                            as.numeric(adm40) %in% c(2) ~ 2,
                                            as.numeric(adm40) %in% c(3) ~3),
                        HIV = case_when(as.numeric(adm40) %in% c(4) ~ 1,
                                        as.numeric(adm40) %in% c(1:3) ~ 2,
                                        as.numeric(adm40) %in% c(5) ~3))|>
                dplyr::mutate(HIV=factor(HIV,levels = c(1:3),
                                         labels = c("Negative",
                                                    "Positive",
                                                    "Unknown")),
                              HIV_ART=factor(HIV_ART, levels = c(1:3),
                                             labels = c("On ART",
                                                        "Not on ART",
                                                        "ART status unknown")))
}
        


# FACILITY: add country and facility short name
add_facility_short <- function(data_in){
        data_in<- data_in |>
                dplyr::mutate(admsit=unlabel(admsit))
        data_out <- data_in |>
                dplyr::mutate(
                        facility_short = case_when(
                                admsit%in%c("Corporacion de lucha contra el SIDA", 
                                            "Corporacion de Lucha contra el SIDA", "CORPORACION DE LUCHA CONTRA EL SIDA", 
                                            "Corporación de lucha contra el SIDA ", "Corporación de Lucha contra el SIDA ", 
                                            "Corporación de Lucha Contra el SIDA ")~"Corporacion de lucha contra el SIDA",
                                admsit=="MONDONG"~"Mondong",
                                admsit=="Centro de tratamiento de mondong"~"Mondong",
                                admsit=="Centro de tratamiento de Mondong"~"Mondong",
                                admsit=="Centro de tratamiento de mondong "~"Mondong",
                                .default=admsit
                        ),
                        facility_2 = str_c(facility_short, " (", iso3, ")")
                )
        return(data_out)
}


# calculate age in years and create age groups
age_fun<- function(data_in){
        data_in<- data_in|>
                dplyr::mutate(adm03_1=as.numeric(adm03_1))|>
                # dplyr::mutate(across(all_of(c("adm03_1")), as.numeric))|>
                dplyr::mutate(Age=case_when(is.na(adm03_1)~round(as.numeric((as.Date(adm01)-as.Date(adm03)))/365.25,1),
                                                       .default = unlabel(adm03_1)) )|>
                dplyr::mutate(`Age group` =  cut(Age,
                                                 breaks=c(-Inf, 5,15,25,45, 65,+Inf),
                                                 right=TRUE,
                                                 labels = c("≤5 yrs","5-15 yrs","15-25 yrs","25-45 yrs",
                                                            "45-65 yrs",">65 yrs")))
}



sex_age_fun<- function(data_in){
        data_in|>dplyr::filter(is.na(redcap_repeat_instrument)|redcap_repeat_instrument=="")|>
                age_fun()|>
                dplyr::select("admcou.factor","admpar", "Age","adm02","adm12","adm14")|>
                sjlabelled::label_to_colnames()|>
                unlabel()|>
                dplyr::select(where(~ !all(is.na(.))))|>
                dplyr::mutate(`Sex at birth`=case_when(`Sex at birth`=="1"~"Male",
                                                       `Sex at birth`=="2"~"Female",
                                                       `Sex at birth`=="3"~NA))|>
                dplyr::mutate(`Age group` =  cut(Age,
                                                 breaks=c(-Inf, 5,15,25,45, 65,+Inf),
                                                 right=TRUE,
                                                 labels = c("≤5 yrs","5-15 yrs","15-25 yrs","25-45 yrs",
                                                            "45-65 yrs",">65 yrs")))|>
                dplyr::mutate(date = coalesce(`Symptom onset (date of first/earliest symptom)`, `Admission date or visit date at this facility`),
                              date_type = case_when(date == `Symptom onset (date of first/earliest symptom)` ~ "Onset",
                                                    date == `Admission date or visit date at this facility` ~ "Admission")) %>%
                dplyr::filter(between(date, as.Date(date_start), as.Date(date_end)))|>
                dplyr::mutate(date=as.Date(date))
        
        
}

hiv_fun<- function(data_in){
        data_out<- data_in|>
                dplyr::mutate(adm03_1=unlabel(adm03_1))|>
                dplyr::mutate(hiv_status=case_when(
                adm40.factor=="Yes (on antiretroviral therapy/ART)"~"People with HIV",
                adm40.factor=="Yes (not on antiretroviral therapy/ART)"~"People with HIV",
                adm40.factor=="Yes (unknown if on antiretroviral therapy/ART)"~"People with HIV",
                adm40.factor=="No"~"Peoples without HIV",
                adm40.factor=="Unknown"~"Unknown HIV status",.default = NA))
}
glance_fun<- function(data_in){
        main_plot<- ggplot(data_in, aes(x, y, height = h, width = w, label = info)) +
                ## Create the tiles using the `color` column
                geom_tile(aes(fill = color)) +
                ## Add the numeric values as text in `value` column
                geom_text(color = "white", fontface = "bold", size = 10,
                          aes(label = value, x = x - 2.9, y = y + 1), hjust = 0) +
                ## Add the labels for each box stored in the `info` column
                geom_text(color = "white", fontface = "bold",
                          aes(label = info, x = x - 2.9, y = y - 1), hjust = 0) +
                coord_fixed() +
                scale_fill_brewer(type = "qual",palette = "Dark2") +
                ## Use `geom_text()` to add the icons by specifying the unicode symbol.
                geom_text(size = 20, aes(label = icon, family = font_family,
                                         x = x + 1.5, y = y + 0.5), alpha = 0.25) +
                theme_void() +
                guides(fill = FALSE)
        main_plot
}



df_mpox_fun_world <- function(data_in){
        
        total_cases <- data_in$admpar|>n_distinct()
        country_df<- data_in|>distinct(country)|>dplyr::filter(!is.na(country))
        total_countries <- country_df$country|>n_distinct()
        
        # total_cases_country=data$admpar|>n_distinct()
        total_facility=data_in$facility_short|>n_distinct()
        
        hiv_df<- data_in|>distinct(admpar,hiv_status)|>dplyr::filter(hiv_status=="People with HIV")
        total_hiv=hiv_df$admpar|>n_distinct()
        
        death_df<- data_in|>distinct(admpar,dis34.factor)|>dplyr::filter(dis34.factor=="Death")
        total_death=death_df$admpar|>n_distinct()
        
        sex_df<- data_in|>distinct(admpar,adm02.factor)|>dplyr::filter(adm02.factor=="Female")
        female_perc<- sex_df$admpar|>n_distinct()/data_in$admpar|>n_distinct()
        
        df_mpox <- data.frame(
                x = rep(seq(2, 15, 6.5), 2),
                y = c(rep(6.5, 3), rep(2,3)),
                h = rep(4.25, 6),
                w = rep(6.25, 6),
                value = c(total_countries,
                          total_cases,
                          total_facility,
                          round(female_perc,2),
                          total_hiv,
                          total_death
                ),
                info = c("Total countries",
                         "Total cases",
                         "Facilities",
                         "Women (%)",
                         "PLWH",
                         "Fatalities"),
                # icon = c(fontawesome(search_fontawesome("chart")),
                #          emoji("athletic_shoe")),
                icon = c(fontawesome( "fa-pie-chart" ), 
                         fontawesome("fa-bar-chart-o"),
                         fontawesome("fa-line-chart"),
                         fontawesome("fa-venus"),
                         fontawesome("fa-area-chart") , 
                         fontawesome( "fa-bar-chart-o" )  ),
                font_family = c(rep("fontawesome-webfont", 6)),
                color = factor(1:6)
        )
        df_mpox

}



df_mpox_fun_country <- function(data_in){
        
        total_cases <- data_redcap$admpar|>n_distinct()
        country_df<- data_redcap|>distinct(country)|>dplyr::filter(!is.na(country))
        total_countries <- country_df$country|>n_distinct()
        total_cases_country=data_in$admpar|>n_distinct()
        cases_percent=total_cases_country/total_cases
        total_facility_country=data_in$facility_short|>n_distinct()
        hiv_df<- data_in|>distinct(admpar,hiv_status)|>dplyr::filter(hiv_status=="People with HIV")
        total_hiv_country=hiv_df$admpar|>n_distinct()
        
        death_df<- data_in|>distinct(admpar,dis34.factor)|>dplyr::filter(dis34.factor=="Death")
        total_death_country=death_df$admpar|>n_distinct()
        
        sex_df<- data_in|>distinct(admpar,adm02.factor)|>dplyr::filter(adm02.factor=="Female")
        female_perc_country<- sex_df$admpar|>n_distinct()/data_in$admpar|>n_distinct()
        
        
        df_mpox <- data.frame(
                x = rep(seq(2, 15, 6.5), 2),
                y = c(rep(6.5, 3), rep(2,3)),
                h = rep(4.25, 6),
                w = rep(6.25, 6),
                value = c(total_facility_country,
                          total_cases_country,
                          cases_percent,
                          round(female_perc_country,2),
                          total_hiv_country,
                          total_death_country
                ),
                info = c("Total facilities",
                         "Total cases",
                         "% all cases",
                         "Women (%)",
                         "PLWH",
                         "Fatalities"),
                # icon = c(fontawesome(search_fontawesome("chart")),
                #          emoji("athletic_shoe")),
                icon = c(fontawesome( "fa-pie-chart" ), 
                         fontawesome("fa-bar-chart-o"),
                         fontawesome("fa-percent"),
                         fontawesome("fa-venus"),
                         fontawesome("fa-area-chart") , 
                         fontawesome( "fa-bar-chart-o" )  ),
                font_family = c(rep("fontawesome-webfont", 6)),
                color = factor(1:6)
        )
        
        df_mpox
}


glance_fun_country <- function(data_in){
        
        total_cases <- data_redcap$admpar|>n_distinct()
        country_df<- data_redcap|>distinct(country)|>dplyr::filter(!is.na(country))
        total_countries <- country_df$country|>n_distinct()
        total_cases_country=data_in$admpar|>n_distinct()
        cases_percent=total_cases_country/total_cases
        total_facility_country=data_in$facility_short|>n_distinct()
        hiv_df<- data_in|>distinct(admpar,hiv_status)|>dplyr::filter(hiv_status=="People with HIV")
        total_hiv_country=hiv_df$admpar|>n_distinct()
        
        death_df<- data_in|>distinct(admpar,dis34.factor)|>dplyr::filter(dis34.factor=="Death")
        total_death_country=death_df$admpar|>n_distinct()
        
        sex_df<- data_in|>distinct(admpar,adm02.factor)|>dplyr::filter(adm02.factor=="Female")
        female_perc_country<- sex_df$admpar|>n_distinct()/data_in$admpar|>n_distinct()
        
        
        df_mpox <- data.frame(
                x = rep(seq(2, 15, 6.5), 2),
                y = c(rep(6.5, 3), rep(2,3)),
                h = rep(4.25, 6),
                w = rep(6.25, 6),
                value = c(scales::comma(total_facility_country),
                          scales::comma(total_cases_country),
                          scales::percent(cases_percent),
                          scales::percent(round(female_perc_country,2)),
                          scales::comma(total_hiv_country),
                          scales::comma(total_death_country)
                ),
                info = c("Total facilities",
                         "Total cases",
                         "% all cases",
                         "Women (%)",
                         "PLWH",
                         "Fatalities"),
                # icon = c(fontawesome(search_fontawesome("chart")),
                #          emoji("athletic_shoe")),
                icon = c(fontawesome( "fa-pie-chart" ), 
                         fontawesome("fa-bar-chart-o"),
                         fontawesome("fa-percent"),
                         fontawesome("fa-venus"),
                         fontawesome("fa-area-chart") , 
                         fontawesome( "fa-bar-chart-o" )  ),
                font_family = c(rep("fontawesome-webfont", 6)),
                color = factor(1:6)
        )
        
        main_plot<- ggplot(df_mpox, aes(x, y, height = h, width = w, label = info)) +
                ## Create the tiles using the `color` column
                geom_tile(aes(fill = color)) +
                ## Add the numeric values as text in `value` column
                geom_text(color = "white", fontface = "bold", size = 10,
                          aes(label = value, x = x - 2.9, y = y + 1), hjust = 0) +
                ## Add the labels for each box stored in the `info` column
                geom_text(color = "white", fontface = "bold",
                          aes(label = info, x = x - 2.9, y = y - 1), hjust = 0) +
                coord_fixed() +
                scale_fill_brewer(type = "qual",palette = "Dark2") +
                ## Use `geom_text()` to add the icons by specifying the unicode symbol.
                geom_text(size = 20, aes(label = icon, family = font_family,
                                         x = x + 1.5, y = y + 0.5), alpha = 0.25) +
                theme_void() +
                guides(fill = FALSE)
        main_plot
}



glance_fun_world <- function(data_in){
        
        total_cases <- data_in$admpar|>n_distinct()
        country_df<- data_in|>distinct(country)|>dplyr::filter(!is.na(country))
        total_countries <- country_df$country|>n_distinct()
        
        # total_cases_country=data$admpar|>n_distinct()
        total_facility=data_in$facility_short|>n_distinct()
        
        hiv_df<- data_in|>distinct(admpar,hiv_status)|>dplyr::filter(hiv_status=="People with HIV")
        total_hiv=hiv_df$admpar|>n_distinct()
        
        death_df<- data_in|>distinct(admpar,dis34.factor)|>dplyr::filter(dis34.factor=="Death")
        total_death=death_df$admpar|>n_distinct()
        
        sex_df<- data_in|>distinct(admpar,adm02.factor)|>dplyr::filter(adm02.factor=="Female")
        female_perc<- sex_df$admpar|>n_distinct()/data_in$admpar|>n_distinct()
        
        df_mpox <- data.frame(
                x = rep(seq(2, 15, 6.5), 2),
                y = c(rep(6.5, 3), rep(2,3)),
                h = rep(4.25, 6),
                w = rep(6.25, 6),
                value = c(scales::comma(total_countries),
                          scales::comma(total_cases),
                          scales::comma(total_facility),
                          scales::percent(round(female_perc,2)),
                          scales::comma(total_hiv),
                          scales::comma(total_death)
                ),
                info = c("Total countries",
                         "Total cases",
                         "Facilities",
                         "Women (%)",
                         "PLWH",
                         "Fatalities"),
                # icon = c(fontawesome(search_fontawesome("chart")),
                #          emoji("athletic_shoe")),
                icon = c(fontawesome( "fa-pie-chart" ), 
                         fontawesome("fa-bar-chart-o"),
                         fontawesome("fa-line-chart"),
                         fontawesome("fa-venus"),
                         fontawesome("fa-area-chart") , 
                         fontawesome( "fa-bar-chart-o" )  ),
                font_family = c(rep("fontawesome-webfont", 6)),
                color = factor(1:6)
        )
        
        main_plot<- ggplot(df_mpox, aes(x, y, height = h, width = w, label = info)) +
                ## Create the tiles using the `color` column
                geom_tile(aes(fill = color)) +
                ## Add the numeric values as text in `value` column
                geom_text(color = "white", fontface = "bold", size = 10,
                          aes(label = value, x = x - 2.9, y = y + 1), hjust = 0) +
                ## Add the labels for each box stored in the `info` column
                geom_text(color = "white", fontface = "bold",
                          aes(label = info, x = x - 2.9, y = y - 1), hjust = 0) +
                coord_fixed() +
                scale_fill_brewer(type = "qual",palette = "Dark2") +
                ## Use `geom_text()` to add the icons by specifying the unicode symbol.
                geom_text(size = 20, aes(label = icon, family = font_family,
                                         x = x + 1.5, y = y + 0.5), alpha = 0.25) +
                theme_void() +
                guides(fill = FALSE)
        main_plot
}



lab_var=c("adm74lu","dis08lu","adm74o","adm74o","adm74ou",
          "dis08o","dis08ou","adm74n","adm74n","adm74nu",
          "dis08n","dis08n","dis08nu","adm74bu","dis08bu",
          "adm74cu","dis08nu","adm74du","dis08du","adm74iu","dis08iu","dis08a")



main_fun<- function(data_in){
        data_out<- data_in|>
                dplyr::mutate(adm74lu = ifelse(is.na(adm74l), NA, "g/dL" ),
                              dis08lu = ifelse(is.na(dis08l), NA, "g/dL" ),
                              adm74o = as.numeric(gsub(",", "",adm74o )),
                              adm74o = ifelse(as.numeric(adm74o)  %in% c(100000:1000000), 
                                              as.numeric(adm74o) /1000, as.numeric(adm74o) ),
                              adm74ou = ifelse(is.na(adm74o), NA, "x10^9/L" ),
                              dis08o =ifelse(as.numeric(dis08o) %in% c(100000:1000000), as.numeric(dis08o)/1000, 
                                             ifelse(as.numeric(dis08o) %in% c(10000:100000),as.numeric(dis08o)/100, as.numeric(dis08o))),
                              dis08ou= ifelse(is.na(dis08ou), NA, "x10^9/L" ),
                              adm74n = as.numeric(gsub(",", "",adm74n )),
                              adm74n = ifelse(as.numeric(adm74n)  > 100, 
                                              as.numeric(adm74n) /1000, as.numeric(adm74n)  ),
                              adm74nu = ifelse(is.na(adm74nu), NA, "cells/µL" ),
                              dis08n = as.numeric(gsub(",", "",dis08n )),
                              dis08n = ifelse(dis08n > 100, 
                                              dis08n/1000, dis08n ),
                              dis08nu = ifelse(is.na(dis08nu), NA, "cells/µL" ),
                              adm74bu = ifelse(is.na(adm74bu), NA, "u/L" ),
                              dis08bu = ifelse(is.na(dis08bu), NA, "u/L" ),
                              adm74cu = ifelse(is.na(adm74cu), NA, "mg/dL" ),
                              dis08nu = ifelse(is.na(dis08nu), NA, "mg/dL" ),
                              adm74du = ifelse(is.na(adm74du), NA, "mmol/L" ),
                              dis08du = ifelse(is.na(dis08du), NA, "mmol/L" ),
                              adm74iu = ifelse(is.na(adm74iu), NA, "mg/L" ),
                              dis08iu = ifelse(is.na(dis08iu), NA, "mg/L" ))%>%
                
                age_fun()|>
                hiv_fun2()|>
                dplyr::mutate(across(all_of(lab_var), as.numeric))
}

rename_fun<- function(data_in){
        data_out<- data_in|>
                dplyr::rename(#Age = adm03_1,
                Sex=adm02.factor,                  # Sex at birth
                Vaccinated = adm10.factor,            # History of tetanus vaccination
                
                # Existing Chronic comorbidities
                Cardiac = adm23.factor,            # Chronic cardiac disease (non hypertension)
                Diabetes = adm109.factor,          # Diabetes Mellitus (Type 1 or 2)
                Hypertension = adm25.factor,       # Hypertension
                Pulmonary = adm27.factor,          # Chronic Pulmonary disease
                TB = adm28.factor,                 # Tuberculosis (active)
                Neurological = adm35.factor,       # Neurological conditions
                Kidney_disease = adm31.factor,     # Chronic Kidney disease
                Liver_disease = adm33.factor,      # Chronic Liver disease
                malignant_neoplasm = adm34.factor, # Malignant neoplasm
                STI = adm39.factor,                # STI untreated (i.e. herpes, syphilis, chlamydia, gonorrhoea)
                Smoking = adm26.factor,            # Currently smoking
                Alcohol = adm36.factor,            # Alcohol disorder
                
                # Exposure history
                Contact_with_case = adm11a.factor,  # Known link to probable or confirmed case of MPX
                Animal_source = adm11d.factor,      # Contact with possible animal source
                Sexully_active = adm11b.factor,     # Sexually active
                International_travel = adm11c.factor,     # International travel
                
                # Signs and symptoms
                Fatigue = adm73e.factor,        # Fatigue
                Sorethroat = adm73a.factor,
                Muscle_ache = adm73b.factor,
                Headache = adm73c.factor,
                Ocular_symptoms = adm73d.factor,
                Fatigue = adm73e.factor,
                Oral_pain = adm73f.factor,
                Nausea = adm73g.factor,
                Vomiting = adm73h.factor,
                Diarrhoea = adm73i.factor,
                Rectal_pain =adm73j.factor,
                Proctitis = adm73k.factor,
                Pain_swallowing = adm73l.factor,
                Difficulty_swallowing = adm73m.factor,
                Pain_urination = adm73n.factor,
                Urethritis = adm73o.factor,
                Chest_pain = adm73p.factor,
                Decreased_urine_output = adm73q.factor,
                Dizziness = adm73r.factor,
                Joint_pain = adm73s.factor,
                Psychological_disturbance = adm73t.factor,
                Lymphadenopathy = adm73u.factor,
                Axillary = adm73u1.factor,
                Cervical = adm73u2.factor,
                Inguinal = adm73u3.factor,
                
                # Location of lesion
                Face_lesion = adm49.factor,
                Nares_lesion = adm50.factor,
                Mouth_lesion = adm51.factor,
                Chest_lesion = adm52.factor,
                Abdomen_lesion = adm53.factor,
                Back_lesion = adm54.factor,
                Perianal_lesion = adm55.factor,
                Genitals_lesion = adm56.factor,
                Rectal_lesion = adm127.factor,
                Palms_lesion = adm57.factor,
                Arms_lesion = adm58.factor,
                Forearms_lesion = adm59.factor,
                Thighs_lesion = adm60.factor,
                Legs_lesion = adm61.factor,
                Soles_lesion = adm62.factor,
                
                # Lesion type
                Macule = adm64.factor,
                Papule = adm65.factor,
                Early_vesicle   = adm66.factor,
                Small_pustule  = adm67.factor,
                Umbilicated_pustule  = adm68.factor,
                Ulcerated_lesion   = adm69.factor,
                Crusting   = adm70.factor,
                Partially_removed_scab = adm71.factor,
                
                # Number of lesion
                number_lesion = adm43.factor,
                
                # Vital signs
                Temperature = adm15,
                SBP = adm18_1,
                DBP = adm18_2,
                Heart_rate = adm16,
                Respiratory_rate = adm17,
                Weight = adm22,
                Height = adm21,
                
                # Laboratory test : First recorded
                Haemoglobin = adm74l ,
                Platelets = adm74o ,
                wbc_count = adm74n ,
                Glucose = adm74j,
                ALT = adm74a,
                AST = adm74b,
                Creatinine = adm74c,
                Potassium = adm74d ,
                CRP = adm74i ,
                
                
                
                # Laboratory test : Extreme values
                ext_Haemoglobin = dis08l ,
                ext_Platelets = dis08o ,
                ext_wbc_count = dis08n ,
                ext_Glucose = dis08j,
                ext_ALT = dis08a,
                ext_AST = dis08b,
                ext_Creatinine = dis08c,
                ext_Potassium = dis08d ,
                ext_CRP = dis08i ,
                
                # Complications
                Shock_comp = dis07a.factor,
                Sepsis_comp = dis07w.factor,
                Seizure_comp = dis07b.factor,
                Meningitis_comp = dis07c.factor,
                Encephalitis_comp = dis07x.factor,
                Anaemia_comp = dis07d.factor,
                Cardiac_arrhythmia_comp = dis07e.factor,
                Cardiac_arrest_comp = dis07f.factor,
                Pneumonia_comp = dis07g.factor,
                Cellulitis_comp = dis07h.factor,
                ARDS_comp = dis07i.factor,
                Necrotizing_infection_comp = dis07j.factor,
                Abscess_comp = dis07k.factor,
                Bacteraemia_comp = dis07l.factor,
                Bleeding_disorder_comp = dis07m.factor,
                ischaemic_stroke_comp = dis07n.factor,
                GBS_comp = dis07z.factor,
                Myocarditis_comp = dis07o.factor,
                Pericarditis_comp = dis07y.factor,
                Acute_renal_injury_comp = dis07p.factor,
                Pancreatitis_comp = dis07q.factor,
                Liver_dysfunction_comp = dis07r.factor,
                Cardiomyopathy_comp = dis07s.factor,
                Rectal_Pain_comp = dis07aa.factor,
                Urinary_retention_comp = dis07t.factor,
                Ocular_infection_comp = dis07u.factor,
                Proctitis_comp = dis07ab.factor,
                Rectal_Urgency_comp = dis07ac.factor,
                
                
                # Treatment
                Nasogastric = dis09.factor,
                Intravenous_fluids = dis10.factor,
                Antivirals = dis11.factor,
                Tecovirimat = dis12.factor,
                # Tec_dose = dis12b,
                # Tec_freq = dis12c.factor,
                # Tec_route = dis12d.factor,
                Brincidovir = dis13.factor,
                Cidovir = dis14.factor,
                Antibiotics = dis100.factor,
                Antifungals = dis106.factor,
                Analgesia = dis110.factor,
                Concomitant_meds = dis116.factor,
                
                # Supportive care
                ICU_admission = dis25.factor,
                Oxygen_therapy = dis26.factor,
                Non_invasive_ventilation = dis27.factor,
                ECMO_support =dis29.factor,
                Invasive_ventilation = dis28.factor,
                Renal_replacement_therapy = dis31.factor,
                
                
                # Outcome
                Outcome = dis34.factor,
                Hospitalization = dis33.factor,
                self_care = dis35.factor,
                unresolved_lesion = dis36.factor,
                num_unresolved_lesion = dis38.factor,
                
                
                # Diagnosis
                Suspected = dis40.factor,
                Probable = dis41.factor,
                Confirmed = dis42.factor,
                
                
                
                # Symptoms @ discharge
                dis_symp_Sore_throat = dis125a.factor,             #Sore throat
                dis_symp_Proctitis = dis125b.factor,               #Proctitis
                dis_symp_Muscle_aches = dis125c.factor,            #Muscle aches (myalgia )
                dis_symp_Swallowing_pain = dis125d.factor,         #Pain with swallowing
                dis_symp_Headache = dis125e.factor,                #Headache
                dis_symp_Difficulty_swallowing = dis125f.factor,   #Difficulty swallowing
                dis_symp_Ocular_symptoms= dis125g.factor,          # Ocular symptoms (pain, redness,Visual loss)
                dis_symp_Urination_pain = dis125h.factor,          #Pain with urination
                dis_symp_Fatigue = dis125i.factor,                 #Fatigue/malaise
                dis_symp_Urethritis = dis125j.factor,              #Urethritis
                dis_symp_Oral_pain = dis125k.factor,               #Oral pain
                dis_symp_Chest_pain = dis125l.factor,              #Chest pain
                dis_symp_Nausea = dis125m.factor,                  #Nausea
                dis_symp_Decreased_urine = dis125n.factor,         #Decreased urine output
                dis_symp_Vomiting = dis125o.factor,                #Vomiting
                dis_symp_Dizziness = dis125p.factor,               #Dizziness
                dis_symp_Diarrhoea = dis125q.factor,               #Diarrhoea
                dis_symp_Joint_pain = dis125r.factor,              #Joint pain (arthralgia)
                dis_symp_Rectal_pain = dis125s.factor,             #Rectal pain
                dis_symp_Psychologic_disturbance = dis125t.factor, #Psychologic disturbance
                dis_symp_Lymphadenopathy = dis125w.factor,         #Lymphadenopathy:
                dis_symp_Axillary = dis125w1.factor,               #Axillary
                dis_symp_Cervical = dis125w2.factor,               #Cervical
                dis_symp_Inguinal = dis125w3.factor,               #Inguinal
                Country = admcou.factor
        )
        
        
}




make_tbl_symptoms_baseline<-function(data_in, country, list_cases){
        
        tab_1 <- data_in |>dplyr::filter(is.na(redcap_repeat_instrument)|redcap_repeat_instrument=="")|>
                dplyr::select("country","admpar",starts_with("adm73")&!contains("factor"))|>sjlabelled::label_to_colnames()|>
                unlabel()|>
                dplyr::select(!contains("other")&!contains("Other"))|>pivot_longer(
                        cols = -c(country,`Participant ID`),
                        names_to = "name",
                        values_to = "value"
                )|>
                dplyr::mutate(value_num=to_yn_num(value))|>
                dplyr::filter(!is.na(value))|>
                dplyr::summarise(n=sum(value_num,na.rm=T),.by=c(country,`Participant ID`,name))|>
                dplyr::mutate(prev=ifelse(n>0,"Yes","No"),.by=c(country,name))|>
                dplyr::summarise(
                        N = n_distinct(`Participant ID`),#sum(!is.na(prev)),
                        n = sum(str_count("Yes",prev)),
                        pct = n / N,
                        .by = c(country,name)
                ) |>
                dplyr::mutate(
                        lab_pct = str_c(round(pct * 100), "%"),
                        lab_n = str_c(n, " / ", N)
                ) |>
                dplyr::select(-lab_pct,-country)|>
                gt()|>
                # gt_merge_stack(col1 = location, col2 = groups) %>%
                cols_label(name = "Variable",
                           N = "Sample size",
                           n = "Prevalence",
                           lab_n="Frequency",
                ) %>%
                fmt_percent(
                        columns = pct,
                        decimals = 1
                )|>
                cols_align(
                        align = c( "center"),
                        columns = c(pct,N,n,lab_n)
                )|>
                gt_plt_bar(n, fill = colors[1],color =colors[1] )%>%
                gt_color_rows(pct, palette = c("white", colors[1])) |>
                cols_width(name~px(60))%>%
                tab_options(heading.title.font.size = px(24)) %>%
                # tab_header(
                #         title = md(paste0(myflag , " Symptoms"))
                # 
                # ) %>%
                tab_source_note(md("Source: *WHO*"))
        
        return(tab_1)
}


make_tbl_baseline <- function(data_in, country, list_cases){
        tbl <- data_in |>
                age_fun()|> 
                hiv_fun()|>
                dplyr::filter(is.na(redcap_repeat_instrument)|redcap_repeat_instrument=="")|>
                add_facility_short()|>
                dplyr::mutate(date_onset=as.Date(adm12),
                              participant_id = as.character(admpar))|>
                # dplyr::left_join(data_hiv,by=c("facility_short","participant_id","date_admission"))|>
                dplyr::mutate(facility=ifelse(facility_short=="",NA,facility_short))|>
                dplyr::mutate(Weight=as.numeric(adm22),
                              Height=as.numeric(adm21),
                )|>
                dplyr::mutate(Weight=case_when(Weight>0&Weight<300~Weight,
                                               Weight<0&Weight>300~NA,
                                               .default = NA),
                              Height=case_when(Height>0&Height<300~Height,
                                               Height<0&Height>250~NA,
                                               .default = NA)
                )|>
                dplyr::mutate( bmi = Weight/((Height/100)^2), 
                               # `Sexual behavior`=case_when(sexual_behavior=="1"~"Sex with men",
                               #                             sexual_behavior=="2"~"Sex with women",
                               #                             sexual_behavior=="3"~"Sex with men and women",
                               #                             .default=NA),
                               # `Multiple partners during 21 days before symptom onset`=case_when(multiple_partners=="1"~"Yes",
                               #                                                                   multiple_partners=="2"~"No",
                               #                                                                   multiple_partners=="3"~"Unknown",
                               #                                                                   .default=NA),
                               # `Estimated number of sexual partners in the last 21 days`=case_when(number_partners=="1"~"Zero",
                               #                                                                     number_partners=="2"~"One",
                               #                                                                     number_partners=="3"~"2-5",
                               #                                                                     number_partners=="4"~"6-10",
                               #                                                                     number_partners=="5"~"More than 10",
                               #                                                                     number_partners=="6"~"Prefer not to answer",
                               #                                                                     .default=NA),
                               # `Probable mode of transmission`=case_when(transmission_mode=="1"~"Sexual",
                               #                                           transmission_mode=="2"~"Animal-to-human transmission",
                               #                                           transmission_mode=="3"~"Associated with health care",
                               #                                           transmission_mode=="4"~"Transmission in the laboratory",
                               #                                           transmission_mode=="5"~"Transmission from mother to child",
                               #                                           transmission_mode=="6"~"Person-to-person direct contact transmission",
                               #                                           transmission_mode=="7"~"Contact with contaminated material",
                               #                                           transmission_mode=="8"~"Blood transfusion",
                               #                                           transmission_mode=="9"~"Unknown",
                               #                                           .default=NA),
                               # hiv_status=case_when(
                               #         adm40.factor=="Yes (on antiretroviral therapy/ART)"~"People with HIV",
                               #         adm40.factor=="Yes (not on antiretroviral therapy/ART)"~"People with HIV",
                               #         adm40.factor=="Yes (unknown if on antiretroviral therapy/ART)"~"People with HIV",
                               #         adm40.factor=="No"~"Peoples without HIV",
                               #         adm40.factor=="Unknown"~"Unknown HIV status",.default = NA),
                               # `Age group` =  cut(age,
                               #                    breaks=c(-Inf, 5,15,25,45, 65,+Inf),
                               #                    right=TRUE,
                               #                    labels = c("≤5 yrs","5-15 yrs","15-25 yrs","25-45 yrs",
                               #                               "45-65 yrs",">65 yrs")),
                               
                               pregnant = to_yn_num(adm07)
                )|>
                labelled::set_variable_labels(
                        adm02.factor="Sex",
                        adm04.factor="Health care worker",
                        pregnant="Pregnant",
                        adm03_1 = "Age",
                        `Age group` = "Age group",
                        bmi = "BMI",
                        adm40.factor="HIV",
                        hiv_status = "HIV status"  ) |>
                tbl_summary(
                        include = c(adm03_1,`Age group`,adm02.factor,adm04.factor, bmi, hiv_status,pregnant,
                                    # `Probable mode of transmission`
                                    ),
                        type = list(c( bmi) ~ "continuous"),
                        missing = "no"
                ) |>
                add_n()
        
        return(tbl)
        
}

plot_map_lgd_fun_norender_country<- function(data_in){
        # NGA_box <- sf::st_bbox(data_in)
        # mylon<- mean(countryref[countryref$iso3==myiso,]$centroid.lon)
        # mylat<- mean(countryref[countryref$iso3==myiso,]$centroid.lat)
        mylon<- mean(countryref[countryref$iso3==reactive_db_iso3(),]$centroid.lon)
        mylat<- mean(countryref[countryref$iso3==reactive_db_iso3(),]$centroid.lat)
        # death_range <- range(c(data_in$death_2020,
        #                        data_in$death_2021,
        #                        data_in$death_2022,
        #                        data_in$death_2023,
        #                        data_in$death_2024), na.rm = TRUE)
        # death_colors <- c("white", "#F8B195", "#F67280","#b53737", "#C06C84", "#6C5B7B", "#355C7D")
        # bins_death <- c(0,seq(1,to=death_range[2],length=length(death_colors)))
        # cv_pal_death <- colorBin(palette = death_colors, domain = data_in_renamed$death, bins = bins_death, na.color = "transparent")
        # case_range <- range(c(data_in$confirmed_case_2020,
        #                       data_in$confirmed_case_2021,
        #                       data_in$confirmed_case_2022,
        #                       data_in$confirmed_case_2023,
        #                       data_in$confirmed_case_2024), na.rm = TRUE)
        # case_colors <- c("white", "#c9f0d3", "#9ad1c4", "#77b1a9", "#5f867a", "#287c6f","#4b5d67")
        # bins_case <- c(0,seq(1,to=case_range[2],length=length(case_colors)))
        # cv_pal_case <- colorBin(palette = case_colors, domain = data_in_renamed$confirmed, bins = bins_case, na.color = "transparent")
        
        
        case_range <- range(c(data_in$cases), na.rm = TRUE) 
        cases_colors <- c("white", "#bfddff","#FFFFBF", "#FEE08B","#FDAE61","#F46D43","#D53E4F")
        bins_case <- c(0, seq(1, to = case_range[2], length = length(cases_colors)))
        cv_pal_case <- colorBin(palette = cases_colors, domain = data_in$cases, bins = bins_case, na.color = "transparent")
        leaflet(data_in,
                options = leaflet::leafletOptions(attributionControl=T, maxZoom = 9, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
                # addProviderTiles(providers$Stadia) |>
                addProviderTiles(providers$CartoDB)|>
                leaflet::fitBounds(~-100,-60,~60,70) %>%
                setView(lng = mylon, lat = mylat, zoom = 3) |>  # Adjust lng, lat, and zoom as needed
                addMapPane("polygons", zIndex = 420) %>%  
                addMapPane("markers_pane", zIndex = 470) %>%
                leafem::addLogo(img = "https://www.who.int/images/default-source/fallback/header-logos/h-logo-blue.svg?sfvrsn=aaed4f35_18", 
                                src = "remote", position = "bottomleft", width = 200, offset.y = 70) |>
                addLayersControl(
                        position = "topright",
                        baseGroups = c("Cases"),
                        options = layersControlOptions(collapsed = FALSE)
                ) %>%
                # hideGroup("Deaths") %>%
                addScaleBar(
                        position = c("topleft"),
                        options = scaleBarOptions(
                                maxWidth = 100,
                                metric = TRUE,
                                imperial = TRUE,
                                updateWhenIdle = TRUE)
                ) %>%
                
                addPolygons(
                        fillColor = ~cv_pal_case(cases),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Cases",
                        popup = ~paste(adm0_viz_name, ": ", cases, " case(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_case, 
                          values = ~cases, 
                          title = "Reported cases", 
                          opacity = 1, 
                          group = "Cases",
                          labFormat = labelFormat(digits = 0))
        
}


plot_map_lgd_fun_norender_who2<- function(data_in){
  tilesURL1<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
  tilesURL2<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Disputed_Areas_and_Borders/MapServer/tile/{z}/{y}/{x}"
  tilesURL3<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Point_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
  tilesURL4<- "https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Basemap_Disputed_Areas_and_Borders_VTP/VectorTileServer"
  
  contributors_range <- range(c(data_in$contributors), na.rm = TRUE) 
  contributors_colors <- c("white",  "#a5c5ff", "#8badfe", "#7396ee", "#5b82d8", "#436ec1", "#265bab")
  bins_contributors <- c(0, seq(1, to = contributors_range[2], length = length(contributors_colors)))
  cv_pal_contributors <- colorBin(palette = contributors_colors, domain = data_in$contributors, bins = bins_contributors, na.color = "transparent")
  
  
  case_range <- range(c(data_in$cases), na.rm = TRUE) 
  cases_colors <- c("white", "#bfddff","#FFFFBF", "#FEE08B","#FDAE61","#F46D43","#D53E4F")
  bins_case <- c(0, seq(1, to = case_range[2], length = length(cases_colors)))
  cv_pal_case <- colorBin(palette = cases_colors, domain = data_in$cases, bins = bins_case, na.color = "transparent")
  leaflet(data_in,
          options = leaflet::leafletOptions(attributionControl=T, maxZoom = 9, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
    addMapPane("tiles1", zIndex = 410) %>%  # Level 1: bottom
    addMapPane("tiles2", zIndex = 430) %>%          # Level 3: top
    addMapPane("tiles3", zIndex = 440) %>%          # Level 4:
    addMapPane("tiles4", zIndex = 450) %>%          # Level 4:
    addMapPane("tiles5", zIndex = 460) %>%
    addMapPane("polygons", zIndex = 420) %>%  
    addMapPane("markers_pane", zIndex = 470) %>%
    leaflet::addTiles(tilesURL1,options  = tileOptions(opacity=1,weight = 1,pane = "tiles1", noWrap = F))|>
    addTiles(urlTemplate = "", attribution = "The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of WHO concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate border lines for which there may not yet be full agreement.",
             options  = tileOptions(pane = "tiles1", noWrap = T))|>
    leaflet::addTiles(tilesURL2,options = tileOptions(transparent=F,weight = 1,color = "#9c9c9c",pane = "tiles3", noWrap = F) ) |>
    addPolylines(data = spatialborder,dashArray = (1),color = "white",weight=1, options = pathOptions(pane = "tiles4"))|>
    addPolylines(data = spatialborder,dashArray = (3),weight=1, color = "#9c9c9c",options = pathOptions(pane = "tiles5"))|>
    
    # addProviderTiles(providers$Stadia) |>
    # addProviderTiles(providers$CartoDB)|>
    leaflet::fitBounds(~-100,-60,~60,70) %>%
    # setView(lng = mylon, lat = mylat, zoom = 3) |>  # Adjust lng, lat, and zoom as needed
    leafem::addLogo(img = "https://www.who.int/images/default-source/fallback/header-logos/h-logo-blue.svg?sfvrsn=aaed4f35_18", 
                    src = "remote", position = "bottomleft", width = 200, offset.y = 70) |>
    addLayersControl(
      position = "topright",
      baseGroups = c("Cases","Contributors"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup("Contributors") %>%
    addScaleBar(
      position = c("topleft"),
      options = scaleBarOptions(
        maxWidth = 100,
        metric = TRUE,
        imperial = TRUE,
        updateWhenIdle = TRUE)
    ) %>%
    
    addPolygons(
      fillColor = ~cv_pal_case(cases),
      color = "#BDBDBD",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.8,
      group = "Cases",
      popup = ~paste(adm0_viz_name, ": ", cases, " case(s)"),
      options = pathOptions(pane = "polygons")
    ) |>
    addPolygons(
      fillColor = ~cv_pal_contributors(contributors),
      color = "#BDBDBD",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.8,
      group = "Contributors",
      popup = ~paste(adm0_viz_name, ": ", contributors, " contributor(s)"),
      options = pathOptions(pane = "polygons")
    ) |>
    # Add legends, initially hidden
    addLegend("bottomright", 
              pal = cv_pal_case, 
              values = ~cases, 
              title = "Reported cases", 
              opacity = 1, 
              group = "Cases",
              labFormat = labelFormat(digits = 0))|>
    addLegend("bottomright", 
              pal = cv_pal_contributors, 
              values = ~contributors, 
              title = "Contributors", 
              opacity = 1, 
              group = "Contributors",
              labFormat = labelFormat(digits = 0))
  
}




plot_map_lgd_fun_norender_who<- function(data_in){
        tilesURL1<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
        tilesURL2<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Disputed_Areas_and_Borders/MapServer/tile/{z}/{y}/{x}"
        tilesURL3<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Point_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
        tilesURL4<- "https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Basemap_Disputed_Areas_and_Borders_VTP/VectorTileServer"
        
        
        case_range <- range(c(data_in$cases), na.rm = TRUE) 
        cases_colors <- c("white", "#bfddff","#FFFFBF", "#FEE08B","#FDAE61","#F46D43","#D53E4F")
        bins_case <- c(0, seq(1, to = case_range[2], length = length(cases_colors)))
        cv_pal_case <- colorBin(palette = cases_colors, domain = data_in$cases, bins = bins_case, na.color = "transparent")
        leaflet(data_in,
                options = leaflet::leafletOptions(attributionControl=T, maxZoom = 9, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
        addMapPane("tiles1", zIndex = 410) %>%  # Level 1: bottom
        addMapPane("tiles2", zIndex = 430) %>%          # Level 3: top
        addMapPane("tiles3", zIndex = 440) %>%          # Level 4:
        addMapPane("tiles4", zIndex = 450) %>%          # Level 4:
        addMapPane("tiles5", zIndex = 460) %>%
        addMapPane("polygons", zIndex = 420) %>%  
        addMapPane("markers_pane", zIndex = 470) %>%
                leaflet::addTiles(tilesURL1,options  = tileOptions(opacity=1,weight = 1,pane = "tiles1", noWrap = F))|>
                addTiles(urlTemplate = "", attribution = "The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of WHO concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate border lines for which there may not yet be full agreement.",
                         options  = tileOptions(pane = "tiles1", noWrap = T))|>
                leaflet::addTiles(tilesURL2,options = tileOptions(transparent=F,weight = 1,color = "#9c9c9c",pane = "tiles3", noWrap = F) ) |>
                addPolylines(data = spatialborder,dashArray = (1),color = "white",weight=1, options = pathOptions(pane = "tiles4"))|>
                addPolylines(data = spatialborder,dashArray = (3),weight=1, color = "#9c9c9c",options = pathOptions(pane = "tiles5"))|>
                
                # addProviderTiles(providers$Stadia) |>
                # addProviderTiles(providers$CartoDB)|>
                leaflet::fitBounds(~-100,-60,~60,70) %>%
                # setView(lng = mylon, lat = mylat, zoom = 3) |>  # Adjust lng, lat, and zoom as needed
                leafem::addLogo(img = "https://www.who.int/images/default-source/fallback/header-logos/h-logo-blue.svg?sfvrsn=aaed4f35_18", 
                                src = "remote", position = "bottomleft", width = 200, offset.y = 70) |>
                addLayersControl(
                        position = "topright",
                        baseGroups = c("Cases"),
                        options = layersControlOptions(collapsed = FALSE)
                ) %>%
                # hideGroup("Deaths") %>%
                addScaleBar(
                        position = c("topleft"),
                        options = scaleBarOptions(
                                maxWidth = 100,
                                metric = TRUE,
                                imperial = TRUE,
                                updateWhenIdle = TRUE)
                ) %>%
                
                addPolygons(
                        fillColor = ~cv_pal_case(cases),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Cases",
                        popup = ~paste(adm0_viz_name, ": ", cases, " case(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_case, 
                          values = ~cases, 
                          title = "Reported cases", 
                          opacity = 1, 
                          group = "Cases",
                          labFormat = labelFormat(digits = 0))
        
}



plot_map_lgd_fun_norender<- function(data_in){
        # NGA_box <- sf::st_bbox(data_in)
        # mylon<- mean(countryref[countryref$iso3==myiso,]$centroid.lon)
        # mylat<- mean(countryref[countryref$iso3==myiso,]$centroid.lat)
        
        # death_range <- range(c(data_in$death_2020,
        #                        data_in$death_2021,
        #                        data_in$death_2022,
        #                        data_in$death_2023,
        #                        data_in$death_2024), na.rm = TRUE)
        # death_colors <- c("white", "#F8B195", "#F67280","#b53737", "#C06C84", "#6C5B7B", "#355C7D")
        # bins_death <- c(0,seq(1,to=death_range[2],length=length(death_colors)))
        # cv_pal_death <- colorBin(palette = death_colors, domain = data_in_renamed$death, bins = bins_death, na.color = "transparent")
        # case_range <- range(c(data_in$confirmed_case_2020,
        #                       data_in$confirmed_case_2021,
        #                       data_in$confirmed_case_2022,
        #                       data_in$confirmed_case_2023,
        #                       data_in$confirmed_case_2024), na.rm = TRUE)
        # case_colors <- c("white", "#c9f0d3", "#9ad1c4", "#77b1a9", "#5f867a", "#287c6f","#4b5d67")
        # bins_case <- c(0,seq(1,to=case_range[2],length=length(case_colors)))
        # cv_pal_case <- colorBin(palette = case_colors, domain = data_in_renamed$confirmed, bins = bins_case, na.color = "transparent")
        
        
        case_range <- range(c(data_in$cases), na.rm = TRUE) 
        cases_colors <- c("white", "#bfddff","#FFFFBF", "#FEE08B","#FDAE61","#F46D43","#D53E4F")
        bins_case <- c(0, seq(1, to = case_range[2], length = length(cases_colors)))
        cv_pal_case <- colorBin(palette = cases_colors, domain = data_in$cases, bins = bins_case, na.color = "transparent")
        leaflet(data_in,
                options = leaflet::leafletOptions(attributionControl=T, maxZoom = 9, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
                # addProviderTiles(providers$Stadia) |>
                addProviderTiles(providers$CartoDB)|>
                leaflet::fitBounds(~-100,-60,~60,70) %>%
                # setView(lng = mylon, lat = mylat, zoom = 3) |>  # Adjust lng, lat, and zoom as needed
                addMapPane("polygons", zIndex = 420) %>%  
                addMapPane("markers_pane", zIndex = 470) %>%
                leafem::addLogo(img = "https://www.who.int/images/default-source/fallback/header-logos/h-logo-blue.svg?sfvrsn=aaed4f35_18", 
                                src = "remote", position = "bottomleft", width = 200, offset.y = 70) |>
                addLayersControl(
                        position = "topright",
                        baseGroups = c("Cases"),
                        options = layersControlOptions(collapsed = FALSE)
                ) %>%
                # hideGroup("Deaths") %>%
                addScaleBar(
                        position = c("topleft"),
                        options = scaleBarOptions(
                                maxWidth = 100,
                                metric = TRUE,
                                imperial = TRUE,
                                updateWhenIdle = TRUE)
                ) %>%
                
                addPolygons(
                        fillColor = ~cv_pal_case(cases),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Cases",
                        popup = ~paste(adm0_viz_name, ": ", cases, " case(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_case, 
                          values = ~cases, 
                          title = "Reported cases", 
                          opacity = 1, 
                          group = "Cases",
                          labFormat = labelFormat(digits = 0))

}




# c("#EFF5E7", "#DCEAC9", "#AACF7F","#80BC00",  "#629633",  "#437022")
# 
# c("#FEECE0", "#FFE3C2", "#FCBC71","#F26829", "#BE511E",  "#8D390A")
# c("#DDEFF9", "#C9DDF3","#79B5E3","#009ADE", "#167BAB",  "#045B81")
# 
# c("#FEE8E1", "#FBCCBF", "#F48373", "#EF3842", "#BB2932", "#8C171E")

plot_map_official_fun_contributors_cases<- function(data_in){
        NGA_box <- sf::st_bbox(data_in)
        tilesURL1<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
        tilesURL2<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Disputed_Areas_and_Borders/MapServer/tile/{z}/{y}/{x}"
        tilesURL3<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Point_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
        tilesURL4<- "https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Basemap_Disputed_Areas_and_Borders_VTP/VectorTileServer"
        
        
        cases_range <- range(c(data_in$cases), na.rm = TRUE) 
        cases_colors <- c("white", "#FEE8E1", "#FBCCBF", "#F48373", "#EF3842", "#BB2932", "#8C171E")
        bins_cases <- c(0, seq(1, to = cases_range[2], length = length(cases_colors)))
        cv_pal_cases <- colorBin(palette = cases_colors, domain = data_in$cases, bins = bins_cases, na.color = "transparent")
        
        contributors_range <- range(c(data_in$contributors), na.rm = TRUE) 
        contributors_colors <- c("white", "#DDEFF9", "#C9DDF3","#79B5E3","#009ADE", "#167BAB",  "#045B81")
        bins_contributors <- c(0, seq(1, to = contributors_range[2], length = length(contributors_colors)))
        cv_pal_contributor <- colorBin(palette = contributors_colors, domain = data_in$contributors, bins = bins_contributors, na.color = "transparent")
        leaflet(data_in,
                options = leaflet::leafletOptions(attributionControl=T, maxZoom = 6, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
                addMapPane("tiles1", zIndex = 410) %>%  # Level 1: bottom
                addMapPane("tiles2", zIndex = 430) %>%          # Level 3: top
                addMapPane("tiles3", zIndex = 440) %>%          # Level 4:
                addMapPane("tiles4", zIndex = 450) %>%          # Level 4:
                addMapPane("tiles5", zIndex = 460) %>%
                addMapPane("polygons", zIndex = 420) %>%  
                addMapPane("markers_pane", zIndex = 470) %>%
                leaflet::addTiles(tilesURL1,options  = tileOptions(opacity=1,weight = 1,pane = "tiles1", noWrap = F))|>
                addTiles(urlTemplate = "", attribution = "The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of WHO concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate border lines for which there may not yet be full agreement.",
                         options  = tileOptions(pane = "tiles1", noWrap = T))|>
                leaflet::addTiles(tilesURL2,options = tileOptions(transparent=F,weight = 1,color = "#9c9c9c",pane = "tiles3", noWrap = F) ) |>
                addPolylines(data = spatialborder,dashArray = (1),color = "white",weight=1, options = pathOptions(pane = "tiles4"))|>
                addPolylines(data = spatialborder,dashArray = (3),weight=1, color = "#9c9c9c",options = pathOptions(pane = "tiles5"))|>
                
                leaflet::fitBounds(~-100,-60,~60,70) %>%
                # setView(lng = mylon, lat = mylat, zoom = 3) |>  # Adjust lng, lat, and zoom as needed

                leafem::addLogo(img = "https://www.who.int/images/default-source/fallback/header-logos/h-logo-blue.svg?sfvrsn=aaed4f35_18", 
                                src = "remote", position = "bottomleft", width = 200, offset.y = 70) |>
                addLayersControl(
                        position = "topright",
                        baseGroups = c("Contributors","Cases"),
                        options = layersControlOptions(collapsed = FALSE)
                ) %>%
                # hideGroup("Deaths") %>%
                addScaleBar(
                        position = c("topleft"),
                        options = scaleBarOptions(
                                maxWidth = 100,
                                metric = TRUE,
                                imperial = TRUE,
                                updateWhenIdle = TRUE)
                ) %>%
                
                addPolygons(
                        fillColor = ~cv_pal_cases(cases),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Cases",
                        popup = ~paste(adm0_viz_name, ": ", cases, " case(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                addPolygons(
                        fillColor = ~cv_pal_contributor(contributors),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Contributors",
                        popup = ~paste(adm0_viz_name, ": ", contributors, " contributor(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_contributor, 
                          values = ~contributors, 
                          title = "Contributors", 
                          opacity = 1, 
                          group = "Contributors",
                          labFormat = labelFormat(digits = 0)) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_cases, 
                          values = ~cases, 
                          title = "Cases", 
                          opacity = 1, 
                          group = "Cases",
                          labFormat = labelFormat(digits = 0))
        
}

plot_map_official_fun_contributors_cases_whites<- function(data_in){
        NGA_box <- sf::st_bbox(data_in)
        tilesURL1<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
        tilesURL2<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Raster_Disputed_Areas_and_Borders/MapServer/tile/{z}/{y}/{x}"
        tilesURL3<-"https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Point_Raster_Basemap/MapServer/tile/{z}/{y}/{x}" # no names
        tilesURL4<- "https://tiles.arcgis.com/tiles/5T5nSi527N4F7luB/arcgis/rest/services/WHO_Polygon_Basemap_Disputed_Areas_and_Borders_VTP/VectorTileServer"
        
        
        cases_range <- range(c(data_in$cases), na.rm = TRUE) 
        cases_colors <- c("white", "#FEE8E1", "#FBCCBF", "#F48373", "#EF3842", "#BB2932", "#8C171E")
        bins_cases <- c(0, seq(1, to = cases_range[2], length = length(cases_colors)))
        cv_pal_cases <- colorBin(palette = cases_colors, domain = data_in$cases, bins = bins_cases, na.color = "white")
        
        contributors_range <- range(c(data_in$contributors), na.rm = TRUE) 
        contributors_colors <- c("white", "#DDEFF9", "#C9DDF3","#79B5E3","#009ADE", "#167BAB",  "#045B81")
        bins_contributors <- c(0, seq(1, to = contributors_range[2], length = length(contributors_colors)))
        cv_pal_contributor <- colorBin(palette = contributors_colors, domain = data_in$contributors, bins = bins_contributors, na.color = "white")
        leaflet(data_in,
                options = leaflet::leafletOptions(attributionControl=T, maxZoom = 6, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
                addMapPane("tiles1", zIndex = 410) %>%  # Level 1: bottom
                addMapPane("tiles2", zIndex = 430) %>%          # Level 3: top
                addMapPane("tiles3", zIndex = 440) %>%          # Level 4:
                addMapPane("tiles4", zIndex = 450) %>%          # Level 4:
                addMapPane("tiles5", zIndex = 460) %>%
                addMapPane("polygons", zIndex = 420) %>%  
                addMapPane("markers_pane", zIndex = 470) %>%
                leaflet::addTiles(tilesURL1,options  = tileOptions(opacity=1,weight = 1,pane = "tiles1", noWrap = F))|>
                addTiles(urlTemplate = "", attribution = "The designations employed and the presentation of the material in this publication do not imply the expression of any opinion whatsoever on the part of WHO concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate border lines for which there may not yet be full agreement.",
                         options  = tileOptions(pane = "tiles1", noWrap = T))|>
                leaflet::addTiles(tilesURL2,options = tileOptions(transparent=F,weight = 1,color = "#9c9c9c",pane = "tiles3", noWrap = F) ) |>
                addPolylines(data = spatialborder,dashArray = (1),color = "white",weight=1, options = pathOptions(pane = "tiles4"))|>
                addPolylines(data = spatialborder,dashArray = (3),weight=1, color = "#9c9c9c",options = pathOptions(pane = "tiles5"))|>
                
                # leaflet::fitBounds(~-100,-60,~60,70) %>%
                # setView(lng = mylon, lat = mylat, zoom = 3) |>  # Adjust lng, lat, and zoom as needed
                
                leafem::addLogo(img = "https://www.who.int/images/default-source/fallback/header-logos/h-logo-blue.svg?sfvrsn=aaed4f35_18", 
                                src = "remote", position = "bottomleft", width = 200, offset.y = 70) |>
                addLayersControl(
                        position = "topright",
                        baseGroups = c("Contributors","Cases"),
                        options = layersControlOptions(collapsed = FALSE)
                ) %>%
                # hideGroup("Deaths") %>%
                addScaleBar(
                        position = c("topleft"),
                        options = scaleBarOptions(
                                maxWidth = 100,
                                metric = TRUE,
                                imperial = TRUE,
                                updateWhenIdle = TRUE)
                ) %>%
                
                addPolygons(
                        fillColor = ~cv_pal_cases(cases),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Cases",
                        popup = ~paste(adm0_viz_name, ": ", cases, " case(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                addPolygons(
                        fillColor = ~cv_pal_contributor(contributors),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Contributors",
                        popup = ~paste(adm0_viz_name, ": ", contributors, " contributor(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_contributor, 
                          values = ~contributors, 
                          title = "Contributors", 
                          opacity = 1, 
                          group = "Contributors",
                          labFormat = labelFormat(digits = 0)) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_cases, 
                          values = ~cases, 
                          title = "Cases", 
                          opacity = 1, 
                          group = "Cases",
                          labFormat = labelFormat(digits = 0))
        
}

plot_map_lgd_fun_contributors_cases<- function(data_in){
        NGA_box <- sf::st_bbox(data_in)
        # mylon<- mean(countryref[countryref$iso3==myiso,]$centroid.lon)
        # mylat<- mean(countryref[countryref$iso3==myiso,]$centroid.lat)
        
        # death_range <- range(c(data_in$death_2020,
        #                        data_in$death_2021,
        #                        data_in$death_2022,
        #                        data_in$death_2023,
        #                        data_in$death_2024), na.rm = TRUE)
        # death_colors <- c("white", "#F8B195", "#F67280","#b53737", "#C06C84", "#6C5B7B", "#355C7D")
        # bins_death <- c(0,seq(1,to=death_range[2],length=length(death_colors)))
        # cv_pal_death <- colorBin(palette = death_colors, domain = data_in_renamed$death, bins = bins_death, na.color = "transparent")
        # case_range <- range(c(data_in$confirmed_case_2020,
        #                       data_in$confirmed_case_2021,
        #                       data_in$confirmed_case_2022,
        #                       data_in$confirmed_case_2023,
        #                       data_in$confirmed_case_2024), na.rm = TRUE)
        # case_colors <- c("white", "#c9f0d3", "#9ad1c4", "#77b1a9", "#5f867a", "#287c6f","#4b5d67")
        # bins_case <- c(0,seq(1,to=case_range[2],length=length(case_colors)))
        # cv_pal_case <- colorBin(palette = case_colors, domain = data_in_renamed$confirmed, bins = bins_case, na.color = "transparent")
        
        
        cases_range <- range(c(data_in$cases), na.rm = TRUE) 
        cases_colors <- c("white", "#F8B195", "#F67280","#b53737", "#C06C84", "#6C5B7B", "#355C7D")
        cases_colors <- c("white", "#FEE8E1", "#FBCCBF", "#F48373", "#EF3842", "#BB2932", "#8C171E")
        bins_cases <- c(0, seq(1, to = cases_range[2], length = length(cases_colors)))
        cv_pal_cases <- colorBin(palette = cases_colors, domain = data_in$cases, bins = bins_cases, na.color = "transparent")
        
        contributors_range <- range(c(data_in$contributors), na.rm = TRUE) 
        contributors_colors <- c("white", "#bfddff","#FFFFBF", "#FEE08B","#FDAE61","#F46D43","#D53E4F")
        contributors_colors <- c("white", "#DDEFF9", "#C9DDF3","#79B5E3","#009ADE", "#167BAB",  "#045B81")
        bins_contributors <- c(0, seq(1, to = contributors_range[2], length = length(contributors_colors)))
        cv_pal_contributor <- colorBin(palette = contributors_colors, domain = data_in$contributors, bins = bins_contributors, na.color = "transparent")
        leaflet(data_in,
                options = leaflet::leafletOptions(attributionControl=T, maxZoom = 6, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
                # addProviderTiles(providers$Stadia) |>
                addProviderTiles(providers$CartoDB)|>
                leaflet::fitBounds(~-100,-60,~60,70) %>%
                # setView(lng = mylon, lat = mylat, zoom = 3) |>  # Adjust lng, lat, and zoom as needed
                addMapPane("polygons", zIndex = 420) %>%  
                addMapPane("markers_pane", zIndex = 470) %>%
                leafem::addLogo(img = "https://www.who.int/images/default-source/fallback/header-logos/h-logo-blue.svg?sfvrsn=aaed4f35_18", 
                                src = "remote", position = "bottomleft", width = 200, offset.y = 70) |>
                addLayersControl(
                        position = "topright",
                        baseGroups = c("Contributors","Cases"),
                        options = layersControlOptions(collapsed = FALSE)
                ) %>%
                # hideGroup("Deaths") %>%
                addScaleBar(
                        position = c("topleft"),
                        options = scaleBarOptions(
                                maxWidth = 100,
                                metric = TRUE,
                                imperial = TRUE,
                                updateWhenIdle = TRUE)
                ) %>%
                
                addPolygons(
                        fillColor = ~cv_pal_cases(cases),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Cases",
                        popup = ~paste(adm0_viz_name, ": ", cases, " case(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                addPolygons(
                        fillColor = ~cv_pal_contributor(contributors),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Contributors",
                        popup = ~paste(adm0_viz_name, ": ", contributors, " contributor(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_contributor, 
                          values = ~contributors, 
                          title = "Contributors", 
                          opacity = 1, 
                          group = "Contributors",
                          labFormat = labelFormat(digits = 0)) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_cases, 
                          values = ~cases, 
                          title = "Cases", 
                          opacity = 1, 
                          group = "Cases",
                          labFormat = labelFormat(digits = 0))
        
}

plot_map_lgd_fun<- function(data_in){
        NGA_box <- sf::st_bbox(data_in)
        mylon<- mean(countryref[countryref$iso3==myiso,]$centroid.lon)
        mylat<- mean(countryref[countryref$iso3==myiso,]$centroid.lat)
        
        # death_range <- range(c(data_in$death_2020,
        #                        data_in$death_2021,
        #                        data_in$death_2022,
        #                        data_in$death_2023,
        #                        data_in$death_2024), na.rm = TRUE)
        # death_colors <- c("white", "#F8B195", "#F67280","#b53737", "#C06C84", "#6C5B7B", "#355C7D")
        # bins_death <- c(0,seq(1,to=death_range[2],length=length(death_colors)))
        # cv_pal_death <- colorBin(palette = death_colors, domain = data_in_renamed$death, bins = bins_death, na.color = "transparent")
        # case_range <- range(c(data_in$confirmed_case_2020,
        #                       data_in$confirmed_case_2021,
        #                       data_in$confirmed_case_2022,
        #                       data_in$confirmed_case_2023,
        #                       data_in$confirmed_case_2024), na.rm = TRUE)
        # case_colors <- c("white", "#c9f0d3", "#9ad1c4", "#77b1a9", "#5f867a", "#287c6f","#4b5d67")
        # bins_case <- c(0,seq(1,to=case_range[2],length=length(case_colors)))
        # cv_pal_case <- colorBin(palette = case_colors, domain = data_in_renamed$confirmed, bins = bins_case, na.color = "transparent")
        
        
        case_range <- range(c(data_in$cases), na.rm = TRUE) 
        cases_colors <- c("white", "#bfddff","#FFFFBF", "#FEE08B","#FDAE61","#F46D43","#D53E4F")
        
        bins_case <- c(0, seq(1, to = case_range[2], length = length(cases_colors)))
        cv_pal_case <- colorBin(palette = cases_colors, domain = data_in$cases, bins = bins_case, na.color = "transparent")
        leaflet(data_in,
                options = leaflet::leafletOptions(attributionControl=T, maxZoom = 9, zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
                # addProviderTiles(providers$Stadia) |>
                addProviderTiles(providers$CartoDB)|>
                leaflet::fitBounds(~-100,-60,~60,70) %>%
                # setView(lng = mylon, lat = mylat, zoom = 3) |>  # Adjust lng, lat, and zoom as needed
                addMapPane("polygons", zIndex = 420) %>%  
                addMapPane("markers_pane", zIndex = 470) %>%
                leafem::addLogo(img = "https://www.who.int/images/default-source/fallback/header-logos/h-logo-blue.svg?sfvrsn=aaed4f35_18", 
                                src = "remote", position = "bottomleft", width = 200, offset.y = 70) |>
                addLayersControl(
                        position = "topright",
                        baseGroups = c("Cases"),
                        options = layersControlOptions(collapsed = FALSE)
                ) %>%
                # hideGroup("Deaths") %>%
                addScaleBar(
                        position = c("topleft"),
                        options = scaleBarOptions(
                                maxWidth = 100,
                                metric = TRUE,
                                imperial = TRUE,
                                updateWhenIdle = TRUE)
                ) %>%
                
                addPolygons(
                        fillColor = ~cv_pal_case(cases),
                        color = "#BDBDBD",
                        weight = 1,
                        opacity = 1,
                        fillOpacity = 0.8,
                        group = "Cases",
                        popup = ~paste(adm0_viz_name, ": ", cases, " case(s)"),
                        options = pathOptions(pane = "polygons")
                ) |>
                # Add legends, initially hidden
                addLegend("bottomright", 
                          pal = cv_pal_case, 
                          values = ~cases, 
                          title = "Reported cases", 
                          opacity = 1, 
                          group = "Cases",
                          labFormat = labelFormat(digits = 0)) |>
                # Layer control and legend toggling
                onRender("
    function(el, x) {
      var casesLegend = document.getElementsByClassName('legend')[2];
      var deathsLegend = document.getElementsByClassName('legend')[1];
      var cfrLegend = document.getElementsByClassName('legend')[0];
      
      deathsLegend.style.display = 'none';
      cfrLegend.style.display = 'none';

      this.on('baselayerchange', function(e) {
        if (e.name === 'Cases') {
          casesLegend.style.display = 'block';
          deathsLegend.style.display = 'none';
          cfrLegend.style.display = 'none';
        } else if (e.name === 'Deaths') {
          casesLegend.style.display = 'none';
          deathsLegend.style.display = 'block';
          cfrLegend.style.display = 'none';
        } else if (e.name === 'CFRs') {
          casesLegend.style.display = 'none';
          deathsLegend.style.display = 'none';
          cfrLegend.style.display = 'block';
        }
      });
    }
  ")
        
        
}






varlist=c("record_id", "admpar", "adm01", "adm14","Age", "Age group", "Sex", "Vaccinated", "Country",
          
          # Existing chronic condition
          "Cardiac", "Diabetes", "Hypertension", "Pulmonary", "HIV", "HIV_ART",
          "TB", "Neurological", "Kidney_disease", "Liver_disease", "malignant_neoplasm",
          "STI", "Smoking", "Alcohol",
          
          # Exposure
          "Contact_with_case", "Animal_source", "Sexully_active", "International_travel",
          
          # Signs & symptoms at admission
          "Fatigue", "Sorethroat", "Muscle_ache", "Headache", "Ocular_symptoms", 
          "Oral_pain", "Nausea", "Vomiting", "Diarrhoea", "Rectal_pain",  
          "Proctitis", "Pain_swallowing", "Difficulty_swallowing", "Pain_urination",
          "Urethritis","Chest_pain", "Decreased_urine_output", "Dizziness", 
          "Joint_pain", "Psychological_disturbance", "Lymphadenopathy", 
          "Axillary", "Cervical","Inguinal",
          
          # Lesion
          "Face_lesion", "Nares_lesion", "Mouth_lesion", "Chest_lesion", "Abdomen_lesion",
          "Back_lesion", "Perianal_lesion", "Genitals_lesion", "Rectal_lesion", "Palms_lesion",
          "Arms_lesion", "Forearms_lesion", "Thighs_lesion", "Legs_lesion", "Soles_lesion", "number_lesion",
          
          # Lesion type
          "Macule", "Papule", "Early_vesicle", "Small_pustule", "Umbilicated_pustule", "Ulcerated_lesion",
          "Crusting", "Partially_removed_scab",
          
          # Vitals
          "Temperature","SBP", "DBP", "Heart_rate", "Respiratory_rate", "Weight", "Height",
          
          #Laboratory test @ start
          "Haemoglobin", "Platelets", "wbc_count", "Glucose", "ALT", "AST", "Creatinine","Potassium", "CRP",
          
          #Extreme Laboratory tests
          "ext_Haemoglobin", "ext_Platelets", "ext_wbc_count", "ext_Glucose", "ext_ALT",
          "ext_AST", "ext_Creatinine", "ext_Potassium", "ext_CRP",
          
          # Complications
          "Shock_comp", "Sepsis_comp", "Seizure_comp", "Meningitis_comp", "Encephalitis_comp", 
          "Anaemia_comp", "Cardiac_arrhythmia_comp", "Cardiac_arrest_comp", "Pneumonia_comp", 
          "Cellulitis_comp", "ARDS_comp", "Necrotizing_infection_comp", "Abscess_comp", 
          "Bacteraemia_comp", "Bleeding_disorder_comp", "ischaemic_stroke_comp", 
          "GBS_comp", "Myocarditis_comp", "Pericarditis_comp", "Acute_renal_injury_comp",
          "Pancreatitis_comp", "Liver_dysfunction_comp",  "Cardiomyopathy_comp", "Rectal_Pain_comp",
          "Urinary_retention_comp", "Ocular_infection_comp", "Proctitis_comp", "Rectal_Urgency_comp",
          
          
          
          
          # Treatment 
          "Nasogastric", "Intravenous_fluids", "Antivirals", "Tecovirimat", "Brincidovir",
          "Cidovir", "Antibiotics", "Antifungals", "Analgesia", "Concomitant_meds", 
          
          # Supportive care
          "Renal_replacement_therapy", "ICU_admission", "Oxygen_therapy",
          "Non_invasive_ventilation", "ECMO_support", "Invasive_ventilation",
          
          # Diagnosis
          "Suspected", "Probable", "Confirmed",
          
          # Outcome
          "Outcome", "Hospitalization", "unresolved_lesion", "num_unresolved_lesion", "self_care", 
          
          # Symptoms at discharge
          "dis_symp_Sore_throat", "dis_symp_Proctitis", "dis_symp_Muscle_aches",
          "dis_symp_Swallowing_pain", "dis_symp_Headache", "dis_symp_Difficulty_swallowing",
          "dis_symp_Ocular_symptoms", "dis_symp_Urination_pain", "dis_symp_Fatigue",
          "dis_symp_Urethritis", "dis_symp_Oral_pain", "dis_symp_Chest_pain",
          "dis_symp_Nausea", "dis_symp_Decreased_urine", "dis_symp_Vomiting",
          "dis_symp_Dizziness", "dis_symp_Diarrhoea", "dis_symp_Joint_pain",
          "dis_symp_Rectal_pain", "dis_symp_Psychologic_disturbance", "dis_symp_Lymphadenopathy",
          "dis_symp_Axillary", "dis_symp_Cervical", "dis_symp_Inguinal")


identification =   c("record_id", "admpar", "adm01", "adm14")

demo_list = c("Age", "Age group", "Sex", "Vaccinated", "Country")

chronic_condition = c("Cardiac", "Diabetes", "Hypertension", "Pulmonary", "HIV", "HIV_ART",
                      "TB", "Neurological", "Kidney_disease", "Liver_disease", "malignant_neoplasm",
                      "STI", "Smoking", "Alcohol")

exposure_list = c("Contact_with_case", "Animal_source", "Sexully_active", "International_travel")

symptoms_list = c("Fatigue", "Sorethroat", "Muscle_ache", "Headache", "Ocular_symptoms", 
                  "Oral_pain", "Nausea", "Vomiting", "Diarrhoea", "Rectal_pain", 
                  "Proctitis", "Pain_swallowing", "Difficulty_swallowing", "Pain_urination",
                  "Urethritis","Chest_pain", "Decreased_urine_output", "Dizziness", 
                  "Joint_pain", "Psychological_disturbance", "Lymphadenopathy", 
                  "Axillary", "Cervical","Inguinal")

lesion_list <- c("Face_lesion", "Nares_lesion", "Mouth_lesion", "Chest_lesion", "Abdomen_lesion",
                 "Back_lesion", "Perianal_lesion", "Genitals_lesion", "Rectal_lesion", 
                 "Palms_lesion", "Arms_lesion", "Forearms_lesion", "Thighs_lesion",
                 "Legs_lesion", "Soles_lesion","number_lesion")

lesion_type <- c("Macule", "Papule", "Early_vesicle", "Small_pustule", "Umbilicated_pustule", 
                 "Ulcerated_lesion", "Crusting", "Partially_removed_scab")


vitals_list <- c("Temperature","SBP", "DBP", "Heart_rate", "Respiratory_rate", "Weight", "Height")

lab_test <- c( "Haemoglobin", "Platelets", "wbc_count", "Glucose", "ALT", "AST", "Creatinine","Potassium", "CRP")

ext_lab_test <- c("ext_Haemoglobin", "ext_Platelets", "ext_wbc_count", "ext_Glucose", 
                  "ext_ALT","ext_AST", "ext_Creatinine", "ext_Potassium", "ext_CRP")



treatment = c("Nasogastric", "Intravenous_fluids", "Antivirals", "Tecovirimat", "Brincidovir",
              "Cidovir", "Antibiotics", "Antifungals", "Analgesia", "Concomitant_meds")

supportive_care = c("Renal_replacement_therapy", "ICU_admission", "Oxygen_therapy",
                    "Non_invasive_ventilation", "ECMO_support", "Invasive_ventilation")


diagnosis = c("Suspected", "Probable", "Confirmed")


ouctome_list = c("Hospitalization", "Outcome", "self_care", "unresolved_lesion","num_unresolved_lesion")


discharge_symptoms = c("dis_symp_Sore_throat", "dis_symp_Proctitis", "dis_symp_Muscle_aches",
                       "dis_symp_Swallowing_pain", "dis_symp_Headache", "dis_symp_Difficulty_swallowing",
                       "dis_symp_Ocular_symptoms", "dis_symp_Urination_pain", "dis_symp_Fatigue",
                       "dis_symp_Urethritis", "dis_symp_Oral_pain", "dis_symp_Chest_pain",
                       "dis_symp_Nausea", "dis_symp_Decreased_urine", "dis_symp_Vomiting",
                       "dis_symp_Dizziness", "dis_symp_Diarrhoea", "dis_symp_Joint_pain",
                       "dis_symp_Rectal_pain", "dis_symp_Psychologic_disturbance", "dis_symp_Lymphadenopathy",
                       "dis_symp_Axillary", "dis_symp_Cervical", "dis_symp_Inguinal")


complications =  c("Shock_comp", "Sepsis_comp", "Seizure_comp", "Meningitis_comp", "Encephalitis_comp", 
                   "Anaemia_comp", "Cardiac_arrhythmia_comp", "Cardiac_arrest_comp", "Pneumonia_comp", 
                   "Cellulitis_comp", "ARDS_comp", "Necrotizing_infection_comp", "Abscess_comp", 
                   "Bacteraemia_comp", "Bleeding_disorder_comp", "ischaemic_stroke_comp", 
                   "GBS_comp", "Myocarditis_comp", "Pericarditis_comp", "Acute_renal_injury_comp",
                   "Pancreatitis_comp", "Liver_dysfunction_comp",  "Cardiomyopathy_comp", "Rectal_Pain_comp",
                   "Urinary_retention_comp", "Ocular_infection_comp", "Proctitis_comp", "Rectal_Urgency_comp")


label_fun<- function(data_in){

        data_out <- expss::apply_labels(data_in,
                                 Age = "Age (in years)",
                                 vaccine = "History of tetanus vaccination",
                                 HIV_ART = "On HIV ART", 
                                 Kidney_disease = "Kidney disease",
                                 Liver_disease = "Liver disease",
                                 malignant_neoplasm = "Malignant neoplasm",
                                 Contact_with_case = "Known contact with a case",
                                 Animal_source = "Contact with possible animal source",
                                 Sexully_active = "Sexual activity within preceding 21 days",
                                 International_travel = "Travel within preceding 21 days",
                                 Shock_comp = "Shock",
                                 Sepsis_comp = "Sepsis",
                                 Seizure_comp = "Seizure",
                                 Meningitis_comp = "Meningitis",
                                 Encephalitis_comp = "Encephalitis",
                                 Anaemia_comp = "Anaemia",
                                 Cardiac_arrhythmia_comp = "Cardiac arrhythmia",
                                 Cardiac_arrest_comp = "Cardiac arrest",
                                 Pneumonia_comp = "Pneumonia",
                                 Cellulitis_comp = "Cellulitis",
                                 ARDS_comp = "ARDS",
                                 Necrotizing_infection_comp = "Necrotizing infection",
                                 Abscess_comp = "Abscess",
                                 Bacteraemia_comp = "Bacteraemia",
                                 Bleeding_disorder_comp = "Bleeding disorder",
                                 ischaemic_stroke_comp = "ischaemic stroke",
                                 GBS_comp = "Guillain-Barré Syndrome",
                                 Myocarditis_comp = "Myocarditis",
                                 Pericarditis_comp = "Pericarditis",
                                 Acute_renal_injury_comp = "Acute renal injury",
                                 Pancreatitis_comp = "Pancreatitis",
                                 Liver_dysfunction_comp = "Liver dysfunction",
                                 Cardiomyopathy_comp = "Cardiomyopathy",
                                 Rectal_Pain_comp = "Rectal Pain",
                                 Urinary_retention_comp = "Urinary retention",
                                 Ocular_infection_comp = "Ocular infection",
                                 Proctitis_comp = "Proctitis",
                                 Rectal_Urgency_comp = "Rectal Urgency",
                                 Nasogastric = "Did the patient receive nasogastric fluids during course monkeypox infection?",
                                 Intravenous_fluids = "Did the patient receive intravenous fluids during course monkeypox infection?",
                                 Antivirals = "Did the patient receive experimental antiviral during course monkeypox infection?",
                                 Tecovirimat = "Tecovirimat",
                                 Brincidovir = "Brincidofovir",
                                 Cidovir = "Cidofovir",
                                 Antibiotics = "Did the patient receive antibiotics during course monkeypox infection",
                                 Antifungals = "Did the patient receive antifungals during course monkeypox infection",
                                 Analgesia = "Did the patient receive analgesia for pain management during course monkeypox infection",
                                 Concomitant_meds = "Is the patient taking any new concomitant medications",
                                 self_care = "If discharged alive, Is patient able to self-care at discharge versus before illness:",
                                 unresolved_lesion = "Are lesions resolved?", 
                                 num_unresolved_lesion = "If no, what is the number of lesions on the entire body that are NOT resolved (resolved = scabbed and desquamated with fresh layer of skin):")
}
