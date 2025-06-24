# AUTHOR:   Karishma Srikanth
# PURPOSE:  CHAI site-level IUD analysis
# REF ID:   1fca6384 
# DATE:     2025-06-23

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(readxl)
library(glamr) #custom workflow package
library(glitr) #custom viz package
library(gt)
library(gtExtras)
library(glue)
library(ggtext)


# GLOBAL VARIABLES --------------------------------------------------------
  
data_folder <- "Data"

# IMPORT ------------------------------------------------------------------
  
df <- data_folder %>% 
  list.files(full.names = TRUE) %>% 
  read_excel() %>% 
  janitor::clean_names()

# MUNGE -------------------------------------------------------------------   

#The purpose of this analysis is to select 40 facilities in which the program will introduce hormonal IUD  by providing commodities and training for staff.
#The team would like to select facilities that: 
# 1)	already have most of the equipment required to provide IUDs (exam couch, lamp, autoclave, IUD insertion equipment kit) and 
# 2)	have at least two health workers who can be trained, preferably health workers who are already providing copper IUD (and can therefore be more easily trained to provide hormonal IUD).

#first, we inspect the variables
df %>% str()

# shows we need to do some cleaning
df %>% 
  count(examination_couch, anglepoise_lamp, autoclave, iud_insertion_kit)

#shows that we have errors where # HCWs that are trained > total number of HCWs - filter these out
df %>% 
  filter(total_number_of_health_workers_in_fp_unit < number_of_health_workers_in_fp_unit_trained_in_copper_iud) %>% 
  count(facility_name)

#rename categories to account for spelling errors
# for sites that have missing # of HCW trained, change to zero
# flag sites where # HCWs trained > Total HCWs
df_tidy <- df %>% 
  mutate(across(
    examination_couch:iud_insertion_kit,
    ~ case_when(
      str_detect(., regex("available.*fuctional", ignore_case = TRUE)) ~ "Available and functional",
      str_detect(., regex("available but not functional", ignore_case = TRUE)) ~ "Available but not functional",
      str_detect(., regex("partially", ignore_case = TRUE)) ~ "Partially available",
      str_detect(., regex("not availble", ignore_case = TRUE)) ~ "Not available",
      TRUE ~ .
    )
  )) %>% 
  pivot_longer(cols = examination_couch:iud_insertion_kit, names_to = "equipment", values_to = "equipment_status") %>% 
  mutate(number_of_health_workers_in_fp_unit_trained_in_copper_iud = ifelse(is.na(number_of_health_workers_in_fp_unit_trained_in_copper_iud), 0, number_of_health_workers_in_fp_unit_trained_in_copper_iud)) %>%
  mutate(hcw_flag = ifelse(total_number_of_health_workers_in_fp_unit < number_of_health_workers_in_fp_unit_trained_in_copper_iud, "Flag", "No Flag")) %>%
  filter(hcw_flag != "Flag")

#let's first assume that we are trying to select for all of the criteria
  # 2+ HCWs who are trained in copper UID
  # Available and functional equipment
#check if any have all functional and 2+ HCWs for copper: 4 facilities meet all metrics
df_tidy %>% 
  filter(number_of_health_workers_in_fp_unit_trained_in_copper_iud >= 2) %>% 
  group_by(facility_name) %>%
  summarise(all_functional = all(equipment_status %in% c("Available and functional", "Available"))) %>%
  filter(all_functional) 

#hence, we need a scoring system
  # equipment score: 1 for Available & Functional, 0.5 for Available but not functional / partially available, 0 for Not
  # HCW score: +1 if site has >=2 FP staff, +1 additional for copped IUD trained staff
  #arrange in desc order for total score - if sites are tied, rank by number of trained HCW and then equip score
df_facilities_list <- df_tidy %>% 
  mutate(equipment_score = case_when(equipment_status %in% c("Available and functional", "Available") ~ 1,
                                     equipment_status %in% c("Available but not functional", "Partially available") ~ 0.5,
                                     equipment_status %in% c("Not available") ~0)) %>% 
  mutate(hcw_score = case_when(
    total_number_of_health_workers_in_fp_unit >= 2 ~ #scores only for those that have 2+ HCWs
      (number_of_health_workers_in_fp_unit_trained_in_copper_iud * 1.0), #weight added for extra trained HCWs
    TRUE ~ 0
  )) %>% 
  group_by(facility_name, province, facility_type, total_number_of_health_workers_in_fp_unit, number_of_health_workers_in_fp_unit_trained_in_copper_iud) %>%
  summarise(
    total_equipment_score = sum(equipment_score, na.rm = TRUE),
    hcw_score = first(hcw_score),
    total_score = total_equipment_score + hcw_score,
    .groups = "drop"
  ) %>%
  arrange(
    desc(total_score),
    desc(number_of_health_workers_in_fp_unit_trained_in_copper_iud),
    desc(total_equipment_score)
  ) %>%
  mutate(select_facility = row_number() <= 40) 

write_csv(df_facilities_list, "Dataout/facilities_list.csv")

# GT TABLE ------------------------------------------------------------------

df_gt <- df_facilities_list %>% 
  filter(select_facility == TRUE) %>% 
  select(facility_name, province, facility_type, total_equipment_score, hcw_score, total_score) %>% 
  mutate(facility_type = case_when(str_detect(facility_type, "Primary") ~ "Primary",
                                   str_detect(facility_type, "Secondary") ~ "Secondary"))

tbl1 <- df_gt %>% 
  gt() %>% 
  sub_missing(missing_text = ".") %>%  
  fmt_number(columns = where(is.numeric)) %>% 
  cols_label(
    facility_name = "Facility",
    facility_type = "Site type",
    total_equipment_score = "Equipment Score (0-4)",
    hcw_score = "Personnel Score (weighted by training)",
    total_score = "Total Score"
  ) %>%
#  opt_row_striping() %>%
  gt_theme_nytimes() %>% 
  gt_color_rows(columns = c(4:5), na.color = "white", 
                palette = c("#f7f7f7",  "#5BB5D5")) %>% 
  tab_header(
    title = glue("Selected Facilities for Hormonal IUD Introduction") %>% toupper(), 
    subtitle = "Top 40 facilities ranked by combined score of equipment readiness and availability of trainable family planning staff"
  ) %>% 
  tab_source_note(
    source_note = "Note: Facilities with the same total score may differ in equipment readiness vs. personnel capacity, which should be considered during rollout"
  ) 

tbl1

gtsave(tbl1, filename = "Images/facilities_table_list.png", expand = 10, vwidth = 1400, vheight = 800)

# VIZ --------------------------------------------------------------------------

# dumbbell viz
df_dumbbell <- df_facilities_list %>%
  filter(select_facility == TRUE) %>%
  mutate(
    facility_name = fct_reorder(facility_name, total_score),
    province = as.factor(province)
  ) 

## Long data for points and legend
df_long <- df_dumbbell %>%
  pivot_longer(
    cols = c(total_equipment_score, hcw_score, total_score),
    names_to = "score_type",
    values_to = "score"
  ) %>%
  mutate(score_type = recode(score_type,
                             "total_equipment_score" = "Equipment",
                             "hcw_score" = "Staffing",
                             "total_score" = "Total Readiness"
  ))

ggplot() +
  geom_segment(data = df_dumbbell,
               aes(x = total_equipment_score, xend = hcw_score,
                   y = facility_name, yend = facility_name),
               color = "gray70", linewidth = 1) +
    geom_point(data = df_long,
             aes(x = score, y = facility_name, color = score_type, size = score_type),
             alpha = 0.8) +
  geom_text(data = df_dumbbell,
            aes(x = total_equipment_score, y = facility_name,
                label = round(total_equipment_score, 1)),
            color = "#2b8cbe", size = 2.5, hjust = 0.8, vjust = -1.5) +
  geom_text(data = df_dumbbell,
            aes(x = hcw_score, y = facility_name,
                label = round(hcw_score, 1)),
            color = "#E0AD30", size = 2.5, hjust = -0.1, vjust = -1.5) +
  geom_text(data = df_dumbbell,
            aes(x = total_score, y = facility_name,
                label = round(total_score, 1)),
            color = "white", size = 2.8, fontface = "bold") +
  scale_color_manual(values = c(
    "Equipment" = "#2b8cbe",
    "Staffing" = "#F9C555",
    "Total Readiness" = "#419164"
  )) +
  scale_size_manual(values = c(
    "Equipment" = 4,
    "Staffing" = 4,
    "Total Readiness" = 6
  ), guide = "none") +
  facet_wrap(~ province, scales = "free_y") +
  labs(
    title = "The 40 selected facilities achieve similar <span style='color:#419164;'>total readiness</span> through different compositions 
    in <span style='color:#E0AD30;'>staffing</span> and <span style='color:#2b8cbe;'>equipment</span>",
    subtitle = "Each point represents a facility's score across equipment availability and trained family planning staff",
    color = "Score Component",
    x = "Total Composite Score of Equipment and Staffing",
    y = "Facility",
    caption = "Source: Facilities IUD Dataset [2025-06-24]"
  ) +
  si_style_xgrid() +#custom styling element
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_markdown(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

si_save("Graphics/01_dumbbell.svg")












# Plot
ggplot(df_dumbbell) +
  geom_segment(aes(x = total_equipment_score, xend = hcw_score,
                   y = facility_name, yend = facility_name),
               color = "gray70", size = 1) +
  
  geom_point(aes(x = total_equipment_score, y = facility_name),
             color = "#2b8cbe", size = 4, alpha = 0.7) +
  geom_text(aes(x = total_equipment_score, y = facility_name,
                label = round(total_equipment_score, 1)),
            color = "#2b8cbe", size = 2.5, hjust = 0.8, vjust = -1.5) +
  geom_point(aes(x = hcw_score, y = facility_name),
             color = "#F9C555", size = 4, alpha = 0.7) +
  geom_text(aes(x = hcw_score, y = facility_name,
                label = round(hcw_score, 1)),
            color = "#F9C555", size = 2.5, hjust = -0.1, vjust = -1.5) +
  geom_point(aes(x = total_score, y = facility_name),
             color = "#419164", size = 6) +
  geom_text(aes(x = total_score, y = facility_name,
                label = round(total_score, 1)),
            color = "white", size = 2.8, fontface = "bold") +
  facet_wrap(~ province, scales = "free_y") + 
  si_style_xgrid() +
  labs(
    title = "Selected facilities achieve similar <span style='color:#419164;'>total readiness</span> through different compositions 
    in staffing and equipment",
    subtitle = "Each point represents a facility's score across equipment availability and trained family planning staff",
    x = "Total Composite Score of Equipment and Staffing",
    y = "Facility"
  ) +
  theme(
    axis.text.y = element_text(size = 6),
    plot.title = element_markdown(),
    plot.subtitle = element_text(size = 10)
  ) 
 
