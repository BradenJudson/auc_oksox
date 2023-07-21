setwd("~/oksox_auc/aucs/osoyoos")

library(tidyverse); library(flextable)

auc <- read.csv("oso_auc.csv")

ho <- read.csv("OSO_SOX_deadpitch_data_tidy.csv") %>% 
  filter(section == "Lower Okanagan") %>% 
  filter(fish != "kokanee") %>% 
  filter(!is.na(thermal_mark)) %>% 
  filter(!is.na(fish)) %>% 
  group_by(year, thermal_mark) %>% 
  summarise(n_sox = n()) %>% 
  filter(thermal_mark != "Unknown") %>% 
  mutate(propsox = round(n_sox/sum(n_sox), 3)) %>% 
  select(1,2,4) %>% 
  pivot_wider(names_from = "thermal_mark",
              values_from = "propsox") %>% 
  `colnames<-`(., c("year", "propHO", "propNO"))


soko <- read.csv("OSO_SOX_deadpitch_data_tidy.csv") %>% 
  filter(section == "Lower Okanagan") %>% 
  filter(!is.na(fish)) %>% 
  group_by(year, fish) %>% 
  summarise(n_sox = n()) %>% 
  mutate(propsox = round(n_sox/sum(n_sox),3)) %>% 
  merge(., auc, by = "year", all = TRUE) %>% 
  mutate(nerkids = case_when(
    year == 2011 ~ 41207,
    year != 2011 ~ nerkids
  ),
  abundance = round(propsox * nerkids, 0)) %>% 
  merge(., ho, by = "year", all = TRUE) %>% 
  mutate(NOsockeye = case_when(
    fish == "kokanee" ~ 0,
    fish == "sockeye" ~ round(abundance * propNO, 0)
  ),
  HOsockeye = case_when(
    fish == "kokanee" ~ 0,
    fish == "sockeye" ~ abundance - NOsockeye
  ),
  propHO = case_when(
    year > 2006 ~ propHO,
    year < 2007 ~ 0
  ),
  propNO = case_when(
    year > 2006 ~ propNO,
    year < 2007 ~ 1
  ),
  NOsockeye = case_when(
    year > 2006 ~ NOsockeye,
    year < 2007 ~ abundance
  ),
  HOsockeye = case_when(
    year > 2006 ~ HOsockeye,
    year < 2007 ~ 0
  )) 

sox <- soko[soko$fish != "kokanee", ] %>% 
  select(1,5,6,9:11)

kok <- soko[soko$fish == "kokanee", ] %>% 
  select(1,4,7) %>% 
  `colnames<-`(., c("year", "propKokanee", "Kokanee"))

total <- merge(sox, kok, by = "year", all = TRUE) %>% 
  replace(is.na(.), 0) %>% 
  filter(year != 0) %>% 
  mutate(propNO = round(propNO*100,1))

library(flextable); library(officer)

set_flextable_defaults(background.color = "white",
                       digits = 3)


(ft <- total %>% 
    mutate(propSox = round(1 - propKokanee, 3) * 100,
           propHO = round(100 - propNO, 1),
           Sockeye = round(nerkids * propSox/100, 0),
           propKokanee = round(propKokanee, 3) * 100,
           year = as.factor(year)) %>% 
    select("year", "nerkids", "Kokanee",
           "propKokanee", "Sockeye", "propSox",
           "NOsockeye", "propNO", "HOsockeye", 
           "propHO") %>% 
    replace(. == "0 (0)", NA) %>% 
    regulartable() %>% 
    set_header_labels(., 
                      year = "Year",
                      nerkids = "Total O. nerka",
                      Kokanee = "Total Kokanee",
                      Sockeye = "Total Sockeye",
                      propSox = "% of all O. nerka",
                      propKokanee = "% of all O. nerka",
                      NOsockeye = "N-O Sockeye",
                      propNO = "% of all Sockeye",
                      HOsockeye = "H-O Sockeye",
                      propHO = "% of all Sockeye")  %>% 
    bold(part = "header") %>% 
    add_header_row(
      values = c("", "Total", "Natural-origin", "Hatchery-origin"),
      colwidths = c(4,2,2,2)
    ) %>% 
    add_header_row(
      values = c("", "", "Kokanee", "Sockeye"),
      colwidths = c(1,1,2,6)
    ) %>% 
    set_table_properties(layout = "autofit") %>% 
    align(align = "center", part = "all") %>% 
    autofit())

fc <- total %>% 
  mutate(propSox = round(1 - propKokanee, 3) * 100,
         propHO = round(100 - propNO, 3) * 100,
         Sockeye = round(nerkids * propSox, 0),
         propKokanee = round(propKokanee, 3) * 100,
         year = as.factor(year)) %>% 
  select("year", "nerkids", "Kokanee",
         "propKokanee", "Sockeye", "propSox",
         "NOsockeye", "propNO", "HOsockeye", 
         "propHO") %>% 
  replace(. == "0 (0)", NA) 

write.csv(fc, "oso_summ.csv", row.names = FALSE)

save_as_image(x = ft, 
              path = paste0(getwd(), "/osoauc.png"))

save_as_docx(x = ft,
             pr_section = prop_section(page_size = page_size(
               orient = "landscape")),
             path = paste0(getwd(), "/osoauc.docx"))

