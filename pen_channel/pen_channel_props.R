
library(tidyverse); library(flextable)
setwd("~/oksox_auc/aucs/pen_channel")

auc <- read.csv("pen_auc.csv")

# Read in dead pitch to assess proportion of spawners as Kokanee/Sockeye.
deadpitch <- read.csv("skaha_deadpitch_tidy.csv") %>%   # Read in all the skaha dead pitch data that I have.
  select(c(1,ncol(.)-1)) %>%                            # Select relevant columns.
  mutate(year = as.factor(year),                        # Set everything to factors (for counting).
         fish = as.factor(fish)) %>%
  group_by(year, fish) %>%                              # Count fish (e.g., Sockeye / Kokanee) by year.
  summarise(n_sox = n()) %>%                            # Counts (n) of sockeye.
  mutate(propsox = round(n_sox/sum(n_sox),3)) %>%       # Estimate annual proportions.
  filter(fish != "kokanee" & fish != "unknown")         # Only keep sockeye proportions.

(soxauc <- auc %>%                            # AUC values for all nerkids.
    merge(., deadpitch[,c(1,4)]) %>%          # Merge with dead pitch data (% sockeye vs. kokanee).
    mutate(                                   # Divide total by residence time = total spawners.
      ko = round(nerkids * (1-propsox), 0),   # Abundance of Kokanee spawners.
      pKo = 1-propsox,
      sox = round(nerkids * propsox, 0),      # Abundance of Sockeye spawners.
      pHO = c(NA,NA, 0.594, 0.408, NA, 0.007, # Proportion of H-O spawners by year.
              0.003, 0.111, 0.426,            # Based on otolith analyses.
              0.203, 0.179, NA),              # NA = no data.
      HO = round(sox*pHO, 0),                 # Abundance of H-O Sockeye.
      NO = round(sox*(1-pHO), 0),             # Abundance of N-O Sockeye.  
      pNO = 100-pHO*100,      # Add brackets around %s.
      pHO = pHO*100,        # Proportion H-O.
      year = as.character(year)) %>%          # Easier formatting for flextable.
    select(1,3,5:7,4,10:11,9,8))
    # select(1,3:6,8,7,9,10))                   # Reorder columns.


(penft <- soxauc %>% 
    regulartable() %>% 
    set_header_labels(., 
                      year = "Year",
                      nerkids = "Total O. nerka",
                      propsox = "% of all O. nerka",
                      ko = "Total Kokanee",
                      sox = "Total Sockeye",
                      HO = "H-O Sockeye",
                      NO = "N-O Sockeye",
                      pNO = "% of all Sockeye",
                      pHO = "% of all Sockeye",
                      pKo = "% of all O. nerka") %>% 
    bold(part = "header") %>% 
    set_table_properties(layout = "autofit") %>% 
    align(align = "right", part = "all") %>% 
    autofit() %>% 
    add_header_row(
      values = c("", "Total", "Natural-origin", "Hatchery-origin"),
      colwidths = c(4,2,2,2)
    ) %>% 
    add_header_row(
      values = c("", "", "Kokanee", "Sockeye"),
      colwidths = c(1,1,2,6)
    ))

save_as_image(x = penft, 
              path = paste0(getwd(), "/penauc.png"))

# save_as_docx(x = penft,
#              pr_section = prop_section(page_size = page_size(
#                orient = "landscape")),
#              path = paste0(getwd(), "/penauc.docx"))

