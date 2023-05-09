# Braden Judson
# Function for making AUC plots.

# Set project directory.
setwd("~/auc_code")

# Load libraries.
library(tidyverse); library(lubridate)

# Develop function. 
auc <- function(data, date, count) {
  
  # Function for rounding values to a given accuracy.
  round_any <- function(x, accuracy, f = round) 
               {f(x/accuracy)*accuracy}
  
  # Function for setting year-specific y-axis breaks (n=3).
  cbs <- function(y) {c(0, round_any(max(y)/2.1, 100),
                        round_any(max(y) - 0.05*(max(y)), 100))}
  
  # Function for extending y axis limits to best see the data.
  ylim <- function(y) {c(min(y) - 0.25*max(y), max(y) + max(y)*0.15)}
  
  # Reformat variables.
  data$year = as.factor(lubridate::year(data$date))
  data$date = yday(data$date)
  data[[count]] = as.numeric(data[[count]])
  
  # Pass to ggplot.
  (plot <- ggplot(data = data, 
                  aes(x = .data[[date]],
                      y = .data[[count]])) +
      
      # Fill in area under AUC with gray.
      geom_area(aes(y = .data[[count]]), 
                fill = "gray",   
                alpha = 0.25) +
      
      # Connect data points with black line.
      geom_line() +
      
      # Plot distirbutions year-by-year with free y-axes.
      facet_wrap(~ year, as.table = TRUE,
                 scales = "free_y",
                 ncol = 4) +
      
      # Add blue dotted line indicating zero.
      geom_hline(yintercept = 0,
                 color = "deepskyblue3",
                 linewidth = 0.5,
                 linetype = "dashed") +
      
      # Add individual data points on top of other geoms.
      geom_point(shape = 21, color = "black",
                 fill = "gray99") +
      
      # Use custom function to add 3 evenly spaced y-axis ticks.
      scale_y_continuous(expand = c(0,0), 
                         breaks = cbs,
                         limits = ylim) +
      
      # Keep x-axis consistent.
      scale_x_continuous(breaks = c(240, 280, 320)) +
      
      # Black and white theme.
      theme_bw() +
      
      # Remove grid lines.
      theme(panel.grid = element_blank()) +
      xlab("Day of the year") + 
      ylab("Spawner Abundance (# of live individuals)") +
      
      # Lighten background ribbon colour.
      theme(strip.background = element_rect(fill = "gray97")))
  
      aucs <- data %>%            
      # Assign to data to new variable to calculate AUCs.
      
      group_by(year) %>%   
      # Group by sampling year.
      
      mutate(tdiff = date - lag(date),                      
             # Difference in dates (time diff).
             
             tdiff = replace(tdiff, which(tdiff < 0), NA),
             # Negative times (spanning years) should be NA.
             
             xbar = (.data[[count]] + lag(.data[[count]]))/2, 
             # Average of counts wrt previous count
             
             fishdays = case_when(                          
               is.na(xbar) ~ .data[[count]] * 11/2,         
               # First survey, calculate left tail.
               !is.na(xbar) ~ tdiff * xbar
               # If not last survey, follow algorithmin of Hillborn.
             ),
             # Counts are time diff * average count.
             cumulative = collapse::fcumsum(fishdays)) %>%  
            # Cumulative summary vals - ignores NAs,
      group_by(year) %>%                                    
      summarise(total_auc = max(cumulative, na.rm = TRUE) +
                  # Take the total count...
                  (.data[[count]][which.max(date)]*(11/2)),
                  # ... and estimate right-most tail with known residency time.
                nerkids = round(total_auc/11,0))
                  # Divide the whole thing by the residency time.
  
  list(plots = plot,
       summ = aucs)
  
}


# Lower Osoyoos River ----------------------------------------------------------

# Read enumeration data in and manipulate it into two columns.
counts <- read.csv("lowerOSO_counts.csv", na.strings = "") %>% 
  select(c(1,2,7)) %>% 
  filter(!is.na(index_date)) %>% 
  mutate(index_date = dmy(index_date)) %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(total = rowSums(.[,c(2,3)], na.rm = TRUE)) %>% 
  select(c(1,ncol(.))) %>% 
  `colnames<-`(., c("date", "live")) %>% 
  arrange(date); head(counts)

# Run plotting function.
(oso_plots <- auc(data = counts, date = "date", count = "live"))

# Save figures.
ggsave("plots/oso_auc.png", units = "px", 
       width = 2000, height = 2000)


# Penticton --------------------------------------------------------------------

# Read in data. Requires some manipulation.
pen <- read.csv("nerkids_all_yrs.csv") %>% 
  arrange(date) %>% 
  filter(stream != "Shingle Creek") %>% 
  select(1,3); head(pen)
  # Check that date format is interpretable by function.

(pen_plots <- auc(data = pen, date = "date", count = "count"))

ggsave("plots/pen_auc.png", units = "px", 
       width = 2000, height = 1300)

# -------------------------------------------------------------------------

