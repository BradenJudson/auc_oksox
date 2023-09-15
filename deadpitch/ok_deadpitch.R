# Set project directory.
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

library(tidyverse); library(ggplot2); library(ggridges)

# Read in Skaha dead pitch data.
ska <- read.csv("skaha_deadpitch_tidy.csv") %>%
    # Remove Shingle Creek.
    filter(location == "penticton channel") %>%
    select(c("year","doy", "fish")) %>%
    # Add section factor.
    mutate(section = "Penticton Channel")

shin <- read.csv("shingle_deadpitch.csv") %>% 
  mutate(doy = yday(mdy(date)),
         section = "Shingle Creek") %>%  
  select(c("year", "doy", "fish", "section"))

# Check.
colnames(ska); colnames(oso); colnames(shin)

# Bind together, check sections factor.
data <- rbind(ska, shin); unique(data$section)

# Summarize dead pitch collections.
(uprok_summ <- shin %>%
        group_by(year, section) %>%
        summarise(range = max(doy) - min(doy),  # Range in collection dates.
                  sd = round(sd(doy), 1),       # SD in collection dates.
                  n = length(doy),              # Number of collections.
                  first = min(doy),
                  last = max(doy),
                  median = median(doy)) %>%
        select(c("section" ,"year", "n", "first",
                 "last","range", "median", "sd")))

# Write as csv - put into *.docx.
write.csv(uprok_summ, "uprok_deadpitch_collssumm.csv",
          row.names = FALSE)

# Reformat.
data$doy <- as.numeric(data$doy)
data$year <- as.factor(data$year)

# Weird July surveys account for ~0.1% of data.
quantile(data$doy,
         probs = c(0.001, 0.003, 0.005,
                   0.01, 0.1, 0.5, 0.9))

# Collections by year and section.
(counts <- data[data$doy > 270,] %>%
    group_by(year, section) %>%
    tally())

# Remove July surveys.
ggplot(data = data[data$doy > 275,],
       # Input for ggplot.
       aes(y = year,
           x = doy,
           fill = year)) +
    # Add segments for standard dates.
    # October 31.
    geom_segment(aes(x = 304, xend = 304,
                     y = as.factor("2011"),
                     yend = as.numeric("10")),
                     colour = "royalblue") +
    # For wrapping around header text.
    geom_segment(aes(x = 304, xend = 304,
                     y = as.numeric("11.7"),
                     yend = as.numeric("0")),
                 colour = "royalblue") +
    # November 1.
    geom_segment(aes(x = 319, xend = 319,
                     y = as.numeric("11.7"),
                     yend = as.numeric("0")),
                 colour = "royalblue") +
    # October 1.
    geom_segment(aes(x = 274, xend = 274,
                     y = as.numeric("11.7"),
                     yend = as.numeric("0")),
                 colour = "royalblue",
                 linewidth = 0.5) +
    # Plot for each section, default nrow = 1.
    facet_wrap(~ section) +
    # Standardize axes so segment geoms outside of plot margins.
    coord_cartesian(clip = "off",
                    xlim = c(274,
                    max(data$doy) + 3),
                    ylim = c(1, 11.25)) +
    # Add ridges as numeric histograms, not kernal densities.
    geom_density_ridges(alpha = 0.85,
                        scale = 0.90,
                        stat = "binline",
                        bins = 20,
                        # Baseline off - looks much better.
                        draw_baseline = FALSE) +
    ylab("") +
    xlab("\n Day of the year") +
    # Static x axis breaks.
    scale_x_continuous(breaks = seq(280, 330, 15)) +
    theme_bw() +
    # Custom theme elements. No legend, extended margins, etc.
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          plot.margin = unit(c(1,2,0.2,0.2), "cm"),
          axis.title.x = element_text(size = 14),
          panel.spacing = unit(2, "cm"),
          strip.background = element_rect(fill = alpha("gray50", 0.2)),
          strip.text = element_text(size = 10)) +
    # Label vertical segment geoms for standard dates.
    annotate("text", label = "October 1",
             size = 4, x = 276,
             y = -Inf, vjust = 3.5) +
    annotate("text", label = "November 15",
             size = 4, x = 318,
             y = -Inf, vjust = 3.5) +
    annotate("text", label = "October 31",
             size = 4, x = 300,
             y = -Inf, vjust = 3.5) +
    # Add sample size text to right of each year.
    geom_text(data = counts,
              aes(x = 328,  # Outside of plot, to the right.
                  y = year,
                  # Format text.
                  label = format(round(n, digits = 0),
                                 big.mark= ",",
                                 scientific = FALSE)),
              hjust = 0) +
    # Text for sample size "column".
    annotate("text", label = "Fish
sampled", # Weird formatting for wrapped text in single geom.
             size = 3.5, x = 331,
             y = Inf, hjust = 0.5,
             vjust = -0.05)

# Save as png.
ggsave("upr_deadpitch_ridgeline_bins.png",
       units = "px", width = 3300,
       height = 2000)


# lower -------------------------------------------------------------------

# read in tidied Osoyoos dead pitch data.
oso <- read.csv("OSO_SOX_deadpitch_data_tidy.csv") %>%
  # Chuck out Shingle Creek and other tributaries for now.
  filter(section == "Lower Okanagan") %>%
  # Retain relevant columns.
  select(c("year", "doy", "fish", "section"))


mcn <- read.csv("OSO_SOX_deadpitch_data_tidy.csv") %>%
  # Chuck out Shingle Creek and other tributaries for now.
  filter(section == "Skaha") %>%
  # Retain relevant columns.
  select(c("year", "doy", "fish")) %>% 
  mutate(section = "Middle Okanagan")


colnames(oso); colnames(mcn)

# Bind together, check sections factor.
lowr <- rbind(oso, mcn); unique(lowr$section)

(lowrok_summ <- mcn %>%
    filter(doy > 270) %>% 
    group_by(year, section) %>%
    summarise(range = max(doy) - min(doy),  # Range in collection dates.
              sd = round(sd(doy), 1),       # SD in collection dates.
              n = length(doy),              # Number of collections.
              first = min(doy),
              last = max(doy),
              median = median(doy)) %>%
    select(c("section" ,"year", "n", "first",
             "last","range", "median", "sd")))

lowr$year <- as.factor(lowr$year)

(counts <- lowr[lowr$doy > 270,] %>%
    group_by(year, section) %>%
    tally())

write.csv(lowrok_summ, "mcn_summ.csv", row.names = F)

# Remove July surveys.
ggplot(data = lowr[lowr$doy > 275,],
       # Input for ggplot.
       aes(y = year,
           x = doy,
           fill = year)) +
  # Add segments for standard dates.
  # October 31.
  geom_segment(aes(x = 304, xend = 304,
                   y = as.factor("2011"),
                   yend = as.numeric("10")),
               colour = "royalblue") +
  # For wrapping around header text.
  geom_segment(aes(x = 304, xend = 304,
                   y = as.numeric("22.7"),
                   yend = as.numeric("-0.5")),
               colour = "royalblue") +
  # November 1.
  geom_segment(aes(x = 319, xend = 319,
                   y = as.numeric("22.7"),
                   yend = as.numeric("-0.5")),
               colour = "royalblue") +
  # October 1.
  geom_segment(aes(x = 274, xend = 274,
                   y = as.numeric("22.7"),
                   yend = as.numeric("-0.5")),
               colour = "royalblue",
               linewidth = 0.5) +
  # Plot for each section, default nrow = 1.
  facet_wrap(~ section) +
  # Standardize axes so segment geoms outside of plot margins.
  coord_cartesian(clip = "off",
                  xlim = c(274,
                           max(data$doy) + 3),
                  ylim = c(1, 22.25)) +
  # Add ridges as numeric histograms, not kernal densities.
  geom_density_ridges(alpha = 0.85,
                      scale = 0.90,
                      stat = "binline",
                      bins = 20,
                      # Baseline off - looks much better.
                      draw_baseline = FALSE) +
  ylab("") +
  xlab("\n Day of the year") +
  # Static x axis breaks.
  scale_x_continuous(breaks = seq(280, 330, 15)) +
  theme_bw() +
  # Custom theme elements. No legend, extended margins, etc.
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        plot.margin = unit(c(1,2,0.2,0.2), "cm"),
        axis.title.x = element_text(size = 14),
        panel.spacing = unit(2, "cm"),
        strip.background = element_rect(fill = alpha("gray50", 0.2)),
        strip.text = element_text(size = 10)) +
  # Label vertical segment geoms for standard dates.
  annotate("text", label = "October 1",
           size = 4, x = 276,
           y = -Inf, vjust = 3.5) +
  annotate("text", label = "November 15",
           size = 4, x = 318,
           y = -Inf, vjust = 3.5) +
  annotate("text", label = "October 31",
           size = 4, x = 300,
           y = -Inf, vjust = 3.5) +
  # Add sample size text to right of each year.
  geom_text(data = counts,
            aes(x = 328,  # Outside of plot, to the right.
                y = year,
                # Format text.
                label = format(round(n, digits = 0),
                               big.mark= ",",
                               scientific = FALSE)),
            hjust = 0) +
  # Text for sample size "column".
  annotate("text", label = "Fish
sampled", # Weird formatting for wrapped text in single geom.
           size = 3.5, x = 331,
           y = Inf, hjust = 0.5,
           vjust = -0.05)


# Save as png.
ggsave("lwr_deadpitch_ridgeline_bins.png",
       units = "px", width = 3300,
       height = 2000)



