# Plotting Lake Ontario Microbial Cell Abundances
# By: Roger Calvas
# Date: January 29th, 2025

#First install the packages
install.packages("tidyverse")
library("tidyverse")

#load in the data
sample_data <- read_csv("sample_data.csv")

#round test
round(3.1415,3)

#Plotting!
ggplot( data = sample_data)+ aes(x=temperature)
#what does our data look like
view(sample_data)
str(sample_data)

#Plotting!
ggplot(data = sample_data) +
  aes(x = temperature, y = cells_per_ml, colour = env_group, size = chlorophyll) +
  labs(x = "Temp (C)", y = "Cell Abundance (cells/ml)",
       title = "Does temperrature affect microbial abundance?",
       color = "Environmental Group", size = "Chlorophyll (ug/L)") +
  geom_point()


#load in the buoy data
buoy_data <- read.csv("buoy_data.csv")

#buoy 
dim(buoy_data)
glimpse(buoy_data)
length(unique(buoy_data$sensor))
  
#Plotting of buoy data
ggplot(data = buoy_data) + 
  aes(x = day_of_year, y = temperature, group = sensor, color = depth) +
  geom_line()
  
#facet plot
ggplot(data = buoy_data) + 
  aes(x = day_of_year, y = temperature, group = sensor, color = depth) +
  geom_line() +
  facet_wrap(~buoy, axes = "all")

#cell abundances by environmental group
ggplot(data = sample_data) +
  aes(x = env_group, y = cells_per_ml,
      color = env_group) + 
  geom_jitter(aes(size = chlorophyll)) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA)

ggsave(filename = "cells_per_env_group.png", width = 6, height = 4)

#convert cells_per_ml to million cells per ml
sample_data$cells_per_million <- sample_data$cells_per_ml / 1e6

#total nitrogen vs cell abundance
ggplot(data = sample_data) +
  aes(x = total_nitrogen, y = cells_per_million, size = temperature,
      color = env_group) +
  labs(x = "Total Nitrogen (mg/L)", y = "Microbial cell abundance (Million Cells/ml",
       tittle = "Impact of Nitrogen on cell abundance",
       color = "Environmental Group", 
       size = "Temperature (C)") +
  geom_point()
  
ggsave(filename = "cells_per_Nitrogen_concentration.png", width = 6, height = 4)

#total phosphorus vs cell abundance
ggplot(data = sample_data) +
  aes(x = total_phosphorus, y = cells_per_million, size = temperature,
      color = env_group) +
  labs(x = "Total phosphorus (mg/L)", y = "Microbial cell abundance (Million Cells/ml",
       tittle = "Impact of Nitrogen on cell abundance",
       color = "Environmental Group", 
       size = "Temperature (C)") +
  geom_point()

ggsave(filename = "cells_per_Phosphorus_concentration.png", width = 6, height = 4)

# load the data
sample_data <- read.csv("Data/sample_data.csv")
taxon_abundance <- read_csv("Data/taxon_abundance.csv", skip = 2)

#merge them
merged_Phyla_data <- taxon_abundance %>%
  left_join(sample_data, by = "sample_id") %>%
  mutate(env_group = ifelse(is.na(env_group), "Shallow_September", env_group))

#reshaping the data in long format
merged_Phyla_data_long <- merged_Phyla_data %>%
  select(sample_id, env_group, Chloroflexi, Cyanobacteria, Bacteroidota) %>%
  pivot_longer(cols = c( Chloroflexi, Cyanobacteria, Bacteroidota), 
               names_to = "Phyla", 
               values_to = "Abundance")

#filter the data
filtered_data <- merged_Phyla_data_long %>%
  filter(Phyla %in% c("Chloroflexi", "Cyanobacteria", "Bacteroidota"))

# faceted creation

ggplot(data = filtered_data) +
  aes(x = env_group, y = Abundance, color = env_group, fill = env_group) +
  facet_grid(. ~ Phyla) +
  labs(x = "Depth and Season", 
       y = "Phylum Abundance", 
       title = "Phylum abundance changes by depth and season.") +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  theme(axis.text.x = element_text(angle = 35, hjust  =1),
        legend.position = "none")

ggsave("phylum_abundance_plot.png", width = 10, height = 6)
  
