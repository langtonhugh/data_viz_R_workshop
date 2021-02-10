# Load packages
library(readr)
library(ggplot2)
library(dplyr)
library(sf)

# Load data
burglary_df <- read_csv("data/gmp_2017.csv")

# Scatter 1
ggplot(data = burglary_df, mapping = aes(x = incscore, y = burglary_count)) +
  geom_point() +
  geom_smooth(method = "lm")

# Scatter 2
ggplot(data = burglary_df) +
  geom_point(mapping = aes(x = incscore, y = burglary_count)) +
  geom_smooth(mapping = aes(x = incscore, y = burglary_count), method = "lm")

# Scatter 3
ggplot(data = burglary_df) +
  geom_point(mapping = aes(x = incscore, y = burglary_count, colour = LAname))

# Bar 1
ggplot(data = burglary_df) +
  geom_bar(mapping = aes(x = IMDdeci))

# Bar 2
ggplot(data = burglary_df) +
  geom_bar(mapping = aes(x = LAname, fill = as.factor(IMDdeci)), position = "dodge") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  labs(x = NULL, fill = NULL,
       title = "Deprivation by Local Authority in Greater Manchester",
       subtitle = "Deciles calculated for England",
       caption = "Copyright ONS 2019 data") +
  theme(text = element_text(family = "mono", colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_line(colour = "snow"))

# Load data
monthly_df <- read_csv(file = "https://github.com/langtonhugh/data_viz_R_workshop/raw/master/data/gmp_monthly_2017.csv")
burg_records_df <- read_csv("https://github.com/langtonhugh/data_viz_R_workshop/raw/master/data/burglary_records.csv")
manc_sf <- st_read(dsn = "data/burglary_lsoa.shp")
hex_sf <- st_read(dsn = "data/geogrid_manc.shp")

