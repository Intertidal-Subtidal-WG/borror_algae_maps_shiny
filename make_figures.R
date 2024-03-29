library(sf)
library(ggplot2)
setwd(here::here())

# The Data
#load("spp_shapes_cleaned.Rds")
load("island_perimeter_segments.rds")
shapes_cleaned <- st_as_sf(#shapes_cleaned,
  df %>% ungroup(),
  crs = 4326) %>%
  mutate(species_general = gsub("Saccharina", 
                                "Saccharina latissima", 
                                species_general),
         species_general = gsub("Kelps", 
                                "kelps", 
                                species_general),
  )


years <- min(shapes_cleaned$year):max(shapes_cleaned$year)
years[!(years %in% unique(shapes_cleaned$year))] <- NA
init_year <- min(shapes_cleaned$year)

#color
source("color_pal.R")

algae_pal <- colorFactor(palette = colors$color, 
                         ordered = TRUE,
                         levels = colors$species_general)

#write out jpgs
for(i in unique(shapes_cleaned$year)) {
  ggplot(shapes_cleaned %>% filter(year == i),
         aes(color = species_general)) +
    geom_sf(size = 3) +
    scale_color_manual(breaks = colors$species_general,
                       values = colors$color,
                       limits = colors$species_general) +
    theme_void() +
    labs(color = "") +
    theme(legend.key.size = unit(0.3, 'cm'))
  
  ggsave(glue::glue("www/figures/{i}.jpg"), bg = "white",
         width = 800, height = 600, units = "px", dpi = 200)
}


library(ricv)
ricv(img1 = "www/figures/1983.jpg", img2 = "www/figures/2014.jpg",
     options = list(addCircle = T, hoverStart = T))
