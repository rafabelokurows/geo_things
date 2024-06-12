library(sf)
library(tidyverse)
library(leaflet)
library(sfdep)
library(RColorBrewer)
#Steps in case you download the original data
#geo = st_read(".\\data\\Natl_WI.gdb") #https://www.epa.gov/smartgrowth/smart-location-mapping
#dc_limits = st_read(".\\data\\Washington_DC_Boundary.geojson") #Open Data DC
# dc_limits = st_transform(dc_limits,st_crs(geo)) #transforming projection of file with DC city limits to same projection as initial data
# geo_dc = geo %>% st_intersection(dc_limits) #filtering only polygons within DC's limits
# geo_dc_transf = st_transform(geo_dc, crs = 4326) #transforming again for better plotting
#sf::st_write(geo_dc_transf, dsn = "~./dc_walk_idx.json", layer = "data",driver = "GeoJSON")

geo_dc_transf = st_read( "~./dc_walk_idx.json")

geo_dc_hot_spots = geo_dc_transf %>%
  mutate(nb = st_contiguity(geometry),
         wt  = st_weights(nb),
         ind_lag = st_lag(NatWalkInd,nb,wt)) %>%
  mutate(Gi = local_g_perm(NatWalkInd,nb,wt,nsim = 500)) %>%
  unnest(Gi)|>
  select(gi, p_folded_sim,NatWalkInd) |>
  mutate(
    classification = case_when(
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    # we now need to make it look better :)
    # if we cast to a factor we can make diverging scales easier
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold")
    )
  )

geo_dc_hot_spots |>
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  #scale_fill_manual(values = rev(pal_manual))+
  scale_fill_brewer(palette = "RdBu",direction = 1) +
  #theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "Hot Spots - Walkability Index in Washington D.C."
  )


labels <- sprintf(
  "<strong>Classification</strong>: %s<br/><strong>p-value</strong>: %s<br/><strong>Walkability Index</strong>: %s",
  geo_dc_hot_spots$classification, round(geo_dc_hot_spots$p_folded_sim,4),round(geo_dc_hot_spots$NatWalkInd,2)) %>%
  lapply(htmltools::HTML)

pal <- colorFactor(brewer.pal(7, "RdBu"), geo_dc_hot_spots$classification)
# Plot using leaflet
leaflet(data = geo_dc_hot_spots) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # Adding a tile layer
  addPolygons(
              color = "black",
              weight = 1,
              opacity = 0.2,
              fillOpacity = 0.7,
              fillColor = ~pal(classification),
              label = labels) %>%
  addLegend(
    pal = pal,
    values = ~classification,
    title = "Walkability Index in Washington D.C.",
    opacity = 1
  )




