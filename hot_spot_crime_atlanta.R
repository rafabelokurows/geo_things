library(sf)
library(sfdep)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(leaflet)

crimes_raw = read_sf(".\\data\\atlanta-crimes.geojson")
crimes_raw

crimes_raw %>% ggplot(aes(fill = robbery)) +
  geom_sf(color = "black",lwd = 0.15)

crimes_nbs = crimes_raw %>%
  mutate(nb = st_contiguity(geometry),
         wt  = st_weights(nb),
         robbery_lag = st_lag(robbery,nb,wt))
crimes_nbs %>% ggplot(aes(fill = robbery_lag)) +
  geom_sf(color = "black",lwd = 0.15)


global_g_test(crimes_nbs$robbery,crimes_nbs$nb,crimes_nbs$wt)


crimes_nbs %>%
  mutate(Gi = local_g_perm(robbery,nb,wt,nsim = 500)) %>%
  unnest(Gi) %>%
  ggplot(aes(fill=gi)) + geom_sf(color="black",lwd = 0.15)+
  scale_fill_gradient2(trans = 'reverse')


crimes_hot_spots = crimes_nbs %>%
  mutate(Gi = local_g_perm(robbery,nb,wt,nsim = 500)) %>%
  unnest(Gi)

crime_data <- crimes_hot_spots |>
  select(gi, p_folded_sim,robbery) |>
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


pal <- colorFactor(brewer.pal(11, "RdYlBu"), crime_data$classification)

crime_data |>
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  #theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "Robbery Hot Spots in Metro Atlanta"
  )


labels <- sprintf(
  "<strong>Classification</strong>: %s<br/><strong>p-value</strong>: %s<br/><strong>Robbery incidents</strong>: %s",
  crime_data$classification, round(crime_data$p_folded_sim,4),crime_data$robbery) %>%
  lapply(htmltools::HTML)


leaflet(data = crime_data) %>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons(
    fillColor = ~pal(classification),
    color = "black",
    weight = 0.1,
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels
  ) %>%
  addLegend(
    pal = pal,
    values = ~classification,
    title = "Crime Hot Stops in Atlanta, GA",
    opacity = 1
  )
