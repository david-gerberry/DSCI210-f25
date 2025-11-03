#run magic number and election results first. 
ThresholdsRes = magic_number_average * 1.5
ThresholdStrong = magic_number_average * 2
ThresholdsSwing = magic_number_average * .8
max_support = max(AvgMap$AvgProx, na.rm = T)


leaf_mapN = function(df) {
  pal <- colorNumeric(
    palette = brewer.pal(n = 10, name = "RdBu"),
    domain = c(0, max_support),
    na.color = "transparent"
  )
  
  # Create the leaflet map
  leaflet(df) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(AvgProx),
      fillOpacity = 0.7,
      color = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste0("Support: ", round(AvgProx * 100, 1), "%"),
      layerId = ~PRECINCT  # Use formula notation, not AvgMap$PRECINCT
    ) %>%
    addLegend(
      pal = pal,
      values = ~AvgProx,
      opacity = 0.7,
      title = "Support Level",
      position = "bottomright",
      labFormat = labelFormat(
        suffix = "%",
        transform = function(x) x * 100
      )
    )
}




save(AvgMap, ThresholdsSwing, ThresholdStrong, ThresholdsRes, max_support, leaf_mapN, ourMap, file = "data/Elec.RData")

SmallMap %>% 
  ggplot(aes(fill = PrecentPorxy)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo") +
  labs(title = "Votes for Mapp") +
  labs(fill = "Percent of Vote") 

SmallMap %>%
  mutate(MappBaseSwing = cut(PrecentPorxy, breaks = c(-0.001, ThresholdsSwing, ThresholdsRes, 1), label = c('Residual', 'Swing', 'Base'))) %>% 
  ggplot(aes(fill = MappBaseSwing)) +
  geom_sf() +
  scale_fill_manual(
    values = c(
      "Residual" = "red",   
      "Base" = "blue",   
      "Swing" = "gold"   
    )
  )
SmallMap19 %>%
  mutate(MappBaseSwing = cut(PrecentPorxy, breaks = c(-0.001,ThresholdsSwing, ThresholdsRes, 1), label = c('Residual', 'Swing', 'Base'))) %>% 
  ggplot(aes(fill = MappBaseSwing)) +
  geom_sf() +
  scale_fill_manual(
    values = c(
      "Residual" = "red",   
      "Base" = "blue",   
      "Swing" = "gold"   
    )
  )
AvgMap %>%
  mutate(BaseSwing = cut(
    AvgProx,
    breaks = c(-0.001, ThresholdsSwing, ThresholdsRes, ThresholdStrong, 1),
    labels = c('Residual', 'Swing', 'Base', 'Strong Base')
  )) %>%
  ggplot(aes(fill = BaseSwing)) +
  geom_sf() +
  scale_fill_manual(
    values = c(
      "Residual" = "red",
      "Base" = "blue",
      "Swing" = "gold",
      "Strong Base" = "darkblue"
    )
  ) +
  theme(
    legend.text = element_text(size = 16),   # bigger legend labels
    legend.title = element_text(size = 20)   # bigger legend title
  )
AvgMap %>% 
  ggplot(aes(fill = AvgProx)) +
  geom_sf() +
  scale_fill_gradientn(colours=brewer.pal(n=10,name="RdBu"),na.value = "transparent",
                       values = c(0,ThresholdsSwing, ThresholdsRes, max_support),
                       breaks = c(0, ThresholdsSwing, ThresholdsRes, max_support), 
                       labels = c("0%", "24%", "44%", "56%"),
                       name = "Support Level",
                       limits=c(0,1)) +
  labs(
    title = "Base Swing Map for the School Board Race"
  ) +
  theme(
    legend.text = element_text(size = 14),   # bigger legend labels
    legend.title = element_text(size = 16)   # bigger legend title
  )



