states <- absmapsdata::lga2021 %>%
  rmapshaper::ms_simplify(0.9)

dat_df <- dat %>%
  mutate(Coordinates = str_replace(Coordinates, "\\s", "")) %>%
  separate(Coordinates, into = c("lat", "long"), sep = ",") %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long)) %>%
  drop_na(lat, long) %>%
  st_as_sf(coords = c("long", "lat")) %>%
  # st_as_sf(coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84")  %>%
  mutate(lab = glue("<b>Insert Name: </b>{number_variable}<br/><b>Emitter: </b>{number_variable}<br/><b>Insert Name: </b>{scales::comma_format(digits = 0)(number_variable)}") %>%
           lapply(htmltools::HTML))

pal <- colorNumeric(palette = "Reds",
                    domain = safeguard_sf$`pick_number_variable`)

map_var <- leaflet(dat_df) %>%
  #addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(data = states, fillOpacity = 1, opacity = 1,
              color = unname(cah_palettes$neutrals["abbey"]),
              weight = 1,
              fillColor = "#FFFFFF") %>%
  leaflet::addCircleMarkers(radius = 5,
                            popup = ~lab,
                            label = ~`Facility name`,
                            opacity = 1,
                            fillOpacity = 1,
                            #fillColor = ~`Reported covered Emissions`,#unname(cah::cah_palettes$graph["wattle"]),
                            fillColor = ~pal(pick_number_variable),
                            color = unname(cah_palettes$neutrals["abbey"]),
                            weight = 0.5) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~pick_number_variable,
            title = "Reportable emissions")
