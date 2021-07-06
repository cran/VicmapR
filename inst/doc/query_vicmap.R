## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(VicmapR)
library(sf)
library(leaflet)

#check sf installation
sf::sf_extSoftVersion()

## -----------------------------------------------------------------------------
available_layers <- listLayers()

head(available_layers, 10)

## ----query--------------------------------------------------------------------
# query the watercourse layer
vicmap_query(layer = "datavic:VMHYDRO_WATERCOURSE_DRAIN")

## ----query_arguments----------------------------------------------------------
vicmap_query(layer = "datavic:VMHYDRO_WATERCOURSE_DRAIN") %>%
  head(50) %>% #return only 50 rows
  filter(HIERARCHY == "L") %>% # filter the column 'HIERACHY' to values of 'L'
  select(HIERARCHY, PFI) %>% # select columns 'HIERARCHY' and 'PFI'
  show_query()

## ----collect_query------------------------------------------------------------
watercourse_data <- vicmap_query(layer = "datavic:VMHYDRO_WATERCOURSE_DRAIN") %>%
  head(50) %>% #return only 50 rows
  filter(HIERARCHY == "L") %>% # filter the column 'HIERACHY' to values of 'L'
  select(HIERARCHY, PFI) %>% # select columns 'HIERARCHY' and 'PFI'
  collect()

str(watercourse_data)

## ----filter_spatial, warning = FALSE, message=FALSE, out.width="100%", eval = knitr::is_html_output(excludes = "markdown")----
#### Return objects that intersect melbourne ####
# Read in an example shape to restrict our query to using geometric filtering
melbourne <- sf::st_read(system.file("shapes/melbourne.geojson", package="VicmapR"), quiet = F) %>% 
  sf::st_transform(4283)

# Return data that intersects melbourne
rail_intersects <- vicmap_query(layer = "datavic:VMTRANS_TR_RAIL") %>% # layer to query
  filter(INTERSECTS(melbourne)) %>% # more advanced geometric filter
  collect()

rail_bbox <- vicmap_query(layer = "datavic:VMTRANS_TR_RAIL") %>%
  filter(BBOX(sf::st_bbox(melbourne))) %>%
  collect()

rail_dwithin <- vicmap_query(layer = "datavic:VMTRANS_TR_RAIL") %>%
  filter(DWITHIN(melbourne %>% sf::st_centroid(), distance = 10000, units = "meters")) %>%
  collect()

leaflet(width = "100%") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = melbourne, color = "grey", group = "Melbourne polygon") %>%
  addPolygons(data = sf::st_bbox(melbourne) %>% st_as_sfc(), color = "black", group = "Melbourne bbox") %>%
  addPolylines(data = rail_intersects, color = "Red", group = "INTERSECTS") %>% 
  addPolylines(data = rail_bbox, color = "Blue", group = "BBOX") %>%
  addPolylines(data = rail_dwithin, color = "Green", group = "DWITHIN") %>%
  addLayersControl(baseGroups = c("Melbourne polygon", "Melbourne bbox"), 
                   overlayGroups = c("INTERSECTS", "BBOX", "DWITHIN")) %>%
  hideGroup(c("BBOX", "DWITHIN"))

