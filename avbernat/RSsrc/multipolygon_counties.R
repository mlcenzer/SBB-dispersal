######## Create mappable multipolygon shapes for designated areas. Sites to use:

# (i) Go to OpenStreetMaps: https://nominatim.openstreetmap.org

# (ii) Type in the area you want your multipolygon for.

# (iii) Find the OSM ID and copy it.

# (iv) Go to Polygon Creation: http://polygons.openstreetmap.fr/index.py

# (v) Past the OSM ID into the 'Id of relation' box.

# (vi) Click on 'image' to confirm that is the shape you want on OpenStreetMaps or Google Maps.

# (vii) Click on GeoJSON and copy the url.

# (viii) Find a spatial reference number or (espg) for Florida. I went with 2236, East Florida as 
# pictured here on OpenStreetMaps: https://maps.omniscale.com/en/openstreetmap/epsg-2236. 


######## Just For Florida

map_multipolygons = function(dispersal_data, is_run_pops=FALSE) {

  florida <- "http://polygons.openstreetmap.fr/get_geojson.py?id=162050&params=0"

  # URL Florida (not using this one)
  florida_read <- st_read(florida)
  FL_url <- mutate(florida_read, population = "Florida")
  FL_url_transformed <- st_transform(FL_url, 2236)
  
  # State Maps Data
  states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
  florida_state <- states %>%
    filter(ID == "florida")
  FL <- mutate(florida_state, State = "FLORIDA, U.S.")
  FL_transformed <- st_transform(FL, 2236)
  
  
  # (ix) Convert the GeoJSON dat into a nested list using geojson_read() or lapply() and st_read as the second argument. 
  # lapply() and st_read() are much more efficient. The map() function is also similar to lapply(), but for some reason 
  # it does not work here. Similar to pandas in Python, the map() function transforms its input by applying a function 
  # to each element and returning a vector (or whichever class you want) with the same length as the input.
  
  # (x) If there is a gap in one or more of the polygons created, use st_make_valid() to bypass the error and make the 
  # polygon valid.
  
  # (xi) Transform the GEOMETRY COLLECTIONS to MULTIPOLYGONS using st_cast().
  
  
  ######## For Other Populations and Their Respective Counties
  
  lake_wales <- "http://polygons.openstreetmap.fr/get_geojson.py?id=117732&params=0"
  polk <- "http://polygons.openstreetmap.fr/get_geojson.py?id=1214525&params=0"
  
  lake_placid <- "http://polygons.openstreetmap.fr/get_geojson.py?id=117660&params=0"
  highlands <- "http://polygons.openstreetmap.fr/get_geojson.py?id=1210702&params=0"
  
  homestead <-"http://polygons.openstreetmap.fr/get_geojson.py?id=1216731&params=0" 
  miami_dade <- "http://polygons.openstreetmap.fr/get_geojson.py?id=1210692&params=0"
  
  gainesville <- "http://polygons.openstreetmap.fr/get_geojson.py?id=118870&params=0"
  alachua <- "http://polygons.openstreetmap.fr/get_geojson.py?id=1210739&params=0"
  
  leesburg <- "http://polygons.openstreetmap.fr/get_geojson.py?id=117476&params=0"
  lake <- "http://polygons.openstreetmap.fr/get_geojson.py?id=389026&params=0"
  
  ######## Have the same counties (Monroe), so kept on an island level. 
  
  north_key_largo <- "http://polygons.openstreetmap.fr/get_geojson.py?id=119071&params=0" 
  key_largo <- "http://polygons.openstreetmap.fr/get_geojson.py?id=119070&params=0"
  plantation_key <-  "http://polygons.openstreetmap.fr/get_geojson.py?id=117963&params=0"
  
  populations <- c("Lake Wales", "Lake Placid", "Homestead", "North Key Largo",
                   "Gainesville", "Leesburg", "Key Largo", "Plantation Key")
  
  counties <- c("Polk", "Highlands", "Miami-Dade", "Alachua", "Lake", "North Key Largo", "Key Largo", "Plantation Key")
  
  pop_urls <- c(lake_wales, lake_placid, homestead, north_key_largo, gainesville,
                leesburg, key_largo, plantation_key)
  county_urls <- c(polk, highlands, miami_dade, alachua, lake, north_key_largo, key_largo, plantation_key)
  
  
  if (is_run_pops) {
    ######## For Populations
    polylist <- lapply(pop_urls, st_read)
    sf_df <- do.call(rbind, polylist)
    pops <- cbind(sf_df, populations)
    df_transformed <- st_transform(pops, 2236)
    
    ######## Geometry reports
    
    poly_valid_report <- st_is_valid(df_transformed)
    poly_valid_report # see that LW, LP, GV, LB, and PK are invalid
    
  }
  

  ######## For Counties
  countylist <- lapply(county_urls, st_read)
  sf_df_county <- do.call(rbind, countylist)
  counties_df <- cbind(sf_df_county, counties)
  counties_transformed <- st_transform(counties_df, 2236)
  
  ######## Geometry reports
  
  county_valid_report <- st_is_valid(counties_transformed)
  county_valid_report
  
  # Make any invalid polygons valid
  df_final_counties <- st_make_valid(counties_transformed) %>%
    st_cast("MULTIPOLYGON") 
  
  # Check the final report
  county_valid_report2 <- st_is_valid(df_final_counties)
  county_valid_report2
  
  
  ######## this stopped working so don't run:
  
  # df_final_pop <- st_make_valid(df_transformed) %>% # stopped working but not necessary for map making
  #   st_cast("MULTIPOLYGON") 
  # 
  # poly_valid_report2 <- st_is_valid(df_final_pop)
  # poly_valid_report2
  # 

  # (xii) Combine the comprehensive sf dataframe with the sf dataframe mapping the populations to the polygon shapes.
  
  # (xiii) Write your data to a csv or gpkg file using write_sf(). (Still unsure what application opens gpkg files)
  
  
  ######## Add Counties to the Populations
  combined <- st_join(df_final_counties, dispersal_data) 
  
  #write_sf(combined, "flight_spatial_data.csv", layer_options="GEOMETRY=AS_WKT")
  #write_sf(combined_df, "combined2.gpkg")
  
  multipolygons = list(FL_transformed, combined)
  return(multipolygons)

}