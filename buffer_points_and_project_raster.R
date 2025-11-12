require(sf)
require(tibble)
require(terra)
require(XML)
require(dplyr)


get_utm_zone<-function(x_center, y_center)
{
  zone_number <- round((x_center + 180) / 6) + 1
  if(y_center >0)
  {
    epsg_code <- 32600 + zone_number
  }
  else
  {
    epsg_code <- 32700 + zone_number
  }
  return( epsg_code)
}
  
point_layer_name=choose.files( multi=FALSE, caption="Select input vector layer", filters = matrix(c(
  " Geopackage", "*.gpkg",
  " shapefile", "*.shp",
  " all", "*.*"
), ncol = 2, byrow = TRUE))
raster_layer_name=choose.files( multi=FALSE, caption="Select input raster layer", filters = matrix(c(
  " tif", "*.tif",
  " tiff", "*.tiff",
  " all", "*.*"
), ncol = 2, byrow = TRUE))
raster_style_name=choose.files( multi=FALSE, caption="Select input raster style", filters = matrix(c(
  " qml", "*.qml",
  " xml", "*.xml",
  " all", "*.*"
), ncol = 2, byrow = TRUE))


#attention GEOS >3.12 pour buffer métriques sur WGS84
print("enter your radius un meters")
buffer_m<-scan (what=integer(), nmax=1, quiet=T)

point_layer<-read_sf(dsn = point_layer_name)
df_point_layer <- as.data.frame(point_layer)
df_point_layer$ID_ROW<-seq_len(nrow(df_point_layer))
df_point_layer<-df_point_layer[ , -which(names(df_point_layer) %in% c("geometry"))]
buffered_layer=st_buffer(point_layer, dist=buffer_m)

raster_layer<-rast(raster_layer_name)


x_center=(xmin(raster_layer)+xmax(raster_layer))/2
y_center=(ymin(raster_layer)+ymax(raster_layer))/2


crs_raster<-crs(raster_layer)
if(is.lonlat(crs_raster))
{
  utm_zone<-get_utm_zone(x_center, y_center)
  point_1<-st_point(c(xmin(raster_layer),ymin(raster_layer)))
  g1<-st_sfc(point_1, crs = st_crs(4326))
  point_2<-st_point(c(xmax(raster_layer),ymin(raster_layer)))
  g2<-st_sfc(point_2, crs = st_crs(4326))
  utm_g1<-st_transform(g1, st_crs(utm_zone))
  utm_g2<-st_transform(g2, st_crs(utm_zone))
  dist_deg<-st_distance(point_1,point_2 )
  dist_m<-st_distance(utm_g1,utm_g2 )
  m_to_deg<-dist_deg/dist_m
  deg_to_met<-dist_m/dist_deg
  cell_size<-res(raster_layer)
  rs_x<-cell_size[1]
  rs_y<-cell_size[2]
  m_x<-rs_x*deg_to_met
  m_y<-rs_y*deg_to_met
  area_sq_m<-m_x*m_y
  area_ha<-as.numeric(area_sq_m/10000)
} else
{
  cell_size<-res(raster_layer)
  rs_x<-cell_size[1]
  rs_y<-cell_size[2]
  area_sq_m<-rs_x*rs_y
  area_ha<-as.numeric(area_sq_m/10000)
}
#raster_layer<-cellSize(raster_layer, mask=FALSE, lyrs=FALSE, unit="ha", transform=TRUE)

raster_styles <- xmlParse(raster_style_name)


style_items<-getNodeSet(raster_styles, "//item")
attrs_list <- lapply(style_items, xmlAttrs)
df_style <- do.call(rbind, lapply(attrs_list, as.data.frame.list, stringsAsFactors = FALSE))

intersection<-extract(raster_layer,buffered_layer)
df_intersection<-as.data.frame(intersection)

colnames(df_intersection) <- c( "ID","item_type")
#df_intersection %>% distinct(ID)

df_intersection<- merge(x=df_intersection,y=df_style, 
                        by.x=c("item_type"), 
                        by.y=c("value"),
                        all.x=TRUE)
df_intersection<-df_intersection[, c("item_type", "ID", "label")]
df_intersection["area_ha"]<-rep(area_ha,nrow(df_intersection))

df_final<-df_intersection %>%
  group_by(ID, item_type, label ) %>%
  summarise(count_cell = n(), area_h = sum(area_ha))

df_final<- merge(x=df_final,y=df_point_layer, 
                        by.x=c("ID"), 
                        by.y=c("ID_ROW"),
                        all.x=TRUE)

write.csv(df_final,file.choose(), row.names = FALSE)





