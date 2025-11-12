An R script rawing a buffer in meters around a point shapefile, then projecting and aggregating the values ​​from a background raster layer, containing, for example, elevations or soil types.

https://github.com/ftheeten/R_BUFFER_AND_RASTER_PROJECTION It takes three files as input parameters:

1. A vector file with the points
2. A raster (Geotif)
3. A QML file (QGIS style) for mapping the numerical values  to the legend (developped with the JRC raster of the deforestation types :  https://forobs.jrc.ec.europa.eu/TMF/data#downloads )
