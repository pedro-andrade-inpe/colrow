
cr = readShapePoints("brasil-maps/brasilcr_pt.shp")
cr@data$COLROW30 = as.character(cr@data$COLROW30)

create_polygon = function(x, y, id)
{
    vx = c(x-0.25, x+0.25, x+0.25, x-0.25, x-0.25)
    vy = c(y-0.25, y-0.25, y+0.25, y+0.25, y-0.25)
    return (Polygons(list(Polygon(cbind(vx, vy))), id))
}

pol_list = list()

for(i in 1:dim(cr@data)[1])
{
    print(i)
    pol_list[[i]] = create_polygon(cr@data[i,2], cr@data[i,3], cr@data[i, 1])
}

brazil_cr = SpatialPolygons(pol_list)

ids = data.frame(COLROW30=getID(brazil_cr))
rownames(ids) = getID(brazil_cr)

brazil_cr = SpatialPolygonsDataFrame(brazil_cr, ids)

brazil_cr@data$COLROW30 = as.character(brazil_cr@data$COLROW30)


