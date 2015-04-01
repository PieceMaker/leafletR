spToGeoJSONConsole <- function(data) {
	if(!requireNamespace('sp', quietly=TRUE)) stop("'sp' package required for spatial object conversion")
	if(requireNamespace('rgdal', quietly=TRUE)) data <- sp::spTransform(data, sp::CRS('+proj=longlat +ellps=WGS84'))

	json <- c()

	# heading
	json <- append(json, '{')
	json <- append(json, ('"type":"FeatureCollection",'))
	json <- append(json, ('"features":['))

	if(class(data)[1]=='SpatialPoints' || class(data)[1]=='SpatialPointsDataFrame') {	# Points
		# features
		coord <- data@coords
		for(f in 1:nrow(coord)) {
			json <- append(json, ('{'))
			json <- append(json, ('"type":"Feature",'))

			# properties
			if(class(data)[1]=='SpatialPointsDataFrame') {
				dat <- data@data
				if(!is.null(dat)) {
					json <- append(json, ('"properties":{'))
					for(p in 1:length(dat)) {
						json <- append(json, (paste0('"', names(dat)[p], '":"', dat[f,p], '"')))
						if(p==length(dat)) json <- append(json, (''))
						else json <- append(json, (','))
					}
					json <- append(json, ('},'))
				}
			}

			# geometry
			json <- append(json, ('"geometry":{'))
			json <- append(json, ('"type":"Point",'))
			json <- append(json, (paste0('"coordinates":[', coord[f,1], ',', coord[f,2], ']')))
			json <- append(json, ('}'))

			if(f==nrow(data)) json <- append(json, ('}'))
			else json <- append(json, ('},'))
		}
	} else if(class(data)[1]=='SpatialLines' || class(data)[1]=='SpatialLinesDataFrame') {	# Lines
		# features
		num.f <- length(data@lines)
		f.len <- sapply(slot(data, 'lines'), function(x) length(slot(x, 'Lines')))
		for(f in 1:num.f) {
			json <- append(json, ('{'))
			json <- append(json, ('"type":"Feature",'))

			# properties
			json <- append(json, ('"properties":{'))
			if(class(data)[1]=='SpatialLinesDataFrame') {
				dat <- data@data
				if(!is.null(dat)) {
					for(p in 1:length(dat)) {
						json <- append(json, (paste0('"', names(dat)[p], '":"', dat[f,p], '"')))
						json <- append(json, (','))
					}
				}
			}
			json <- append(json, (paste0('"ID":"', slot(slot(data, 'lines')[[f]], 'ID'), '"')))
			json <- append(json, ('},'))

			# geometry
			json <- append(json, ('"geometry":{'))
			if(f.len[f]==1) {	# SingleLines
				json <- append(json, ('"type":"LineString",'))
				coord <- paste0('[', sp::coordinates(data@lines[[f]])[[1]][1,1], ',', sp::coordinates(data@lines[[f]])[[1]][1,2], ']')
				for(i in 2:length(sp::coordinates(data@lines[[f]])[[1]][,1])) coord <- append(coord, paste0('[', sp::coordinates(data@lines[[f]])[[1]][i,1], ',', sp::coordinates(data@lines[[f]])[[1]][i,2], ']'))
				coord <- paste(coord, collapse=',')
			} else {	# MultiLines
				json <- append(json, ('"type": "MultiLineString",'))
				coord <- NULL
				for(l in 1:f.len[f]) {
					ln <- paste0('[', sp::coordinates(data@lines[[f]])[[l]][1,1], ',', sp::coordinates(data@lines[[f]])[[l]][1,2], ']')
					for(i in 2:length(sp::coordinates(data@lines[[f]])[[l]][,1])) ln <- append(ln, paste0('[', sp::coordinates(data@lines[[f]])[[l]][i,1], ',', sp::coordinates(data@lines[[f]])[[l]][i,2], ']'))
					ln <- paste0('[', paste(ln, collapse=','), ']')
					if(is.null(coord)) coord <- ln
					else coord <- append(coord, ln)
				}
				coord <- paste(coord, collapse=',')
			}
			json <- append(json, (paste0('"coordinates":[', coord, ']')))
			json <- append(json, ('}'))

			if(f==num.f) json <- append(json, ('}'))
			else json <- append(json, ('},'))
		}
	} else if(class(data)[1]=='SpatialPolygons' || class(data)[1]=='SpatialPolygonsDataFrame') {	# Polygons
		# features
		num.f <- length(data@polygons)
		f.len <- sapply(slot(data, 'polygons'), function(x) length(slot(x, 'Polygons')))
		for(f in 1:num.f) {
			json <- append(json, ('{'))
			json <- append(json, ('"type":"Feature",'))

			# properties
			json <- append(json, ('"properties":{'))
			if(class(data)[1]=='SpatialPolygonsDataFrame') {
				dat <- data@data
				if(!is.null(dat)) {
					for(p in 1:length(dat)) {
						json <- append(json, (paste0('"', names(dat)[p], '":"', dat[f,p], '"')))
						json <- append(json, (','))
					}
				}
			}
			json <- append(json, (paste0('"ID":"', slot(slot(data, 'polygons')[[f]], 'ID'), '"')))
			json <- append(json, ('},'))

			# geometry
			json <- append(json, ('"geometry":{'))
			if(f.len[f]==1) {	# SinglePolygon without holes
				json <- append(json, ('"type":"Polygon",'))
				coord.raw <- slot(slot(slot(data, 'polygons')[[f]], 'Polygons')[[1]], 'coords')
				coord <- paste0('[', coord.raw[1,1], ',', coord.raw[1,2], ']')
				for(i in 2:length(coord.raw[,1])) coord <- append(coord, paste0('[', coord.raw[i,1], ',', coord.raw[i,2], ']'))
				coord <- paste0('[', paste(coord, collapse=','), ']')
			} else {
				hole <- sapply(slot(slot(data, 'polygons')[[f]], 'Polygons'), function(x) slot(x, 'hole'))
				if(length(hole[hole==TRUE])==0) {	# MultiPolygon without holes
					json <- append(json, ('"type":"MultiPolygon",'))
					coord.raw <- lapply(slot(slot(data, 'polygons')[[f]], 'Polygons'), function(x) slot(x, 'coords'))
					coord <- NULL
					for(p in 1:length(coord.raw)) {
						coord.p <- paste0('[', coord.raw[[p]][1,1], ',', coord.raw[[p]][1,2], ']')
						for(i in 2:length(coord.raw[[p]][,1])) coord.p <- append(coord.p, paste0('[', coord.raw[[p]][i,1], ',', coord.raw[[p]][i,2], ']'))
						coord.p <- paste0('[', paste(coord.p, collapse=','), ']')
						if(is.null(coord)) coord <- coord.p
						else coord <- append(coord, coord.p)
					}
					coord <- paste0('[', paste(coord, collapse=','), ']')
				} else {
					if(length(hole[hole==FALSE])==1) {	# SinglePolygon with hole(s)
						json <- append(json, ('"type":"Polygon",'))
						coord.raw <- lapply(slot(slot(data, 'polygons')[[f]], 'Polygons'), function(x) slot(x, 'coords'))
						pol <- which(hole==FALSE)
						coord <- paste0('[', coord.raw[[pol]][1,1], ',', coord.raw[[pol]][1,2], ']')
						for(i in 2:length(coord.raw[[pol]][,1])) coord <- append(coord, paste0('[', coord.raw[[pol]][i,1], ',', coord.raw[[pol]][i,2], ']'))
						coord <- paste0('[', paste(coord, collapse=','), ']')
						coord.raw[[pol]] <- NULL
						for(p in 1:length(coord.raw)) {
							coord.h <- paste0('[', coord.raw[[p]][1,1], ',', coord.raw[[p]][1,2], ']')
							for(i in 2:length(coord.raw[[p]][,1])) coord.h <- append(coord.h, paste0('[', coord.raw[[p]][i,1], ',', coord.raw[[p]][i,2], ']'))
							coord.h <- paste0('[', paste(coord.h, collapse=','), ']')
							coord <- append(coord, coord.h)
						}
						coord <- paste(coord, collapse=',')
					} else {	# MultiPolygon with hole(s)
						json <- append(json, ('"type":"MultiPolygon",'))
						coord.raw <- lapply(slot(slot(data, 'polygons')[[f]], 'Polygons'), function(x) slot(x, 'coords'))
						pol <- which(hole==FALSE)
						hol <- which(hole==TRUE)
						hol.idx <- NULL
						for(h in 1:length(hol)) {
							for(p in 1:length(pol)) {
								if(checkPolyHole(coord.raw[[pol[p]]], coord.raw[[hol[h]]])) {
									hol.idx <- append(hol.idx, pol[p])
									break
								}
							}
						}
						if(length(hol.idx)<length(hol)) warning('Warning: one or more holes could not be assigned to polygon', call.=FALSE)

						coord <- NULL
						for(p in 1:length(pol)) {
							coord.p <- paste0('[', coord.raw[[pol[p]]][1,1], ',', coord.raw[[pol[p]]][1,2], ']')
							for(i in 2:length(coord.raw[[pol[p]]][,1])) coord.p <- append(coord.p, paste0('[', coord.raw[[pol[p]]][i,1], ',', coord.raw[[pol[p]]][i,2], ']'))
							coord.p <- paste0('[', paste(coord.p, collapse=','), ']')

							idx <- which(hol.idx==pol[p])
							if(length(idx)>0) {
								coord.hs <- NULL
								for(i in 1:length(idx)) {
									coord.h <- paste0('[', coord.raw[[hol[i]]][1,1], ',', coord.raw[[hol[i]]][1,2], ']')
									for(j in 2:length(coord.raw[[hol[i]]][,1])) coord.h <- append(coord.h, paste0('[', coord.raw[[hol[i]]][j,1], ',', coord.raw[[hol[i]]][j,2], ']'))
									coord.h <- paste0('[', paste(coord.h, collapse=','), ']')
									if(is.null(coord.hs)) coord.hs <- coord.h
									else coord.hs <- append(coord.hs, coord.h)
								}
								coord.p <- paste0(coord.p, ',', paste(coord.hs, collapse=','))
							}
							if(is.null(coord)) coord <- coord.p
							else coord <- append(coord, coord.p)
						}
						coord <- paste0('[', paste(coord, collapse='],['), ']')
					}
				}
			}
			json <- append(json, (paste0('"coordinates":[', coord, ']')))
			json <- append(json, ('}'))

			if(f==num.f) json <- append(json, ('}'))
			else json <- append(json, ('},'))
		}
	}

	# close
	json <- append(json, (']'))
	json <- append(json, ('}'))

	json <- paste(json, collapse = '')

	return(json)
}