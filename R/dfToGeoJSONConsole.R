dfToGeoJSONConsole <- function(data, name, lat.lon) {
	if(length(lat.lon)!=2) stop("'lat.lon' must be a vector of two: c(latitude, longitude)")
	if(any(!is.numeric(lat.lon))) {
		if(!any(names(data)==lat.lon[1])) stop('Longitude column not found')
		if(!any(names(data)==lat.lon[2])) stop('Latitude column not found')
		lat.lon <- c(which(names(data)==lat.lon[1]), which(names(data)==lat.lon[2]))
	}
	if(is.na(data[,lat.lon[1]]) || is.na(data[,lat.lon[2]])) stop('Coordinate columns not found')

	json <- c()

	# heading
	json <- append(json, '{')
	json <- append(json, '"type":"FeatureCollection",')
	json <- append(json, '"features":[')

	# features
	for(f in 1:nrow(data)) {
		json <- append(json, '{')
		json <- append(json, '"type":"Feature",')

		# properties
		if(length(data)>2) {
			json <- append(json, '"properties":{')
			dat <- data[f,-lat.lon]
			if(!is.data.frame(dat)) names(dat) <- names(data)[-lat.lon]

			if(length(dat)==1) {
				json <- append(json, paste0('"', names(data)[-lat.lon], '":"', dat, '"'))
			} else {
				for(p in 1:length(dat)) {
					json <- append(json, paste0('"', names(dat)[p], '":"', dat[p], '"'))
					if(p==length(dat)) json <- append(json, '')
					else json <- append(json, ',')
				}
			}
			json <- append(json, '},')
		}

		# geometry
		json <- append(json, '"geometry": {')
		json <- append(json, '"type":"Point",')
		json <- append(json, paste0('"coordinates":[', data[f,lat.lon[2]], ',', data[f,lat.lon[1]], ']'))
		json <- append(json, '}')

		if(f==nrow(data)) json <- append(json, '}')
		else json <- append(json, '},')
	}

	json <- append(json, ']')
	json <- append(json, '}')

	json <- paste(json, collapse = '')

	return(json)
}
