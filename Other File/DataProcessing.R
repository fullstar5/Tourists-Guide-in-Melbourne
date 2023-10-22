library(ggmap)

# Google Maps Geocoding API Key
api_key <- "AIzaSyBP7dJFg5pLlkiVlBfaYnHHr4UBHsErw-M"
register_google(key = api_key)

# Read dataset
landmarks <- read.csv('raw_data/melbourne_and_metropolitan_hotels_pubs_and_publicans.csv')
# landmarks <- landmarks[!is.na(landmarks$Location) & landmarks$Location != "", ]
landmarks$Title <- sapply(strsplit(landmarks$Title, ","), function(x) x[1])
landmarks$Title

# feature about location
landmarks_address <- paste(landmarks$Title, landmarks$Location, landmarks$Suburb.State, sep = ", ")
geocoded <- geocode(landmarks_address, output = "latlon")

# add lat and lon to dataset
landmarks$latitude <- geocoded$lat
landmarks$longitude <- geocoded$lon
landmarks <- landmarks[!is.na(landmarks$longitude) & !is.na(landmarks$latitude), ]

# csv file
write.csv(landmarks, file = 'new_data/melbourne_and_metropolitan_hotels_pubs_and_publicans(new).csv', row.names = FALSE)

