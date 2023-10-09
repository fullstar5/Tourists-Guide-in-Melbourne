library(ggmap)

# 获取Google Maps Geocoding API密钥
api_key <- "AIzaSyBP7dJFg5pLlkiVlBfaYnHHr4UBHsErw-M"
# 使用register_google()函数注册API密钥
register_google(key = api_key)

# Read dataset
landmarks <- read.csv('raw_data/melbourne_city_landmarks.csv')
# landmarks <- landmarks[!is.na(landmarks$Location) & landmarks$Location != "", ]

# Title 包含名称，部分包含地址，Location中有空的情况
landmarks_address <- paste(landmarks$Title, landmarks$Location, sep = ", ")
geocoded <- geocode(landmarks_address, output = "latlon")

# 将生成的经纬度坐标添加回原始数据集
landmarks$latitude <- geocoded$lat
landmarks$longitude <- geocoded$lon
landmarks <- landmarks[!is.na(landmarks$longitude) & !is.na(landmarks$latitude), ]

write.csv(landmarks, file = 'new_data/melbourne_city_landmarks(new).csv', row.names = FALSE)
