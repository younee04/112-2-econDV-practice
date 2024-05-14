# 安裝 choroplethr 包
#install.packages("choroplethr")
#install.packages("choroplethrMaps")

library(tidyverse)
library(sf)

# CSV 文件中應該包含兩列：region（縣市名稱）和 ratio（比率）
popu_data <- read_csv("C:/Users/ruby1/Desktop/econdv/ratio.csv")
# 檢查數據的前3行
glimpse(ratio_data)

#讀取台灣地圖
shapeData <- st_read("C:/Users/ruby1/Downloads/mapdata202301070205/COUNTY_MOI_1090820.shp")
glimpse(shapeData)

# 匯入縣市所得數據的 CSV 文件
# CSV 文件中應該包含兩列：COUNTYNAME（縣市名稱）和 ratio（比例數據）
ratio_data <- read_csv("C:/Users/ruby1/Desktop/econdv/ratio.csv")

# 將所得數據與台灣地理數據合併
combined_data <- shapeData %>%
  left_join(ratio_data, by = c("COUNTYNAME" = "COUNTYNAME"))

############################

# 創建依照所得數據的面量圖

ratio_choropleth <- ggplot(data = combined_data) +
  geom_sf(aes(fill = ratio), color = "gray") +
  scale_fill_gradientn(colors = c("#EEEFEA", "#E2AB7F", "#AA3A46"), 
                       values = c(0, 0.5, 1), limits = c(7, 8.5)) +
  scale_fill_gradient(low = "#E2AB7F", high = "#AA3A46", limits = c(7, 8.5),
                      na.value = "#EEEFEA") +
  theme_void() +
  labs(title = "台灣各區醫學中心護病比",
       subtitle = "資料單位: 每一護理人員照護之病人數",
       caption = "資料來源：衛生福利部中央健康保險署 ")+ 
  coord_sf(xlim = c(119.5, 122.5), ylim = c(21.5, 25.5), expand = FALSE)

ratio_choropleth


