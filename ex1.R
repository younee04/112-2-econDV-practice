library(ggplot2)

ggplot()
# 範例資料框 ----

# 匯入使用的套件
library(tidyverse)
library(lubridate)

# 建立資料框
exchange_rate <- tibble(
  date = rep(seq(as.Date("1960-01-01"), as.Date("1962-02-01"), by = "months"), 3),
  country = rep(c("美元", "日元", "英鎊"), each = 26),
  rate = runif(78, 20, 40)
)

# 計算匯率升值率 
exchange_rate <- exchange_rate %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(rise_rate = (rate - lag(rate)) / lag(rate))

head(exchange_rate)

# 計算匯率升值率 
exchange_rate <- exchange_rate %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(rise_rate = (rate - lag(rate)) / lag(rate))

# 濾出時間範圍內且國家為"美元", "日元", "英鎊" 的資料並繪圖
exchangeRateExample <- exchange_rate %>%
  filter(year(date) >= 1960 & year(date) <= 1962,
         month(date) >= 1 & month(date) <= 2,
         country %in% c("美元", "日元", "英鎊")) 


# 範例圖型 ------

exchangeRateExample %>%
  ggplot(aes(x = date, y = rise_rate, color = country)) +
  geom_line() +
  labs(x = "日期（1960年1月到1962年2月）",
       y = "對台幣匯率升值率", 
       color = "國家") +
  theme_minimal()

# 引入資料 ----
library(readr)
exchangeRate <- read_csv("C:/Users/ruby1/Downloads/BP01M01.csv")
View(exchangeRate)

## 轉換日期格式 ------
exchangeRate <- exchangeRate %>%
  tidyr::separate(期間, into = c("year", "month"), sep = "M", remove = FALSE) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"))

## 以台幣為準的匯率 -----
library(dplyr)

# 計算新台幣對美元的匯率
ntd_usd_rate <- exchangeRate$`新台幣NTD/USD`

# 計算其他貨幣的匯率，對新台幣的匯率，並儲存到新的欄位
exchangeRate <- exchangeRate %>%
  mutate(`日圓` = ntd_usd_rate / `日圓JPY/USD`,
         `英鎊` = ntd_usd_rate / (1/`英鎊USD/GBP`),
         `港幣` = ntd_usd_rate / `港幣HKD/USD`,
         `韓元` = ntd_usd_rate / `韓元KRW/USD`,
         `美元` = `新台幣NTD/USD`) %>%
  select(date, `日圓`:`美元`)


## 計算匯率成長率 -----
exchangeRate <- exchangeRate %>%
  arrange(date) %>%
  mutate(across(c("美元", "日圓", "英鎊", "港幣", "韓元"), 
                ~( . - lag(.) ) / lag(.), 
                .names = "成長率_{.col}"))

## 改成長格式 -----

exchangeRate <- exchangeRate %>% 
  pivot_longer(cols = starts_with("成長率"),
               names_to = "country",
               values_to = "升值率")

## 作圖 ------

exchangeRate %>%
  ggplot(aes(x = date, y = 升值率, color = country)) +
  geom_line() +
  labs(x = "日期（1960年1月到1962年2月）",
       y = "對台幣匯率升值率", 
       color = "國家") +
  theme_minimal()

