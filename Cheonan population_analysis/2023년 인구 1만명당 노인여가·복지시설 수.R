library(ggplot2)
library(dplyr)
library(tidyr)

#동남구 vs 서북구 시설수 막대그래프
# 데이터 불러오기
df <- read.csv("C:/Users/김효림/Downloads/천안시 연령별 인구수 - 2023천안시_노인여가복지시설.csv", row.names = 1, fileEncoding = "UTF-8")

# long형 변환
df_long <- df %>%
  tibble::rownames_to_column("시설") %>%
  pivot_longer(cols = c("동남구", "서북구"),
               names_to = "구",
               values_to = "시설수")

# 막대그래프
ggplot(df_long, aes(x = 시설, y = 시설수, fill = 구)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "2023년 천안시 노인여가·복지시설 현황",
       x = "시설 종류", y = "시설 수") +
  theme_minimal(base_family = "AppleGothic") +  # 윈도우면 family="NanumGothic"
  scale_fill_manual(values = c("동남구" = "#1f77b4", "서북구" = "#ff7f0e"))

# 2023년 인구 (예시값, 실제 인구로 교체 권장)
dongnam_pop <- 43608
seobuk_pop  <- 42267

# 인구 1만명당 시설 수 계산
df_rate <- df %>%
  mutate(
    동남구 = round((동남구 / dongnam_pop) * 10000, 2),
    서북구 = round((서북구 / seobuk_pop) * 10000, 2)
  )

# long형 변환
df_rate_long <- df_rate %>%
  tibble::rownames_to_column("시설") %>%
  pivot_longer(cols = c("동남구", "서북구"),
               names_to = "구",
               values_to = "인구1만명당_시설수")

# 히트맵
ggplot(df_rate_long, aes(x = 시설, y = 구, fill = 인구1만명당_시설수)) +
  geom_tile(color = "white") +
  geom_text(aes(label = 인구1만명당_시설수), color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "2023년 인구 1만명당 노인여가·복지시설 수",
       x = "시설 종류", y = "구") +
  theme_minimal(base_family = "AppleGothic")
