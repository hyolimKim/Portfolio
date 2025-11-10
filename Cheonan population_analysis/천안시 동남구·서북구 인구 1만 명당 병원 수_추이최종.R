##노인 복지 인프라 현황

library(tidyverse)
library(readr)

# 1) 데이터 불러오기
data <- read_csv("C:/Users/김효림/Downloads/2018_2024_인구대비 병원.csv")
head(data)
# 2) 인구 1만명당 노인 복지시설 수 계산
data <- data %>%
  mutate(
    동남구_노인_1만명당_노인_복지시설수 = (동남구노인여가_복지시설/ 동남구_노년인구) * 10000,
    서북구_노인_1만명당노인_복지시설수 = (서북구노인여가_복지시설 / 서북구_노년인구) * 10000
  )

# 3) 긴 형식으로 변환
data_long <- data %>%
  select(연도, 동남구_1만명당_병원수, 서북구_1만명당_병원수) %>%
  pivot_longer(cols = -연도,
               names_to = "구",
               values_to = "인구1만명당병원수")

# 4) Heatmap 시각화
ggplot(data_long, aes(x = 연도, y = 구, fill = 인구1만명당병원수)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(인구1만명당병원수, 2)), color = "black") + # 수치 표시
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "천안시 동남구·서북구 인구 1만 명당 병원 수 (Heatmap)",
       x = "연도", y = "구", fill = "1만명당 병원 수") +
  theme_minimal(base_family = "NanumGothic")
