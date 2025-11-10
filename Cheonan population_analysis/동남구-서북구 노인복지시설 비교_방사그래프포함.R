#동남구·서북구 데이터 시각화 
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra) # 여러 그래프 한 화면에 배치

# 1. 데이터 불러오기 및 전처리
population_data <- read_csv("C:/Users/김효림/Downloads/2023노인인구.csv", col_types = cols(.default = "c"))
facility_data <- read_csv("C:/Users/김효림/Downloads/노인여가복지시설.csv", col_types = cols(.default = "c"))

# 쉼표 제거 후 숫자로 변환
clean_numbers <- function(df) {
  df %>% mutate(across(everything(), ~ as.numeric(gsub(",", "", .))))
}

population_data <- clean_numbers(population_data)
facility_data <- clean_numbers(facility_data)

# 2. 데이터 구조화
district_data <- data.frame(
  구 = c("동남구", "서북구"),
  노년인구 = c(population_data$동남구_노년인구, population_data$서북구_노년인구),
  시설수 = c(facility_data$동남구_복지시설, facility_data$서북구_복지시설)
)

# 3. 지표 계산
district_data <- district_data %>%
  mutate(
    인구_대_시설 = 노년인구 / 시설수,
    시설비율 = (시설수 / 노년인구) * 100
  )

# 4. 시각화: 각 지표별 그래프 생성
# 4-1. 시설수 그래프
p1 <- ggplot(district_data, aes(x = 구, y = 시설수, fill = 구)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = 시설수), vjust = -0.5, size = 4) +
  labs(title = "구별 노인여가복지시설 수", y = "시설 수") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 12))

# 4-2. 인구_대_시설 그래프
p2 <- ggplot(district_data, aes(x = 구, y = 인구_대_시설, fill = 구)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(인구_대_시설, 2)), vjust = -0.5, size = 4) +
  labs(title = "시설 1개당 담당 노년인구", y = "인구 수") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 12))

# 4-3. 시설비율 그래프
p3 <- ggplot(district_data, aes(x = 구, y = 시설비율, fill = 구)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(시설비율, 2)), vjust = -0.5, size = 4) +
  labs(title = "노년인구 100명당 시설 수(%)", y = "비율(%)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 12))

# 여러 그래프 한 화면에 배치
grid.arrange(p1, p2, p3, ncol = 3)

# 1. 기술통계량 계산
summary_stats <- district_data %>%
  summarise(
    시설수_평균 = mean(시설수),
    시설수_차이 = max(시설수) - min(시설수),
    인구_대_시설_평균 = mean(인구_대_시설),
    시설비율_평균 = mean(시설비율),
    시설비율_차이_퍼센트 = (max(시설비율) - min(시설비율)) / mean(시설비율) * 100
  )

print(summary_stats)

# 2. 상대적 차이 계산
comparison <- district_data %>%
  mutate(
    시설수_상대비율 = 시설수 / sum(시설수) * 100,
    노년인구_상대비율 = 노년인구 / sum(노년인구) * 100,
    효율성지수 = 시설수_상대비율 / 노년인구_상대비율
  )

print(comparison)

# 3. 시각화: 방사형 차트로 구별 지표 비교
install.packages("fmsb")
library(fmsb)

# 데이터 준비
install.packages("tibble")
library(tibble)
radar_data <- district_data %>%
  select(구, 시설수, 노년인구, 인구_대_시설, 시설비율) %>%
  mutate(across(-구, ~scale(.) %>% as.vector())) %>%
  column_to_rownames(var = "구")

# 최대/최소값 추가
radar_data <- rbind(rep(1,4), rep(-1,4), radar_data)

# 방사형 차트 그리기
library(fmsb)
par(mar = c(1, 1, 1, 1))
radarchart(radar_data, 
           pcol = c("red", "blue"),
           pfcol = scales::alpha(c("red", "blue"), 0.3),
           plwd = 2,
           cglcol = "grey", 
           cglty = 1,
           axistype = 1,
           title = "동남구-서북구 노인복지시설 비교")

legend("topright", 
       legend = rownames(radar_data)[3:4], 
       col = c("red", "blue"),
       lty = 1,
       lwd = 2,
       pch = 20)

