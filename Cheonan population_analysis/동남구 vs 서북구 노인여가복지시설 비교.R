library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# 필요한 패키지 설치 및 로드
install.packages(c("readr", "dplyr", "tidyr", "ggplot2"))
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. 파일 불러오기 (모두 문자열로 읽어온 후 숫자 변환)
# 노년인구 파일 불러오기
population_data <- read_csv("C:/Users/김효림/Downloads/2023노인인구.csv", 
                            col_types = cols(.default = "c"))

# 시설수 파일 불러오기
facility_data <- read_csv("C:/Users/김효림/Downloads/노인여가복지시설.csv", 
                          col_types = cols(.default = "c"))

# 2. 데이터 정리 - 쉼표 제거 후 숫자로 변환
clean_numbers <- function(df) {
  df %>% mutate(across(everything(), ~as.numeric(gsub(",", "", .))))
}

population_data <- clean_numbers(population_data)
facility_data <- clean_numbers(facility_data)

# 3. 데이터 구조화하기
data <- data.frame(
  구 = c("총계", "동남구", "서북구"),
  노년인구 = c(population_data$`2023_총노년인구`, 
           population_data$동남구_노년인구, 
           population_data$서북구_노년인구),
  시설수 = c(facility_data$`2023_노인여가_복지시설`, 
          facility_data$동남구_복지시설, 
          facility_data$서북구_복지시설)
)

# 4. 지표 계산하기
data <- data %>%
  mutate(
    인구_대_시설 = 노년인구 / 시설수,  # 시설 1개당 노년인구 수
    시설비율 = (시설수 / 노년인구) * 100  # 노년인구 100명당 시설 수
  )

# 5. 동남구와 서북구만 추출 (총계 제외)
district_data <- data %>% filter(구 != "총계")

# 6. 데이터 확인
print(district_data)

# 7. 구별 시설 현황 시각화

# 7-1. 시설수 그래프
ggplot(district_data, aes(x = 구, y = 시설수, fill = 구)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = 시설수), vjust = -0.5) +
  labs(title = "동남구 vs 서북구 노인여가복지시설 수", y = "시설 수") +
  theme_minimal() +
  theme(legend.position = "none")

# 7-2. 시설 1개당 노년인구 그래프
ggplot(district_data, aes(x = 구, y = 인구_대_시설, fill = 구)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(인구_대_시설, 1)), vjust = -0.5) +
  labs(title = "시설 1개당 담당 노년인구 수", y = "인구 수") +
  theme_minimal() +
  theme(legend.position = "none")

# 7-3. 이중 축 그래프 (시설수와 시설비율)
# 축 스케일 조정을 위한 계수 계산
coef <- max(district_data$시설수) / max(district_data$시설비율)

# 이중축 그래프 단순화 버전
# 이중축 그래프 단순화 버전
dev.off()
# 단순화된 이중축 그래프
ggplot(district_data, aes(x = 구)) +
  geom_bar(aes(y = 시설수), stat = "identity", fill = "skyblue") +
  geom_point(aes(y = 시설비율 * coef), color = "tomato", size = 3) +
  geom_text(aes(y = 시설수, label = 시설수), vjust = -0.5) +
  geom_text(aes(y = 시설비율 * coef, label = round(시설비율, 2)), 
            vjust = -0.5, color = "tomato") +
  scale_y_continuous(
    name = "시설수"
  ) +
  labs(title = "동남구 vs 서북구 노인여가복지시설 비교") +
  theme_minimal()

# 8. 구별 노년인구 대비 시설 충분성 비교 요약
summary_data <- district_data %>%
  select(구, 노년인구, 시설수, 인구_대_시설, 시설비율) %>%
  arrange(인구_대_시설)  # 시설 1개당 인구수가 적을수록 시설이 상대적으로 많음

print(summary_data)

