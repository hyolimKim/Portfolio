#서북구 인구 피라미드
# 필요한 패키지 설치 및 불러오기
library(readr)
library(dplyr)
library(ggplot2)

# CSV 파일 불러오기
population_data <- read_csv("C:/Users/김효림/Downloads/천안시 연령별 인구수 - 서북구 인구 (1).csv", 
                            col_types = cols(.default = "c"),
                            locale = locale(encoding = "UTF-8"))

# 데이터 전처리 (빈 행 제거, 쉼표 제거 및 숫자로 변환)
population_clean <- population_data %>%
  # 빈 행 제거
  filter(!is.na(연령) & 연령 != "") %>%
  # 쉼표 제거 후 숫자로 변환
  mutate(
    계 = as.numeric(gsub(",", "", 계)),
    남 = as.numeric(gsub(",", "", 남)),
    여 = as.numeric(gsub(",", "", 녀))
  )

# 연령대 순서 설정
age_order <- c("0∼4", "5∼9", "10∼14", "15∼19", "20∼24", "25∼29", 
               "30∼34", "35∼39", "40∼44", "45∼49", "50∼54", "55∼59", 
               "60∼64", "65∼69", "70∼74", "75∼79", "80∼84", "85∼89", 
               "90∼94", "95∼99", "100이상")
population_clean$연령 <- factor(population_clean$연령, levels = age_order)

# 인구 피라미드용 데이터 준비
pyramid_data <- population_clean %>%
  mutate(
    male = -남,  # 남성 인구는 음수로 변환
    female = 여
  ) %>%
  select(연령, male, female)

# 인구 피라미드 그리기
ggplot(pyramid_data, aes(x = 연령)) +
  geom_bar(aes(y = male, fill = "남성"), stat = "identity") +
  geom_bar(aes(y = female, fill = "여성"), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("남성" = "#4682B4", "여성" = "#FF69B4")) +
  labs(
    title = "천안시 서북구 인구 피라미드",
    subtitle = "2025년 기준",
    x = "연령대",
    y = "인구 수",
    fill = "성별"
  ) +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )