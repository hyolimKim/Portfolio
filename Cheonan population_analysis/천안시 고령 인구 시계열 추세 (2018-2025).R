# 필요한 패키지
library(readr)     # CSV 불러오기
library(dplyr)     # 데이터 가공
library(tidyr)     # long format 변환
library(ggplot2)   # 시각화


aging <- read_csv("C:/Users/김효림/Downloads/천안시 연령별 인구수 - 시트8 (1).csv")


# 컬럼명 강제로 지정 (한글이 꼬였을 경우 대비)
colnames(aging) <- c("연도","총노년인구","여성인구","남성인구")

#성별에 따른 노년 인구 추이
# wide → long 변환 (총노년인구, 여성, 남성 모두 포함)
aging_long <- aging %>%
  pivot_longer(cols = c("총노년인구","여성인구","남성인구"),
               names_to = "구분",
               values_to = "인구")

# 시계열 그래프
ggplot(aging_long, aes(x=연도, y=인구, color=구분)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  theme_minimal(base_size=14) +
  scale_color_manual(values=c("총노년인구"="black",
                              "여성인구"="red",
                              "남성인구"="blue")) +
  labs(title="천안시 고령 인구 시계열 추세 (2018-2025)",
       x="연도", y="인구(명)", color="구분")