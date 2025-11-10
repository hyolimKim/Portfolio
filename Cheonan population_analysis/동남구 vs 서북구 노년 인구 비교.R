#동남구 vs 서북구 노년 인구 비교 
library(ggplot2)
library(tidyr)
library(dplyr)

# 데이터 입력 (예시)
df <- data.frame(
  연도 = 2018:2025,
  총노년인구 = c(48444, 67177, 72151, 76090, 80766, 85875,89589,97029),
  동남구 = c(33147, 34905, 37236, 39033, 41199, 43608,45470,48842),
  서북구 = c(15297, 32272, 34915, 37057, 39567, 42267,44119, 48187)
)

# long 형식으로 변환 (동남구/서북구 구분 컬럼 추가)
df_long <- df %>%
  pivot_longer(cols = c("동남구", "서북구"),
               names_to = "구",
               values_to = "노년인구")

# 히트맵
ggplot(df_long, aes(x = 구, y = as.factor(연도), fill = 노년인구)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="lightyellow", high="darkred") +
  labs(title="연도별 동남구 vs 서북구 노년인구 히트맵",
       x="구", y="연도", fill="노년인구") +
  theme_minimal()
