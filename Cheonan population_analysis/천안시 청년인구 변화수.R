#천안시 청년인구 변화수
library(tidyverse)
library(gganimate)
library(gifski)

df <- read_csv("C:/Users/김효림/Downloads/천안시 2018~2025 청년인구.csv")

df_long <- df %>%
  pivot_longer(cols = c("동남구", "서북구", "총청년인구"),
               names_to = "region",
               values_to = "population")

ggplot(df_long, aes(x = 연도, y = population, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("총청년 인구"="black", "동남구"="blue", "서북구"="red")) +
  labs(title = "천안시 총청년 인구 변화 (2018~2025)",
       x = "연도", y = "인구 수",
       color = "지역") +
  theme_minimal(base_size = 14)

#선 애니메이션
p2 <- ggplot(df_long, aes(x = 연도, y = population, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("총 청년인구"="black", "동남구"="blue", "서북구"="red")) +
  labs(title = "천안시 청년 인구 추세 (2018~2023)\n연도: {frame_along}",
       x = "연도", y = "인구 수") +
  theme_minimal(base_size = 14) +
  transition_reveal(연도)

#누적막대그래프
ggplot(df_long %>% filter(region != "청년 인구"),
       aes(x = factor(연도), y = population, fill = region)) +
  geom_col(position = "stack") +
  labs(title = "천안시 청년 인구 비중",
       x = "연도", y = "청년 인구 수", fill = "지역") +
  theme_minimal(base_size = 14)

#비율 막대 그래프 (100% stacked bar)
ggplot(df_long %>% filter(region != "총청년인구"),
       aes(x = factor(연도), y = population, fill = region)) +
  geom_col(position = "fill") +  # fill = 비율 100% 기준
  scale_y_continuous(labels = scales::percent) +
  labs(title = "천안시 청년 인구 비율 변화",
       x = "연도", y = "비율", fill = "지역") +
  theme_minimal(base_size = 14)
#막대 그래프

ggplot(df_long, aes(x = factor(연도), y = population, fill = region)) +
  geom_col(position = "dodge") +  # dodge = 지역별 막대 나란히
  scale_fill_manual(values = c("총인구"="black", "동남구"="blue", "서북구"="red")) +
  labs(title = "천안시 인구 변화 (막대 그래프)",
       x = "연도", y = "인구 수",
       fill = "지역") +
  theme_minimal(base_size = 14)


# GIF 저장
anim_save("천안시_청년 인구추세.gif", animation = animate(p2, fps = 10, width = 800, height = 600))