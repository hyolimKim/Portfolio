##천안시 총 인구수 변화 동남구VS 서북구 
#선그래프
library(tidyverse)

# 데이터 불러오기
df <- read_csv("C:/Users/김효림/Downloads/천안시 연령별 인구수 2018~2025 최종.csv")

# wide → long 변환
df_long <- df %>%
  pivot_longer(cols = c("동남구", "서북구", "총인구"),
               names_to = "region",
               values_to = "population")

# 확인
head(df_long)

ggplot(df_long, aes(x = 연도, y = population, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("총인구"="black", "동남구"="blue", "서북구"="red")) +
  labs(title = "천안시 총인구 변화 (2018~2025)",
       x = "연도", y = "인구 수",
       color = "지역") +
  theme_minimal(base_size = 14)

#막대그래프
library(ggplot2)

ggplot(df_long, aes(x = factor(연도), y = population, fill = region)) +
  geom_col(position = "dodge") +  # dodge = 지역별 막대 나란히
  scale_fill_manual(values = c("총인구"="black", "동남구"="blue", "서북구"="red")) +
  labs(title = "천안시 인구 변화 (막대 그래프)",
       x = "연도", y = "인구 수",
       fill = "지역") +
  theme_minimal(base_size = 14)

#누적막대그래프
ggplot(df_long %>% filter(region != "총인구"),
       aes(x = factor(연도), y = population, fill = region)) +
  geom_col(position = "stack") +
  labs(title = "천안시 구별 인구 비중",
       x = "연도", y = "인구 수", fill = "지역") +
  theme_minimal(base_size = 14)

#비율 막대 그래프 (100% stacked bar)

ggplot(df_long %>% filter(region != "총인구"),
       aes(x = factor(연도), y = population, fill = region)) +
  geom_col(position = "fill") +  # fill = 비율 100% 기준
  scale_y_continuous(labels = scales::percent) +
  labs(title = "천안시 구별 인구 비율 변화",
       x = "연도", y = "비율", fill = "지역") +
  theme_minimal(base_size = 14)


##움짤

library(gganimate)
library(gifski)

# CSV 불러오기 & long format 변환
df <- read_csv("C:/Users/김효림/Downloads/천안시 연령별 인구수 2018~2025 최종.csv")

df_long <- df %>%
  pivot_longer(cols = c("동남구", "서북구", "총인구"),
               names_to = "region",
               values_to = "population")

# 애니메이션 막대 그래프
p <- ggplot(df_long, aes(x = region, y = population, fill = region)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~연도) +
  scale_fill_manual(values = c("총인구"="black", "동남구"="blue", "서북구"="red")) +
  labs(title = "천안시 인구 변화: {closest_state}",
       x = "지역", y = "인구 수") +
  theme_minimal(base_size = 14) +
  transition_states(연도, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')

# GIF 저장
anim_save("천안시_인구변화.gif", animation = animate(p, fps = 10, width = 800, height = 600))


#선 애니메이션
p2 <- ggplot(df_long, aes(x = 연도, y = population, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("총인구"="black", "동남구"="blue", "서북구"="red")) +
  labs(title = "천안시 인구 추세 (2018~2025)\n연도: {frame_along}",
       x = "연도", y = "인구 수") +
  theme_minimal(base_size = 14) +
  transition_reveal(연도)

# GIF 저장
anim_save("천안시_인구추세.gif", animation = animate(p2, fps = 10, width = 800, height = 600))