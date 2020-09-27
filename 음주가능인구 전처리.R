library(readxl)
library(stringr)
library(dplyr)
install.packages('dplyr')

# '2018_국가별_연령별_인구' 자료 전처리
population <- read_excel(file.choose(), col_names = TRUE, skip = 16)
View(population)
str(population)
colnames(population)[3] <- 'Country'
colnames(population)[8] <- 'Year'

# '관세율+맥주수입량' 자료 전처리
data <- read_excel(file.choose())
View(data)
data <- data %>% select(nation, drinkable_age,ttlbeer)

# '관세율+맥주수입량'자료의 TOP 19 나라 데이터만 분리
countries <- data$nation
population_filter <- filter(population, Country %in% countries)
View(population_filter)
str(population_filter)
population_filter$Country <- factor(population_filter$Country)
levels(population_filter$Country)

# 2018년 데이터만 분리 후 두 개의 데이터 조인
population_year <- filter(population_filter, Year == '2018')
str(population_year)
population_modify <- population_year %>% select(-Index, -Variant, -Notes, -`Country code`, -Type, -`Parent code`)
View(population_modify)
colnames(population_modify)[1] <- '나라'
colnames(data)[1] <- '나라'
population_expand <- inner_join(data, population_modify, by = '나라')
View(population_expand)
str(population_expand)

#자료 데이터프레임화(계산을 위해)
df <- as.data.frame(population_expand)
colnames(df)[3] <- '맥주소비량'
colnames(df)[2] <- '실질 음주 가능 나이'


# 나이별 인구 수 숫자화
for(i in 4:ncol(df)){
  df[,i] <- as.numeric(df[,i])
}
str(df)

#음주가능나이 인구 계산
df$`음주가능나이인구` <- 0
last_idx <- which(colnames(df) == '100')
for(i in 1:nrow(df)){
  idx <- which(colnames(df) == df[i, ]$`실질 음주 가능 나이`)
  print(idx)
  df[i, ]$`음주가능나이인구` <- sum(df[i, idx:last_idx])
}

View(df)


#최종 데이터 도출
newdata <- df %>% select('나라', '실질 음주 가능 나이', '음주가능나이인구','맥주소비량')
View(newdata)

str(newdata)
newdata$음주가능나이인구 <- newdata$음주가능나이인구*1000

newdata$`1인당맥주소비량` <- 0
newdata$`1인당맥주소비량` <- newdata$`맥주소비량`/newdata$`음주가능나이인구`


View(newdata)
