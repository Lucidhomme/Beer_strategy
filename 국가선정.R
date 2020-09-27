library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

beer <- read_xlsx(file.choose())
str(beer)
head(beer)
View(beer)
tail(beer)

#필요 행/필요열 추출
ttl_beer <- select(beer,c(1),c(4),c(5),c(6),c(7))

#형변환, 결측치 처리, 단위정리
ttl_beer$importtariff <- as.numeric(ttl_beer$importtariff)
ttl_beer$importtariff <- ifelse(is.na(ttl_beer$importtariff),0,ttl_beer$importtariff)
ttl_beer$ttlbeer <- ttl_beer$ttlbeer / 1000 

#산점도 확인
ggplot(data = ttl_beer, aes(x=ttlbeer, y=nation))+
  labs(x="국가 전체 맥주 소비량(킬로리터/년)",y="국가") +
  geom_point() +
  scale_x_continuous(labels = scales::comma)

ggplot(data = ttl_beer, aes(x=beerpercapita, y=nation))+
  labs(x="1인당 맥주 소비량(리터/년)",y="국가")+
  geom_point()

ggplot(data = ttl_beer, aes(x=beerimport, y=nation))+
  labs(x="맥주수입량($/년)",y="국가")+
  geom_point() +
  scale_x_continuous(labels = scales::comma)

ggplot(data = ttl_beer, aes(x=importtariff, y=nation))+
  labs(x="수입관세율(0~1.0)",y="국가")+
  geom_point()+
  scale_x_continuous(limits = c(0, 1))

#항목 간 백분위 수 확인
quantile(ttl_beer$ttlbeer,probs = c(0.2,0.4,0.6,0.8,1.0))
quantile(ttl_beer$beerpercapita,probs = c(0.2,0.4,0.6,0.8,1.0))
quantile(ttl_beer$beerimport,probs = c(0.2,0.4,0.6,0.8,1.0))
quantile(ttl_beer$importtariff,probs = c(0.2,0.4,0.6,0.8,1.0))

#분위별로 가중치 부여
ttl_beer$ttlconsumppoint <- ifelse(ttl_beer$ttlbeer <= quantile(ttl_beer$ttlbeer,probs = c(0.2)),1,
                                   ifelse(ttl_beer$ttlbeer <= quantile(ttl_beer$ttlbeer,probs = c(0.4)),2,
                                          ifelse(ttl_beer$ttlbeer <= quantile(ttl_beer$ttlbeer,probs = c(0.6)),3,
                                                 ifelse(ttl_beer$ttlbeer <= quantile(ttl_beer$ttlbeer,probs = c(0.8)),4,
                                                        ifelse(ttl_beer$ttlbeer <= quantile(ttl_beer$ttlbeer,probs = c(1.0)),5,'ER'
                                                 )))))
                                          
ttl_beer$percapoint <- ifelse(ttl_beer$beerpercapita <= quantile(ttl_beer$beerpercapita,probs = c(0.2)),1,
                                   ifelse(ttl_beer$beerpercapita <= quantile(ttl_beer$beerpercapita,probs = c(0.4)),2,
                                          ifelse(ttl_beer$beerpercapita <= quantile(ttl_beer$beerpercapita,probs = c(0.6)),3,
                                                 ifelse(ttl_beer$beerpercapita <= quantile(ttl_beer$beerpercapita,probs = c(0.8)),4,
                                                        ifelse(ttl_beer$beerpercapita <= quantile(ttl_beer$beerpercapita,probs = c(1.0)),5,'ER'
                                                        )))))

ttl_beer$importpoint <- ifelse(ttl_beer$beerimport <= quantile(ttl_beer$beerimport,probs = c(0.2)),1,
                                   ifelse(ttl_beer$beerimport <= quantile(ttl_beer$beerimport,probs = c(0.4)),2,
                                          ifelse(ttl_beer$beerimport <= quantile(ttl_beer$beerimport,probs = c(0.6)),3,
                                                 ifelse(ttl_beer$beerimport <= quantile(ttl_beer$beerimport,probs = c(0.8)),4,
                                                        ifelse(ttl_beer$beerimport <= quantile(ttl_beer$beerimport,probs = c(1.0)),5,'ER'
                                                        )))))

ttl_beer$tariffpoint <- ifelse(ttl_beer$importtariff <= quantile(ttl_beer$importtariff,probs = c(0.2)),5,
                                   ifelse(ttl_beer$importtariff <= quantile(ttl_beer$importtariff,probs = c(0.4)),4,
                                          ifelse(ttl_beer$importtariff <= quantile(ttl_beer$importtariff,probs = c(0.6)),3,
                                                 ifelse(ttl_beer$importtariff <= quantile(ttl_beer$importtariff,probs = c(0.8)),2,
                                                        ifelse(ttl_beer$importtariff <= quantile(ttl_beer$importtariff,probs = c(1.0)),1,'ER'
                                                        )))))
#최종 점수확인
beer_point <- ttl_beer[c(1,6:9)]

beer_point$ttl_point <- rowSums(beer_point[c(2:5)])
beer_point <- arrange(beer_point,desc(ttl_point))

ggplot(data = beer_point,aes(x=ttl_point,y=reorder(nation,ttl_point)))+
  labs(x="총 점수(포인트)",y="국가")+
  geom_bar(stat = "identity")




#뷰로 top6 수치확인
top6 <- ttl_beer %>%
filter(nation == 'United States of America' | nation == 'Germany' | nation == 'United Kingdom' | nation == 'Spain' | nation == 'Poland' | nation == 'China')
View(top6)

