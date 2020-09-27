library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

#탐색적 데이터 분석
#타겟시장인 미국시장과 한국시장의 비교 시각화

us_rank <- read_xlsx(file.choose())
tail(us_rank)

#필요행 추출
us_ranking <- us_rank[c(1:25),]

#필요열 추출
us_ranking <- select(us_ranking,c(2:6),c(9))

#형변환
us_ranking$ABV <- as.numeric(us_ranking$ABV)

#미국시장의 TOP25 확인하기
sum(us_ranking$Market_Share) # Top25가 전체 맥주시장의 70퍼센트를 차지
ggplot(data = us_ranking,aes(x = Market_Share, y = reorder(Name,Market_Share))) +
  labs(x= "미국 시장 내 점유율(0~1)", y = "맥주")+
  geom_bar(stat = "identity") 

# 미국 선호맥주 ABV, IBU, SRM 확인
summary(us_ranking) #Top25의 ABV 평균은 4.894, IBU는 12.42, SRM은 4.958이다

ggplot(us_ranking,aes(x=Name,y=ABV,fill=ABV))+
  geom_bar(stat="identity",position = "dodge")+
  coord_flip()

ggplot(us_ranking,aes(x=Name,y=IBU,fill=IBU))+
  geom_bar(stat="identity",position = "dodge")+
  coord_flip()

ggplot(us_ranking,aes(x=Name,y=SRM,fill=SRM))+
  geom_bar(stat="identity",position = "dodge")+
  coord_flip()

# 맥주의 스타일에 따라 색,도수,쓴맛은 달라질 수 있기때문에 Style에 따른 특징을 더 알아 봐야함
# 미국 선호맥주 스타일 별 누적 시장점유율
ggplot(us_ranking,aes(x=Market_Share, y = Style, group=Style))+
  labs(x= "누적 시장점유율", y = "맥주 스타일")+
  geom_bar(stat = "identity")                                   # 미국사람들은 가벼운 American-Style Light Lager를 많이 먹는다

# American Light Lager의 특징(ABV,IBU,SRM)
ggplot(us_ranking,aes(x=Style, y = ABV))+
  geom_bar(stat = "identity")+
  coord_flip()

ggplot(us_ranking,aes(x=Style, y = IBU))+
  geom_bar(stat = "identity")+
  coord_flip()

ggplot(us_ranking,aes(x=Style, y = SRM))+
  geom_bar(stat = "identity")+
  coord_flip()

ASLL <- subset(us_ranking,Style == "American-Style Light Lager")
summary(ASLL) # 아메리칸스타일 라이트 라거의 ABV평균은 4.43, IBU의 평균은 8.4, SRM의 평균은 2.575


#한국데이터셋 로드
kobeer <- read_xlsx(file.choose())
View(kobeer)

#결측 행 제거, 필요 열만 추출
ko_beer <- kobeer[!is.na(kobeer$제조사),]
ko_beer <- select(ko_beer,c(1:5),c(8))


# 국내시장에서 특정 브랜드에 편중되지않고 타겟시장의 특징을 기준으로 가장 적합한 상품을 찾기 위해 수제+대기업 맥주 브랜드들을 골고루 초이스
# 제조사 별 상품 개수
ggplot(data= ko_beer, aes(x=제조사, group = 제조사))+
  geom_bar()+
  coord_flip()

# 한국 맥주 스타일 별 ABV, IBU, SRM 확인
ggplot(data= ko_beer, aes(x=ABV, y=Style))+
  geom_bar(stat = "identity", position = "dodge")

ggplot(data= ko_beer, aes(x=IBU, y=Style))+
  geom_bar(stat = "identity", position = "dodge")

ggplot(data = ko_beer, aes(x=SRM, y=Style))+
  geom_bar(stat = "identity", position = "dodge")


#한국 맥주 중 미국에서 가장 선호되는 스타일(아메리칸 라이트 라거)의 특징과 가장 가까운거 찾기

#American Light Lager 평균 변수 담기
us_mean <- c(mean(ASLL$ABV),mean(ASLL$IBU),mean(ASLL$SRM))
us_mean[1]

#미국맥주와 한국맥주의 ABS,IBU,SRM 차이가 최소값인 인덱스를 찾아 추적 
ko_beer[which.min(abs(ko_beer$ABV - us_mean[1])),]
ko_beer[which.min(abs(ko_beer$IBU - us_mean[2])),]
ko_beer[which.min(abs(ko_beer$SRM - us_mean[3])),]

#하지만 각각의 ABV,IBU,SRM차이가 최소값인 맥주는 여러개인데 반해 인덱스는 하나씩만 출력
#하여 한국맥주의 ABV,IBU,SRM의 합계를 새로운 변수로 담아 미국맥주의 값과 비교
ko_beer$sum <- ko_beer$ABV + ko_beer$IBU + ko_beer$SRM
ko_beer[which.min(abs(ko_beer$sum - sum(us_mean))),] 

#결론적으로 ABV,IBU,SRM을 가지고 비교를 하였을때,
#미국시장 내 선호 맥주들의 평균적인 특징과 가장 비슷한 국내 맥주는 CASS LIGHT이다

