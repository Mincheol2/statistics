## 기초의학통계
## 2018/11/16

################
# Data 준비
################
# excel 파일을 읽어주는 readxl 라이브러리 설치 필요
# install.packages('readxl')
# 실습용 서버에는 이미 설치되어 있음

# readxl 라이브러리 사용을 R에게 통보
library(readxl)

# Data 읽어오기 & pre-processing
data.pop <- "~/data/seoul/서울시 주민등록인구 (구별).xls"
df.pop <- read_excel(data.pop, skip=2)
head(df.pop)
df.pop <- df.pop[2:nrow(df.pop), c(2,4)]
head(df.pop)
colnames(df.pop) <- c("자치구", "인구")
head(df.pop)

df.doc <- read_excel("~/data/seoul/서울시_의료기관종사_의료인력_(구별).xls")
head(df.doc)
df.doc <- df.doc[2:nrow(df.doc), c(2, 4:7, 9:10)]
head(df.pop)
colnames(df.doc)[1] <- "자치구"
head(df.pop)

df <- merge(df.pop, df.doc)
head(df)
rownames(df) <- df$자치구
head(df.pop)
df$자치구 <- NULL
head(df)

##################
# Covariance
##################
cov(df)

attach(df)
cov(인구,의사)
cov(인구,치과의사)
detach(df)

df, method='spearman') # Spearman's

###################
# Correlation
###################
cor(df) # 모든 pair의 Pearson's correlation 보여줘
cor(df, method='spearman') # 모든 pair의 Spearman's correlation 보여줘
cor(df, method='kendall') # 모든 pair의 Kendall's correlation 보여줘

cor(df$인구, df$의사) # df의 인구 컬럼과 df의 의사 컬럼 사이의 Pearson's correlation 보여줘
cor(df$인구, df$의사, method='spearman') # df의 인구 컬럼과 df의 의사 컬럼 사이의 Spearman's correlation 보여줘
cor(df$인구, df$의사, method='kendall') # df의 인구 컬럼과 df의 의사 컬럼 사이의 Kendall's correlation 보여줘
cor.test(df$인구, df$의사, method='kendall') # df의 인구 컬럼과 df의 의사 컬럼 사이를 좀 더 자세히 보여줘

attach(df) # 당분간 명시적으로 df$ 라고 안 써도 df라고 이해해 줘
cor(인구,의사)
cor(인구,치과의사)
cor(의사,치과의사)
cor.test(의사,치과의사)
plot(의사,치과의사)
detach(df) # df$ 라고 안 해도 df 라고 알아듣는 rule 폐기. 명시적으로 안 쓰면 에러 뱉기 바람.

# dataframe을 명시하는 또 다른 방법: with(dataframe 이름, 명령문)
with(df, cor(의사,간호사))
with(df, cor.test(의사,간호사))

##########################
# Simple Linear Regression
##########################
lm(의사 ~ 인구, data=df) # 데이타와 ax + b 선 사이 오차가 최소가 되는 a,b를 구해줘
summary(lm(의사 ~ 인구, data=df)) # linear regression 결과 요약 해줘

attach(df) # 당분간 명시적으로 df$ 라고 안 써도 df라고 이해해 줘
out <- lm(의사 ~ 인구) # 데이타와 ax + b 선 사이 오차가 최소가 되는 a,b를 구해서 out에 넣어줘
summary(out) # linear regression 결과(out)을 요약 해줘
detach(df) # df$ 라고 안 해도 df 라고 알아듣는 rule 폐기. 명시적으로 안 쓰면 에러 뱉기 바람.

plot(의사 ~ 인구, data=df, col='blue')
out <- with(df, lm(의사 ~ 인구))
summary(out)
abline(out, col='red')

with(df, plot(의사 ~ 치과의사, col='blue'))
abline(lm(의사 ~ 치과의사, data=df), col='red')
