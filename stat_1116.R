## 기초의학통계
## 2018/11/16

################
# Data 준비
################

# install.packages('readxl') # readxl package를 설치해야 한다면 실행
library(readxl)

data.pop <- "~/data/seoul/서울시 주민등록인구 (구별).xls"
df.pop <- read_excel(data.pop, skip=2)
head(df.pop)
df.pop <- df.pop[2:nrow(df.pop), c(2,4)]
colnames(df.pop) <- c("자치구", "인구")

df.doc <- read_excel("~/data/seoul/서울시_의료기관종사_의료인력_(구별).xls")
head(df.doc)
df.doc <- df.doc[2:nrow(df.doc), c(2, 4:7, 9:10)]
colnames(df.doc)[1] <- "자치구"

df <- merge(df.pop, df.doc)
head(df)
rownames(df) <- df$자치구
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
attach(df)
cor(인구,의사)
cor(인구,치과의사)
cor(의사,치과의사)
plot(의사,치과의사)
detach(df)

with(df, cor(의사,간호사))
with(df, cor.test(의사,간호사))

##########################
# Simple Linear Regression
##########################
lm(의사 ~ 인구, data=df)

attach(df)
lm(의사 ~ 인구)
detach(df)

out <- with(df, lm(의사 ~ 인구))
summary(out)
plot(의사 ~ 인구, data=df, col='blue')
abline(out, col='red')

out <- with(df, lm(의사 ~ 치과의사))
summary(out)
plot(의사 ~ 치과의사, data=df, col='blue')
abline(out, col='red')

