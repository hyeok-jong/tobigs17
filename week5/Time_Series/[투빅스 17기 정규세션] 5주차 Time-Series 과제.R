##############################################################
## [투빅스 17기 정규세션] 5주차 Time-Series 과제 - 17기 OOO ##
##############################################################


# 패키지 설치
install.packages("forecast")
library(forecast)


# 경로 설정
# data가 있는 directory
path = "D:/git/tobigs_personal/220223_session_5/Time-Series/[투빅스 17기 정규세션] 5주차 Time-Series_16기 이예림/과제"
setwd(path)
print(getwd())


# 데이터 불러오기
data <- read.csv("kingage.csv")
class(data)
head(data)
nrow(data)
# data.frame 형식의 총 42개의 정수(age)로 이루어진 data입니다.

Y <- ts(data) # to timeseries
is.ts(Y)      # True
plot.ts(Y)    # 정상성을 만족하지 않아보입니다.


# 따라서 차분을 통하여 정상시계열로 바꿔줍니다.
# 차분을 1회만 진행하고 간격을 1과 12 두가지를 사용합니다.
diff1_Y <- diff(Y, differences = 1, lag = 1)
plot.ts(diff1_Y)

diff12_Y <- diff(diff1_Y, differences = 1, lag = 12)
plot.ts(diff12_Y)



          

# 1. 모형 식별
# acf와 pacf를 확인하여  ARIMA모형의 상수를 정합니다. 
par(mfrow=c(2,1))     # 한번에 확인하기 위함.
acf(diff12_Y, main = "ACF")
pacf(diff12_Y, main = "PACF")


# 2. 모수 추정
fit1 <- arima(diff12_Y, c(3, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12))
fit2 <- arima(diff12_Y, c(3, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
fit3 <- arima(diff12_Y, c(2, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
fit1
fit2
fit3


# AIC가 작으면 좋다.


# > fit1
# 
# Call:
#   arima(x = diff12_Y, order = c(3, 1, 1), seasonal = list(order = c(1, 1, 0), 
#                                                           period = 12))
# 
# Coefficients:
#   ar1      ar2      ar3      ma1     sar1
# -0.6570  -0.7991  -0.5166  -0.9997  -0.9056
# s.e.   0.2147   0.2630   0.2386   0.4606   0.1045
# 
# sigma^2 estimated as 272.2:  log likelihood = -80.71,  aic = 173.41



# > fit2
# 
# Call:
#   arima(x = diff12_Y, order = c(3, 1, 0), seasonal = list(order = c(1, 1, 0), 
#                                                           period = 12))
# 
# Coefficients:
#   ar1      ar2      ar3     sar1
# -0.9931  -1.0869  -0.7680  -0.9512
# s.e.   0.1888   0.1828   0.1557   0.0652
# 
# sigma^2 estimated as 255.1:  log likelihood = -82.69,  aic = 175.38



# > fit3
# 
# Call:
#   arima(x = diff12_Y, order = c(2, 1, 0), seasonal = list(order = c(1, 1, 0),
#                                                           period = 12))
# 
# Coefficients:
#   ar1      ar2     sar1
# -0.7047  -0.5740  -0.8999
# s.e.   0.2332   0.3036   0.1074
# 
# sigma^2 estimated as 841:  log likelihood = -86.99,  aic = 181.98









# 3. 모형 적합성 진단
tsdiag(fit1)


# 4. 모형 확정 및 예측 (예측값 17개 출력하기)
diff12_Y.forecasts <- forecast(fit2, h = 12)
diff12_Y.forecasts
plot(diff12_Y.forecasts)

