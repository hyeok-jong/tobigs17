##############################################################
## [투빅스 17기 정규세션] 5주차 Time-Series 과제 - 17기 이혁 ##
##############################################################


# 패키지 설치
#install.packages("forecast")
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
diff12_Y.forecasts <- forecast(fit2, h = 17)
diff12_Y.forecasts
plot(diff12_Y.forecasts)

# > diff12_Y.forecasts
# Point Forecast       Lo 80       Hi 80       Lo 95      Hi 95
# 43     -11.222145 -31.6913432   9.2470541  -42.527082  20.082793
# 44      19.889239  -0.5804468  40.3589251  -11.416444  51.194922
# 45     -28.974325 -49.5337020  -8.4149481  -60.417179   2.468529
# 46      11.859851  -9.7027760  33.4224775  -21.117341  44.837043
# 47      -3.169953 -30.0143600  23.6744531  -44.224930  37.885024
# 48      49.640299  22.7771825  76.5034162    8.556707  90.723891
# 49     -26.077526 -52.9559340   0.8008826  -67.184504  15.029452
# 50     -10.169660 -38.9109288  18.5716081  -54.125637  33.786317
# 51      14.597723 -16.5927865  45.7882319  -33.104045  62.299490
# 52       4.521725 -26.7284249  35.7718741  -43.271255  52.314704
# 53      24.830662  -6.4508458  56.1121699  -23.010276  72.671600
# 54     -63.378197 -96.8365436 -29.9198497 -114.548323 -12.208071
# 55     -21.820597 -56.6397490  12.9985544  -75.071895  31.430700
# 56      30.484283  -4.3970659  65.3656314  -22.862136  83.830702
# 57      33.682150  -1.4213213  68.7856219  -20.003977  87.368277
# 58     -33.236065 -70.4696523   3.9975217  -90.179923  23.707792
# 59      -3.891713 -41.6806479  33.8972224  -61.684902  53.901476
