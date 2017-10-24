# ROC 그래프 이용한 성능 평가

###### 예제 ######
# infert 자료에 대한 분류 분석 모형 평가 비교를 위하여 의사결정나무 모형은 R패키지 {50}dml
# C5.0() 함수를 사용하고 신경망 모형은 {neuralnet}의 neuralnet()함수를 사용한다. 
# 모형 학습 및 검증을 위하여 70%의 훈련용 자료와 30%의 검증용 자료로 구분한다. 

## Step.1 - set 나누기
set.seed(1234)
infert <- infert[sample(nrow(infert)),]  # Randomly shuffle the data
infert <- infert[,c("age", "parity", "induced","spontaneous","case")]
trainData <- infert[1:nrow(infert)*0.7, ]
testData <- infert[((nrow(infert)*0.7)+1):nrow(infert), ]

## Step.2 - 알고리즘 적용
# 학습용 데이터에서 다층신경망으로 역전파알고리즘을 활용한 신경망모형을 생성한다.
library(neuralnet)  # neural network
net.infert <- neuralnet(case ~ age+parity+induced+spontaneous, data=trainData, hidden=3,
                        err.fct = "ce", linear.output = FALSE, likelihood = TRUE)
n_test <- subset(testData, select = -case)
nn_pred <- compute(net.infert, n_test)
testData$net_pred <- nn_pred$net.result
head(testData)
plot(net.infert)

# 학습용데이터에서 c5.0을 활용한 모형을 생성한다. 
library(C50)  # decision tree
trainData$case <- factor(trainData$case)
dt.infert <- C5.0(case ~ age+parity+induced+spontaneous, data=trainData)
testData$dt_pred <- predict(dt.infert, testData, type="prob")[,2]
head(testData)

## Step.3 ROC 그래프
library(Epi) 

par(mfrow=c(1,2))
# ROC 1 - neural network
neural_ROC <- ROC(form = case~net_pred, data = testData, plot = "ROC")
# ROC 2 - decision tree
dtree_ROC <- ROC(form = case~dt_pred, data = testData, plot = "ROC")

# neural network의 AUC는 0.723, decision tree의 AUC는 0.661이므로 
# neural network 알고리즘 기반의 모형의 성능이 더 좋다.



#분류기준값cutoff이 클수록 민감도와 1-특이도가 모두 작아지고
#분류기준값cutoff이 작을수록 민감도와 1-특이도가 모두 증가한다. 
#븐류기준값에 따라 민감도의 변화를 관찰하여 분류자의 성능을 평가한다. 
#1로 분류된 것 같운데 민감도 = (1->1) /(1의 개수), 즉 1에서 정분류 된 비율이며 
#1-xmrdleh = (0->1) /(0의 개수) 즉, 0에서 오분류된 비율이다






