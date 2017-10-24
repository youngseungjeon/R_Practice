
### 모형의 평가 ###

## 1. 홀드 아웃 방법

#-1. 데이터 불러오기
data(iris)
nrow(iris)
set.seed(1234)

#-2. train, test set 나누기
idx <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7,0.3))
trainData <- iris[idx==1,]
testData <- iris[idx==2,]

#-3. 확인하기
nrow(trainData)
nrow(testData)


## 2. 교차 검증 방법 ##
data(iris)
set.seed(1234)
k=10 #10-fold corss validtion

iris <- iris[sample(nrow(iris)),] # Randomly shuffle the data
folds <- cut(seq(1,nrow(iris)), breaks=k, labels=FALSE)
trainData = list(0) # an empty list of length k
testData =list(0)

for (i in 1:k) { # Perform 10 fold cross validation
  testIdx <- which(folds==i, arr.ind=TRUE)
  testData[[i]] <- iris[testIdx,]
  trainData[[i]] <- iris[-testIdx,]
}

head(trainData[[1]])
head(trainData[[2]])



## 3. 오분류표를 사용한 검증 ##
# iris 자료에 대해 범주가 2개인 분류 모형을 구축하기 위해 iris 자료의 일부분 이용
# Species가 setosa와 versicolor인 100개의 자료만을 이요하며 70%의 훈련용 자료를 추출한다.

iris <- subset(iris, Species == 'setosa' | Species =="versicolor")
iris$Species <- factor(iris$Species)
set.seed(1234)
iris <- iris[sample(nrow(iris)),] # Randomly shuffle the data
trainData <- iris[1:(nrow(iris)*0.7),]
testData <- iris[((nrow(iris)*0.7)+1):nrow(iris),] #71번째 부터 끝까지 조건 설정
nrow(trainData)

## 신경망 알고리즘, 의사결정 나무 알고리즘 성능 비교
## 신경망 알고리즘과, 의사결정 나무 알고리즘을 사용해서 데이터를 훈련시켜 예측모형의 성능을 비교
library(nnet)  # 신경망
library(rpart) # 의사결정 나무

# -1. 모형 실행 - 신경망, 의사결정나무

# 신경망
nn.iris <- nnet(Species~., data=trainData, size=2, rang=0.1, decay=5e-4, maxit=200)
# decay 옵션은 overfitting(과학습) 예방을 위해, 망가중치 학습을 감소키도록 설정
# size 옵션은 hidden layer의 노드 개수 
# rang : 망가중치에 랜덤값으로 기본값이 설정될 때, 랜덤값이 특정 범위 내에서 발생하도록 유도
# maxit 옵션은 학습 최대 반복횟수를 설정

#의사결정나무
dt.iris <- rpart(Species~., data=trainData)


# -2. 예측
nn_pred <- predict(nn.iris, testData, type="class")
dt_pred <- predict(dt.iris, testData, type="class")

# -3, 오분류표 도출
library(caret)
library(e1071)
nn_con = confusionMatrix(nn_pred, testData$Species)
dt_con = confusionMatrix(dt_pred, testData$Species)
nn_con$table
dt_con$table

accuracy <- c(nn_con$overall['Accuracy'], dt_con$overall['Accuracy'])
precision <- c(nn_con$byClass['Pos Pred Value'], dt_con$byClass['Pos Pred Value'])
recall <- c(nn_con$byClass['Sensitivity'],dt_con$byClass['Sensitivity'])
f1 <- 2 * ((precision * recall) / (precision + recall))
result <- data.frame(rbind(accuracy, precision, recall, f1))
names(result) <- c("Nueral Network", "Decision Tree")
result
