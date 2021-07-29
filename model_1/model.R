library("caret")
df <- read.csv(file.choose())

split<-createDataPartition(y = df$sqrt.dd., p = 0.7, list = FALSE)
train_ds<- df[split,]
test_ds<- df[-split,]
library(corrplot)
featurePlot(x=train_ds[,2:14],y=train_ds$sqrt.dd.)
featurePlot(x=test_ds[,2:14],y=test_ds$sqrt.dd.)



trc= trainControl(method="repeatedcv",number=10,repeats=5)
model_lt = train(sqrt.dd.~.,train_ds,method="lm",trControl=trc)
summary(model_lt)
model_lt$finalModel

model_lt$resample
model_lt$results
# diagnostic plots
plot(model_lt$finalModel)

# custom plots to test fit
obs=predict(model_lt, test_ds)
plot(test_ds$sqrt.dd.,obs)
abline(0,1)
p_dd = predict(model_lt,test_ds)

plot(p_dd)

