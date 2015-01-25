training = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")
library(caret)

# remove colums (predicators) with NAs
training = training[,colSums(is.na(training[,])) == 0]

#remove columns with low variability
training = training[,-nearZeroVar(training)]

#remove X variable, which doesn't provide much assistance in prediction
training$X=NULL

#remove timestamp variables which don't provide assistance in predicting classe
training$raw_timestamp_part_1=NULL
training$raw_timestamp_part_2=NULL
training$cvtd_timestamp=NULL

#remove USERid
training$user_name=NULL

#build model with principal components analysis as preprocessing method
model = train(classe~.,preProcess="pca",method="rf",data=training)

#predict with new data
prediction = predict(model,newdata=testing)

#write submission files
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(prediction)