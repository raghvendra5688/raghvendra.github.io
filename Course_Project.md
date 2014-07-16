Course Project: Practical Machine Learning
========================================================


```r
library('caret');
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library('kernlab');
# Convert all the #DIV/0! into NAs in the data
df <- read.delim("pml-training.csv",na.strings=c("NA","#DIV/0!"),sep=",");

# Try to identify columns with all NAs
variables <- names(df);
output <- sapply(df,function(x) all(is.na(x)));
dump_variables <- names(which(output));
newdf <- df[,!(variables %in% dump_variables)];
remove(df);

# Remove variables which are unnecessary based on less variation in the data (from observation) or categorical data
newvariables <- names(newdf);
unnecessary_variables <- c(newvariables[1:6],newvariables[23],newvariables[96],newvariables[132],newvariables[154]);
finaldf <- data.frame(newdf[,c("user_name","classe")]);
moddf <- newdf[,!(newvariables %in% unnecessary_variables)];
remove(newdf);

# Replace missing values with the median values in columns containing missing values
for (i in 1:144) {
  if (any(is.na(moddf[[i]]))==TRUE)
	{
		val <- median(moddf[[i]],na.rm=TRUE);
        moddf[[i]][is.na(moddf[[i]])] <- val;
	}
}
median_df <- sapply(moddf,median);
finaldf <- cbind(finaldf,moddf);
remove(moddf);

# Build the model using random-forest as it works well in case of multi-class classification and can handle some non-linearity in the data. 

# Perform 10-fold cross validation to get an expectation of out-of-sample error
library('randomForest');
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
set.seed('1234');
mod_rf <- train(classe~.,method="rf",data=finaldf,trControl=trainControl(method="cv",number=10));
results <- mod_rf$results

#Estimate the best value of tuning parameter w.r.t. to best Cross-validation accuracy 
parameter_value <- results$mtry[results$Accuracy==max(results$Accuracy)];

#Build the random forest model corresponding to the parameter which gave the maximum accuracy
mod_oob <- train(classe~.,method="rf",data=finaldf,tuneGrid=data.frame(.mtry=parameter_value),trControl=trainControl(method="oob"));

#Use the 10-fold cross validation accuracy of this model as an estimation of out-of-sample accuracy 
oob_results<-mod_oob$results;
oob_results$Accuracy;
```

```
## [1] 0.9987
```

```r
#Obtain the test data and pre-process it in same manner as training data
test_df <- read.delim("pml-testing.csv",na.strings=c("NA","#DIV/0!"),sep=",");
test_variables <- names(test_df);
newtest_df <- test_df[,!(variables %in% dump_variables)];
remove(test_df);
new_testvariables <- names(newtest_df);

#Remove unnecessary test variables same as that in train data
invalid_variables <- c(new_testvariables[1:6],new_testvariables[23],new_testvariables[96],new_testvariables[132],new_testvariables[154]);
user_name <- newtest_df[,"user_name"];
modtest_df <- newtest_df[,!(new_testvariables %in% invalid_variables)];
remove(newtest_df);

#Replace missing values in columns with median values as estimated from the train data
for (i in 1:144){
    if (any(is.na(modtest_df[[i]]))==TRUE)
    {
        val <- median_df[[i]];
        modtest_df[[i]][is.na(modtest_df[[i]])] <- val;
    }
}
finaltest_df <- cbind(user_name,modtest_df);
remove(modtest_df);

#Use the best training model on the test data
pred_final <- predict(mod_oob,finaltest_df);

#Obtain the results of classification
answers <- as.character(pred_final);

#Writing the output files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(answers);
```




