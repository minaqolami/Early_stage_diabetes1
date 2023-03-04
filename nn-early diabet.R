######Read Data
library(readxl)
data =read_excel("F:/Uni/Seminar/Early diabetic/early_stage_diabetes1.xlsx")
View(data)
head(data)
tail(data)
summary(data)
str(data)
attach(data)
#######Plots of attribute
hist(Age,main = "Age")
barplot(table(Gender),main = "Gender")
barplot(table(Polyuria),main = "polyuria",col="18")
barplot(table(Polydipsia),main = "Polydips",col="18")
barplot(table(SuddenWeightLoss),main = "SuddenWeightLoss",col="18")
barplot(table(Weakness),main = "Weakness",col="18")
barplot(table(GenitalThrush),main = "GenitalThrush",col="18")
barplot(table(VisualBlurring),main = "VisualBlurring",col="18")
barplot(table(Itching),main = "Itching",col="18")
barplot(table(Irritability),main = "Irritability",col="18")
barplot(table(PartialParesis),main = "PartialParesi",col="18")
barplot(table(Alopecia),main = "Alopecia",col="18")
barplot(table(Obesity ),main = "Obesity",col="18")
barplot(table(Class ),main = "Class")
###### Handling Categorical Data 
#Gender
Gender[Gender=="Male"]=0
Gender[Gender=="Female"]=1
Gender=as.numeric(Gender)

#Polyuria
Polyuria[Polyuria=="Yes"]=0
Polyuria[Polyuria=="No"]=1
Polyuria=as.numeric(Polyuria)

#Polydipsia
Polydipsia[Polydipsia=="Yes"]=0
Polydipsia[Polydipsia=="No"]=1
Polydipsia=as.numeric(Polydipsia)

#SuddenWeightLoss
SuddenWeightLoss[SuddenWeightLoss=="Yes"]=0
SuddenWeightLoss[SuddenWeightLoss=="No"]=1
SuddenWeightLoss=as.numeric(SuddenWeightLoss)

#Weakness
Weakness[Weakness=="Yes"]=0
Weakness[Weakness=="No"]=1
Weakness=as.numeric(Weakness)

#Polyphagia
Polyphagia[Polyphagia=="Yes"]=0
Polyphagia[Polyphagia=="No"]=1
Polyphagia=as.numeric(Polyphagia)

#GenitalThrush
GenitalThrush[GenitalThrush=="Yes"]=0
GenitalThrush[GenitalThrush=="No"]=1
GenitalThrush=as.numeric(GenitalThrush)

#VisualBlurring
VisualBlurring[VisualBlurring=="Yes"]=0
VisualBlurring[VisualBlurring=="No"]=1
VisualBlurring=as.numeric(VisualBlurring)

#Itching
Itching[Itching=="Yes"]=0
Itching[Itching=="No"]=1
Itching=as.numeric(Itching)

#Irritability
Irritability[Irritability=="Yes"]=0
Irritability[Irritability=="No"]=1
Irritability=as.numeric(Irritability)

#DelayedHealing
DelayedHealing[DelayedHealing=="Yes"]=0
DelayedHealing[DelayedHealing=="No"]=1
DelayedHealing=as.numeric(DelayedHealing)

#PartialParesis
PartialParesis[PartialParesis=="Yes"]=0
PartialParesis[PartialParesis=="No"]=1
PartialParesis=as.numeric(PartialParesis)

#MuscleStiffness
MuscleStiffness[MuscleStiffness=="Yes"]=0
MuscleStiffness[MuscleStiffness=="No"]=1
MuscleStiffness=as.numeric(MuscleStiffness)

#Alopecia
Alopecia[Alopecia=="Yes"]=0
Alopecia[Alopecia=="No"]=1
Alopecia=as.numeric(Alopecia)

#Obesity
Obesity[Obesity=="Yes"]=0
Obesity[Obesity=="No"]=1
Obesity=as.numeric(Obesity)

#Class
Class[Class=="Positive"]=0
Class[Class=="Negative"]=1
Class=as.numeric(Class)

data1=cbind(Age,Gender,Polyuria,Polydipsia,SuddenWeightLoss,Weakness,Polyphagia,GenitalThrush,VisualBlurring,Itching,Irritability,DelayedHealing,PartialParesis,MuscleStiffness,Alopecia,Obesity,Class)
data1

####mising Value
table(is.na(data1))
##############duplicate
table(duplicated(data))

##cor plot matrix
c=cor(data1,method = c( "kendall"))
Corrplot
library("corrplot")
corrplot(c,method="color",cl.length=5)

v=cov(data1)
###############pca by cov
pca=princomp((covmat=v))
summary(pca,loading=TRUE)
plot(pca$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(pca$sdev^2), xlab = "Component number",
     ylab = "log(Component variance)", type="l",
     main = "Log(eigenvalue) diagram")

######################pca by cor
pca2=princomp((cormat=c))
summary(pca2,loading=TRUE)

plot(pca2$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(pca2$sdev^2), xlab = "Component number",
     ylab = "log(Component variance)", type="l",
     main = "Log(eigenvalue) diagram")


nrow(testNN )

#########creating training and test set
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data1 ) ), size = samplesize )
trainNN =as.data.frame(data1[index, ])
testNN = as.data.frame(data1[-index , ])
########NN Model
library(neuralnet)
set.seed(2)
NN = neuralnet( Class ~  Age +  Gender + Polyuria  + Polydipsia , trainNN, hidden = 2 ,algorithm ="backprop",threshold = 0.01, learningrate = 0.0001, rep=3 )
#####plot neural network
plot(NN)
summary(NN)
NN$weightsN
NN$result.matrix
predict_testNN = compute(NN, testNN)
nn.results = compute(NN, testNN)
results = data.frame(actual = testNN$Class, prediction = nn.results$net.result)
roundedresults=sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(roundedresultsdf$actual,roundedresultsdf$prediction)
#########confusion_Matrix
confusion_table = table(roundedresultsdf$actual,roundedresultsdf$prediction)
TN=as.numeric(confusion_table[1,1])
FN=as.numeric(confusion_table[1,2])
FP=as.numeric(confusion_table[2,1])
TP=as.numeric(confusion_table[2,2])
confusion_table
accuracy =((TP + TN) / sum(TP,FP,TN,FN))
classification_error_rate = (FP + FN) / sum(TP,FP,TN,FN)
precision = (TP / (TP + FP))
sensitivity =(TP / (TP + FN))
specificity = (TN / (TN + FP))
length(roundedresultsdf$actual)
barplot(table(actual,prediction))
#########Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((actual -prediction )^2) / nrow(testNN )) ^ 0.5






