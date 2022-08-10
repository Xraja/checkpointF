Cleandata <- read.csv("C:/Users/H.A/Downloads/CleanCreditScoring.csv",header = TRUE)
summary(Cleandata)
mean(Cleandata$Amount)
max(Cleandata$Amount)
min(Cleandata$Amount)
median(Cleandata$Amount)
var(Cleandata$Amount)
sd(Cleandata$Amount)

mean(Cleandata$Income)
max(Cleandata$Income)
min(Cleandata$Income)
median(Cleandata$Income)
var(Cleandata$Income)
sd(Cleandata$Income)

cor(Cleandata$Amount, Cleandata$Income)
#"cor=0.19082011 < 1 => Amount and Income are not on strong positive relationship"
linearMod <- lm(Cleandata$Amount ~ Cleandata$Income , Cleandata=Cleandata)
print(linearMod)

#  Amount = 1,13*Income+ 879,87

standdata <- select(Cleandata,Seniority,Time,Age,Expenses,Income,Assets,Debt,Amount,Price,Finrat,Savings)

x_standardized <- as.data.frame(scale(standdata))
#x_standardized

standdata.mat <- as.matrix(x_standardized)
cov.mat <- cor(x_standardized)

pca <- prcomp(x_standardized,center = T,scale. = T)
summary(pca)


library(ggfortify)
autoplot(pca, data = Cleandata, colour = 'Status')

gatdata <- select(Cleandata,Status,Home,Marital,Records,Job,seniorityR,timeR,ageR,expensesR,incomeR,assetsR,debtR,amountR,priceR,finratR,savingsR)

res.mca <- MCA(gatdata,
               quanti.sup = 1:2, # Supplementary quantitative variable
               quali.sup = 3:4,  # Supplementary qualitative variable
               graph=FALSE)

