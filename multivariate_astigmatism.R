
# load continuous univariate astigmatism
d.left <- read('left_astigmatism-univariate-continuous.csv')
left.cont.covariates <- d.left$var[ which(d.left$pvalue>0)]
# ignore all covariates which are more than 95% misisng
left.cont.covariates <- left.cont.covariates[sapply(left.cont.covariates, function(x) length(which(is.na(d[,x])))/length(d[,x]))<.95]
# remove right when dealing with left
left.cont.covariates <- left.cont.covariates[-grep('right', dict[left.cont.covariates,'Description'])]
# only keep initial measurements
left.cont.covariates <- grep('.0.0$',left.cont.covariates,value=TRUE)
#
cor(d[,left.cont.covariates])


dict[which(dict$Description== 'Weight'),]

dict[grep('Standing height',dict$Description),]

dict[which('Body fat percentage'==dict$Description),]

dict[which('Basal metabolic rate'==dict$Description),]


cor(na.omit(d[,c( 'f.23098.0.0', 'f.23099.0.0', 'f.23105.0.0')]))


 "3mm asymmetry index (left)"
 "3mm regularity index (left)"
 "logMAR, final (left)"
 "logMAR, initial (left)"
 "logMAR in round (left)"
 "6mm strong meridian angle (left)"
 "6mm asymmetry index (left)"
 
 "Age when attended assessment centre"
 "Age at recruitment"
 "Year of birth"

 "3mm weak meridian (left)"
 "Intra-ocular pressure, corneal-compensated (left)"

 "3mm asymmetry angle (left)"

s <- summary(lm(as.formula(paste('log(left_astigmatism) ~', paste(xx,collapse=' + '))),data=d))
# remove measurements with NA coefficient
print(setdiff(left.cont.covariates, names((s$left.cont.coefficients[,1]))))
left.cont.covariates <- intersect(covariates,names((s$left.cont.coefficients[,1])))
#






 "Intra-ocular pressure, Goldmann-correlated (left)"

 "6mm asymmetry angle (left)"

 "Average weekly beer plus cider intake Uses data-coding 100291"

 "Time spend outdoors in summer Uses data-coding 100329"
 "Systolic blood pressure, automated reading"
 "Average Y chromosome intensities for determining sex"
 "Corneal hysteresis (left)"
 "Forced expiratory volume in 1-second (FEV1), Best measure"
 "Astigmatism angle (left)"
 "3mm index of best keratometry results (left)"
 "Time spent outdoors in winter Uses data-coding 100329"
 "Age at cancer diagnosis"
 "6mm regularity index (left)"
 "Forced expiratory volume in 1-second (FEV1)"
 "Pulse wave peak to peak time"
 "6mm index of best keratometry results (left)"
 "Pulse wave Arterial Stiffness index"
 "Mean time to correctly identify matches"
 "Age high blood pressure diagnosed Uses data-coding 100291"
 "Diastolic blood pressure, automated reading"
 "6mm cylindrical power angle (left)"
 "Fluid intelligence score"
 "6mm weak meridian angle (left)"



imp <- rfImpute(y=d[1:n,'left_astigmatism'],x=d[1:n,covariates])


covariates <- c("f.21003.0.0", "f.34.0.0", "f.5096.0.0", "f.5292.0.0", "f.5089.0.0", "f.1050.0.0", "f.1060.0.0", "f.5206.0.0", "f.50.0.0", "f.4079.0.0", "f.4080.0.0", "f.4196.0.0", "f.5208.0.0", "f.21021.0.0", "f.20023.0.0", "f.5163.0.0", "f.5156.0.0", "f.5111.0.0", "f.23105.0.0", "f.23098.0.0",'age','gender','height','weight')
X <- na.omit(d[,c('left_astigmatism',covariates)])
(s <- summary(m <- lm(as.formula(paste('log(left_astigmatism) ~', paste(covariates,collapse=' + '))),data=X)))
res <- data.frame(description=dict[ rownames(s$coefficients),'Description'],s$coefficients)
print(res[order(res[,'Pr...t..']),])


library(missForest)

imp <- rfImpute(y=d[1:n,'left_astigmatism'],x=d[1:n,covariates])




# load catergorical univariate astigmatism
d.left <- read('left_astigmatism-univariate-categorical.csv')
covariates <- d.left$var[ which(d.left$pvalue>0)]
# ignore all covariates which are more than 95% misisng
covariates <- covariates[sapply(covariates, function(x) length(which(is.na(d[,x])))/length(d[,x]))<.95]
# remove right when dealing with left
covariates <- covariates[-grep('right', dict[covariates,'Description'])]
# only keep initial measurements
covariates <- grep('.0.0$',covariates,value=TRUE)
#
s <- summary(lm(as.formula(paste('log(left_astigmatism) ~', paste(covariates,collapse=' + '))),data=d))
# remove measurements with NA coefficient
#print(setdiff(covariates, names((s$coefficients[,1]))))
#covariates <- intersect(covariates,names((s$coefficients[,1])))
#
print(s <- summary(lm(as.formula(paste('log(left_astigmatism) ~', paste(covariates,collapse=' + '))),data=d)))



# load continuous univariate astigmatism
d.right <- read('right_astigmatism-univariate-continuous.csv')
covariates <- d.right$var[ which(d.right$pvalue>0)]
# ignore all covariates which are more than 95% misisng
covariates <- covariates[sapply(covariates, function(x) length(which(is.na(d[,x])))/length(d[,x]))<.95]
# remove left when dealing with right
covariates <- covariates[-grep('left', dict[covariates,'Description'])]
# only keep initial measurements
covariates <- grep('.0.0$',covariates,value=TRUE)
#
s <- summary(lm(as.formula(paste('log(right_astigmatism) ~', paste(covariates,collapse=' + '))),data=d))
# remove measurements with NA coefficient
print(setdiff(covariates, names((s$coefficients[,1]))))
covariates <- intersect(covariates,names((s$coefficients[,1])))
#
print(s <- summary(lm(as.formula(paste('log(right_astigmatism) ~', paste(covariates,collapse=' + '))),data=d)))


# load catergorical univariate astigmatism
d.right <- read('right_astigmatism-univariate-categorical.csv')
covariates <- d.right$var[ which(d.right$pvalue>0)]
# ignore all covariates which are more than 95% misisng
covariates <- covariates[sapply(covariates, function(x) length(which(is.na(d[,x])))/length(d[,x]))<.95]
# remove left when dealing with right
covariates <- covariates[-grep('left', dict[covariates,'Description'])]
# only keep initial measurements
covariates <- grep('.0.0$',covariates,value=TRUE)
#
s <- summary(lm(as.formula(paste('log(right_astigmatism) ~', paste(covariates,collapse=' + '))),data=d))
# remove measurements with NA coefficient
#print(setdiff(covariates, names((s$coefficients[,1]))))
#covariates <- intersect(covariates,names((s$coefficients[,1])))
#
print(s <- summary(lm(as.formula(paste('log(right_astigmatism) ~', paste(covariates,collapse=' + '))),data=d)))






covariates2 <- c()
for (x in covariates) {
    covariates2 <- c(covariates2,x)
    form <- as.formula(paste('log(left_astigmatism) ~', paste(covariates2,collapse=' + ')))
    print(s<-summary(lm(form, data=d)))
    which(is.na(s$coefficients))

}


form <- as.formula(paste('log(left_astigmatism) ~', paste(covariates3,collapse=' + ')))

s <- summary(m <- lm(form, data=d))


#library(randomForest)
#fit <- randomForest(form, data=d, importance=TRUE, ntree=2000,na.action=na.omit)



dict <- read('ukb6749.csv')

rownames(dict) <- dict$UDI


univariate <- function (y,x) {
    s<-summary(m <- lm( trans(d[,y]) ~ d[,x]))
    print(x)
    print(s$coefficients)
    return(data.frame( var=as.character(x), coeff=m$coefficients[[2]], lower.conf=confint(m)[2,1], upper.conf=confint(m)[2,2], pvalue=s$coefficients[,'Pr(>|t|)'][[2]]))
}

#library(parallel)
#no_cores <- detectCores() - 1
#cl <- makeCluster(no_cores)
#clusterExport(cl,'d')
#clusterExport(cl,'univariate')
#X <- do.call('rbind',parLapply(cl, colnames(d), function(x) {univariate('astigmatism',x)}))

# continuous variables

continuous.factors <- dict[grep('Continuous|Integer',dict$Type),'UDI']
continuous.factors <- intersect(continuous.factors,colnames(d))
colVar <- function(x) apply(x,2,var,na.rm=TRUE)
d.cont<-d[,continuous.factors] 
d.cont <- d.cont[,-which(colVar(d.cont)==0)]

# left astigmatism
X.cont.left <- do.call('rbind',lapply(colnames(d.cont), function(x) {univariate('left_astigmatism',x)}))
X.cont.left$var <- as.character(X.cont.left$var)
X.cont.left <- X.cont.left[which(X.cont.left$pvalue < bonferroni),] 
X.cont.left$Description <- dict[X.cont.left$var,'Description']
xx <- univariate('left_astigmatism','age')
xx$Description <- 'age'
X.cont.left <- rbind(X.cont.left,xx)
head(X.cont.left<-X.cont.left[order(abs(X.cont.left$pvalue)),])
write.csv(X.cont.left,file='left_astigmatism-univariate-continuous.csv',row.names=FALSE)


# right astigmatism
X.cont.right <- do.call('rbind',lapply(colnames(d.cont), function(x) {univariate('right_astigmatism',x)}))
X.cont.right$var <- as.character(X.cont.right$var)
X.cont.right <- X.cont.right[which(X.cont.right$pvalue < bonferroni),] 
X.cont.right$Description <- dict[X.cont.right$var,'Description']
xx <- univariate('right_astigmatism','age')
xx$Description <- 'age'
X.cont.right <- rbind(X.cont.right,xx)
head(X.cont.right<-X.cont.right[order(abs(X.cont.right$pvalue)),])
write.csv(X.cont.right,file='right_astigmatism-univariate-continuous.csv',row.names=FALSE)




X2 <- rbind(
univariate('left_astigmatism','age'),
univariate('left_astigmatism','gender'),
univariate('left_astigmatism','height'),
univariate('left_astigmatism','weight'),
univariate('left_astigmatism','iop'),
univariate('left_astigmatism','corrected_visual_acuity'),
univariate('left_astigmatism','crf'),
univariate('left_astigmatism','ethnicity')
)


# categorical variables

univariate.cat <- function (y,x) {
    print(x)
    s<-summary(aov(m <- lm( trans(d[,y]) ~ d[,x])))
    print(s)
    return(data.frame(var=as.character(x), pvalue=s[[1]]$`Pr(>F)`[[1]]))
}



categorical.factors <- dict[grep('Categorical',dict$Type),'UDI']
categorical.factors <- intersect(categorical.factors,colnames(d))
d.cat<-d[,categorical.factors] 
d.cat<-d.cat[,which(apply( d.cat,2,function(x) 1<length(unique(na.omit(x)))))]
d.cat <- d.cat[,-which(colSums(is.na(d.cat))>(nrow(d.cat)-10))]
categorical.factors <- intersect(categorical.factors,colnames(d.cat))

#X.cat <- do.call('rbind',lapply(categorical.factors, function(x) {univariate.cat('astigmatism',x)}))

X.cat <- data.frame()
for (x in categorical.factors) {
X.cat <- rbind(X.cat,univariate.cat('left_astigmatism',x))
}
X.cat$var <- as.character(X.cat$var)
X.cat <- X.cat[which(X.cat$pvalue < bonferroni),] 
X.cat$Description <- dict[X.cat$var,'Description']
head(X.cat<-X.cat[order(abs(X.cat$pvalue)),])
write.csv(X.cat,file='left_astigmatism-univariate-categorical.csv',row.names=FALSE)

X.cat <- data.frame()
for (x in categorical.factors) {
X.cat <- rbind(X.cat,univariate.cat('right_astigmatism',x))
}
X.cat$var <- as.character(X.cat$var)
X.cat <- X.cat[which(X.cat$pvalue < bonferroni),] 
X.cat$Description <- dict[X.cat$var,'Description']
head(X.cat<-X.cat[order(abs(X.cat$pvalue)),])
write.csv(X.cat,file='right_astigmatism-univariate-categorical.csv',row.names=FALSE)




# random forest imputation

i <- sample(1:nrow(d),nrow(d)*.25)
imp <- rfImpute(y=d[i,'left_astigmatism'],x=d[i,covariates])
colnames(imp) <- c("left_astigmatism", "f.21003.0.0", "f.34.0.0", "f.5096.0.0", "f.5292.0.0", "f.5089.0.0", "f.1050.0.0", "f.1060.0.0", "f.5206.0.0", "f.50.0.0", "f.4079.0.0", "f.4080.0.0", "f.4196.0.0", "f.5208.0.0", "f.21021.0.0", "f.20023.0.0", "f.5163.0.0", "f.5156.0.0", "f.5111.0.0", "f.23105.0.0", "f.23098.0.0", "age", "gender", "height", "weight")
X <- d
X[i,colnames(imp)]<-imp


covariates <- dict[which(dict$Description %in% c('Age started wearing glasses or contact lenses Uses data-coding 100291', 'Age when attended assessment centre', 'Whole body fat-free mass', 'Intra-ocular pressure, corneal-compensated (right)', 'Body fat percentage', 'Intra-ocular pressure, Goldmann-correlated (right)', 'Basal metabolic rate', 'Standing height', 'Impedance of whole body', 'Age started wearing glasses or contact lenses Uses data-coding 100291', 'Average weekly beer plus cider intake Uses data-coding 100291', 'Average Y chromosome intensities for determining sex', 'Systolic blood pressure, automated reading', 'Forced expiratory volume in 1-second (FEV1), Best measure', 'Ethnicity code', 'Time spent outdoors in winter Uses data-coding 100329', 'Age started wearing glasses or contact lenses Uses data-coding 100291', 'Whole body fat mass', 'Forced expiratory volume in 1-second (FEV1)', 'Weight', 'Pulse wave Arterial Stiffness index', 'Pulse wave peak to peak time', 'Corneal hysteresis (right)', 'Diastolic blood pressure, automated reading', 'Age high blood pressure diagnosed Uses data-coding 100291', 'Diastolic blood pressure, automated reading')),'UDI']

covariates <- unique(grep('.0.0$',covariates,value=TRUE))


s <- summary(lm(as.formula(paste('log(right_astigmatism) ~', paste(covariates,collapse=' + '))),data=d))

# remove measurements with NA coefficient
#print(setdiff(left.cont.covariates, names((s$left.cont.coefficients[,1]))))
#left.cont.covariates <- intersect(covariates,names((s$left.cont.coefficients[,1])))


covariates <- c('age','gender','ethnicity_code','f.2217.0.0')
s <- summary(lm(as.formula(paste('log(right_astigmatism) ~', paste(covariates,collapse=' + '))),data=d))



