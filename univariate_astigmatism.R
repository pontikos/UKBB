
# dictionary of names
dict <- read('ukb6749.csv')
rownames(dict) <- dict$UDI


univariate <- function (y,x) {
    s<-summary(m <- lm( trans(d[,y]) ~ d[,x]))
    print(x)
    print(s$coefficients)
    #untrans <- function(
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
d.cont <- d.cont[,-which(is.na(colVar(d.cont)))]

# left astigmatism
X.cont.left <- do.call('rbind',lapply(colnames(d.cont), function(x) {univariate('left_astigmatism',x)}))
X.cont.left$var <- as.character(X.cont.left$var)
X.cont.left <- X.cont.left[which(X.cont.left$pvalue < bonferroni),] 
X.cont.left$Description <- dict[X.cont.left$var,'Description']
#xx <- univariate('left_astigmatism','age')
#xx$Description <- 'age'
#X.cont.left <- rbind(X.cont.left,xx)
head(X.cont.left<-X.cont.left[order(abs(X.cont.left$pvalue)),])
write.csv(X.cont.left,file='left_astigmatism-univariate-continuous.csv',row.names=FALSE)


# right astigmatism
X.cont.right <- do.call('rbind',lapply(colnames(d.cont), function(x) {univariate('right_astigmatism',x)}))
X.cont.right$var <- as.character(X.cont.right$var)
X.cont.right <- X.cont.right[which(X.cont.right$pvalue < bonferroni),] 
X.cont.right$Description <- dict[X.cont.right$var,'Description']
#xx <- univariate('right_astigmatism','age')
#xx$Description <- 'age'
#X.cont.right <- rbind(X.cont.right,xx)
head(X.cont.right<-X.cont.right[order(abs(X.cont.right$pvalue)),])
write.csv(X.cont.right,file='right_astigmatism-univariate-continuous.csv',row.names=FALSE)


#X2 <- rbind( univariate('left_astigmatism','age'),
#univariate('left_astigmatism','gender'),
#univariate('left_astigmatism','height'),
#univariate('left_astigmatism','weight'),
#univariate('left_astigmatism','iop'),
#univariate('left_astigmatism','corrected_visual_acuity'),
#univariate('left_astigmatism','crf'),
#univariate('left_astigmatism','ethnicity')
#)


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
d.cat <- d[,categorical.factors]
categorical.factors <- intersect(categorical.factors,colnames(d.cat))

# ignore
# Operative procedures - main OPCS Summary Information (operations)  
# too many categories
d.cat <- d.cat[,-grep('f.41200.',colnames(d.cat))]

# number of categories
num.cat <- apply(d.cat,2,function(x) length(table(x)))
d.cat <- d.cat[,which(1<num.cat&num.cat<1000)]
# remove if only 2 non-NA categories
sum.cat <- apply(d.cat,2,function(x) sum(table(x)))
d.cat <- d.cat[,which(sum.cat>2)]
categorical.factors <- colnames(d.cat)

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

