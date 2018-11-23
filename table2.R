f <- '/home/pontikos/d/UKBB/scripts/UKBB/table3_variables.csv'
vars <- read.csv(f,colClasses = c("character", "character", "character") ) 

age.40 <- which(40<=d$age & d$age<49)
age.50 <- which(50<=d$age & d$age<59)
age.60 <- which(60<=d$age & d$age<69)

make.row.cont <- function(param,Description,d)  {
    print(param)
    females <- which(d$gender=='F')
    males <- which(d$gender=='M')
    total <- sprintf('%.1f (%.1f)', mean(d[,param],na.rm=TRUE), sd(d[,param],na.rm=TRUE))
    total.males <- sprintf('%.1f (%.1f)', mean(d[males,param],na.rm=TRUE), sd(d[males,param],na.rm=TRUE))
    total.females <- sprintf('%.1f (%.1f)', mean(d[females,param],na.rm=TRUE), sd(d[females,param],na.rm=TRUE))
    print(length(d[males,param]))
    print(length(d[females,param]))
    pvalue <- t.test( d[males,param] , d[females,param])$p.value
    if (pvalue>.001) { pvalue <- '' }
    else { pvalue <- '<.001' }
    d <- data.frame('total'=total,'total males'=total.males,'total females'=total.females,pvalue=as.character(pvalue))
    d$pvalue <- ''
    d$pvalue[1]<-pvalue
    rownames(d) <- Description
    return(d)
}

make.row.cat <- function(param,Description,d) {
    print(param)
    row.names <- names(table(d[,param]))
    females <- which(d$gender=='F')
    males <- which(d$gender=='M')
    print(total <- prop.table(table.total <- table(d[,param])))
    print(total.males <- prop.table(table.total.males <- table(factor(d[males,param],levels=names(total)))))
    print(total.females <- prop.table(table.total.females <- table(factor(d[females,param],levels=names(total)))))
    X<-round(100*cbind(total,total.males,total.females),1)
    X <- as.data.frame((X[order(rowSums(X),decreasing=TRUE),]))
    for (r in row.names) {
        X[r,] <- as.character(X[r,])
    }
    print(as.numeric(table.total.males))
    print(as.numeric(table.total.females))
    print(pvalue<-chisq.test(rbind(as.numeric(table.total.males),as.numeric(table.total.females)))$p.value)
    #pvalue = tryCatch({
    #fisher.test(as.numeric(table.total.males),as.numeric(table.total.females))$p.value
    #}, warning = function(w) {
    #fisher.test(as.numeric(table.total.males),as.numeric(table.total.females))$p.value
    #}, error = function(e) {
        #1
    #}, finally = {
    #})
    if (pvalue>.001) { pvalue <- '' }
    else { pvalue <- '<.001' }
    X$pvalue <- ''
    X$pvalue[1]<-pvalue
    rownames(X) <- paste(Description,rownames(X),sep=' ')
    rownames(X) <- gsub('.baseline.*=.*%s.','',rownames(X))
    return(X)
}


for (Variable in vars[which(vars$DataType=='as.factor'),'Variable']) {
        print(levels <- names(sort(table(d[,Variable]),decreasing=TRUE)))
        x <- factor(d[,Variable],levels=levels)
        d[,Variable] <- x
}


make.row <- function (Variable, DataType, Description) {
    if (DataType=='as.numeric') {
        return(make.row.cont(Variable,Description,d))
    } else if (DataType=='as.factor') {
        return(make.row.cat(Variable,Description,d))
    }
}


res <- data.frame()
for (i in 1:nrow(vars)) {
    res <- rbind(res,make.row(Variable=vars[i,'Variable'], DataType=vars[i,'DataType'], Description=vars[i,'Description']))
}


res$Variable <- rownames(res)

res <- res[,c('Variable','total','total.males','total.females','pvalue')]

colnames(res) <- c('variable','total','total males','total females','pvalue')

res <- res[-grep('Sex',res$variable),]

write.table(res,sep=';',file='',row.names=FALSE)

table.2 <- res


#write.csv(
#rbind(
## age
#make.row.cont('age',d) ,
## ethnicity 
#make.row.cat('ethnicity',d),
## skin colour
#make.row.cat('skin_colour',d),
## hair colour
#make.row.cat('hair_colour',d),
## uv protection
#make.row.cat('uv_protection',d),
## assessment center
#make.row.cat('sites',d),
## season of assessment
#make.row.cat('season_of_assessment',d),
## Townsend deprivation index
#make.row.cont('townsend_deprivation_index',d) ,
#make.row.cont('corneal_hysterisis_left',d),
## iop
#make.row.cont('left_corneal_corrected_iop',d) ,
## Height, m (n¼110 127) 1.69 (0.09) 1.63 (0.06) 1.76 (0.07) <0.001
#make.row.cont('height',d)  ,
## Weight, kg (n¼110 102) 78.2 (16.1) 71.6 (14.2) 86.0 (14.5) <0.001
#make.row.cont('weight',d)  ,
## BMI, kg/m2 (n¼105 113) 27.4 (4.8) 27.1 (5.2) 27.8 (4.3) <0.001
#make.row.cont('BMI',d)  ,
## Waist circumference, cm (n¼110 268) 90.6 (13.6) 85.1 (12.7) 97.1 (11.5) <0.001
## SBP, mmHg (n¼110 510) 137.4 (18.4) 135.1 (19.0) 140.0 (17.2) <0.001
#make.row.cont('SBP',d)  ,
## DBP, mmHg (n¼110 510) 81.9 (10.0) 80.5 (9.9) 83.6 (9.9) <0.001
#make.row.cont('DBP',d)  ,
## Pulse rate, min1 (n¼110 510) 68.6 (11.1) 69.4 (10.4) 67.7 (11.8) <0.001 
## Refractive error, D 
## Right eye (n¼1 109 376) 0.36 (2.8) 0.31 (2.8) 0.35 (2.7) 0.14 
##Left eye (n¼109 059) 0.31 (2.8) 0.31 (2.8) 0.30 (2.7) 0.42
#make.row.cat('alcohol_intake',d),
## Current smoking status, % (n¼107 115)
#make.row.cat('current_smoking_status',d) ,
## diabetes
#make.row.cat('diabetes',d)
## insulin
##make.row.cat('insulin',d),
##
##make.row.cat('cholesterol_lowering_medication',d),
##
##make.row.cat('hormone_replacement_therapy',d),
##
##make.row.cat('blood_pressure_medication',d),
##
##make.row.cat('oral_contraception',d)
#)
#,
#file=''
#)
#


#Self-reported glaucoma, %
#Right eye (n¼110 573) 1.46 1.17 1.80 <0.001
#Left eye (n¼110 573) 1.45 1.14 1.82 <0.001

#Self-reported macular degeneration, %
#Right eye (n¼110 573) 0.85 0.92 0.77 0.006
#Left eye (n¼110 573) 0.82 0.90 0.72 0.001


# Season of test, % (n¼110 573)
# Spring 35.0 34.9 35.25 0.040
# Summer 19.9 20.1 19.6
# Autumn 23.1 23.2 22.95



