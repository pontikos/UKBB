
table.3.fun <- function(eye='right',
                        y='right_corneal_astigmatism',
                        trans=function(x){return(x)},
                        y.trans='right_corneal_astigmatism',
                        pvalue.threshold=0.001) {

  f <- sprintf('/home/pontikos/d/UKBB/scripts/UKBB/table3_variables_%s.csv',eye)
  vars <- read.csv(f,colClasses = c("character", "character", "character") )
  setdiff(vars$Variable, colnames(d))

  univariate <- function (Variable, DataType, Description) {
    print(Variable)
    print(DataType)
    x <- Variable
    if (DataType=='as.numeric') {
      print(dim( d <- na.omit(d[which(d[,y]>0),c(x,y)])))
      s<-summary(m <- lm( trans(d[,y]) ~ d[,x]))
      print(s$coefficients)
      print(summary(aov(s)))
      return(data.frame( Description=as.character(Description), Variable=as.character(Variable), var=Variable, coeff=m$coefficients[[2]], lower.conf=confint(m)[2,1], upper.conf=confint(m)[2,2], pvalue=s$coefficients[,'Pr(>|t|)'][[2]]))
    } else if (DataType=='as.factor') {
      print(levels <- names(sort(table(d[,Variable]),decreasing=TRUE)))
      x <- factor(d[,Variable],levels=levels)
      cat('Baseline:', baseline <- levels[1],'\n')
      cat('Description:', Description <- sprintf(Description,baseline), '\n')
      s<-summary(m <- lm( trans(d[,y]) ~ x))
      #print(s$coefficients)
      #print(summary(aov(s)))
      #print(s)
      res <- as.data.frame(cbind(m$coefficients,confint(m),s$coefficients[,'Pr(>|t|)']))
      res <- res[-1,]
      print(res)
      print(class(res))
      rownames(res) <- as.character(levels[2:length(levels)])
      colnames(res) <- c('coeff','lower.conf','upper.conf','pvalue')
      print(res <- as.data.frame(res))
      res$var <- paste(Variable,rownames(res),sep='')
      res$Description <- as.character(Description)
      res$Variable <- as.character(Variable)
      res <- res[, c('Description','Variable','var','coeff','lower.conf','upper.conf','pvalue')]
      #res <- rbind(data.frame(Description=Description,Variable=baseline,var=NA,coeff=NA,lower.conf=NA,upper.conf=NA,pvalue=NA),res)
      return(res)
      #return(data.frame(var=as.character(x), pvalue=s[[1]]$`Pr(>F)`[[1]]))
    }
  }
  
  for (Variable in vars[which(vars$DataType=='as.factor'),'Variable']) {
    print(levels <- names(sort(table(d[,Variable]),decreasing=TRUE)))
    x <- factor(d[,Variable],levels=levels)
    d[,Variable] <- x
  }

  res <- data.frame()
  for (i in 1:nrow(vars)) {
    print(i)
    res <- rbind(res,univariate(Variable=vars[i,'Variable'], DataType=vars[i,'DataType'], Description=vars[i,'Description']))
  }
  
  # multivariate regression
  # beta
  # lower confidence 95%
  # upper confidence 95%
  # pvalue
  adjusted_r_squared <- 0
  multi.vars <- unique(as.character(res[ which(res$pvalue<pvalue.threshold),'Variable']))
  
  print(s <- summary(m <- lm(formula(paste(y, '~', paste(multi.vars,collapse='+'))),d)))
  adjusted_r_squared <<- s$adj.r.squared
  multivariable_rsquared <- s$r.squared
  
  multi.res <- data.frame(coeff=s$coefficients[,'Estimate'], pvalue=s$coefficients[,'Pr(>|t|)'])
  
  conf <- confint(m)
  colnames(conf) <- c('lower.conf','upper.conf')
  multi.res <- as.data.frame(cbind(multi.res,conf))
  multi.res$var <- rownames(multi.res)
  multi.res <- multi.res[,c('var','coeff','lower.conf','upper.conf','pvalue')]
  all.res <- merge(res,multi.res,by='var',all.x=TRUE, suffixes=c(' univariate',' multivariate'))
  all.res <- all.res[,c('Description','var','coeff univariate','lower.conf univariate','upper.conf univariate','pvalue univariate','coeff multivariate','lower.conf multivariate','upper.conf multivariate','pvalue multivariate')]
  
  for (i in grep('coeff|conf',colnames(all.res))) {
    all.res[,i] <- round(all.res[,i],3)
  }
  
  for (i in grep('pvalue',colnames(all.res))) {
    all.res[,i] <- round(all.res[,i],3)
  }
  
  all.res[which(all.res[,'pvalue univariate']<pvalue.threshold),'pvalue univariate']<-paste('<',pvalue.threshold,sep='')
  all.res[which(all.res[,'pvalue multivariate']<pvalue.threshold),'pvalue multivariate']<-paste('<',pvalue.threshold,sep='')
  all.res[,'univariate beta (95% CI)'] <- sprintf('%.3f (%.3f to %.3f)', all.res[,'coeff univariate'], all.res[,'lower.conf univariate'], all.res[,'upper.conf univariate'])
  all.res[,'multivariate beta (95% CI)'] <- sprintf('%.3f (%.3f to %.3f)', all.res[,'coeff multivariate'], all.res[,'lower.conf multivariate'], all.res[,'upper.conf multivariate'])
  all.res[(is.na(all.res))] <- ''
  all.res$Description <- as.character(all.res$Description)
  all.res$Description[which(duplicated(all.res$Description))] <- ''
  all.res$var <- gsub('.*:','', all.res$var)
  #all.res$Description[which(all.res$Description=='')] <- all.res$var[which(all.res$Description=='')]
  all.res <- all.res[,c('Description','var','univariate beta (95% CI)','pvalue univariate', 'multivariate beta (95% CI)', 'pvalue multivariate')]
  for (i in 1:nrow(all.res)) {
    if (grepl( all.res[i,'var'], all.res[i,'Description'], ignore.case=TRUE )) {
      all.res[i,'var'] <- ''
    }
  }
  all.res[,'Description'] <- paste(all.res[,'Description'],all.res[,'var'],sep=' ')
  all.res <- all.res[,c('Description','univariate beta (95% CI)','pvalue univariate', 'multivariate beta (95% CI)', 'pvalue multivariate')]
  
  colnames(all.res)[ which( colnames(all.res) == 'pvalue multivariate' ) ] <- 'multivariate pvalue'
  colnames(all.res)[ which( colnames(all.res) == 'pvalue univariate' ) ] <- 'univariate pvalue'
  all.res[ "NA (NA to NA)"== all.res ] <- ''
  all.res$Description <- gsub('ethnicityasian','asian',all.res$Description)
  all.res$Description <- gsub('ethnicityblack','black',all.res$Description)
  all.res$Description <- gsub('ethnicitymixed','mixed',all.res$Description)
  all.res$Description <- gsub('ethnicitychinese','chinese',all.res$Description)
  all.res$Description <- gsub('season_of_assessment','',all.res$Description)
  all.res$Description <- gsub('\\S+_\\S+','',all.res$Description)
  #alcohol_intake_code|age_completed_education|skin_colour_code|uv_protection_code|right_corneal_corrected_iop|right_corrected_visual_acuity
  write.table(all.res,sep=';',file='',row.names=FALSE)
  return(all.res)
}

right_corneal_astigmatism <- table.3.fun(eye='right', y='right_corneal_astigmatism', trans=function(x){return(x)}, y.trans='right_corneal_astigmatism')
right_log_corneal_astigmatism <- table.3.fun(eye='right', y='right_corneal_astigmatism', trans=function(x){return(log(x))}, y.trans='log(right_corneal_astigmatism)')
left_corneal_astigmatism <- table.3.fun(eye='left', y='left_corneal_astigmatism', trans=function(x){return(x)}, y.trans='left_corneal_astigmatism')
left_log_corneal_astigmatism <- table.3.fun(eye='left', y='left_corneal_astigmatism', trans=function(x){return(log(x))}, y.trans='log(left_corneal_astigmatism)') 
colnames(right_log_corneal_astigmatism) <- colnames(right_corneal_astigmatism) <- c("Description", "right univariate beta (95% CI)", "right univariate pvalue", "right multivariate beta (95% CI)", "right multivariate pvalue")
colnames(left_log_corneal_astigmatism) <- colnames(left_corneal_astigmatism) <- c("Description", "left univariate beta (95% CI)", "left univariate pvalue", "left multivariate beta (95% CI)", "left multivariate pvalue")


# univariable
table3.1 <- cbind(right_corneal_astigmatism[,c("Description", "right univariate beta (95% CI)", "right univariate pvalue")],left_corneal_astigmatism[,c("left univariate beta (95% CI)", "left univariate pvalue")])
table3.1 <- table3.1[which(rowSums(table3.1[,2:4]=='')<3),]
write.table( table3.1, file='', row.names=FALSE, sep=';')
# order by magnitude
X.right <- table3.1[which(table3.1[,'right univariate pvalue']=='<0.001'),c('Description','right univariate beta (95% CI)')]
X.right[order(abs(as.numeric(unlist(lapply(strsplit( X.right[,2], ' '),'[[',1)))),decreasing=TRUE),]
X.left <- table3.1[which(table3.1[,'left univariate pvalue']=='<0.001'),c('Description','left univariate beta (95% CI)')]
X.left[order(abs(as.numeric(unlist(lapply(strsplit( X.left[,2], ' '),'[[',1)))),decreasing=TRUE),]
X <- merge(X.right,X.left)
X[order(abs(as.numeric(unlist(lapply(strsplit( X[,2], ' '),'[[',1)))),decreasing=TRUE),]


# multivariable
table3.2 <- cbind(right_corneal_astigmatism[,c("Description", "right multivariate beta (95% CI)", "right multivariate pvalue")],left_corneal_astigmatism[,c("left multivariate beta (95% CI)", "left multivariate pvalue")])
table3.2 <- table3.2[which(rowSums(table3.2[,2:4]=='')<3),]
write.table( table3.2, file='', row.names=FALSE, sep=';')
# order by magnitude
X.right <- table3.2[which(table3.2[,'right multivariate pvalue']=='<0.001'),c('Description','right multivariate beta (95% CI)')]
X.right[order(abs(as.numeric(unlist(lapply(strsplit( X.right[,2], ' '),'[[',1)))),decreasing=TRUE),]
X.left <- table3.2[which(table3.2[,'left multivariate pvalue']=='<0.001'),c('Description','left multivariate beta (95% CI)')]
X.left[order(abs(as.numeric(unlist(lapply(strsplit( X.left[,2], ' '),'[[',1)))),decreasing=TRUE),]
X <- merge(X.right,X.left)
X[order(abs(as.numeric(unlist(lapply(strsplit( X[,2], ' '),'[[',1)))),decreasing=TRUE),]


tableS2.2 <- cbind(right_log_corneal_astigmatism[,c("Description", "right multivariate beta (95% CI)", "right multivariate pvalue")],left_log_corneal_astigmatism[,c("left multivariate beta (95% CI)", "left multivariate pvalue")])
tableS2.2 <- tableS2.2[which(rowSums(tableS2.2[,2:4]=='')<3),]
write.table( tableS2.2, file='', row.names=FALSE, sep=';')





