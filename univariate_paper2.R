

# dictionary of names
dict <- read('ukb6749.csv')
rownames(dict) <- dict$UDI


univariate <- function (y,x,d,levels,trans=log) {
    print(x)
    
    print(dim( d <- na.omit(d[which(d[,y]>0),c(x,y)])))
    s<-summary(m <- lm( trans(d[,y]) ~ d[,x]))
    print(s$coefficients)
    print(summary(aov(s)))
    return(data.frame( var=as.character(x), coeff=m$coefficients[[2]], lower.conf=confint(m)[2,1], upper.conf=confint(m)[2,2], pvalue=s$coefficients[,'Pr(>|t|)'][[2]]))
}


# Age, years
#univariate('left_corneal_astigmatism','age',na.omit(data1[,c('left_corneal_astigmatism','age')]))
univariate('left_corneal_astigmatism','age',d)

# Sex (baseline = female) 
univariate('left_corneal_astigmatism','gender',d)

# Ethnicity (baseline = white)
# Asian
# Black
# Chinese
# Mixed 
# Others
univariate('left_corneal_astigmatism','ethnicity_code',d)
univariate('left_corneal_astigmatism','ethnicity',d)

# Age completed full time education 
mean(d$f.845.0.0,na.rm=TRUE)
sd(d$f.845.0.0,na.rm=TRUE) 
univariate('left_corneal_astigmatism','f.845.0.0',d)

# Skin colour (baseline = fair or very fair)
# Olive: light or dark
# Brown
# Black 
t(t(table(d$f.1717.0.0))) 
univariate('left_corneal_astigmatism','f.1717.0.0',d)

# Hair colour (baseline = blonde or red)
# Light brown
# Dark brown
# Black 
t(t(table(d$f.1747.0.0))) 
univariate('left_corneal_astigmatism','f.1747.0.0',d)


# Use of UV protection (baseline = never/ rarely)
# Sometimes
# Most of the time
# Always 
t(t(table(d$f.2267.0.0))) 
univariate('left_corneal_astigmatism','f.2267.0.0',d)

# Alcohol intake (baseline = never)
# Daily or almost daily
# Three or four times a week
# Once or twice a week
# One to three times a month or less
t(t(table(d$f.1558.0.0)))
univariate('left_corneal_astigmatism','f.1558.0.0',d)

# Cholesterol medication
# Do you regularly take any of the following medications?
# None of the above -7 149217
# Prefer not to answer -3
# Do not know -1
# Insulin 3
# Blood pressure medication 2
# Cholesterol lowering medication 1
t(t(table(d$f.6177.0.0)))
univariate('left_corneal_astigmatism','f.6177.0.0',d)

univariate('left_corneal_astigmatism','medication',d)

# Month of birth
t(t(table(d$f.52.0.0)))
univariate('left_corneal_astigmatism','f.52.0.0',d)

# corneal corrected
# IOPg, mmHg
mean(d$f.5262.0.0,na.rm=TRUE)
sd(d$f.5262.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.5262.0.0',d)

# Corneal resistance factor
mean(d$f.5265.0.0,na.rm=TRUE)
sd(d$f.5265.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.5265.0.0',d)

# Corneal hysteresis
mean(d$corneal_hysterisis_left,na.rm=T)
sd(d$corneal_hysterisis_left,na.rm=T)
univariate('left_corneal_astigmatism','corneal_hysterisis_left',d)

# Height, m
mean(d$f.50.0.0,na.rm=TRUE)
sd(d$f.50.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.50.0.0',d)

# Weight, 10 kg
mean(d$f.23098.0.0,na.rm=TRUE)
sd(d$f.23098.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.23098.0.0',d) 

# BMI, kg/m2
mean(d$f.23104.0.0,na.rm=TRUE)
sd(d$f.23104.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.23104.0.0',d)

# Waist, cm
#mean(d$f.48.0.0,na.rm=TRUE)
#sd(d$f.48.0.0,na.rm=TRUE)

# SBP, mmHg
mean(d$f.4080.0.0,na.rm=TRUE)
sd(d$f.4080.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.4080.0.0',d)

# DBP, mmHg
mean(d$f.4079.0.0,na.rm=TRUE)
sd(d$f.4079.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.4079.0.0',d)

# Townsend deprivation index
mean(d$f.189.0.0,na.rm=TRUE)
sd(d$f.189.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.189.0.0',d)

# Smoker (field 1239)
# Prefer not to answer: -3
# No: 0
# Only occasionaly: 2
# Yes on most or all days: 1
t(t(table(d$f.1239.0.0,exclude=NULL)))
univariate('left_corneal_astigmatism','f.1239.0.0',d)

# current_smoking
t(t(table(d$current_smoking,exclude=NULL)))
univariate('left_corneal_astigmatism','current_smoking',d)

# Maternal smoking
t(t(table(d$f.1787.0.0,exclude=NULL)))
univariate('left_corneal_astigmatism','f.1787.0.0',d)

# Age Atopy (hayfever, allergic rhinitis)
#t(t(table(d$f.3761.0.0,exclude=NULL)))
mean(d$f.3761.0.0,na.rm=TRUE)
sd(d$f.3761.0.0,na.rm=TRUE)
univariate('left_corneal_astigmatism','f.3761.0.0',d)

# Asthma, self reported (n = )
t(t(table(!is.na( d$f.3786.0.0))))
mean(d$f.3786.0.0,na.rm=T)
sd(d$f.3786.0.0,na.rm=T)
univariate('left_corneal_astigmatism','f.3786.0.0',d)

# Diabetes, self reported (n = )
t(t(table(!is.na(d$f.2443.0.0))))
t(t(table(d$f.2443.0.0)))
univariate('left_corneal_astigmatism','f.2443.0.0',d)

univariate('left_corneal_astigmatism','weight',d[which(d$gender==0),])
univariate('left_corneal_astigmatism','weight',d[which(d$gender==1),])

d$season_of_assessment <- relevel(as.factor(d$season_of_assessment),ref='winter')
univariate('left_corneal_astigmatism','season_of_assessment',d)

univariate('left_corneal_astigmatism','age_asthma_diagnosed',d)


