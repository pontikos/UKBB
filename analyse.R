#!/usr/bin/env Rscript

setwd('/SAN/vyplab/NCMD/UKBB')

read('ukb6749.tab')->data1
read('ukb7618.tab')->data2
rownames(data1) <- data1$f.eid
rownames(data2) <- data2$f.eid
print(setdiff(data1$f.eid,data2$f.eid))
data1[rownames(data2),colnames(data2)]<-data2
data1 ->d

# EXCLUSION criteria:
# 3mm keratometry result unreliable (right)
d <- d[which(is.na(d[,'f.5140.0.0'])),]
# Ever had laser refractive 5325-0.0
#d <- d[which(is.na(d[,'f.5325.0.0'])),]
# Ever had glaucoma surgery 5326-0.0
#d <- d[which(is.na(d[,'f.5326.0.0'])),]
# Ever had corneal graft 5328-0.0
#d <- d[which(is.na(d[,'f.5328.0.0'])),]
# Loss vision trauma 5430-0.0
#d <- d[which(is.na(d[,'f.5430.0.0'])),]
# Ever had eye surgery 5181-0.0
d <- d[which(is.na(d[,'f.5181.0.0'])|d[,'f.5181.0.0']==0),]
# 3mm asymmetry index unreliable (right)
d <- d[which(is.na(d[,'f.5144.0.0'])),]
# only keep ones with keratometry values
d <- d[which(!is.na(d[,'f.5132.0.0'])),]
d <- d[which(!is.na(d[,'f.5099.0.0'])),]

# EXCLUDE:
# Ever had cataract surgery 5324-0.0
# Ever had laser refractive 5325-0.0
# Ever had glaucoma surgery 5326-0.0
# Ever had corneal graft 5328-0.0
# Loss vision trauma 5430-0.0
# Ever had eye surgery 5181-0.0

f.5085.0.0


# RESPONSE variables

# Astigmatism defined as:
# Astigmatism 3mm right =  3mm strong meridian (right) 5132-0.0 minus  3mm weak meridian (right) 5099-0.0 ie. ((5132-0.0) – (5099-0.0))
# 3mm astigmatism (right) = 3mm strong meridian (right) -  3mm weak meridian (right)
d[,'astigmatism'] <- d[,'f.5132.0.0'] - d[,'f.5099.0.0']
pdf('/SAN/vyplab/NCMD/UKBB/plots/astigmatism_dist.pdf')
hist(d[,'astigmatism'],xlim=c(0,4),breaks=seq(0,14,.5),xlab='3mm astigmatism',main='')
dev.off()

# Additional descriptor for astigmatism 3mm right:  3mm strong meridian angle (right) 5107-0.0
# axis of astigmatism
pdf('/SAN/vyplab/NCMD/UKBB/plots/axis_astigmatis_dist.pdf')
hist(d[,'f.5107.0.0'],xlab='Axis of Astigmatism',main='')
dev.off()

# Mean corneal power right =   3mm strong meridian (right) 5132-0.0 plus  3mm weak meridian (right) 5099-0.0 /2  ie.  ((5132-0.0) + (5099-0.0))/2
d[,'mean_corneal_power_right'] <- (d[,'f.5132.0.0'] + d[,'f.5099.0.0'])/2
pdf('/SAN/vyplab/NCMD/UKBB/plots/mean_corneal_power_right.pdf')
hist(d[,'mean_corneal_power_right'],xlab='Mean Corneal Power',main='')
dev.off()

# 3mm asymmetry index for irregular astigmatism level (right)
print(table(d[,'f.5152.0.0']))

d[,'KC.prox1'] <- 0
d[which(d[,'mean_corneal_power_right']>=48) ,'KC.prox1'] <- 1
d[,'KC.prox2'] <- 0
d[which(d[,'mean_corneal_power_right']>=49),'KC.prox2'] <- 1
d[,'KC.prox3'] <- 0
d[which(d[,'mean_corneal_power_right']>=50),'KC.prox3'] <- 1
d[,'KC.prox4'] <- 0
d[which(d[,'mean_corneal_power_right']>=48&d[,'f.5152.0.0']>1),'KC.prox4'] <- 1

print(100*length(which(d[,'KC.prox1']>0))/length(which(!is.na(d[,'mean_corneal_power_right']))))
print(100*length(which(d[,'KC.prox2']>0))/length(which(!is.na(d[,'mean_corneal_power_right']))))
print(100*length(which(d[,'KC.prox3']>0))/length(which(!is.na(d[,'mean_corneal_power_right']))))
print(100*length(which(d[,'KC.prox4']>0))/length(which(!is.na(d[,'mean_corneal_power_right']))))

# Astigmatism 3mm right associated with:

# Sex 31-0.0
print(summary(lm(d[,'astigmatism'] ~ d[,'f.31.0.0'])))
pdf('/SAN/vyplab/NCMD/UKBB/plots/astigmatism_gender.pdf')
boxplot(astigmatism ~ f.31.0.0,d)

pdf('/SAN/vyplab/NCMD/UKBB/plots/boxplot_mean_corneal_power_gender.pdf')
boxplot(d[,'mean_corneal_power_right'] ~ d[,'f.31.0.0'],ylab='mean_corneal_power',xlab='gender')
#lines( lowess(d[,'mean_corneal_power_right'] ~ d[,'age'],f=.2), col = 'red')
dev.off()



# age

# Age (2010) 2010 minus 34-0.0
# year of birth
d[,'yob'] <- d[,'f.34.0.0']
# month of birth
d[,'mob'] <- d[,'f.52.0.0']
# date of assessment
# year of assesment
d[,'yoa'] <- as.numeric(unlist(lapply(strsplit( (d[,'f.53.0.0']), '-'),'[[',1)))
# age at assessment
d[,'age'] <- d[,'yoa']-d[,'yob']

# age distribution
pdf('/SAN/vyplab/NCMD/UKBB/plots/age.pdf')
hist(d[,'age'],main='',xlab='')
dev.off()

print(summary(lm(d[,'astigmatism'] ~ d[,'age'])))

pdf('/SAN/vyplab/NCMD/UKBB/plots/astigmatism_age.pdf')
plot(d[,'astigmatism'] ~ d[,'age'],ylab='astigmatism',xlab='age')
lines( lowess(d[,'astigmatism'] ~ d[,'age'],f=.2), col = 'red')
dev.off()


pdf('/SAN/vyplab/NCMD/UKBB/plots/astigmatism_age_boxplot.pdf')
boxplot(d[,'astigmatism'] ~ d[,'age'],ylab='astigmatism',xlab='age')
dev.off()

pdf('/SAN/vyplab/NCMD/UKBB/plots/mean_corneal_power_age.pdf')
plot(d[,'mean_corneal_power_right'] ~ d[,'age'],ylab='mean_corneal_power',xlab='age')
lines( lowess(d[,'mean_corneal_power_right'] ~ d[,'age'],f=.2), col = 'red')
dev.off()

pdf('/SAN/vyplab/NCMD/UKBB/plots/boxplot_mean_corneal_power_age.pdf')
boxplot(d[,'mean_corneal_power_right'] ~ d[,'age'],ylab='mean_corneal_power',xlab='age')
lines( lowess(d[,'mean_corneal_power_right'] ~ d[,'age'],f=.2), col = 'red')
dev.off()


# age vs gender

d$gender <- d[,'f.31.0.0']

pdf('/SAN/vyplab/NCMD/UKBB/plots/boxplot_age_gender.pdf')
boxplot( d[,'age'] ~ d[,'f.31.0.0'],xlab='age by gender')
dev.off()


# educational achievement – age completed higher education 845.0.0 or qualifications – 6138.0.0
d[,'educational_achievement'] <- as.factor(d[,'f.6138.0.0'])
d[,'university'] <- 0
d[which(d[,'f.6138.0.0']==1),'university'] <- 1

pdf('/SAN/vyplab/NCMD/UKBB/plots/educational_achievement_mean_corneal_power.pdf')
boxplot( d[,'mean_corneal_power_right'] ~ d[,'educational_achievement'],xlab='educational achievement')
dev.off()



# Intra-ocular pressure IOP Goldmann-correlated, 5255.0.0
d[,'iop'] <- d[,'f.5255.0.0']
quantile(d[,'f.5255.0.0'],seq(0,1,.05),na.rm=TRUE)
print(summary(lm( d[,'ast'] ~ d[,'f.5255.0.0'])))




# Height 50-0.0
# Deprivation index 189-0.0
# Wears glasses or contact lens 2207-0.0
# Diabetes 2443-0.0
# Asthma, eczema, rhinitis etc 6152-0.0

# Weight 23098-0.0
d[,'weight'] <- d[,'f.23098.0.0']

# Age completed education 845-0.0
# Qualifications 6138-0.0
# Sphere power right 5084-0.0

# Description:	Spherical power (left)
d$right_spherical_power <- d$f.5085.0.0

# Description:	Spherical power (right)
d$left_spherical_power <- d$f.5084.0.0

# Corneal hysteresis right 5256-0.0
d[,'hysteresis'] <- d[,'f.5256.0.0']

# cholesterol lowering medication f.6153.0.0  f.6177.0.0
table( d[,'f.6153.0.0'] )
d[,'cholesterol'] <- 0
d[which((d[,'f.6153.0.0']==1)|(d[,'f.6177.0.0']==1)),'cholesterol']  <- 1


# Corneal resistance right 5257-0.0

# IOPcc right 5254-0.0

# COVARIATES
# Hysteresis, 5256.0.0
# CRF, 5257.0.0
# IOP, 5255.0.0
# CCT (not collected)
# Atopy; use hayfever, rhinitis, eczema 3761.0.0
# Asthma: 3786.0.0 any age; consider 6152.0.0 asthma diagnosed by doctor
# Ethnicity: 21000.0.0
# Gender:  31.0.0
# Genetic disease - there area whole range of syndromic conditions (e.g. Down syndrome), but these may have been excluded before entry: use 41202.0.0; find
# Q87.4 of field 41202 for Marfans; q21.1 for ASDs; J45 for asthma
# Eye rubbing - probably not collected
# Smoking (auto - cross linking): look at use 1239.0.0 and 1249.0.0; look at 20116.0.0; 20160.0.0;
# Maternal smoking – 1787.0.0
# Facial aging: - 1757.0.0
# Breast fed as baby – 1677.0.0
# Floppiness (mitral valve disease, floppy lid, fractures)- snoring 1210.0.0; fracture or broken bone in past 5 years 2463.0.0;
# Maternal age (not collected)
# Corrected visual acuity – explore 5185.0.0; 5201.0.0
# Refractive astigmatism -
# There are some fields about irregular astigmatism or asymmetry. I am not sure what was measured for these
# Age started wearing glasses or contact lenses – 2217.0.0
# Myopia and close work
# Age- 21003-0-0
# Height – 50.0.0
# Weight – 23098.0.0
# BMI? – 23104.0.0
# Blood pressure – explore 2966.0.0
# Diabetes diagnosed by doctor – 2443.0.0
# educational achievement – age completed higher education 845.0.0 or qualifications – 6138.0.0
# Townsend deprivation – 189.0.0
# Time spend outdoors in summer/ winter – 1050.0.0; 1060.0.0


# REGRESSIONS
# predicting ast (astigmatism)
# predicting mean_corneal_power_right
# predicting whichever keratoconus proxy to go for

# Hysteresis (should not be over 20)
quantile(d[,'f.5256.0.0'],seq(0,1,.05),na.rm=TRUE)
print(summary(lm( d[,'ast'] ~ d[,'f.5256.0.0'])))

# Corneal resistance factor CRF, 5257.0.0
d[,'crf'] <- d[,'f.5257.0.0']
quantile(d[,'f.5257.0.0'],seq(0,1,.05),na.rm=TRUE)
print(summary(lm( d[,'ast'] ~ d[,'f.5257.0.0'])))
# CCT (not collected)

# Atopy; use hayfever, rhinitis, eczema 3761.0.0
quantile(d[,'f.3761.0.0'],seq(0,1,.05),na.rm=TRUE)
d[,'atopy'] <- d[,'f.3761.0.0']
print(summary(lm( d[,'ast'] ~ d[,'f.3761.0.0'])))

# Asthma: 3786.0.0 any age; consider 6152.0.0 asthma diagnosed by doctor
table(d[,'f.6152.0.0'])
# -7     -3      5      6      7      8      9
# 326922   1118   9749   7486   2631  51270  83472
#8 Asthma
d[,'asthma']<-0
d[which(d[,'f.6152.0.0']==8),'asthma']<-1
#9 Hayfever, allergic rhinitis or eczema
d[,'atopy']<-0
d[which(d[,'f.6152.0.0']==9),'atopy']<-1



# Gender:  31.0.0
# female 0
# male 1
d[,'gender']<-d[,'f.31.0.0']
table(d[,'f.31.0.0'])



# Genetic disease - there area whole range of syndromic conditions (e.g. Down syndrome), but these may have been excluded before entry: use 41202.0.0; find
# Q874 of field 41202 for Marfans
d[,'marfan'] <- 0
d[which(as.character(d[,'f.41202.0.0'])=='Q874'), 'marfan'] <- 1
# Q211 for ASDs: congenital cardiac malformation
d[,'ASD'] <- 0
d[which(as.character(d[,'f.41202.0.0'])=='Q211'), 'ASD'] <- 1
# J45 for asthma from hospital record
d[,'asthma_hospital'] <- 0
d[grep('^J45',d[,'f.41202.0.0']), 'asthma_hospital'] <- 1
print(table(d[,'asthma_hospital']))


# Eye rubbing - probably not collected

# Smoking (auto - cross linking): look at use 1239.0.0 and 1249.0.0; look at 20116.0.0; 20160.0.0;
d[,'current_smokers'] <- 0
d[which(d[,'f.1239.0.0']>0),'current_smokers'] <- 1
d[,'past_smokers'] <- 0
d[which(d[,'f.1249.0.0']%in%c(1,2)),'past_smokers'] <- 1
d[,'smokers'] <- d[,'current_smokers']|d[,'past_smokers']

# Maternal smoking – 1787.0.0
d[,'maternal_smoking'] <- 0
d[which(d[,'f.1787.0.0']==1),'maternal_smoking'] <- 1

# Facial aging: - 1757.0.0
d[,'looks_older'] <- 0
d[which(d[,'f.1757.0.0']==2),'looks_older'] <- 1

# Breast fed as baby – 1677.0.0
d[,'breast_fed'] <- 0
d[which(d[,'f.1677.0.0']==1),'breast_fed'] <- 1

# Floppiness (mitral valve disease, floppy lid, fractures)- snoring 1210.0.0
d[,'snoring'] <- 0
d[which(d[,'f.1210.0.0']==1),'snoring'] <- 1

# fracture or broken bone in past 5 years 2463.0.0;
d[,'broken_bones'] <- 0
d[which(d[,'f.2463.0.0']==1),'broken_bones'] <- 1

# Maternal age (not collected)

# Corrected visual acuity – explore 5185.0.0; 5201.0.0
quantile(d[,'f.5201.0.0'],na.rm=TRUE)
d[,'corrected_visual_acuity'] <- d[,'f.5201.0.0']


# Refractive astigmatism -

# There are some fields about irregular astigmatism or asymmetry. I am not sure what was measured for these
# Age started wearing glasses or contact lenses – 2217.0.0
# Myopia and close work


# Age- 21003-0-0
d[,'age'] <- d[,'f.21003.0.0']

# Height – 50.0.0
d[,'height'] <- d[,'f.50.0.0']

# Weight – 23098.0.0
d[,'weight'] <- d[,'f.23098.0.0']

# BMI? – 23104.0.0

# Age at which High Blood pressure diagnosed – explore 2966.0.0
# code as yes if there's a value otherwise no
d[,'high_bp'] <- 0
d[which((d[,'f.6153.0.0']==2)|(d[,'f.6177.0.0']==2|(d[,'f.2966.0.0']>0))),'high_bp']  <- 1

# Diabetes diagnosed by doctor – 2443.0.0
d[,'diabetes'] <- 0
d[ which(d[,'f.2443.0.0']==1),'diabetes'] <- 1

# Townsend deprivation – 189.0.0
d[,'townsend_deprivation'] <- d[,'f.189.0.0']

# Time spend outdoors in summer/ winter – 1050.0.0; 1060.0.0
# time spend outdoors in summer
d[,'hours_outdoor'] <- d[,'f.1050.0.0']


print(summary(lm(astigmatism ~ age + gender + ethnicity + iop + asthma + atopy + crf + marfan + asthma_hospital + smokers + maternal_smoking + diabetes + hysteresis + cholesterol + breast_fed + high_bp + hours_outdoor + townsend_deprivation + educational_achievement + university, data=d)))

print(summary(lm(KC.prox1 ~ age + gender + ethnicity + iop + asthma + atopy + crf + marfan + asthma_hospital + smokers + maternal_smoking + diabetes + hysteresis + cholesterol + breast_fed + high_bp + hours_outdoor + townsend_deprivation + university, data=d)))

print(summary(lm(KC.prox2 ~ age + gender + ethnicity + iop + asthma + atopy + crf + marfan + asthma_hospital + smokers + maternal_smoking + diabetes + hysteresis + cholesterol + breast_fed + high_bp + hours_outdoor + townsend_deprivation + university, data=d)))

print(summary(lm(KC.prox3 ~ age + gender + ethnicity + iop + asthma + atopy + crf + marfan + asthma_hospital + smokers + maternal_smoking + diabetes + hysteresis + cholesterol + breast_fed + high_bp + hours_outdoor + townsend_deprivation + university, data=d)))

print(summary(lm(KC.prox4 ~ age + gender + ethnicity + iop + asthma + atopy + crf + marfan + asthma_hospital + smokers + maternal_smoking + diabetes + hysteresis + cholesterol + breast_fed + high_bp + hours_outdoor + townsend_deprivation + university, data=d)))


print(summary(lm(astigmatism ~ smokers, data=d)))
print(summary(lm(KC.prox1 ~ smokers, data=d)))
print(summary(lm(KC.prox2 ~ smokers, data=d)))
print(summary(lm(KC.prox3 ~ smokers, data=d)))
print(summary(lm(KC.prox4 ~ smokers, data=d)))


print(summary(lm(mean_corneal_power_right ~ age + gender + ethnicity + iop + asthma + atopy + crf + marfan + asthma_hospital + smokers + maternal_smoking + diabetes + hysteresis + cholesterol, data=d)))

print(summary(lm(mean_corneal_power_right ~ age + gender + ethnicity + iop + asthma + atopy + crf + marfan + asthma_hospital + smokers + maternal_smoking, data=d)))



## random forest prediction

library(randomForest)


classes <- sapply(1:ncol(d),function(i) { class(d[,i]) })

X <- d[,grep('numeric',classes)]

X$mean_corneal_power <- d$mean_corneal_power_right

x <- na.omit(cbind(mean_corneal_power=X$mean_corneal_power,X[,1:100]))


model <- randomForest(x$mean_corneal_power ~ ., data=x)


by(d$right_corneal_astigmatism, d$age.group, mean)
by(d$left_corneal_astigmatism, d$age.group, mean)

by(d$right_corneal_corrected_iop, d$age.group, mean, na.rm=TRUE)
by(d$left_corneal_corrected_iop, d$age.group, mean, na.rm=TRUE)


d1 <- d[ which(d$age.group=='40-44'),]
summary(lm(right_corneal_astigmatism ~ right_corneal_corrected_iop,d1))

d2 <- d[ which(d$age.group=='45-49'),]
summary(lm(right_corneal_astigmatism ~ right_corneal_corrected_iop,d2))

d1 <- d[ which(d$age==45),]
summary(lm(right_corneal_astigmatism ~ right_corneal_corrected_iop,d1))




