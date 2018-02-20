
library(data.table)

read <- function(...) as.data.frame(fread(header=TRUE,...))

rowMedian <- function(x,...) apply(x,1,median,...)

read('ukb6749.tab')->data1
read('ukb7618.tab')->data2
rownames(data1) <- data1$f.eid
rownames(data2) <- data2$f.eid
print(setdiff(data1$f.eid,data2$f.eid))
data1[rownames(data2),colnames(data2)]<-data2
data1 ->d 

data1[,'left_corneal_astigmatism'] <- data1[,'f.5135.0.0'] - data1[,'f.5096.0.0']
data1[,'yob'] <- data1[,'f.34.0.0']
data1[,'yoa1'] <- as.numeric(unlist(lapply(strsplit( (data1[,'f.53.0.0']), '-'),'[[',1)))
data1['age'] <- data1[,'yoa1']-data1[,'yob']

d <- d[,-which(colSums(is.na(d))>(nrow(d)-4))]

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

# age
# Age (2010) 2010 minus 34-0.0 
# year of birth
d[,'yob'] <- d[,'f.34.0.0']
# month of birth
d[,'mob'] <- d[,'f.52.0.0']


lvl.0008 <- c(1,2,3,4,5,6,7,8,9,10,11,12)
lbl.0008 <- c("January","February","March","April","May","June","July","August","September","October","November","December")
d$f.52.0.0 <- ordered(d$f.52.0.0, levels=lvl.0008, labels=lbl.0008)

print(t(t(table(d$f.52.0.0))))

# date of assessment
# year of assesment
d[,'yoa1'] <- as.numeric(unlist(lapply(strsplit( (d[,'f.53.0.0']), '-'),'[[',1)))
d[,'yoa2'] <- as.numeric(unlist(lapply(strsplit( (d[,'f.53.1.0']), '-'),'[[',1)))
d[,'yoa3'] <- as.numeric(unlist(lapply(strsplit( (d[,'f.53.2.0']), '-'),'[[',1)))

# age at assessment 1
d[,'age1'] <- d[,'yoa1']-d[,'yob']
# age at assessment 2
d[,'age2'] <- d[,'yoa2']-d[,'yob']
# age at assessment 3
d[,'age3'] <- d[,'yoa3']-d[,'yob']

d[,'age']<-d[,'age1']


d[,'gender'] <- d[,'f.31.0.0']
d[,'iop'] <- d[,'f.5255.0.0']
#
d[,'left_corrected_visual_acuity'] <- d[,'f.5208.0.0']
d[,'right_corrected_visual_acuity'] <- d[,'f.5201.0.0']
# Corneal resistance factor
d[,'crf'] <- d[,'f.5257.0.0']


# LEFT
# 3mm weak meridian (left)
# visit 0 (2006-2010) astigmatism
d$f.5096.0 <- rowMeans(d[,c('f.5096.0.0','f.5096.0.1','f.5096.0.2','f.5096.0.3','f.5096.0.4','f.5096.0.5')],na.rm=TRUE)
# visit 1 (2012-2013) astigmatism
d$f.5096.1 <- rowMeans(d[,c('f.5096.1.0','f.5096.1.1','f.5096.1.2','f.5096.1.3','f.5096.1.4','f.5096.1.5')],na.rm=TRUE)
# 3mm weak meridian (left)
d$f.5096 <- ifelse(is.na(d$f.5096.0), d$f.5096.1, d$f.5096.0)
# 3mm strong meridian (left): visit 0 (2006-2010) astigmatism
d$f.5135.0 <- rowMeans(d[,c("f.5135.0.0", "f.5135.0.1", "f.5135.0.2", "f.5135.0.3", "f.5135.0.4", "f.5135.0.5")],na.rm=T)
# visit 1 (2012-2013) astigmatism
d$f.5135.1 <- rowMeans(d[,c("f.5135.1.0", "f.5135.1.1", "f.5135.1.2", "f.5135.1.3", "f.5135.1.4", "f.5135.1.5")],na.rm=T)
# 3mm strong meridian (left)
d$f.5135 <- ifelse(is.na(d$f.5135.0), d$f.5135.1, d$f.5135.0)
# Astigmatism 3mm left =  3mm strong meridian (left) minus  3mm weak meridian (left) 
# 3mm astigmatism (left) = 3mm strong meridian (left) -  3mm weak meridian (left)
d[,'left_astigmatism'] <- d[,'f.5135'] - d[,'f.5096']
print(dim(d <- d[which(d$left_astigmatism>0),]))


# RIGHT
# 3mm weak meridian (right)
# visit 0 (2006-2010) astigmatism
d$f.5132.0 <- rowMeans(d[,c('f.5132.0.0','f.5132.0.1','f.5132.0.2','f.5132.0.3','f.5132.0.4','f.5132.0.5')],na.rm=TRUE)
# visit 1 (2012-2013) astigmatism
d$f.5132.1 <- rowMeans(d[,c('f.5132.1.0','f.5132.1.1','f.5132.1.2','f.5132.1.3','f.5132.1.4','f.5132.1.5')],na.rm=TRUE)
# 3mm weak meridian (right)
d$f.5132 <- ifelse(is.na(d$f.5132.0), d$f.5132.1, d$f.5132.0)
# 3mm strong meridian (right): visit 0 (2006-2010) astigmatism
d$f.5099.0 <- rowMeans(d[,c("f.5099.0.0", "f.5099.0.1", "f.5099.0.2", "f.5099.0.3", "f.5099.0.4", "f.5099.0.5")],na.rm=T)
# visit 1 (2012-2013) astigmatism
d$f.5099.1 <- rowMeans(d[,c("f.5099.1.0", "f.5099.1.1", "f.5099.1.2", "f.5099.1.3", "f.5099.1.4", "f.5099.1.5")],na.rm=T)
# 3mm strong meridian (right)
d$f.5099 <- ifelse(is.na(d$f.5099.0), d$f.5099.1, d$f.5099.0)
# Astigmatism defined as:
# Astigmatism 3mm right =  3mm strong meridian (right) 5132-0.0 minus  3mm weak meridian (right) 5099-0.0 ie. ((5132-0.0) – (5099-0.0))  
# 3mm astigmatism (right) = 3mm strong meridian (right) -  3mm weak meridian (right)
d[,'right_astigmatism'] <- d[,'f.5132'] - d[,'f.5099']
prim(dim(d <- d[which(d$right_astigmatism>0),]))


#thresh <- quantile(abs(d$right_astigmatism-d$left_astigmatism),probs=seq(0,1,.05))[['90%']]
#d <- d[which(abs(d$right_astigmatism-d$left_astigmatism)<thresh),]

d[,'height'] <- d[,'f.50.0.0']
d[,'weight'] <- d[,'f.23098.0.0']

# sites
SITES <- read('sites.csv')
rownames(SITES) <- SITES$id
d$sites <- SITES[as.character(d$f.54.0.0),'name']
table( d$sites_code <- factor(d$sites, labels=names(sort(by(d$left_astigmatism,d$sites,mean),decreasing=TRUE))))
print(t(t(table(d$sites_code <- as.numeric(d$sites_code)))))
print(t(t(table(d$sites))))


# age completed full-time education
# -2 represents "Never went to school"
# -1 represents "Do not know"
# -3 represents "Prefer not to answer" 
d$age_completed_education <- d[,'f.845.0.0']
d[which(d$age_completed_education==-2),'age_completed_education'] <- 0
d[which(d$age_completed_education==-1),'age_completed_education'] <- NA
d[which(d$age_completed_education==-3),'age_completed_education'] <- NA

bd <- d
# cholesterol medication
lvl.100626 <- c(-7,-3,-1,1,2,3,4,5)
lbl.100626 <- c("None of the above","Prefer not to answer","Do not know","Cholesterol lowering medication","Blood pressure medication","Insulin","Hormone replacement therapy","Oral contraceptive pill or minipill")
d$f.6153.0.0 <- ordered(bd$f.6153.0.0, levels=lvl.100626, labels=lbl.100626)
d$f.6153.0.1 <- ordered(bd$f.6153.0.1, levels=lvl.100626, labels=lbl.100626)
d$f.6153.0.2 <- ordered(bd$f.6153.0.2, levels=lvl.100626, labels=lbl.100626)
d$f.6153.0.3 <- ordered(bd$f.6153.0.3, levels=lvl.100626, labels=lbl.100626)
d$f.6153.1.0 <- ordered(bd$f.6153.1.0, levels=lvl.100626, labels=lbl.100626)
d$f.6153.1.1 <- ordered(bd$f.6153.1.1, levels=lvl.100626, labels=lbl.100626)
d$f.6153.1.2 <- ordered(bd$f.6153.1.2, levels=lvl.100626, labels=lbl.100626)
#bd$f.6153.1.3 <- ordered(bd$f.6153.1.3, levels=lvl.100626, labels=lbl.100626)
d$f.6153.2.0 <- ordered(bd$f.6153.2.0, levels=lvl.100626, labels=lbl.100626)
d$f.6153.2.1 <- ordered(bd$f.6153.2.1, levels=lvl.100626, labels=lbl.100626)
d$f.6153.2.2 <- ordered(bd$f.6153.2.2, levels=lvl.100626, labels=lbl.100626)
#bd$f.6153.2.3 <- ordered(bd$f.6153.2.3, levels=lvl.100626, labels=lbl.100626)

# Systolic blood pressure
d$f.4080.0 <- rowMeans(d[,c("f.4080.0.0", "f.4080.0.1")],na.rm=TRUE)
d$f.4080.1 <- rowMeans(d[,c("f.4080.1.0", "f.4080.1.1")],na.rm=TRUE)
d$f.4080.2 <- rowMeans(d[,c("f.4080.2.0", "f.4080.2.1")],na.rm=TRUE)
d$f.4080 <- rowMedian(d[,c('f.4080.0','f.4080.1','f.4080.2')],na.rm=TRUE)

# Diastolic blood pressure, automated reading
d$f.4079.0 <- rowMeans(d[,c("f.4079.0.0", "f.4079.0.1")],na.rm=TRUE)
d$f.4079.1 <- rowMeans(d[,c("f.4079.1.0", "f.4079.1.1")], na.rm=TRUE)
d$f.4079.2 <- rowMeans(d[,c("f.4079.2.0", "f.4079.2.1")],na.rm=TRUE)
d$f.4079 <- rowMedian(d[,c('f.4079.0','f.4079.1','f.4079.2')],na.rm=TRUE)

# Gender
lvl.0009 <- c(0,1)
lbl.0009 <- c("Female","Male")
d$f.31.0.0 <- ordered(bd$f.31.0.0, levels=lvl.0009, labels=lbl.0009)

# Ethnicity
lvl.1001 <- c(-3,-1,1,2,3,4,5,6,1001,1002,1003,2001,2002,2003,2004,3001,3002,3003,3004,4001,4002,4003)
lbl.1001 <- c("Prefer not to answer","Do not know","White","Mixed","Asian or Asian British","Black or Black British","Chinese","Other ethnic group","British","Irish","Any other white background","White and Black Caribbean","White and Black African","White and Asian","Any other mixed background","Indian","Pakistani","Bangladeshi","Any other Asian background","Caribbean","African","Any other Black background")
print(table(bd$f.21000.0.0 <- ordered(bd$f.21000.0.0, levels=lvl.1001, labels=lbl.1001)))
print(table(bd$f.21000.1.0 <- ordered(bd$f.21000.1.0, levels=lvl.1001, labels=lbl.1001)))
print(table(bd$f.21000.2.0 <- ordered(bd$f.21000.2.0, levels=lvl.1001, labels=lbl.1001)))
print(table(bd$f.22001.0.0 <- ordered(bd$f.22001.0.0, levels=lvl.0009, labels=lbl.0009)))
lvl.1002 <- c(1)
lbl.1002 <- c("Caucasian")
print(table(bd$f.22006.0.0 <- ordered(bd$f.22006.0.0, levels=lvl.1002, labels=lbl.1002)))

#ethnicity
white <- grep('^1', as.character(d[,'f.21000.0.0']))
mixed <- grep('^2', as.character(d[,'f.21000.0.0']))
asian <- grep('^3', as.character(d[,'f.21000.0.0']))
black <- grep('^4', as.character(d[,'f.21000.0.0']))
chinese <- grep('^5', as.character(d[,'f.21000.0.0']))
d[white,'ethnicity'] <- 'white'
d[mixed,'ethnicity'] <- 'mixed'
d[asian,'ethnicity'] <- 'asian'
d[black,'ethnicity'] <- 'black'
d[chinese,'ethnicity'] <- 'chinese'
table( d$ethnicity_code <- factor(d$ethnicity, labels=names(sort(by(d$left_astigmatism,d$ethnicity,mean),decreasing=TRUE))))
print(t(t(table(d$ethnicity_code <- as.numeric(d$ethnicity_code)))))
print(t(t(table(d$ethnicity))))

# corneal hysterisis
# Corneal Hysteresis (CH) is an assessment of the cornea's ability to absorb and dissipate energy.
#This is very different from thickness or topography, which are geometrical attributes of the cornea.
# Corneal Hysteresis is independently predictive of visual field progression in glaucoma.
# right
d$corneal_hysterisis_right<- ifelse(!is.na(d$f.5256.0.0),d$f.5256.0.0,d$f.5256.1.0)
# left
d$corneal_hysterisis_left <- ifelse(!is.na(d$f.5264.0.0),d$f.5264.0.0,d$f.5264.1.0)

# Goldman corrected IOP
# left
d$left_goldman_corrected_iop <- ifelse(!is.na(d[,'f.5263.0.0']),d$f.5263.0.0,d$f.5263.1.0)
# right
d$right_goldman_corrected_iop <- ifelse(!is.na(d[,'f.5255.0.0']),d$f.5255.0.0,d$f.5255.1.0)


# Corneal compensated IOP
# left
d$left_corneal_corrected_iop <- ifelse(!is.na(d[,'f.5262.0.0']),d$f.5262.0.0,d$f.5262.1.0)
# right
d$right_corneal_corrected_iop <- ifelse(!is.na(d[,'f.5254.0.0']),d$f.5254.0.0,d$f.5254.1.0)


#d$f.6119
#"f.6119.0.0" "f.6119.1.0" "f.6119.2.0"

# Body fat percentage
d$body_fat_percentage <- ifelse(!is.na(d$f.23099.0.0), d$f.23099.0.0, d$f.23099.1.0)


# Smoking
# Current tobacco smoking
d$current_smoking <- ifelse(!is.na(d$f.1239.0.0), d$f.1239.0.0, d$f.1239.1.0)


print(dim(d))


d$left_corneal_astigmatism <- d$left_astigmatism
d$right_corneal_astigmatism <- d$right_astigmatism




