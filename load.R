# Index of best refractometry result

library(data.table)

read <- function(...) as.data.frame(fread(header=TRUE,...))

rowMedian <- function(x,...) apply(x,1,median,...)

read('ukb6749.tab')->data1
read('ukb7618.tab')->data2
rownames(data1) <- data1$f.eid
rownames(data2) <- data2$f.eid
print(setdiff(data1$f.eid,data2$f.eid))
data1[rownames(data2),colnames(data2)]<-data2

data1[,'yob'] <- data1[,'f.34.0.0']
data1[,'yoa1'] <- as.numeric(unlist(lapply(strsplit(data1[,'f.53.0.0'], '-'),'[[',1)))
data1['age'] <- data1[,'yoa1']-data1[,'yob']
data1 ->d 

count_ukbb_keratometry <- length(which(!is.na(data1$f.5132.0.0)))

count_ukbb_participants <- ukbb_total_number_of_participants <- nrow(data1)
count_ukbb_keratoconus <- length(grep('H186',as.character(data1$f.41202.0.0)))

count_keratoconus <- length(grep('H186',as.character(d$f.41202.0.0)))

sum(sapply(grep('f.41202.0',colnames(data1),value=TRUE), function(col){length(which(data1[,col]=='H185'))}))

d <- d[,-which(colSums(is.na(d))>(nrow(d)-4))]

# 6148	Eye problems/disorders
d$eye_problems <- d$f.6148.0.0
# no eye problems
#which(d$eye_problems==-7)

d$diagnoses <- d$f.41202.0.0

# 4700	Age cataract diagnosed
d$age.cataract.diagnosed <- d$f.4700.0.0

# 3mm weak meridian (right)
# visit 0 (2006-2010) astigmatism

# visit 1 (2012-2013) astigmatism
#d$f.5132.1 <- rowMeans(d[,c('f.5132.1.0','f.5132.1.1','f.5132.1.2','f.5132.1.3','f.5132.1.4','f.5132.1.5')],na.rm=TRUE)
# 3mm weak meridian (right)
#d$f.5132 <- ifelse(is.na(d$f.5132.0), d$f.5132.1, d$f.5132.0)
# 3mm strong meridian (right): visit 0 (2006-2010) astigmatism
#d$f.5099.0 <- rowMeans(d[,c("f.5099.0.0", "f.5099.0.1", "f.5099.0.2", "f.5099.0.3", "f.5099.0.4", "f.5099.0.5")],na.rm=T)
# visit 1 (2012-2013) astigmatism
#d$f.5099.1 <- rowMeans(d[,c("f.5099.1.0", "f.5099.1.1", "f.5099.1.2", "f.5099.1.3", "f.5099.1.4", "f.5099.1.5")],na.rm=T)
# 3mm strong meridian (right)
#d$f.5099 <- ifelse(is.na(d$f.5099.0), d$f.5099.1, d$f.5099.0)
# Astigmatism defined as:
# Astigmatism 3mm right =  3mm strong meridian (right) 5132-0.0 minus  3mm weak meridian (right) 5099-0.0 ie. ((5132-0.0) – (5099-0.0))  
#print(dim(d <- d[which(d$right_corneal_astigmatism>0),]))

# 3mm strong meridian (left): visit 0 (2006-2010) astigmatism
# array indices run from 0 to 5
d$f.5135.0 <- rowMeans(d[,c("f.5135.0.0", "f.5135.0.1", "f.5135.0.2", "f.5135.0.3", "f.5135.0.4", "f.5135.0.5")],na.rm=T)
# visit 1 (2012-2013) astigmatism
#d$f.5135.1 <- rowMeans(d[,c("f.5135.1.0", "f.5135.1.1", "f.5135.1.2", "f.5135.1.3", "f.5135.1.4", "f.5135.1.5")],na.rm=T)
# Use whatever visit is available.
#d$left_3mm_strong_meridian <- ifelse(is.na(d$f.5135.0), d$f.5135.1, d$f.5135.0)


# visit 0 (2006-2010) astigmatism . Average measurements per visit for increased accuracy.
#d$f.5096.0 <- rowMeans(d[,c('f.5096.0.0','f.5096.0.1','f.5096.0.2','f.5096.0.3','f.5096.0.4','f.5096.0.5')],na.rm=TRUE)
# visit 1 (2012-2013) astigmatism . Average measurements per visit for increased accuracy.
#d$f.5096.1 <- rowMeans(d[,c('f.5096.1.0','f.5096.1.1','f.5096.1.2','f.5096.1.3','f.5096.1.4','f.5096.1.5')],na.rm=TRUE)
# Use whatever visit is available.
#d$left_3mm_weak_meridian <- ifelse(is.na(d$f.5096.0), d$f.5096.1, d$f.5096.0)


# 5111	3mm asymmetry angle (left)
# 5108	3mm asymmetry angle (right)
d$left_3mm_asymmetry_angle <- d$f.5111.0.0
d$right_3mm_asymmetry_angle <- d$f.5108.0.0


# 3mm cylindrical power
d$right_3mm_cylindrical_power <- d$f.5116.0.0
d$left_3mm_cylindrical_power <- d$f.5119.0.0
# 3mm cylindrical power angle
d$left_3mm_cylindrical_power_angle <- d$f.5112.0.0
d$right_3mm_cylindrical_power_angle <- d$f.5115.0.0
# Spherical power 
d$right_spherical_power <- d$f.5084.0.0
d$left_spherical_power <- d$f.5085.0.0
# Vertex distance
d$right_vertex_distance <- d$f.5215.0.0
d$left_vertex_distance <- d$f.5274.0.0


# 4689	Age glaucoma diagnosed
d$age_glaucoma_diagnosed <- d$f.4689.0.0
# 5430	Age when loss of vision due to injury or trauma diagnosed
# 4700	Age cataract diagnosed
# 6119	Which eye(s) affected by glaucoma
d$glaucoma <- d$f.6119.0.0


d$right_mean_corneal_power <- (d[,'right_3mm_strong_meridian'] + d[,'right_3mm_weak_meridian'])/2
d$left_mean_corneal_power <- (d[,'left_3mm_strong_meridian'] + d[,'left_3mm_weak_meridian'])/2
d$right_3mm_mean_corneal_power <- (d$right_3mm_strong_meridian + d$right_3mm_weak_meridian)/2
d$left_3mm_mean_corneal_power <- (d$left_3mm_strong_meridian + d$left_3mm_weak_meridian)/2


d$right_spherical_power <- d$f.5084.0.0
d$left_spherical_power <- d$f.5085.0.0
d$right_3mm_spherical_equivalent <- d$right_spherical_equivalent <- d$right_spherical_power + .5 * d$right_3mm_cylindrical_power
d$left_3mm_spherical_equivalent <- d$left_spherical_equivalent <- d$left_spherical_power + .5 * d$left_3mm_cylindrical_power


# axis of astigmatism
#5107 3mm strong meridian angle (right) #5100 3mm weak meridian angle (right)
d$right_3mm_strong_meridian_angle<-d$f.5107.0.0
d$right_3mm_weak_meridian_angle<-d$f.5100.0.0
d$right_axis_of_astigmatism <- d$right_3mm_strong_meridian_angle-d$right_3mm_weak_meridian_angle
#5104 3mm strong meridian angle (left) #5103 3mm weak meridian angle (left)
d$left_3mm_strong_meridian_angle<-d$f.5104.0.0
d$left_3mm_weak_meridian_angle<-d$f.5103.0.0
d$left_axis_of_astigmatism <- d$left_3mm_strong_meridian_angle-d$left_3mm_weak_meridian_angle

d$pos_right_axis_of_astigmatism <- d$right_axis_of_astigmatism>0
d$pos_left_axis_of_astigmatism <- d$left_axis_of_astigmatism>0

cat('Participants in UKBB:\n')
print(dim(d))

i <- which(!is.na(d$left_corneal_astigmatism)&!is.na(d$right_corneal_astigmatism))
cat('Participants with astigmatism measures available:\n')
print(dim(d <- d[i,]))

d[,'right_KC_proxy1'] <- 0
d[which(d[,'right_mean_corneal_power']>48&d$right_3mm_asymmetry_index_for_irregular_astigmatism_level>1) ,'right_KC_proxy1'] <- 1
d[,'right_KC_proxy2'] <- 0
d[which(d[,'right_mean_corneal_power']>49&d$right_3mm_asymmetry_index_for_irregular_astigmatism_level>1),'right_KC_proxy2'] <- 1
d[,'right_KC_proxy3'] <- 0
d[which(d[,'right_mean_corneal_power']>50&d$right_3mm_asymmetry_index_for_irregular_astigmatism_level>1),'right_KC_proxy3'] <- 1
count_right_KC_proxy1 <- length(which(d$right_KC_proxy1==1))
pct_right_KC_proxy1 <- 100*count_right_KC_proxy1/nrow(d)
count_right_KC_proxy2 <- length(which(d$right_KC_proxy2==1))
pct_right_KC_proxy2 <- 100*count_right_KC_proxy2/nrow(d)
count_right_KC_proxy3 <- length(which(d$right_KC_proxy3==1))
pct_right_KC_proxy3 <- 100*count_right_KC_proxy3/nrow(d)

d[,'left_KC_proxy1'] <- 0
d[which(d[,'left_mean_corneal_power']>48&d$left_3mm_asymmetry_index_for_irregular_astigmatism_level>1) ,'left_KC_proxy1'] <- 1
d[,'left_KC_proxy2'] <- 0
d[which(d[,'left_mean_corneal_power']>49&d$left_3mm_asymmetry_index_for_irregular_astigmatism_level>1),'left_KC_proxy2'] <- 1
d[,'left_KC_proxy3'] <- 0
d[which(d[,'left_mean_corneal_power']>50&d$left_3mm_asymmetry_index_for_irregular_astigmatism_level>1),'left_KC_proxy3'] <- 1

both_KC_proxy1_count <- length(which(d$right_KC_proxy1&d$left_KC_proxy1))
both_KC_proxy2_count <- length(which(d$right_KC_proxy2&d$left_KC_proxy2))
both_KC_proxy3_count <- length(which(d$right_KC_proxy3&d$left_KC_proxy3))

d.KC_proxy3 <- d[which(d$right_KC_proxy3&d$left_KC_proxy3),]

both_ukbb_keratoconus <- d.keratoconus <- d[grep('H186',as.character(d$f.41202.0.0)),]

#d.keratoconus$f.41202.0.0
multivariable_townsend_pvalue=0

# EXCLUSION criteria:  
cat('EXCLUSION criteria:\n')

#print(dim(d <- d[which(d$left_corneal_astigmatism>0),]))
#d <- d[i,]
cat('Included/excluded:\n')

# Ever had glaucoma surgery 5326-0.0 
#d <- d[which(is.na(d[,'f.5326.0.0'])),]
# Ever had corneal graft 5328-0.0 
#d <- d[which(is.na(d[,'f.5328.0.0'])),]
# Loss vision trauma 5430-0.0 
#d <- d[which(is.na(d[,'f.5430.0.0'])),]

# Ever had laser refractive 5325-0.0 
i1 <- which(!is.na(d[,'f.5325.0.0']))
laser_refractive_count <- length(i1)
cat('Excluded because of laser refractive:', laser_refractive_count,'\n')
# Ever had eye surgery 5181-0.0 
i2 <- which(!is.na(d[,'f.5181.0.0'])&d[,'f.5181.0.0']!=0)
eye_surgery_count <- length(i2)
cat('Excluded because of eye surgery:', eye_surgery_count,'\n')
# 3mm asymmetry index unreliable (left)
i3 <- which(!is.na(d[,'f.5141.0.0']))
asymmetry_index_unreliable_left_count <- length(i3)
cat('Excluded because of unreliable left index:', asymmetry_index_unreliable_left_count,'\n')
# 3mm asymmetry index unreliable (right)
i4 <- which(!is.na(d[,'f.5144.0.0']))
asymmetry_index_unreliable_right_count <- length(i4)
cat('Excluded because of unreliable right index:', asymmetry_index_unreliable_right_count,'\n')
# 3mm keratometry result unreliable (left)
i5 <- which(!is.na(d[,'f.5136.0.0']))
keratometry_unreliable_left_count <- length(i5)
cat('Excluded because of unreliable left keratometry value:', keratometry_unreliable_left_count,'\n')
# 3mm keratometry result unreliable (right)
i6 <- which(!is.na(d[,'f.5140.0.0']))
keratometry_unreliable_right_count <- length(i6)
cat('Excluded because of unreliable right keratometry value:', keratometry_unreliable_right_count,'\n')
i7 <- which((d$left_3mm_strong_meridian-d$left_3mm_weak_meridian<=0)|(d$right_3mm_strong_meridian-d$right_3mm_weak_meridian<=0))
zero_astigmatism_count <- length(i7)
cat('Excluded because of zero corneal astigmatism:', zero_astigmatism_count,'\n')

keratometry_unreliable_count <- keratometry_unreliable_left_count+keratometry_unreliable_right_count
asymmetry_index_unreliable_count <- asymmetry_index_unreliable_left_count+asymmetry_index_unreliable_right_count

cat('Before participant selection:\n')
print(dim(d))
cat('After participant selection:\n')
print(length(i <- unique(c(i1,i2,i3,i4,i5,i6,i7))))
print(dim(d <- d[-i,]))

# unreliable keratometry result
d$left_3mm_keratometry_result_unreliable <- d[,'f.5136.0.0']
d$right_3mm_keratometry_result_unreliable <- d[,'f.5140.0.0']


# EXCLUDE:  

# Ever had eye surgery 5181-0.0 
# Ever had cataract surgery 5324-0.0 
# Ever had laser refractive 5325-0.0 
# Ever had glaucoma surgery 5326-0.0 
# Ever had corneal graft 5328-0.0 
# Loss vision trauma 5430-0.0 

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

d$month_of_birth <- as.character( d$f.52.0.0 )

# season of birth
d$season_of_birth <- ''
# winter
d[which(d$month_of_birth %in% c("December","January","February")),'season_of_birth'] <- 'winter'
# spring
d[which(d$month_of_birth %in% c('March','April','May')),'season_of_birth']<-'spring'
# summer
d[which(d$month_of_birth %in% c("June","July","August")),'season_of_birth']<-'summer'
# autum
d[which(d$month_of_birth %in% c("September","October","November")),'season_of_birth']<-'autumn'


# date of assessment
# year of assesment
d[,'yoa1'] <- as.numeric(unlist(lapply(strsplit( (d[,'f.53.0.0']), '-'),'[[',1)))
d[,'yoa2'] <- as.numeric(unlist(lapply(strsplit( (d[,'f.53.1.0']), '-'),'[[',1)))
d[,'yoa3'] <- as.numeric(unlist(lapply(strsplit( (d[,'f.53.2.0']), '-'),'[[',1)))

# season of test
#d$f.53.0.0
d[,'moa1'] <- as.numeric(unlist(lapply(strsplit( (d[,'f.53.0.0']), '-'),'[[',2)))
# winter
d[which(d$moa1 %in% c(12,1,2)),'season_of_assessment'] <- 'winter'
# spring
d[which(d$moa1 %in% c(3,4,5)),'season_of_assessment']<-'spring'
# summer
d[which(d$moa1 %in% c(6,7,8)),'season_of_assessment']<-'summer'
# autum
d[which(d$moa1 %in% c(9,10,11)),'season_of_assessment']<-'autumn'


# age at assessment 1
d[,'age1'] <- d[,'yoa1']-d[,'yob']
# age at assessment 2
d[,'age2'] <- d[,'yoa2']-d[,'yob']
# age at assessment 3
d[,'age3'] <- d[,'yoa3']-d[,'yob']
d[,'age']<-d[,'age1']


d[,'gender'] <- d[,'f.31.0.0']
d$gender_code <- d$gender
d$gender <- as.character(d$gender)
d[which(d$gender=='0'),'gender'] <- 'F'
d[which(d$gender=='1'),'gender'] <- 'M'
d$gender <- as.factor(d[,'gender'])

d[,'right_iop_gc'] <- d[,'f.5255.0.0']
#
d[,'left_corrected_visual_acuity'] <- d[,'f.5208.0.0']
d[,'right_corrected_visual_acuity'] <- d[,'f.5201.0.0']

# Corneal resistance factor
d$left_corneal_resistance_factor <- d$f.5265.0.0
d$right_corneal_resistance_factor <-  d$f.5257.0.0

#thresh <- quantile(abs(d$right_corneal_astigmatism-d$left_corneal_astigmatism),probs=seq(0,1,.05))[['90%']]
#d <- d[which(abs(d$right_corneal_astigmatism-d$left_corneal_astigmatism)<thresh),]

d[,'height'] <- d[,'f.50.0.0']
d[,'weight'] <- d[,'f.23098.0.0']

# assessment centre
# sites
SITES <- read('sites.csv')
rownames(SITES) <- SITES$id
d$sites <- SITES[as.character(d$f.54.0.0),'name']
table( d$sites_code <- factor(d$sites, labels=names(sort(by(d$left_corneal_astigmatism,d$sites,mean,na.rm=TRUE),decreasing=TRUE,na.last=TRUE))))
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


# None of the above -7 149217
# Prefer not to answer -3
# Do not know -1
# Insulin 3
# Blood pressure medication 2
# Cholesterol lowering medication 1
d$medication <- d$f.6153.0.0 

d$insulin <- 0
d$insulin[which(d$medication=='Insulin')] <- 1


# "Cholesterol lowering medication","Blood pressure medication","Insulin","Hormone replacement therapy","Oral contraceptive pill or minipill")
d$medication[ which(d$f.6153.0.0 %in% c("Prefer not to answer","Do not know")) ] <- NA


#c( "None of the above", "Prefer not to answer", "Do not know")

#'Blood pressure medication'
d$blood_pressure_medication <- 0
d$blood_pressure_medication[which(d$medication=="Blood pressure medication")] <- 1
#'Cholesterol lowering medication'
d$cholesterol_lowering_medication <- 0
d$cholesterol_lowering_medication[which(d$medication=="Cholesterol lowering medication")] <- 1
#'Hormone replacement therapy'
d$hormone_replacement_therapy <- 0
d$hormone_replacement_therapy[which(d$medication=="Hormone replacement therapy")] <- 1
#"Cholesterol lowering medication"
d$cholesterol_lowering_medication <- 0
d$cholesterol_lowering_medication[which(d$medication=="Cholesterol lowering medication")] <- 1
# Oral contraception
d$oral_contraception <-0 
d$oral_contraception[which(d$medication=="Oral contraceptive pill or minipill")] <- 1



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



print(pct_white<-round(100*length(which(d$ethnicity=='white'))/nrow(d)),2)
print(pct_asian<-round(100*length(which(d$ethnicity=='asian'))/nrow(d),2))
print(pct_black<-round(100*length(which(d$ethnicity=='black'))/nrow(d),2))
print(pct_mixed<-round(100*length(which(d$ethnicity=='mixed'))/nrow(d),2))
print(pct_chinese<-round(100*length(which(d$ethnicity=='chinese'))/nrow(d),2))

sort(by(d$left_corneal_astigmatism,d$ethnicity,mean),decreasing=FALSE)
sort(by(d$right_corneal_astigmatism,d$ethnicity,mean),decreasing=FALSE)

# corneal hysterisis
# Corneal Hysteresis (CH) is an assessment of the cornea's ability to absorb and dissipate energy.
#This is very different from thickness or topography, which are geometrical attributes of the cornea.
# Corneal Hysteresis is independently predictive of visual field progression in glaucoma.
# right
d$right_corneal_hysteresis <- ifelse(!is.na(d$f.5256.0.0),d$f.5256.0.0,d$f.5256.1.0)
# left
d$left_corneal_hysteresis <- ifelse(!is.na(d$f.5264.0.0),d$f.5264.0.0,d$f.5264.1.0)

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
d$current_smoking[which(d$current_smoking==-3)] <- NA

d$current_smoking_status <- NA
# occasional
d$current_smoking_status[ which(d$current_smoking==2) ] <- 'occasional'
# never
d$current_smoking_status[ which(d$current_smoking==0) ] <- 'never'
# regular
d$current_smoking_status[ which(d$current_smoking==1) ] <- 'regular'

d$current_smoking_code <- d$current_smoking
d$current_smoking_code[which(d$current_smoking==0)] <- 0
d$current_smoking_code[which(d$current_smoking==1)] <- -2
d$current_smoking_code[which(d$current_smoking==2)] <- 1
d$current_smoking_code[which(d$current_smoking==-2)] <- 2

print(dim(d))

mean(d$left_corneal_resistance_factor,na.rm=TRUE)
sd(d$left_corneal_resistance_factor,na.rm=TRUE)
mean(d$right_corneal_resistance_factor,na.rm=TRUE)
sd(d$right_corneal_resistance_factor,na.rm=TRUE)


# Skin colour (baseline = fair or very fair)
d$skin_colour <- d$f.1717.0.0
# Prefer not to answer  -3    685
d$skin_colour[which(d$skin_colour==-3)] <- NA
# Do not know -1   6662
d$skin_colour[which(d$skin_colour==-1)] <- NA
# Very fair 1   38559
# Fair 2  336107
# Light olive 3   91689
# Dark olive 4    9254
# Brown 5   14848
# Black 6    3938
d$skin_colour_code <- d$skin_colour
d$skin_colour[which(d$skin_colour==1)] <- 'very fair'
d$skin_colour[which(d$skin_colour==2)] <- 'fair'
d$skin_colour[which(d$skin_colour==3)] <- 'light olive'
d$skin_colour[which(d$skin_colour==4)] <- 'dark olive'
d$skin_colour[which(d$skin_colour==5)] <- 'brown'
d$skin_colour[which(d$skin_colour==6)] <- 'black'

# Hair colour (baseline = blonde or red)
d$hair_colour <- d$f.1747.0.0
# Prefer not to say -3    287
d$hair_colour[which(d$hair_colour==-3)] <- NA
# Do not know -1    946
d$hair_colour[which(d$hair_colour==-1)] <- NA
# Blonde 1   53135
# Red 2   21485
# Light brown 3  192321
# Dark brown 4  186189
# Black 5   41174
# Other 6    6205
d$hair_colour_code <- d$hair_colour
d$hair_colour[which(d$hair_colour==1)] <- 'blonde'
d$hair_colour[which(d$hair_colour==2)] <- 'red'
d$hair_colour[which(d$hair_colour==3)] <- 'light brown'
d$hair_colour[which(d$hair_colour==4)] <- 'dark brown'
d$hair_colour[which(d$hair_colour==5)] <- 'black'
d$hair_colour[which(d$hair_colour==6)] <- 'other'

# Use of UV protection (baseline = never/ rarely)
d$uv_protection <- d$f.2267.0.0
# Prefer not to answer -3    536
d$uv_protection[which(d$uv_protection==-3)] <- NA
# Do not know -1    644
d$uv_protection[which(d$uv_protection==-1)] <- NA
# Never/rarely 1   50899
# Sometimes 2  164986
# Most of the time 3  175143
# Always 4  102655
# Do not go out in sunshine 5    3083
d$uv_protection[which(d$uv_protection==5)] <- 0
d$uv_protection_code <- d$uv_protection
d$uv_protection_code[which(d$uv_protection_code==0)] <- NA
d$uv_protection[which(d$uv_protection==0)] <- 'Do not go out in sunshine'
d$uv_protection[which(d$uv_protection==1)] <- 'Never/rarely'
d$uv_protection[which(d$uv_protection==2)] <- 'Sometimes'
d$uv_protection[which(d$uv_protection==3)] <- 'Most of the time'
d$uv_protection[which(d$uv_protection==4)] <- 'Always'

# deprivation index
d$townsend_deprivation_index <- d$f.189.0.0

# BMI
d$BMI <- d$f.23104.0.0

# SBP
d$SBP <- d$f.4080.0.0

# DBP
d$DBP <- d$f.4079.0.0

# diabetes
d$diabetes <- d$f.2443.0.0
d$diabetes[which(d$diabetes<0)] <- NA

# age asthma diagnosed
print(table(d$age_asthma_diagnosed <- d$f.3786.0.0))
d$age_asthma_diagnosed[which(d$age_asthma_diagnosed<0)] <- NA

cat('Gender distribution:\n')
print(round(100*prop.table(table(d$gender)),1))
count_males<-length(which(d$gender=='M'))
pct_males <- round(100*count_males/nrow(d))
cat('Ethnicity distribution:\n')
print(round(100*prop.table(table(d$ethnicity)),1))


# left KC proxy
d[,'left_KC_proxy1'] <- 0
d[which(d[,'left_mean_corneal_power']>=48) ,'left_KC_proxy1'] <- 1
d[,'left_KC_proxy2'] <- 0
d[which(d[,'left_mean_corneal_power']>=49),'left_KC_proxy2'] <- 1
d[,'left_KC_proxy3'] <- 0
d[which(d[,'left_mean_corneal_power']>=50),'left_KC_proxy3'] <- 1
d[,'left_KC_proxy4'] <- 0
d[which(d[,'left_mean_corneal_power']>=48&d[,'left_3mm_asymmetry_index_for_irregular_astigmatism_level']>1),'left_KC_proxy4'] <- 1
# right KC proxy
d[,'right_KC_proxy1'] <- 0
d[which(d[,'right_mean_corneal_power']>=48) ,'right_KC_proxy1'] <- 1
right_KC_proxy1_count <- length(which(d$right_KC_proxy1==1))
d[,'right_KC_proxy2'] <- 0
d[which(d[,'left_mean_corneal_power']>=49),'right_KC_proxy2'] <- 1
right_KC_proxy2_count <- length(which(d$right_KC_proxy2==1))
d[,'right_KC_proxy3'] <- 0
d[which(d[,'right_mean_corneal_power']>=50),'right_KC_proxy3'] <- 1
right_KC_proxy3_count <- length(which(d$right_KC_proxy3==1))
d[,'right_KC_proxy4'] <- 0
d[which(d[,'right_mean_corneal_power']>=48&d[,'right_3mm_asymmetry_index_for_irregular_astigmatism_level']>1),'right_KC_proxy4'] <- 1
right_KC_proxy4_count <- length(which(d$right_KC_proxy4==1))

# 3mm regularity index
print(table(d$left_3mm_regularity_index <- d$f.5163.0.0))
print(table(d$right_3mm_regularity_index <- d$f.5160.0.0))

#  3mm regularity index unreliable
print(table(d$left_3mm_regularity_index_unreliable <- d$f.5148.0.0))
print(table(d$right_3mm_regularity_index_unreliable <- d$f.5145.0.0))

# 3mm _asymmetry_ index for irregular astigmatism level
print(table(d$left_3mm_asymmetry_index_for_irregular_astigmatism_level <- d$f.5155.0.0))
print(table(d$right_3mm_asymmetry_index_for_irregular_astigmatism_level  <- d$f.5152.0.0))

# 3mm _regularity_ index for irregular astigmatism level
print(table(d$right_3mm_regularity_index_for_irregular_astigmatism <- d$f.5149.0.0))
print(table(d$left_3mm_regularity_index_for_irregular_astigmatism  <- d$f.5164.0.0))

table(d$right_3mm_asymmetry_index_for_irregular_astigmatism,d$right_3mm_regularity_index_for_irregular_astigmatism)
table(d$left_3mm_asymmetry_index_for_irregular_astigmatism,d$left_3mm_regularity_index_for_irregular_astigmatism)

d.right_irregular_astigmatism <- d[which( d$right_3mm_asymmetry_index_for_irregular_astigmatism==3 & d$right_3mm_regularity_index_for_irregular_astigmatism==3 ),]

d.left_irregular_astigmatism <- d[which( d$left_3mm_asymmetry_index_for_irregular_astigmatism==3 & d$left_3mm_regularity_index_for_irregular_astigmatism==3 ),]


cat('Right keratometry steep axis meridian >48D, >49D, >50D:\n')
print(table(d$right_KC_proxy1))
print(table(d$right_KC_proxy2))
print(table(d$right_KC_proxy3))
print(table(d$right_KC_proxy4))


d$left_astigmatism <- d$left_3mm_astigmatism
d$right_astigmatism <- d$right_3mm_astigmatism

# variables for the manuscript
print(total_number_of_participants <- nrow(d))
# number of individuals with a keratometry steep axis meridian
# greater than 48D, 49D and 50D diopters
print(right_3mm_strong_meridian_48D_count <- length(which(d$right_3mm_strong_meridian>48)))
print(right_3mm_strong_meridian_48D_prop <- round(100*right_3mm_strong_meridian_48D_count/nrow(d),1))
print(right_3mm_strong_meridian_49D_count <- length(which(d$right_3mm_strong_meridian>49)))
print(right_3mm_strong_meridian_49D_prop <- round(100*right_3mm_strong_meridian_49D_count/nrow(d),1))
print(right_3mm_strong_meridian_50D_count <- length(which(d$right_3mm_strong_meridian>50)))
print(right_3mm_strong_meridian_50D_prop <- round(100*right_3mm_strong_meridian_50D_count/nrow(d),1))
  
print(pct_greater_than_05D_right_3mm_corneal_astigmatism <- round(100*length(which(d$right_3mm_astigmatism>.5))/nrow(d)))
print(pct_greater_than_1D_right_3mm_corneal_astigmatism <- round(100*length(which(d$right_3mm_astigmatism>1))/nrow(d)))
print(pct_greater_than_15D_right_3mm_corneal_astigmatism <- round(100*length(which(d$right_3mm_astigmatism>1.5))/nrow(d)))
print(pct_greater_than_2D_right_3mm_corneal_astigmatism <- round(100*length(which(d$right_3mm_astigmatism>2))/nrow(d)))

print(pct_greater_than_05D_left_3mm_corneal_astigmatism <- round(100*length(which(d$left_3mm_astigmatism>.5))/nrow(d)))
print(pct_greater_than_1D_left_3mm_corneal_astigmatism <- round(100*length(which(d$left_3mm_astigmatism>1))/nrow(d)))
print(pct_greater_than_15D_left_3mm_corneal_astigmatism <- round(100*length(which(d$left_3mm_astigmatism>1.5))/nrow(d)))
print(pct_greater_than_2D_left_3mm_corneal_astigmatism <- round(100*length(which(d$left_3mm_astigmatism>2))/nrow(d)))



prop.table(table(d$right_3mm_astigmatism>.5))
prop.table(table(d$right_3mm_astigmatism>1))

# age category
# this is the standard age grouping used in most UKBB papers
d[which.min(d$age),'age.group']<-'40-44'
d[which(40<=d$age & d$age<=44),'age.group']<-'40-44'
d[which(45<=d$age & d$age<=49),'age.group']<-'45-49'
d[which(50<=d$age & d$age<=54),'age.group']<- '50-54'
d[which(55<=d$age & d$age<=59),'age.group']<- '55-59'
d[which(60<=d$age & d$age<=64),'age.group']<- '60-64'
d[which(65<=d$age),'age.group']<- '65+'



d$anisometropia <- as.numeric(abs(d$left_3mm_corneal_astigmatism-d$right_3mm_corneal_astigmatism)>2)

100*prop.table(table(d$anisometropia<1))

ukbb_median_corneal_astigmatism <- median(c(d$right_3mm_astigmatism,d$left_3mm_astigmatism))

barplot(table(cut(d$right_3mm_astigmatism,breaks = seq(floor(min(d$right_3mm_astigmatism)),ceiling(max(d$right_3mm_astigmatism)),.25))),las=2)

barplot(table(cut(d$right_spherical_equivalent,breaks = seq(floor(min(d$right_spherical_equivalent)),ceiling(max(d$right_spherical_equivalent)),.25))),las=2)



summary(lm((d$right_3mm_corneal_astigmatism) ~ d$myopia.eye  ))

summary(lm((d$right_3mm_corneal_astigmatism) ~ d$hypermetropia.eye + d$age ))

summary(lm((d$hypermetropia.eye)))

boxplot(d$rig)

