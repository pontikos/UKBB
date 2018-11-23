# Field ID	Description
#2207	Wears glasses or contact lenses
#1	Yes
#0	No
#-3	Prefer not to answer
d$wears_glasses_or_contact_lenses <- d$f.2207.0.0
d$wears_glasses_or_contact_lenses[which(d$wears_glasses_or_contact_lenses==-3)] <- NA
#2217	Age started wearing glasses or contact lenses
d$age_started_wearing_glasses_or_contact_lenses <- d$f.2217.0.0
d$age_started_wearing_glasses_or_contact_lenses.lt.30 <- as.numeric(d$f.2217.0.0<30)
#6147	Reason for glasses/contact lenses
#1	For short-sightedness, i.e. only or mainly for distance viewing such as driving, cinema etc (called 'myopia')
#2	For long-sightedness, i.e. for distance and near, but particularly for near tasks like reading (called 'hypermetropia')
#3	For just reading/near work as you are getting older (called 'presbyopia')
#4	For 'astigmatism'
#5	For a 'squint' or 'turn' in an eye since childhood (called 'strabismus')
#6	For a 'lazy' eye or an eye with poor vision since childhood (called 'amblyopia')
#7	Other eye condition
#-1	Do not know
#-3	Prefer not to answer

# 5843	Which eye(s) affected by myopia (short sight)
# Data-Field 5843
# Description:	Which eye(s) affected by myopia (short sight)
# 1	Right eye
# 2	Left eye
# 3	Both eyes
d$myopia.eye <- d$f.5843.0.0
d$myopia.eye[which(d$myopia.eye==1)] <- 'R'
d$myopia.eye[which(d$myopia.eye==2)] <- 'L'
d$myopia.eye[which(d$myopia.eye==3)] <- 'B'
d$myopia.eye[which(is.na(d$myopia.eye))] <- 'N'
table(d$myopia.eye,useNA = 'ifany')
#5832	Which eye(s) affected by hypermetropia (long sight)
# 1	Right eye
# 2	Left eye
# 3	Both eyes
d$hypermetropia.eye <- d$f.5832.0.0
d$hypermetropia.eye[which(d$hypermetropia.eye==1)] <- 'R'
d$hypermetropia.eye[which(d$hypermetropia.eye==2)] <- 'L'
d$hypermetropia.eye[which(d$hypermetropia.eye==3)] <- 'B'
table(d$hypermetropia.eye,useNA = 'ifany')
#5610	Which eye(s) affected by presbyopia
d$presbyopia.eye <- d$f.5610.0.0
d$presbyopia.eye[which(d$presbyopia.eye==1)] <- 'R'
d$presbyopia.eye[which(d$presbyopia.eye==2)] <- 'L'
d$presbyopia.eye[which(d$presbyopia.eye==3)] <- 'B'
d$presbyopia.eye[which(is.na(d$presbyopia.eye))] <- 'N'
table(d$presbyopia.eye,useNA = 'ifany')
print(presbyopia_count <- length(which(d$presbyopia.eye=='L'))+length(which(d$presbyopia.eye=='R'))+2*length(which(d$presbyopia.eye=='B')))
d$left.presbyopia.eye <- as.numeric(d$presbyopia.eye %in% c('L','B')) 
d$right.presbyopia.eye <- as.numeric(d$presbyopia.eye %in% c('R','B')) 
#5855	Which eye(s) affected by astigmatism
d$astigmatism.eye <- d$f.5855.0.0
d$astigmatism.eye[which(d$astigmatism.eye==1)] <- 'R'
d$astigmatism.eye[which(d$astigmatism.eye==2)] <- 'L'
d$astigmatism.eye[which(d$astigmatism.eye==3)] <- 'B'
d$astigmatism.eye[which(is.na(d$astigmatism.eye))] <- 'N'
table(d$astigmatism.eye,useNA = 'ifany')
#6205	Which eye(s) affected by strabismus (squint)
d$strabismus.eye <- d$f.6205.0.0
d$strabismus.eye[which(d$strabismus.eye==1)] <- 'R'
d$strabismus.eye[which(d$strabismus.eye==2)] <- 'L'
d$strabismus.eye[which(d$strabismus.eye==3)] <- 'B'
d$strabismus.eye[which(is.na(d$strabismus.eye))] <- 'N'
table(d$strabismus.eye,useNA = 'ifany')
print(strabismus_count <- length(which(d$strabismus.eye=='L'))+length(which(d$strabismus.eye=='R'))+2*length(which(d$strabismus.eye=='B')))
d$left.strabismus.eye <- as.numeric(d$strabismus.eye %in% c('L','B')) 
d$right.strabismus.eye <- as.numeric(d$strabismus.eye %in% c('R','B')) 
#5408	Which eye(s) affected by amblyopia (lazy eye)
d$amblyopia.eye <- d$f.5408.0.0
d$amblyopia.eye[which(d$amblyopia.eye==1)] <- 'R'
d$amblyopia.eye[which(d$amblyopia.eye==2)] <- 'L'
d$amblyopia.eye[which(d$amblyopia.eye==3)] <- 'B'
d$amblyopia.eye[which(is.na(d$amblyopia.eye))] <- 'N'
table(d$amblyopia.eye,useNA = 'ifany')
print(amblyopia_count <- length(which(d$amblyopia.eye=='L'))+length(which(d$amblyopia.eye=='R'))+2*length(which(d$amblyopia.eye=='B')))
d$left.amblyopia.eye <- as.numeric(d$amblyopia.eye %in% c('L','B')) 
d$right.amblyopia.eye <- as.numeric(d$amblyopia.eye %in% c('R','B')) 
#5877	Which eye(s) affected by other eye condition
#5934	Which eye(s) affected by other serious eye condition
2227	Other eye problems
#6148	Eye problems/disorders
d$eye_problems_disorders <- ifelse(is.na(d$f.6148.0.0),'-1',as.character(d$f.6148.0.0))
d$eye_problems_disorders <- unlist(list('1'="Diabetes related eye disease",'2'="Glaucoma",'3'="Injury or trauma resulting in loss of vision",'4'="Cataract",'5'="Macular degeneration",'6'="Other serious eye condition",'-7'="None of the above",'-3'="Prefer not to answer",'-1'="Do not know")[d$eye_problems_disorders])
5890	Which eye(s) affected by diabetes-related eye disease
#6119	Which eye(s) affected by glaucoma
d$glaucoma.eye <- d$f.6119.0.0
d$glaucoma.eye[which(d$glaucoma.eye==1)] <- 'R'
d$glaucoma.eye[which(d$glaucoma.eye==2)] <- 'L'
d$glaucoma.eye[which(d$glaucoma.eye==3)] <- 'B'
d$glaucoma.eye[which(is.na(d$glaucoma.eye))] <- 'N'
#5419	Which eye(s) affected by injury or trauma resulting in loss of vision
#5441	Which eye(s) are affected by cataract
d$cataract.eye <- d$f.5441.0.0
d$cataract.eye[which(d$cataract.eye==1)] <- 'R'
d$cataract.eye[which(d$cataract.eye==2)] <- 'L'
d$cataract.eye[which(d$cataract.eye==3)] <- 'B'
d$cataract.eye[which(is.na(d$cataract.eye))] <- 'N'
#5912	Which eye(s) affected by macular degeneration
#5901	Age when diabetes-related eye disease diagnosed
d$age_diabetes_related_eye_disease_diagnosed<-d$f.5901.0.0
#4689	Age glaucoma diagnosed
d$age_glaucoma_diagnosed<-d$f.4689.0.0
#5430	Age when loss of vision due to injury or trauma diagnosed
#4700	Age cataract diagnosed
#5923	Age macular degeneration diagnosed
#5945	Age other serious eye condition diagnosed

