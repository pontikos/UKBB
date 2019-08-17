
library(cowplot)
library(beeswarm)
trans <- function(x) log(x)




png('ethnicity_ease_skin_tanning.png')
plot( f.1727.0.0 ~ ethnicity_code,data=d)
dev.off()


png('ease_skin_tanning.png')
boxplot(log(right_astigmatism) ~ f.1727.0.0, data=d)
abline(lm(log(right_astigmatism) ~ as.numeric(f.1727.0.0)),col='red')
abline(h=mean(log(d$right_astigmatism)))
dev.off()

png('time_spent_outdoors.png')
boxplot(log(right_astigmatism) ~ f.1050.0.0, data=d)
abline(lm(log(right_astigmatism) ~ f.1050.0.0, data=d), col='red')
abline(h=mean(log(d$right_astigmatism)))
dev.off()


summary(lm(log(right_astigmatism) ~ as.numeric(f.1727.0.0) + as.numeric(ethnicity),data=d))

for (x in na.omit(unique(d$f.2217.0.0))) {
   print(summary(lm(log(right_astigmatism) ~ gender, data=d[which(d$f.2217.0.0==x),])))
}


png('right_astigmatism_age_wearing_glasses.png')
# Age started wearing glasses or contact lenses Uses data-coding 100291
plot( log(right_astigmatism) ~ f.2217.0.0, d)
abline( lm(log(right_astigmatism) ~ f.2217.0.0, data=d), col='red')
abline(h=0)
dev.off()
png('left_astigmatism_age_wearing_glasses.png')
plot( log(left_astigmatism) ~ f.2217.0.0, d)
abline( lm(log(left_astigmatism) ~ f.2217.0.0, data=d), col='red')
abline(h=0)
dev.off()




xx <- c( "f.31.0.0", "f.23101.0.0", "f.23099.0.0", "f.23105.0.0", "f.23106.0.0", "f.50.0.0", "f.22015.0.0", "f.23100.0.0", "f.23098.0.0", 'gender', 'age')
X <- d[,xx]
colnames(X) <- dict[xx,'Description']

png('pairs.png')
pairs(X)
dev.off()



# gender age started wearing glasses
png('gender-glasses_age.png')
boxplot( f.2217.0.0 ~ gender, data=d, las=2, main='gender glasses age')
#stripchart(log(left_astigmatism) ~ f.1558.0.0, data = d, pch = 16, method='jitter', jitter=.2, vertical=T, add=TRUE)
abline(lm( f.2217.0.0 ~ gender,data=d),col='red')
abline(h=0)
dev.off()

#png('gender-glasses_age-right.png')
#boxplot( f.2217.0.0 ~ gender, data=d, las=2, main='gender glasses age right')
#stripchart(log(right_astigmatism) ~ f.1558.0.0, data = d, pch = 16, method='jitter', jitter=.2, vertical=T, add=TRUE)
#abline(lm( f.2217.0.0 ~ gender , data=d),col='red')
#abline(h=0)
#dev.off()



# ethnicity
png('ethnicity-left.png')
boxplot(log(left_astigmatism) ~ ethnicity, data = d, pch = 16, main = 'ethnicity left')
stripchart(log(left_astigmatism) ~ ethnicity, data = d, pch = 16, method='jitter', jitter=.2, vertical=T, add=TRUE)
abline(lm(log(left_astigmatism) ~ ethnicity,data=d))
dev.off()
png('ethnicity-right.png')
boxplot(log(left_astigmatism) ~ ethnicity, data = d, pch = 16, main = 'ethnicity right')
stripchart(log(right_astigmatism) ~ ethnicity, data = d, pch = 16,  method='jitter', jitter=.2, vertical=T, add=TRUE)
abline(lm(log(right_astigmatism) ~ ethnicity,data=d))
dev.off()

png('gender_y_chrom.png')
plot( f.22015.0.0 ~ gender, data=d)
dev.off()

# gender age-astigmatism
png('male-age-astigmatism.png')
plot(log(right_astigmatism) ~ age, data=d[which(d$gender==1),], main='male age astigmatism right')
abline(lm(log(right_astigmatism) ~ age, data=d[which(d$gender==1),]),col='red')
abline(h=0)
dev.off()

png('female-age-astigmatism.png')
plot(log(right_astigmatism) ~ age, data=d[which(d$gender==0),], main='male age astigmatism right')
abline(lm(log(right_astigmatism) ~ age, data=d[which(d$gender==0),]),col='red')
abline(h=0)
dev.off()



# age
png('age-left.png')
# left
plot(log(left_astigmatism) ~ age, data=d, main='age left')
abline(lm(log(left_astigmatism) ~ age, data=d), col='red')
dev.off()
# right
png('age-right.png')
plot(log(right_astigmatism) ~ age, data=d, main='age right')
abline(lm(log(right_astigmatism) ~ age, data=d), col='red')
dev.off()


# age vs corneal astigmatism hexbin
h <- hexbin(data.frame(right_corneal_astigmatism=log(d$right_corneal_astigmatism),age=d$age))
plot(h)
h <- hexbin(data.frame(left_corneal_astigmatism=log(d$left_corneal_astigmatism),age=d$age))
plot(h)
dev.off()



# alchohol intake f.1558.0.0
png('alcohol-left.png')
boxplot( log(left_astigmatism) ~ f.1558.0.0, data=d, las=2, main='Alcohol intake left')
#stripchart(log(left_astigmatism) ~ f.1558.0.0, data = d, pch = 16, method='jitter', jitter=.2, vertical=T, add=TRUE)
abline(lm(log(left_astigmatism) ~ f.1558.0.0,data=d),col='red')
abline(h=0)
dev.off()
png('alcohol-right.png')
boxplot( log(right_astigmatism) ~ f.1558.0.0, data=d, las=2, main='Alcohol intake right')
#stripchart(log(right_astigmatism) ~ f.1558.0.0, data = d, pch = 16, method='jitter', jitter=.2, vertical=T, add=TRUE)
abline(lm(log(right_astigmatism) ~ f.1558.0.0,data=d),col='red')
abline(h=0)
dev.off()

par(mfrow=c(1,2))
plot(left_3mm_corneal_astigmatism ~ left_3mm_spherical_equivalent,data=d)
abline(lm(left_3mm_corneal_astigmatism ~ left_3mm_spherical_equivalent,data=d),col='red')
plot(right_3mm_corneal_astigmatism ~ right_3mm_spherical_equivalent,data=d)
abline(lm(right_3mm_corneal_astigmatism ~ right_3mm_spherical_equivalent,data=d),col='red')


pdf('figures.pdf')


# astigmatism distribution density
plot(density(d$left_astigmatism))
lines(density(d$right_astigmatism),lty=2, col='red')



# Figure S1
# astigmatism log transformed distribution
par(mfrow=c(1,2))
plot(density((d$left_3mm_corneal_astigmatism)),main='a)',xlab='raw',col='blue')
lines(density((d$right_3mm_corneal_astigmatism)),lty=2,col='red')
plot(density(log(d$left_3mm_corneal_astigmatism)),main='b)',xlab='transformed',col='blue')
lines(density(log(d$right_3mm_corneal_astigmatism)),lty=2,col='red')

pdf('figureS3.pdf')
# cylindrical power vs age
summary(m<-lm(right_cylindrical_power ~ age,d))
coefs <- coef(m)
p1 <- ggplot(d, aes(x=age.group,y=right_cylindrical_power,group=age.group))+theme_bw()+geom_boxplot()+ylab('right cylindrical power')+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none")+xlab('')
#+geom_abline(intercept = coefs[1], slope = coefs[2])
summary(m<-lm(left_cylindrical_power ~ age,d))
coefs <- coef(m)
p2 <- ggplot(d, aes(x=age.group,y=left_cylindrical_power,group=age.group))+theme_bw()+geom_boxplot()+ylab('left cylindrical power')+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none")+xlab('')
#+geom_abline(intercept = coefs[1], slope = coefs[2])
# mean corneal power vs age
summary(m<-lm(right_mean_corneal_power ~ age,d))
coefs <- coef(m)
p3 <- ggplot(d, aes(x=age.group, y=right_mean_corneal_power,group=age.group))+theme_bw()+geom_boxplot()+ylab('right mean corneal power')+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none")+xlab('')
#+geom_abline(intercept = coefs[1], slope = coefs[2])
summary(m<-lm(left_mean_corneal_power ~ age,d))
coefs <- coef(m)
p4 <- ggplot(d, aes(x=age.group, y=left_mean_corneal_power,group=age.group))+theme_bw()+geom_boxplot()+ylab('left mean corneal power')+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none")+xlab('')
#+geom_abline(intercept = coefs[1], slope = coefs[2])
cowplot::plot_grid(p1,p2,p3,p4,labels='AUTO')
dev.off()

# amblyopia
library(questionr)
par(mfrow=c(2,2))
m <- glm(right.amblyopia.eye ~ right_corneal_astigmatism, family = binomial(), data=d)
odds.ratio(m)
plot(right.amblyopia.eye ~ right_corneal_astigmatism , data=d, ylab='right eye amblyopia', xlab='right corneal astigmatism')
yy <- predict(m,newdata = d, type = 'response')
lines(d$right_corneal_astigmatism[order(d$right_corneal_astigmatism)], yy[order(d$right_corneal_astigmatism)], col='red')
m <- glm(left.amblyopia.eye ~ left_corneal_astigmatism, family = binomial(), data=d)
odds.ratio(m)
plot(left.amblyopia.eye ~ left_corneal_astigmatism , data=d, ylab='left eye amblyopia', xlab='left corneal astigmatism')
yy <- predict(m,newdata = d, type = 'response')
lines(d$left_corneal_astigmatism[order(d$left_corneal_astigmatism)], yy[order(d$left_corneal_astigmatism)], col='red')
# strabismus
m <- glm(right.strabismus.eye ~ right_corneal_astigmatism, family = binomial(), data=d)
odds.ratio(m)
plot(right.strabismus.eye ~ right_corneal_astigmatism , data=d, ylab='right eye strabismus', xlab='right corneal astigmatism')
yy <- predict(m,newdata = d, type = 'response')
lines(d$right_corneal_astigmatism[order(d$right_corneal_astigmatism)], yy[order(d$right_corneal_astigmatism)], col='red')
m <- glm(left.strabismus.eye ~ left_corneal_astigmatism, family = binomial(), data=d)
odds.ratio(m)
plot(left.strabismus.eye ~ left_corneal_astigmatism , data=d, ylab='left eye strabismus', xlab='left corneal astigmatism')
yy <- predict(m,newdata = d, type = 'response')
lines(d$left_corneal_astigmatism[order(d$left_corneal_astigmatism)], yy[order(d$left_corneal_astigmatism)], col='red')

#
par(mfrow=c(1,1))
boxplot(log(d$left_corneal_astigmatism) ~ d$alcohol_intake_code + d$gender,las=2)

boxplot(d$gender ~ d$alcohol_intake_code,las=2)

prop.table(table(d$alcohol_intake_code,d$gender),1)

# FIGURE assymetry
par(mfrow=c(2,2))
#
plot(left_3mm_corneal_astigmatism~right_3mm_corneal_astigmatism,data=d,
     xlab='left 3mm corneal astigmatism',
     ylab='right 3mm corneal astigmatism')
abline(b=1,a=0)
abline(lm(left_3mm_corneal_astigmatism ~ right_3mm_corneal_astigmatism, data=d), col='red')

plot(log(left_3mm_corneal_astigmatism)~log(right_3mm_corneal_astigmatism),data=d,
     xlab='log(left 3mm corneal astigmatism)',
     ylab='log(right 3mm corneal astigmatism)')
abline(b=1,a=0)
abline(lm(log(left_3mm_corneal_astigmatism) ~ log(right_3mm_corneal_astigmatism), data=d[which(d$left_3mm_corneal_astigmatism>0&d$right_3mm_corneal_astigmatism>0),]), col='red')

#
plot(left_3mm_cylindrical_power~right_3mm_cylindrical_power,data=d,
     xlab='left 3mm cylindrical power',
     ylab='right 3mm cylindrical power')
abline(b=1,a=0)
abline(lm(left_3mm_cylindrical_power ~ right_3mm_cylindrical_power, data=d), col='red')
#
plot(left_mean_corneal_power~right_mean_corneal_power,data=d,
     xlab='left 3mm mean corneal power',
     ylab='right 3mm mean corneal power')
abline(b=1,a=0)
abline(lm(left_mean_corneal_power ~ right_mean_corneal_power, data=d), col='red')
#
plot(left_3mm_spherical_power~right_3mm_spherical_power,data=d,
     xlab='left 3mm spherical power',
     ylab='right 3mm spherical power')
abline(b=1,a=0)
abline(lm(left_3mm_spherical_power ~ right_3mm_spherical_power, data=d), col='red')

plot(left_spherical_equivalent~right_spherical_equivalent,data=d,
     xlab='left 3mm spherical equivalent',
     ylab='right 3mm spherical equivalent')
abline(b=1,a=0)
abline(lm(left_spherical_equivalent ~ right_spherical_equivalent, data=d), col='red')
#
plot(left_axis_of_astigmatism~right_axis_of_astigmatism,data=d,
     xlab='left axis of astigmatism',
     ylab='right axis of astigmatism')
abline(b=1,a=0)
abline(lm(left_axis_of_astigmatism ~ right_axis_of_astigmatism, data=d), col='red')

image(prop.table(table(d$right_axis_of_astigmatism>0,d$left_axis_of_astigmatism>0)))


plot(left_3mm_cylindrical_power~left_corneal_astigmatism,data=d,
     xlab='left_3mm_cylindrical_power',
     ylab='left_corneal_astigmatism')
abline(b=1,a=0)
abline(lm(left_3mm_cylindrical_power ~ left_corneal_astigmatism, data=d), col='red')

# age distribution
plot(density(d$age1))

# age - gender
boxplot(age~gender,data=d)
# age - bodyfat
plot(body_fat_percentage ~ age,data=d)
# gender - bodyfat
boxplot(body_fat_percentage ~ gender,data=d)

# sex
boxplot(trans(left_astigmatism) ~ gender, data=d, main='sex left')
boxplot(trans(right_astigmatism) ~ gender, data=d, main='sex right')

# sites code
boxplot(trans(left_astigmatism) ~ sites, data=d, las=2)
boxplot(trans(right_astigmatism) ~ sites, data=d, las=2)

# corneal hysterisis
plot(density(d$corneal_hysterisis_left,na.rm=TRUE))
lines(density(d$corneal_hysterisis_right,na.rm=TRUE),lty=2)

# corneal hysterisis
# left
plot(trans(left_astigmatism) ~ corneal_hysterisis_left, data=d)
abline(lm(trans(left_astigmatism) ~ corneal_hysterisis_left, data=d))
# right
plot(trans(right_astigmatism) ~ corneal_hysterisis_right, data=d)
abline(lm(trans(right_astigmatism) ~ corneal_hysterisis_right, data=d))

# IoP corneal-compensated
# left
plot(log(left_astigmatism) ~ log(left_corneal_corrected_iop), data=d,main='IOPcc left')
plot((left_corneal_astigmatism) ~ (left_corneal_corrected_iop), data=d,main='IOPcc left')
#abline(lm(log(left_astigmatism) ~ log(left_corneal_corrected_iop), data=d))
# right
plot(log(right_astigmatism) ~ log(right_corneal_corrected_iop), data=d,main='IOPcc right')
#abline(lm(log(right_astigmatism) ~ log(right_corneal_corrected_iop), data=d))

# IoP goldman
# left
plot(trans(left_astigmatism) ~ left_goldman_corrected_iop, data=d)
abline(lm(trans(left_astigmatism) ~ left_goldman_corrected_iop, data=d))
# right
plot(trans(right_astigmatism) ~ left_goldman_corrected_iop, data=d)
abline(lm(trans(right_astigmatism) ~ right_goldman_corrected_iop, data=d))

# compare IoP
plot(left_corneal_corrected_iop ~ left_goldman_corrected_iop, data=d)
abline(b=1,a=0)
plot(right_corneal_corrected_iop ~ right_goldman_corrected_iop, data=d)
abline(b=1,a=0)


boxplot( left_astigmatism ~ age_completed_education, data=d, las=2)
dev.off()


library(tidyverse)
ggplot(d[which(d$age<50),], aes(x=right_3mm_strong_meridian_angle, y=left_3mm_strong_meridian_angle) )+
  geom_bin2d()+
  geom_density_2d()+
  theme_bw()

ggplot(d[which(d$age<50),], aes(x=right_3mm_strong_meridian_angle, y=right_3mm_weak_meridian_angle) )+
  geom_bin2d()+
  geom_density_2d()+
  theme_bw()

ggplot(d, aes(x=left_3mm_strong_meridian_angle, y=right_3mm_strong_meridian_angle) )+geom_bin2d()+  geom_density_2d()+  theme_bw()


ggplot(d, aes(x=f.5089.0.0, y=f.5088.0.0) )+
  geom_bin2d()+
  geom_density_2d()+
  theme_bw()

ggplot(d, aes(x=left_3mm_strong_meridian, y=right_3mm_strong_meridian) )+
  geom_bin2d()+
  geom_density_2d()+
  theme_bw()


ggplot(d, aes(x=left_3mm_strong_meridian, y=right_3mm_strong_meridian) )+
  geom_bin2d()+
  geom_density_2d()+
  theme_bw()

ggplot(d, aes(x=left_3mm_weak_meridian, y=right_3mm_weak_meridian) )+
  geom_bin2d()+
  geom_density_2d()+
  theme_bw()

ggplot(d, aes(x=left_3mm_weak_meridian, y=left_3mm_strong_meridian) )+
  geom_bin2d()+
  geom_density_2d()+
  theme_bw()

ggplot(d, aes(x=right_3mm_weak_meridian, y=right_3mm_strong_meridian) )+geom_bin2d()+ geom_density_2d()+  theme_bw()

ggplot(d, aes(x=f.5116.0.0, y=f.5087.0.0) )+  geom_bin2d()+  geom_density_2d()+  theme_bw()

ggplot(d, aes(x=log(right_corneal_astigmatism), y=townsend_deprivation_index) )+  geom_bin2d()+  geom_density_2d()+  theme_bw()

ggplot(d, aes(x=right_logMAR, y=log(right_corneal_astigmatism)) )+  geom_bin2d()+  geom_density_2d()+  theme_bw()


ggplot(d, aes(x=age_completed_education, y=log(right_corneal_astigmatism)) )+  geom_bin2d()+  geom_density_2d()+  theme_bw()

boxplot(log(right_corneal_astigmatism) ~ f.20016.0.0, data=d)

plot(density(na.omit(d$townsend_deprivation_index)))

hist(na.omit(d$age_completed_education))

boxplot(d$we)

plot(density(na.omit(d$right_logMAR)))

# Figure eye condition vs corneal astigmatism
x <- list(
     all=log(d[,'right_corneal_astigmatism']),
     astigmatism=log(d[which(d$astigmatism.eye=='R'),'right_corneal_astigmatism']),
     myopia=log(d[which(d$myopia.eye=='R'),'right_corneal_astigmatism']),
     hypermetropia=log(d[which(d$hypermetropia.eye=='R'),'right_corneal_astigmatism']),
     presbyopia=log(d[which(d$presbyopia.eye=='R'),'right_corneal_astigmatism']),
     amblyopia=log(d[which(d$amblyopia.eye=='R'),'right_corneal_astigmatism']),
     strabismus=log(d[which(d$strabismus.eye=='R'),'right_corneal_astigmatism']),
     cataract=log(d[which(d$cataract.eye=='R'),'right_corneal_astigmatism']))
par(mar=c(7,5,1,1),mfrow=c(1,2))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='right',ylab='log(3mm corneal astigmatism)')
x <- list(
     all=log(d[,'left_corneal_astigmatism']),
     astigmatism=log(d[which(d$astigmatism.eye=='L'),'left_corneal_astigmatism']),
     myopia=log(d[which(d$myopia.eye=='L'),'left_corneal_astigmatism']),
     hypermetropia=log(d[which(d$hypermetropia.eye=='L'),'left_corneal_astigmatism']),
     presbyopia=log(d[which(d$presbyopia.eye=='L'),'left_corneal_astigmatism']),
     amblyopia=log(d[which(d$amblyopia.eye=='L'),'left_corneal_astigmatism']),
     strabismus=log(d[which(d$strabismus.eye=='L'),'left_corneal_astigmatism']),
     cataract=log(d[which(d$cataract.eye=='L'),'left_corneal_astigmatism']))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='left')


x <- list(astigmatism=(d[which(d$astigmatism.eye=='R'),'right_mean_corneal_power']),
          myopia=(d[which(d$myopia.eye=='R'),'right_mean_corneal_power']),
          hypermetropia=(d[which(d$hypermetropia.eye=='R'),'right_mean_corneal_power']),
          presbyopia=(d[which(d$presbyopia.eye=='R'),'right_mean_corneal_power']),
          amblyopia=(d[which(d$amblyopia.eye=='R'),'right_mean_corneal_power']),
          strabismus=(d[which(d$strabismus.eye=='R'),'right_mean_corneal_power']))
par(mar=c(7,5,1,1),mfrow=c(1,2))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='right',ylab='(mean corneal power)')
x <- list(astigmatism=(d[which(d$astigmatism.eye=='L'),'left_mean_corneal_power']),
          myopia=(d[which(d$myopia.eye=='L'),'left_mean_corneal_power']),
          hypermetropia=(d[which(d$hypermetropia.eye=='L'),'left_mean_corneal_power']),
          presbyopia=(d[which(d$presbyopia.eye=='L'),'left_mean_corneal_power']),
          amblyopia=(d[which(d$amblyopia.eye=='L'),'left_mean_corneal_power']),
          strabismus=(d[which(d$strabismus.eye=='L'),'left_mean_corneal_power']))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='left')


# Figure eye condition vs age 
x <- list(
     all=(d[,'age']),
     astigmatism=(d[which(d$astigmatism.eye=='R'),'age']),
     myopia=(d[which(d$myopia.eye=='R'),'age']),
     hypermetropia=(d[which(d$hypermetropia.eye=='R'),'age']),
     presbyopia=(d[which(d$presbyopia.eye=='R'),'age']),
     amblyopia=(d[which(d$amblyopia.eye=='R'),'age']),
     strabismus=(d[which(d$strabismus.eye=='R'),'age']),
     cataract=(d[which(d$cataract.eye=='R'),'age']))
par(mar=c(7,5,1,1),mfrow=c(1,2))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='right',ylab='(age)')
x <- list(
     all=(d[,'age']),
     astigmatism=(d[which(d$astigmatism.eye=='L'),'age']),
     myopia=(d[which(d$myopia.eye=='L'),'age']),
     hypermetropia=(d[which(d$hypermetropia.eye=='L'),'age']),
     presbyopia=(d[which(d$presbyopia.eye=='L'),'age']),
     amblyopia=(d[which(d$amblyopia.eye=='L'),'age']),
     strabismus=(d[which(d$strabismus.eye=='L'),'age']),
     cataract=(d[which(d$cataract.eye=='L'),'age']))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='left')



# Figure eye condition vs age completed education
x <- list(
     all=(d[,'age_completed_education']),
     astigmatism=(d[which(d$astigmatism.eye=='R'),'age_completed_education']),
     myopia=(d[which(d$myopia.eye=='R'),'age_completed_education']),
     hypermetropia=(d[which(d$hypermetropia.eye=='R'),'age_completed_education']),
     presbyopia=(d[which(d$presbyopia.eye=='R'),'age_completed_education']),
     amblyopia=(d[which(d$amblyopia.eye=='R'),'age_completed_education']),
     strabismus=(d[which(d$strabismus.eye=='R'),'age_completed_education']),
     cataract=(d[which(d$cataract.eye=='R'),'age_completed_education']))
par(mar=c(7,5,1,1),mfrow=c(1,2))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='right',ylab='(age completed education)')
x <- list(
     all=(d[,'age_completed_education']),
     astigmatism=(d[which(d$astigmatism.eye=='L'),'age_completed_education']),
     myopia=(d[which(d$myopia.eye=='L'),'age_completed_education']),
     hypermetropia=(d[which(d$hypermetropia.eye=='L'),'age_completed_education']),
     presbyopia=(d[which(d$presbyopia.eye=='L'),'age_completed_education']),
     amblyopia=(d[which(d$amblyopia.eye=='L'),'age_completed_education']),
     strabismus=(d[which(d$strabismus.eye=='L'),'age_completed_education']),
     cataract=(d[which(d$cataract.eye=='L'),'age_completed_education']))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='left')

# Figure eye condition vs right spherical power
x <- list(
     all=(d[,'right_spherical_power']),
     astigmatism=(d[which(d$astigmatism.eye=='R'),'right_spherical_power']),
     myopia=(d[which(d$myopia.eye=='R'),'right_spherical_power']),
     hypermetropia=(d[which(d$hypermetropia.eye=='R'),'right_spherical_power']),
     presbyopia=(d[which(d$presbyopia.eye=='R'),'right_spherical_power']),
     amblyopia=(d[which(d$amblyopia.eye=='R'),'right_spherical_power']),
     strabismus=(d[which(d$strabismus.eye=='R'),'right_spherical_power']),
     cataract=(d[which(d$cataract.eye=='R'),'right_spherical_power']))
par(mar=c(7,5,1,1),mfrow=c(1,2))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='right',ylab='(right spherical power)')
x <- list(
     all=(d[,'left_spherical_power']),
     astigmatism=(d[which(d$astigmatism.eye=='L'),'left_spherical_power']),
     myopia=(d[which(d$myopia.eye=='L'),'left_spherical_power']),
     hypermetropia=(d[which(d$hypermetropia.eye=='L'),'left_spherical_power']),
     presbyopia=(d[which(d$presbyopia.eye=='L'),'left_spherical_power']),
     amblyopia=(d[which(d$amblyopia.eye=='L'),'left_spherical_power']),
     strabismus=(d[which(d$strabismus.eye=='L'),'left_spherical_power']),
     cataract=(d[which(d$cataract.eye=='L'),'left_spherical_power']))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='left')


# qualifications vs astigmatism
par(mar=c(7,5,1,1),mfrow=c(1,2))
boxplot(log(left_corneal_astigmatism) ~ qualifications, d, las=2)
boxplot(log(right_corneal_astigmatism) ~ qualifications, d, las=2 )

# age completed full-time education vs corneal astigmatism

h <- hexbin(data.frame(left_corneal_astigmatism=log(d$left_corneal_astigmatism) , age_completed_full_time_education=d$age_completed_full_time_education))
plot(h)
dev.off()

# age completed full-time education vs age
h <- hexbin(data.frame(age=(d$age) , age_completed_full_time_education=d$age_completed_full_time_education))
plot(h)
dev.off()

#boxplot(log(d$left_corneal_astigmatism) ~ d$age_completed_full_time_education)





library(ggplot2)
# Basic violin plot
ggplot(d, aes(x=right_cylindrical_power, y=age.group)) + geom_violin() + coord_flip() + geom_boxplot(width=0.1)

boxplot(d$right_cylindrical_power~d$age.group)

boxplot(d$right_corneal_astigmatism ~ d$age_started_wearing_glasses_or_contact_lenses.lt.30)

boxplot(d$age_started_wearing_glasses_or_contact_lenses ~ d$myopia.eye=='N')
boxplot(d$age_started_wearing_glasses_or_contact_lenses ~ d$presbyopia.eye=='N')

boxplot(d$right_spherical_equivalent ~ d$myopia.eye=='N')
boxplot(d$right_3mm_corneal_astigmatism ~  d$myopia.eye=='N')

boxplot(log(d$right_3mm_corneal_astigmatism) ~ d$wears_glasses_or_contact_lenses)


table(d$myopia.eye,d$wears_glasses_or_contact_lenses)

table(d[which(d$myopia.eye=='N'&d$amblyopia.eye=='N'&d$presbyopia.eye=='N'&d$strabismus.eye=='N'&d$astigmatism.eye=='N'),'wears_glasses_or_contact_lenses'])

# chinese have lowest SE
boxplot(rowMeans(d[,c('right_spherical_equivalent','left_spherical_equivalent')]) ~ d$ethnicity)
# and highest proporiotn of myopia
table(d$myopia.eye!='N',d$ethnicity)

sort(unlist(as.list(by(d$right_corneal_astigmatism, d$qualifications, mean))))
boxplot(log(right_corneal_astigmatism) ~ qualifications,data=d,las=2)

boxplot(d$age_started_wearing_glasses_or_contact_lenses ~ d$right.amblyopia.eye)

d.RE.amblyopia <- d[which(d$right.amblyopia.eye==1),]
d.LE.amblyopia <- d[which(d$left.amblyopia.eye==1),]

hist(d.RE.amblyopia$right_3mm_corneal_astigmatism)

hist(d.RE.amblyopia$age_started_wearing_glasses_or_contact_lenses)

plot(density(log(d.RE.amblyopia$right_3mm_corneal_astigmatism)),col='gray')
lines(density(log(d.RE.amblyopia[which(d.RE.amblyopia$age_started_wearing_glasses_or_contact_lenses<30),'right_3mm_corneal_astigmatism'])),col='blue')
lines(density(log(d.RE.amblyopia[which(d.RE.amblyopia$age_started_wearing_glasses_or_contact_lenses>=30),'right_3mm_corneal_astigmatism'])),col='red')

which(d.RE.amblyopia$right_3mm_corneal_astigmatism>5)


par(mar=c(10,5,1,1),mfrow=c(1,1))
boxplot(d$age ~ d$alcohol_intake,ylab='age',las=2)


x <- d$age
#boxplot(sort((c(by(x,d$alcohol_intake,median))),las=2)

library(sjPlot)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())

m <- lm((right_corneal_astigmatism) ~ right_corneal_corrected_iop*age,d)
plot_model(m, type = "pred", terms=c('age','right_corneal_corrected_iop') )
m <- lm((left_corneal_astigmatism) ~ left_corneal_corrected_iop*age,d)
plot_model(m, type = "pred", terms=c('age','left_corneal_corrected_iop') )
dev.off()


d$gender <- to_factor(d$gender)
d$alcohol_intake <- to_factor(d$alcohol_intake)
m <- lm(log(right_corneal_astigmatism) ~ gender*age*alcohol_intake,d)
plot_model(m, type = "pred", terms = c("age","gender","alcohol_intake [daily or almost daily, three four times a week, once twice a week, one to three times a month, special occasions, never]"))
m <- lm(log(left_corneal_astigmatism) ~ gender*age*alcohol_intake,d)
plot_model(m, type = "pred", terms = c("age","gender","alcohol_intake [daily or almost daily, three four times a week, once twice a week, one to three times a month, special occasions, never]"))
dev.off()

d$qualifications <- to_factor(d$qualifications)
m <- lm(log(right_corneal_astigmatism) ~ age_completed_full_time_education*qualifications*age,d)
plot_model(m, type = "pred", terms = c("age","age_completed_full_time_education [15, 16, 17, 18]","qualifications [CSEs or equivalent, None of the above, Prefer not to answer]"))
dev.off()

d$right_eye_astigmatism <- to_factor(d$astigmatism.eye=='R')
m <- lm(log(right_corneal_astigmatism) ~ age_completed_full_time_education*right_eye_astigmatism*age,d)
plot_model(m, type = "pred", terms = c("age","right_eye_astigmatism", "age_completed_full_time_education [15, 16, 17, 18]"))
m <- lm(log(right_corneal_astigmatism) ~ age_completed_full_time_education*age,d)
plot_model(m, type = "pred", terms = c("age", "age_completed_full_time_education [15, 16, 17, 18]"))
d$ethnicity <- to_factor(d$ethnicity)
m <- lm(log(right_corneal_astigmatism) ~ ethnicity*age_completed_full_time_education*age,d)
plot_model(m, type = "pred", terms = c("ethnicity", "age", "age_completed_full_time_education [15, 16, 17, 18]"))

m <- lm(log(right_corneal_astigmatism) ~ age*ethnicity*gender,d)
plot_model(m, type = "pred", terms = c("age", "ethnicity [white, asian, black]", "gender"))
m <- lm(log(left_corneal_astigmatism) ~ age*ethnicity*gender,d)
plot_model(m, type = "pred", terms = c("age", "ethnicity [white, asian, black]", "gender"))
dev.off()

d$skin_colour <- to_factor(d$skin_colour)
m <- lm(log(right_corneal_astigmatism) ~ age*skin_colour*gender,d)
plot_model(m, type = "pred", terms = c("age", "skin_colour [very fair, fair, light olive, brown]", "gender"))
m <- lm(log(left_corneal_astigmatism) ~ age*skin_colour*gender,d)
plot_model(m, type = "pred", terms = c("age", "skin_colour [very fair, fair, light olive, brown]", "gender"))
dev.off()

d$skin_colour <- to_factor(d$skin_colour)
m <- lm(log(right_corneal_astigmatism) ~ age_completed_full_time_education*skin_colour*gender,d)
plot_model(m, type = "pred", terms = c("age_completed_full_time_education", "skin_colour [very fair, fair, light olive, brown]", "gender"))
m <- lm(log(left_corneal_astigmatism) ~ age_completed_full_time_education*skin_colour*gender,d)
plot_model(m, type = "pred", terms = c("age_completed_full_time_education", "skin_colour [very fair, fair, light olive, brown]", "gender"))
dev.off()

m <- lm((right_corneal_astigmatism) ~ townsend_deprivation_index*age,d)
plot_model(m, type = "pred", terms = c("age", "townsend_deprivation_index"))



make_labels <- function(x) {
thresh <- list( '0.50D'=0.5, '0.75D'=0.75, '1.00D'=1.00, '1.50D'=1.50, '2.00D'=2.00)
print(labels <- paste(sapply(names(thresh), function(k) { paste(round(100*length(which(x>thresh[k]))/length(x)),'%',' > ',k,sep='') }),collapse=' \n '))
return(geom_label(aes(x = '2.50-2.99', y = 30 , label = labels, fill="white"),fill="white"))
}

pdf('figure1.pdf')
x <- d$right_3mm_corneal_astigmatism
y <- list('0.00-0.49'=length(which(0 <= x & x < 0.49)),'0.50-0.99'=length(which(0.5 <= x & x < 0.99)),'1.00-1.49'=length(which(1.00 <= x & x < 1.49)),'1.50-1.99'=length(which(1.5 <= x & x < 1.99)),'2.00-2.49'=length(which(2.00 <= x & x < 2.49)),'2.50-2.99'=length(which(2.50 <= x & x < 2.99)),'3.00-3.49'=length(which(3.00 <= x & x < 3.49)),'>4.00'=length(which(x>4.00)))
y<-data.frame(names=as.character(names(y)),values=as.numeric(y))
y$names <- as.character(y$names)
#barplot(100*as.numeric(y)/sum(as.numeric(y)),names.arg = names(y),ylab="Prevalence (%)",xlab="Corneal Astigmatism (D)")
dist.right <- ggplot(data=y, aes(x=y$names, y=100*values/sum(y$values))) +  geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1),text=element_text(size=12)) + scale_x_discrete(limits=y$names) + xlab('Corneal Astigmatism (D) (right eye)') + ylab("Prevalence (%)") +  theme(panel.grid.major.x = element_blank() ,  panel.grid.major.y = element_line( size=.1, color="black" )) + scale_y_continuous(breaks=seq(0,40,5)) + make_labels(x)
x <- d$left_3mm_corneal_astigmatism
y <- list('0.00-0.49'=length(which(0 <= x & x < 0.49)),'0.50-0.99'=length(which(0.5 <= x & x < 0.99)),'1.00-1.49'=length(which(1.00 <= x & x < 1.49)),'1.50-1.99'=length(which(1.5 <= x & x < 1.99)),'2.00-2.49'=length(which(2.00 <= x & x < 2.49)),'2.50-2.99'=length(which(2.50 <= x & x < 2.99)),'3.00-3.49'=length(which(3.00 <= x & x < 3.49)),'>4.00'=length(which(x>4.00)))
y<-data.frame(names=as.character(names(y)),values=as.numeric(y))
y$names <- as.character(y$names)
#barplot(100*as.numeric(y)/sum(as.numeric(y)),names.arg = names(y),ylab="Prevalence (%)",xlab="Corneal Astigmatism (D)")
dist.left <- ggplot(data=y, aes(x=y$names, y=100*values/sum(y$values))) +  geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1),text=element_text(size=12)) + scale_x_discrete(limits=y$names) + xlab('Corneal Astigmatism (D) (left eye)') + ylab("Prevalence (%)") +  theme(panel.grid.major.x = element_blank() ,  panel.grid.major.y = element_line( size=.1, color="black" )) + scale_y_continuous(breaks=seq(0,40,5))+make_labels(x)
plot_grid(dist.right,dist.left,labels='AUTO')
dev.off()


pdf('figure2.pdf')
X <- data.frame(do.call('rbind', by(d, d$age.group, function(x) { 100*prop.table(table(x$right_axis)) })))
X$age <- rownames(X)
df <- X %>% select(age, ATR, OB, WTR) %>% gather(key = "variable", value = "value", -age)
prevalence.right <- ggplot(df, aes(x = age, y = value, group=variable, color=variable)) + geom_line()+ylab("Prevalence (%) (right eye)")+labs(colour="Axis of astigmatism")+theme_bw()+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none")
X <- data.frame(do.call('rbind', by(d, d$age.group, function(x) { 100*prop.table(table(x$left_axis)) })))
X$age <- rownames(X)
df <- X %>% select(age, ATR, OB, WTR) %>% gather(key = "variable", value = "value", -age)
prevalence.left <- ggplot(df, aes(x = age, y = value, group=variable, color=variable)) + geom_line()+ylab("Prevalence (%) (left eye)")+labs(colour="Axis of astigmatism")+theme_bw()+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none")
X <- data.frame(do.call('rbind', by(d, d$age.group, function(x) { data.frame(age=unique(x$age.group), ATR=mean(x[which(x$right_axis=='ATR'),'right_corneal_astigmatism']), OB=mean(x[which(x$right_axis=='OB'),'right_corneal_astigmatism']), WTR=mean(x[which(x$right_axis=='WTR'),'right_corneal_astigmatism'])) })))
df <- X %>% select(age, ATR, OB, WTR) %>% gather(key = "variable", value = "value", -age) 
mean.right <- ggplot(df, aes(x = age, y = value, group=variable, color=variable)) + geom_line() + ylab("Mean Corneal Astigmatism (D) (right eye)")+ labs(colour="Axis of astigmatism")+theme_bw()+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none")
X <- data.frame(do.call('rbind', by(d, d$age.group, function(x) { data.frame(age=unique(x$age.group), ATR=mean(x[which(x$left_axis=='ATR'),'left_corneal_astigmatism']), OB=mean(x[which(x$left_axis=='OB'),'left_corneal_astigmatism']), WTR=mean(x[which(x$left_axis=='WTR'),'left_corneal_astigmatism'])) })))
df <- X %>% select(age, ATR, OB, WTR) %>% gather(key = "variable", value = "value", -age)
mean.left <- ggplot(df, aes(x = age, y = value, group=variable, color=variable)) + geom_line() + ylab("Mean Corneal Astigmatism (D) (left eye)")+labs(colour="Axis of astigmatism")+theme_bw()+theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="none")
legend <- get_legend(mean.left+guides(color=guide_legend(nrow=1))+theme(legend.pos='bottom'))
#grid.arrange(prevalence.right,prevalence.left,ncol=2)
pgrid <- plot_grid(prevalence.right,prevalence.left,mean.right,mean.left,labels='AUTO')
plot_grid(pgrid, legend, ncol=1, rel_heights=c(1,.1))
dev.off()

lm_eqn <- function(df){
    m <- lm(y ~ x, df)
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
         list(a = format(unname(coef(m)[1]), digits = 2),
              b = format(unname(coef(m)[2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}

png('figure3.png')
age_ca.right <- ggplot(d, aes(x=age, y=right_corneal_astigmatism) ) + geom_point(alpha=0.1,position='jitter',size=0.1) + geom_smooth(method=lm,level=0.95)+theme_bw()+ylab('Corneal Astigmatism (right eye)')+ylim(low=0,high=2.5) + geom_text(x = 55, y = 2, label = lm_eqn(data.frame(y=d$right_corneal_astigmatism,x=d$age)), colour='blue', parse = TRUE, size=3)
age_ca.left <- ggplot(d, aes(x=age, y=left_corneal_astigmatism) ) + geom_point(alpha=0.1,position='jitter',size=0.1) + geom_smooth(method=lm,level=0.95)+theme_bw()+ylab('Corneal Astigmatism (left eye)')+ylim(low=0,high=2.5)
age_log_ca.right <- ggplot(d, aes(x=age, y=log(right_corneal_astigmatism)) ) + geom_point(alpha=0.1,position='jitter',size=0.1) + geom_smooth(method=lm,level=0.95)+theme_bw()+ylab('Log Corneal Astigmatism (right eye)')
age_log_ca.left <- ggplot(d, aes(x=age, y=log(left_corneal_astigmatism)) ) + geom_point(alpha=0.1,position='jitter',size=0.1) + geom_smooth(method=lm,level=0.95)+theme_bw()+ylab('Log Corneal Astigmatism (left eye)')
plot_grid(age_ca.right,age_ca.left,age_log_ca.right,age_log_ca.left,labels='AUTO')
dev.off()

pdf('figure3.pdf')
ggplot(d, aes(x=age, y=right_corneal_astigmatism) ) + geom_smooth(method=lm,level=0.95)+theme_bw()+ylab('Corneal Astigmatism (right eye)')+ylim(low=0,high=2.5) + geom_text(x = 55, y = 2, label = lm_eqn(data.frame(y=d$right_corneal_astigmatism,x=d$age)), colour='blue', parse = TRUE, size=3) + geom_density_2d()
dev.off()


pdf('figureS3.pdf')
ggplot(d, aes(x=age, y=right_cylindrical_power) ) + geom_smooth(method=lm,level=0.95)+theme_bw()+ylab('Refractive Astigmatism (right eye)')+ylim(low=0,high=2.5) + geom_text(x = 55, y = 2, label = lm_eqn(data.frame(y=d$right_cylindrical_power,x=d$age)), colour='blue', parse = TRUE, size=3) + geom_density_2d()
dev.off()


png('figureS4.png')
#ggplot(d, aes(x=right_corneal_astigmatism, y=right_cylindrical_power) ) + geom_smooth(method=lm,level=0.95)+theme_bw()+xlab('Corneal Astigmatism (right eye)')+ylab('Refractive Astigmatism (right eye)')+ylim(low=0,high=2.5) + geom_text(x = 55, y = 2, label = lm_eqn(data.frame(y=d$right_cylindrical_power,x=d$right_corneal_astigmatism)), colour='blue', parse = TRUE, size=3) + geom_density_2d()
#plot(right_cylindrical_power ~ right_corneal_astigmatism, d)
#lines(lowess(x=d$right_corneal_astigmatism,y=d$right_corneal_astigmatism), col="red")
library(car)
scatterplot(right_corneal_astigmatism ~ right_cylindrical_power , data=d)
dev.off()

boxplot(townsend_deprivation_index ~ right.amblyopia.eye, d)

png('ast_town.png')
plot(right_astigmatism ~ townsend_deprivation_index, d)
abline(lm(right_astigmatism ~ townsend_deprivation_index, d),col='red')
dev.off()

pdf('figureS7.pdf')
boxplot(townsend_deprivation_index ~ age.group, data=d,las=2,ylab='Townsend Deprivation Index')
#abline(m<-lm(townsend_deprivation_index ~ age,d), col='red')
dev.off()



