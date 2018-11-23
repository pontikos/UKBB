
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

# Figure 2
# cylindrical power vs age
par(mfrow=c(3,2))
boxplot(right_cylindrical_power ~ age.group, data=d,las=2,ylab='right cylindrical power')
summary(m<-lm(right_cylindrical_power ~ age,d), col='red')
confint(m)
boxplot(left_cylindrical_power ~ age.group, data=d,las=2,ylab='left cylindrical power')
summary(m<-lm(left_cylindrical_power ~ age,d), col='red')
confint(m)
# mean corneal power vs age
boxplot(right_mean_corneal_power ~ age.group ,d, ylab='right mean corneal power',las=2)
print(summary(lm(right_mean_corneal_power ~ age, data=d)->m.right_mean_corneal_power.age))
print(confint(m.right_mean_corneal_power.age))
#abline(m.right_mean_corneal_power.age, col='red')
boxplot(left_mean_corneal_power ~ age.group ,d, ylab='left mean corneal power',las=2)
print(summary(lm(left_mean_corneal_power ~ age, data=d)->m.left_mean_corneal_power.age))
print(confint(m.left_mean_corneal_power.age))
#abline(m.left_mean_corneal_power.age, col='red')
# axis of astigmatism with age
pt <- prop.table(table(d$right_axis_of_astigmatism>0,d$age.group),2)
barplot(pt[,-1],ylab = 'right axis of astigmatism > 0',las=2)
pt <- prop.table(table(d$left_axis_of_astigmatism>0,d$age.group),2)
barplot(pt[,-1],ylab = 'left axis of astigmatism > 0 ',las=2)

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
x <- list(astigmatism=log(d[which(d$astigmatism.eye=='R'),'right_corneal_astigmatism']),
     myopia=log(d[which(d$myopia.eye=='R'),'right_corneal_astigmatism']),
     hypermetropia=log(d[which(d$hypermetropia.eye=='R'),'right_corneal_astigmatism']),
     presbyopia=log(d[which(d$presbyopia.eye=='R'),'right_corneal_astigmatism']),
     amblyopia=log(d[which(d$amblyopia.eye=='R'),'right_corneal_astigmatism']),
     strabismus=log(d[which(d$strabismus.eye=='R'),'right_corneal_astigmatism']),
     cataract=log(d[which(d$cataract.eye=='R'),'right_corneal_astigmatism']))
par(mar=c(7,5,1,1),mfrow=c(1,2))
boxplot(x[names(x)[order(unlist(lapply(x,median)))]],las=2,main='right',ylab='log(3mm corneal astigmatism)')
x <- list(astigmatism=log(d[which(d$astigmatism.eye=='L'),'left_corneal_astigmatism']),
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
