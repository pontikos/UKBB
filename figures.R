
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






pdf('figures.pdf')


# astigmatism distribution density
plot(density(d$left_astigmatism))
lines(density(d$right_astigmatism),lty=2, col='red')

# symmetry of astigmatism
plot(left_astigmatism~right_astigmatism,data=d)
abline(b=1,a=0)
abline(lm(left_astigmatism ~ right_astigmatism, data=d), col='red')

plot(log(left_astigmatism)~log(right_astigmatism),data=d)
abline(b=1,a=0)
abline(lm(log(left_astigmatism) ~ log(right_astigmatism), data=d), col='red')


# astigmatism log transforrmed distribution
plot(density(trans(d$left_astigmatism)))
lines(density(trans(d$right_astigmatism)),lty=2)

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





