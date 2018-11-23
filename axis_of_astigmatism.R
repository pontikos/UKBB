
png('age_right_axis_of_astigmatism.png')
#plot(d$age,d$right_axis_of_astigmatism,xlab='age',ylab='right axis of astigmatism')
par(mfrow=c(1,2))
boxplot( age ~ right_axis_of_astigmatism, d)
boxplot( age ~ left_axis_of_astigmatism, d)
#abline(s<-lm(right_axis_of_astigmatism ~ age, d),col='red')
dev.off()


png('ethnicity_right_axis_of_astigmatism.png')
#plot(d$age,d$right_axis_of_astigmatism,xlab='age',ylab='right axis of astigmatism')
boxplot( right_axis_of_astigmatism ~ ethnicity, d)
boxplot( left_axis_of_astigmatism ~ ethnicity, d)
#abline(s<-lm(right_axis_of_astigmatism ~ age, d),col='red')
dev.off()



