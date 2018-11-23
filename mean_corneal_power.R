

png('age_right_mean_corneal_power.png')
plot(d$age,d$right_mean_corneal_power,xlab='age',ylab='right mean corneal power')
abline(lm(right_mean_corneal_power ~ age, d),col='red')
dev.off()


