
#Field ID	Description
#20117	Alcohol drinker status
#1558	Alcohol intake frequency.
#1	Daily or almost daily
#2	Three or four times a week
#3	Once or twice a week
#4	One to three times a month
#5	Special occasions only
#6	Never
#-3	Prefer not to answer
# Alcohol intake (baseline = never)
d$alcohol_intake <- d$f.1558.0.0
# prefer not to answer
d$alcohol_intake[which(d$alcohol_intake==-3)] <- NA
# 6 never
d$alcohol_intake[which(d$alcohol_intake==6)] <- 0
# 5 special occasions
d$alcohol_intake[which(d$alcohol_intake==5)] <- -1
# 4 one to three times a month
d$alcohol_intake[which(d$alcohol_intake==4)] <- -2
# 3 once twice a week
d$alcohol_intake[which(d$alcohol_intake==3)] <- -3
# 2 three four times a week
d$alcohol_intake[which(d$alcohol_intake==2)] <- -4
# 1 daily or almost daily
d$alcohol_intake[which(d$alcohol_intake==1)] <- -5
d$alcohol_intake <- abs(d$alcohol_intake)
d$alcohol_intake_code <- d$alcohol_intake 
d$alcohol_intake[which(d$alcohol_intake==0)] <- 'never'
d$alcohol_intake[which(d$alcohol_intake==1)] <- 'special occasions'
d$alcohol_intake[which(d$alcohol_intake==2)] <- 'one to three times a month'
d$alcohol_intake[which(d$alcohol_intake==3)] <- 'once twice a week'
d$alcohol_intake[which(d$alcohol_intake==4)] <- 'three four times a week'
d$alcohol_intake[which(d$alcohol_intake==5)] <- 'daily or almost daily'

# boxplot(log10(right_3mm_astigmatism) ~ alcohol_intake_code, d)

#3731	Former alcohol drinker
#4407	Average monthly red wine intake
#4418	Average monthly champagne plus white wine intake
#4429	Average monthly beer plus cider intake
#4440	Average monthly spirits intake
#4451	Average monthly fortified wine intake
#4462	Average monthly intake of other alcoholic drinks
#1568	Average weekly red wine intake
#1578	Average weekly champagne plus white wine intake
#1588	Average weekly beer plus cider intake
#1598	Average weekly spirits intake
#1608	Average weekly fortified wine intake
#5364	Average weekly intake of other alcoholic drinks
#1618	Alcohol usually taken with meals
#1628	Alcohol intake versus 10 years previously
#2664	Reason for reducing amount of alcohol drunk
#10818	Reason for reducing amount of alcohol drunk (pilot)
#3859	Reason former drinker stopped drinking alcohol
#10853	Reason former drinker stopped drinking alcohol (pilot)

#1	Daily or almost daily
#2	Three or four times a week
#3	Once or twice a week
#4	One to three times a month
#5	Special occasions only
#6	Never
#-3	Prefer not to answer