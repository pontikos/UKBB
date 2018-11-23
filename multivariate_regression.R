
xx <- c()

# Age, years
xx <- c(xx, 'age')

# Sex (baseline = female) 
xx <- c(xx, 'gender')

# Ethnicity (baseline = white)
# Asian
# Black
# Chinese
# Mixed 
# Others
xx <- c(xx, 'ethnicity_code')

xx <- c(xx, 'skin_colour')
xx <- c(xx, 'hair_colour')
xx <- c(xx, 'uv_protection')
xx <- c(xx, 'alcohol_intake')

# Month of birth
#t(t(table(d$f.52.0.0)))
#univariate('left_corneal_astigmatism','f.52.0.0',d)

# corneal corrected
# IOPg, mmHg
d$left_iop_cc <- d$f.5262.0.0
xx <- c(xx, 'left_iop_cc')

# Corrected Visual acuity, logMAR
d$left_logmar <- d$f.5208.0.0
xx <- c(xx, 'left_logmar')

# Corneal hysteresis
xx <- c(xx, 'corneal_hysterisis_left')

# Height, m
d$height <- d$f.50.0.0
xx <- c(xx, 'height')

# Weight, 10 kg
d$weight <- d$f.23098.0.0
xx <- c(xx,'weight')

# SBP, mmHg
d$SBP <- d$f.4080.0.0
xx <- c(xx,'SBP')

# DBP, mmHg
d$DBP <- d$f.4079.0.0
xx <- c(xx,'DBP')


xx <- unique(xx)


s <- summary(m <- lm(as.formula(paste('log(left_astigmatism) ~', paste(xx,collapse=' + '))),data=d))
print(s$coefficients)
confint(m)


s <- summary(m <- lm(as.formula(paste('log(left_astigmatism) ~ weight + height + gender')),data=d))
print(s$coefficients)
confint(m)

s <- summary(m <- lm(as.formula(paste('log(left_astigmatism) ~ weight + height')),data=d))
print(s$coefficients)
confint(m)


s <- summary(m <- lm(as.formula(paste('log(left_astigmatism) ~ gender + height')),data=d))
print(s$coefficients)
confint(m)

s <- summary(m <- lm(as.formula(paste('log(left_astigmatism) ~ gender + weight')),data=d))
print(s$coefficients)
confint(m)


