#Category ID	Description	Items
#100066 #Household	 11
#100064 #Employment	 11
#50498Data-Field 738
#Description:	Average total household income before tax
#d$household_incomde_before_tax <- d$f.738.0.0
  
#100063 #Education	 3
#Field ID	Description
#6138	Qualifications
d$qualifications <- as.character(d$f.6138.0.0)
d$qualifications[which(is.na(d$qualifications))] <- '-3'
d$qualifications <- as.character(list('1'='College or University degree','2'='A levels/AS levels or equivalent','3'='O levels/GCSEs or equivalent','4'='CSEs or equivalent','5'='NVQ or HND or HNC or equivalent','6'='Other professional qualifications eg: nursing, teaching','-7'='None of the above','-3'='Prefer not to answer')[as.character(d$qualifications)])
#10722	Qualifications (pilot)
#845	Age completed full time education
d$age_completed_full_time_education <- d$f.845.0.0
#100065 #Ethnicity	 2
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
#table( d$ethnicity_code <- factor(d$ethnicity, labels=names(sort(by(d$left_corneal_astigmatism,d$ethnicity,mean),decreasing=TRUE))))
table( d$ethnicity_code <- factor(d$ethnicity, levels=names(sort(by(d$left_corneal_astigmatism,d$ethnicity,mean),decreasing=TRUE))))
#print(t(t(table(d$ethnicity_code <- as.numeric(d$ethnicity_code)))))
#print(t(t(table(d$ethnicity))))

#100067#Other sociodemographic factors	 2
