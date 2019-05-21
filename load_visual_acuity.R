#Field ID	Description
#20057	Reason for skipping visual acuity (left)
#20262	Myopia diagnosis
#d$myopia_diagnosis <- list('2'='highly myopic','1'='moderate/low myopia','0'='non-myopic')[as.character(d$f.20262.0.0)]
#20261	avMSE
#20056	Reason for skipping visual acuity (right)
#6075	Glasses worn/required (left)
d$left_glasses_worn_required <- list('0'='none','1'='wearing','2'='elsewhere')[as.character(d$f.6075.0.0)]
#6074	Glasses worn/required (right)
d$right_glasses_worn_required <- list('0'='none','1'='wearing','2'='elsewhere')[as.character(d$f.6074.0.0)]
#5187	Visual acuity measured (left)
d$left_VA_measured <- d$f.5187.0.0
#5185	Visual acuity measured (right)
d$right_VA_measured <- d$f.5185.0.0
#5211	Distance of viewer to screen (left)
5204	Distance of viewer to screen (right)
5212	Direct or mirror view (left)
5205	Direct or mirror view (right)
5074	Number of letters shown in round (left)
5075	Number of letters shown in round (right)
5081	Displayed letters in round (left)
5080	Displayed letters in round (right)
5077	Number of letters correct in round (left)
5076	Number of letters correct in round (right)
5207	Final number of letters displayed (left)
5200	Final number of letters displayed (right)
5209	Number of rounds to result (left)
5202	Number of rounds to result (right)
#5206	logMAR, initial (left)
#5199	logMAR, initial (right)
#5078	logMAR in round (left)
#5079	logMAR in round (right)
#5208	logMAR, final (left)
d$left_logMAR <- d$f.5208.0.0
#5201	logMAR, final (right)
d$right_logMAR <- d$f.5201.0.0
5082	Visual acuity result in round (left)
5083	Visual acuity result in round (right)
5188	Duration visual-acuity screen displayed (left)
5186	Duration visual-acuity screen displayed (right)
