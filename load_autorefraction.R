
#Field ID	Description
#20052	Reason for skipping refractometry (left)
#5193	Duration at which refractometer first shown (left)
#20055	Reason for skipping refractometry (right)
#5190	Duration at which refractometer first shown (right)
#5191	Auto-refraction method (left)
#5189	Auto-refraction method (right)
#5111	3mm asymmetry angle (left)
#5108	3mm asymmetry angle (right)
#5141	3mm asymmetry index unreliable (left)
#5144	3mm asymmetry index unreliable (right)
#5156	3mm asymmetry index (left)
#5159	3mm asymmetry index (right)
#5155	3mm asymmetry index for irregular astigmatism level (left)
#5152	3mm asymmetry index for irregular astigmatism level (right)
#5112	3mm cylindrical power angle (left)
#5115	3mm cylindrical power angle (right)
#5119	3mm cylindrical power (left)
#5116	3mm cylindrical power (right)
#5292	3mm index of best keratometry results (left)
#5237	3mm index of best keratometry results (right)
#5136	3mm keratometry result unreliable (left)
#5140	3mm keratometry result unreliable (right)
#5163	3mm regularity index (left)
#5160	3mm regularity index (right)
#5148	3mm regularity index unreliable (left)
#5145	3mm regularity index unreliable (right)
#5149	3mm regularity index for irregular astigmatism level (left)
#5164	3mm regularity index for irregular astigmatism level (right)
#5104	3mm steep meridian angle (left)
d$left_3mm_steep_meridian_angle <- rowMeans(d[,grep("f.5104.0.",names(d),value=TRUE)],na.rm=T)
#5107	3mm steep meridian angle (right)
d$right_3mm_steep_meridian_angle <- rowMeans(d[,grep("f.5107.0.",names(d),value=TRUE)],na.rm=T)
#5135	3mm steep meridian (left)
d$left_3mm_steep_meridian <- rowMeans(d[,grep("f.5135.0.",names(d),value=TRUE)],na.rm=T)
d$left_3mm_steep_meridian.2 <- rowMeans(d[,grep("f.5135.1.",names(d),value=TRUE)],na.rm=T)
#5132	3mm steep meridian (right)
d$right_3mm_steep_meridian <- rowMeans(d[,grep("f.5132.0.",names(d),value=TRUE)],na.rm=T)
d$right_3mm_steep_meridian.2 <- rowMeans(d[,grep("f.5132.1.",names(d),value=TRUE)],na.rm=T)
#5103	3mm flat meridian angle (left)
#5100	3mm flat meridian angle (right)
#5096	3mm flat meridian (left)
d$left_3mm_flat_meridian <- rowMeans(d[,grep('f.5096.0.',names(d),value=TRUE)],na.rm=TRUE)
d$left_3mm_flat_meridian.2 <- rowMeans(d[,grep('f.5096.1.',names(d),value=TRUE)],na.rm=TRUE)
#5099	3mm flat meridian (right)
d$right_3mm_flat_meridian <- rowMeans(d[,grep('f.5099.0.',names(d),value=TRUE)],na.rm=TRUE)
d$right_3mm_flat_meridian.2 <- rowMeans(d[,grep('f.5099.1.',names(d),value=TRUE)],na.rm=TRUE)
#5306	6mm index of best keratometry results (left)
#5251	6mm index of best keratometry results (right)
#5138	6mm keratometry result unreliable (left)
#5139	6mm keratometry result unreliable (right)
#5110	6mm asymmetry angle (left)
#5109	6mm asymmetry angle (right)
#5157	6mm asymmetry index (left)
#5158	6mm asymmetry index (right)
#5142	6mm asymmetry index unreliable (left)
#5143	6mm asymmetry index unreliable (right)
#5113	6mm cylindrical power angle (left)
#5114	6mm cylindrical power angle (right)
#5118	6mm cylindrical power (left)
#5117	6mm cylindrical power (right)
#5162	6mm regularity index (left)
#5161	6mm regularity index (right)
#5147	6mm regularity index unreliable (left)
#5146	6mm regularity index unreliable (right)
#5105	6mm steep meridian angle (left)
#5106	6mm steep meridian angle (right)
#5134	6mm steep meridian (left)
d$left_6mm_steep_meridian <- rowMeans(d[,c('f.5134.0.0','f.5134.0.1','f.5134.0.2','f.5134.0.3','f.5134.0.4','f.5134.0.5')],na.rm=TRUE)
#5133	6mm steep meridian (right)
d$right_6mm_steep_meridian <- rowMeans(d[,c('f.5133.0.0','f.5133.0.1','f.5133.0.2','f.5133.0.3','f.5133.0.4','f.5133.0.5')],na.rm=TRUE)
#5102	6mm flat meridian angle (left)
#5101	6mm flat meridian angle (right)
#5097	6mm flat meridian (left)
d$left_6mm_flat_meridian <- rowMeans(d[,c('f.5097.0.0','f.5097.0.1','f.5097.0.2','f.5097.0.3','f.5097.0.4','f.5097.0.5')],na.rm=TRUE)
#5098	6mm flat meridian (right)
d$right_6mm_flat_meridian <- rowMeans(d[,c('f.5098.0.0','f.5098.0.1','f.5098.0.2','f.5098.0.3','f.5098.0.4','f.5098.0.5')],na.rm=TRUE)
#5089	Astigmatism angle (left)
#5088	Astigmatism angle (right)
#5086	Cylindrical power (left)
d$left_cylindrical_power <- rowMeans(d[,grep('f.5086.0.',names(d))],na.rm=TRUE)
d$left_cylindrical_power.2 <- rowMeans(d[,grep('f.5086.1.',names(d))],na.rm=TRUE)
#5087	Cylindrical power (right)
d$right_cylindrical_power <- rowMeans(d[,grep('f.5087.0.',names(d))],na.rm=TRUE)
d$right_cylindrical_power.2 <- rowMeans(d[,grep('f.5087.1.',names(d))],na.rm=TRUE)
#5085	Spherical power (left)
d$left_spherical_power <- rowMeans(d[,grep('f.5085.0.',names(d))],na.rm=TRUE)
d$left_spherical_power.2 <- rowMeans(d[,grep('f.5085.1.',names(d))],na.rm=TRUE)
#5084	Spherical power (right)
d$right_spherical_power <- rowMeans(d[,grep('f.5084.0.',names(d))],na.rm=TRUE)
d$right_spherical_power.2 <- rowMeans(d[,grep('f.5084.1.',names(d))],na.rm=TRUE)
#5274	Vertex distance (left)
#5215	Vertex distance (right)
#5276	Index of best refractometry result (left)
#5221	Index of best refractometry result (right)
#5090	Refractometry result unreliable (left)
#5091	Refractometry result unreliable (right)
#5273	Auto-refractor device ID (left)
#5214	Auto-refractor device ID (right)

# spherical equivalent
d$left_spherical_equivalent <- d$left_spherical_power+.5*d$left_cylindrical_power
d$right_spherical_equivalent <- d$right_spherical_power+.5*d$right_cylindrical_power
d$left_spherical_equivalent.2 <- d$left_spherical_power.2+.5*d$left_cylindrical_power.2
d$right_spherical_equivalent.2 <- d$right_spherical_power.2+.5*d$right_cylindrical_power.2

# corneal 3mm astigmatism  = 3mm steep meridian  -  3mm flat meridian 
d$right_3mm_corneal_astigmatism <- d$right_3mm_astigmatism <- d$right_corneal_astigmatism <- d$right_3mm_steep_meridian - d$right_3mm_flat_meridian
d$right_3mm_corneal_astigmatism.2 <- d$right_3mm_steep_meridian.2-d$right_3mm_flat_meridian.2
d$left_3mm_corneal_astigmatism <- d$left_3mm_astigmatism <- d$left_corneal_astigmatism <- d$left_3mm_steep_meridian - d$left_3mm_flat_meridian
d$left_3mm_corneal_astigmatism.2 <- d$left_3mm_steep_meridian.2-d$left_3mm_flat_meridian.2


# 6mm astigmatism  = 6mm steep meridian  -  6mm flat meridian 
d$right_6mm_corneal_astigmatism <- d$right_6mm_steep_meridian-d$right_6mm_flat_meridian
d$left_6mm_corneal_astigmatism <- d$left_6mm_steep_meridian-d$left_6mm_flat_meridian
# spherical equivalent
d$right_spherical_equivalent <- d$right_spherical_power+.5*d$right_cylindrical_power
d$right_emmetropia <- -.99< d$right_spherical_equivalent & d$right_spherical_equivalent < .99
d$right_low_primary_myopia <- -1 < d$right_spherical_equivalent & d$right_spherical_equivalent < -2.99
d$moderate_primary_myopia
d$high_primary_myopia 
d$low_hypermetropia <- 1 < d$right_spherical_equivalent & d$right_spherical_equivalent < 2.99
d$moderate_hypermetropia
d$high_hypermetropia

d$eye.anisometropia <- abs(d$left_spherical_equivalent-d$right_spherical_equivalent)>=10

#X <- d[which(d$eye.anisometropia),grep('eye',names(X),value=T)]

# 3mm regularity index
print(table(d$left_3mm_regularity_index <- d$f.5163.0.0))
print(table(d$right_3mm_regularity_index <- d$f.5160.0.0))

#  3mm regularity index unreliable
print(table(d$left_3mm_regularity_index_unreliable <- d$f.5148.0.0))
print(table(d$right_3mm_regularity_index_unreliable <- d$f.5145.0.0))

# 3mm _asymmetry_ index for irregular astigmatism level
print(table(d$left_3mm_asymmetry_index_for_irregular_astigmatism_level <- d$f.5155.0.0))
print(table(d$right_3mm_asymmetry_index_for_irregular_astigmatism_level  <- d$f.5152.0.0))

# 3mm _regularity_ index for irregular astigmatism level
print(table(d$right_3mm_regularity_index_for_irregular_astigmatism <- d$f.5149.0.0))
print(table(d$left_3mm_regularity_index_for_irregular_astigmatism  <- d$f.5164.0.0))

table(d$right_3mm_asymmetry_index_for_irregular_astigmatism,d$right_3mm_regularity_index_for_irregular_astigmatism)
table(d$left_3mm_asymmetry_index_for_irregular_astigmatism,d$left_3mm_regularity_index_for_irregular_astigmatism)

d.right_irregular_astigmatism <- d[which( d$right_3mm_asymmetry_index_for_irregular_astigmatism==3 & d$right_3mm_regularity_index_for_irregular_astigmatism==3 ),]

d.left_irregular_astigmatism <- d[which( d$left_3mm_asymmetry_index_for_irregular_astigmatism==3 & d$left_3mm_regularity_index_for_irregular_astigmatism==3 ),]

# 3mm flat meridian (right)
# visit 0 (2006-2010) astigmatism
# visit 1 (2012-2013) astigmatism
#d$f.5132.1 <- rowMeans(d[,c('f.5132.1.0','f.5132.1.1','f.5132.1.2','f.5132.1.3','f.5132.1.4','f.5132.1.5')],na.rm=TRUE)
# 3mm flat meridian (right)
# 3mm steep meridian (right): visit 0 (2006-2010) astigmatism
#d$right_3mm_flat_meridian <- d$f.5132 <- ifelse(is.na(d$f.5132.0), d$f.5132.1, d$f.5132.0)
d$right_3mm_steep_meridian <- d$f.5132.0 <- rowMeans(d[,c("f.5132.0.0","f.5132.0.1","f.5132.0.2","f.5132.0.3","f.5132.0.4","f.5132.0.5")],na.rm=T)
d$right_3mm_flat_meridian <- d$f.5099.0 <- rowMeans(d[,c("f.5099.0.0", "f.5099.0.1", "f.5099.0.2", "f.5099.0.3", "f.5099.0.4", "f.5099.0.5")],na.rm=T)
#d$f.5099.1 <- rowMeans(d[,c("f.5099.1.0", "f.5099.1.1", "f.5099.1.2", "f.5099.1.3", "f.5099.1.4", "f.5099.1.5")],na.rm=T)
# 3mm steep meridian (right)
#d$f.5099 <- ifelse(is.na(d$f.5099.0), d$f.5099.1, d$f.5099.0)

# 3mm steep meridian (left): visit 0 (2006-2010) astigmatism
# array indices run from 0 to 5
d$left_3mm_steep_meridian <- d$f.5135.0 <- rowMeans(d[,c("f.5135.0.0", "f.5135.0.1", "f.5135.0.2", "f.5135.0.3", "f.5135.0.4", "f.5135.0.5")],na.rm=T)
d$left_3mm_flat_meridian <- d$f.5096.0 <- rowMeans(d[,c("f.5096.0.0", "f.5096.0.1", "f.5096.0.2", "f.5096.0.3", "f.5135.0.4", "f.5135.0.5")],na.rm=T)
# Use whatever visit is available.
#d$left_3mm_steep_meridian <- ifelse(is.na(d$f.5135.0), d$f.5135.1, d$f.5135.0)

# Astigmatism defined as:
# Astigmatism 3mm right =  3mm steep meridian (right) 5132-0.0 minus  3mm flat meridian (right) 5099-0.0 ie. ((5132-0.0) – (5099-0.0))
# 3mm astigmatism (right) = 3mm steep meridian (right) -  3mm flat meridian (right)
d$right_3mm_astigmatism <- d$right_3mm_steep_meridian - d$right_3mm_flat_meridian
d$left_3mm_astigmatism <- d$left_3mm_steep_meridian - d$left_3mm_flat_meridian

# visit 0 (2006-2010) astigmatism . Average measurements per visit for increased accuracy.
#d$f.5096.0 <- rowMeans(d[,c('f.5096.0.0','f.5096.0.1','f.5096.0.2','f.5096.0.3','f.5096.0.4','f.5096.0.5')],na.rm=TRUE)
# visit 1 (2012-2013) astigmatism . Average measurements per visit for increased accuracy.
#d$f.5096.1 <- rowMeans(d[,c('f.5096.1.0','f.5096.1.1','f.5096.1.2','f.5096.1.3','f.5096.1.4','f.5096.1.5')],na.rm=TRUE)
# Use whatever visit is available.
#d$left_3mm_flat_meridian <- ifelse(is.na(d$f.5096.0), d$f.5096.1, d$f.5096.0)

# 5111	3mm asymmetry angle (left)
# 5108	3mm asymmetry angle (right)
d$left_3mm_asymmetry_angle <- d$f.5111.0.0
d$right_3mm_asymmetry_angle <- d$f.5108.0.0


# 3mm cylindrical power
d$right_3mm_cylindrical_power <- d$f.5116.0.0
d$left_3mm_cylindrical_power <- d$f.5119.0.0
# 3mm cylindrical power angle
d$left_3mm_cylindrical_power_angle <- d$f.5112.0.0
d$right_3mm_cylindrical_power_angle <- d$f.5115.0.0


d$right_3mm_steep_meridian <- d$f.5132.0.0
d$right_3mm_flat_meridian <- d$f.5099.0.0

d$left_3mm_steep_meridian <- d$f.5135.0.0
d$left_3mm_flat_meridian <- d$f.5096.0.0

d$right_mean_corneal_power <- (d[,'right_3mm_steep_meridian'] + d[,'right_3mm_flat_meridian'])/2
d$left_mean_corneal_power <- (d[,'left_3mm_steep_meridian'] + d[,'left_3mm_flat_meridian'])/2
d$right_3mm_mean_corneal_power <- (d$right_3mm_steep_meridian + d$right_3mm_flat_meridian)/2
d$left_3mm_mean_corneal_power <- (d$left_3mm_steep_meridian + d$left_3mm_flat_meridian)/2

d$right_spherical_power <- d$f.5084.0.0
d$left_spherical_power <- d$f.5085.0.0
d$right_3mm_spherical_equivalent <- d$right_spherical_equivalent <- d$right_spherical_power + .5 * d$right_3mm_cylindrical_power
d$left_3mm_spherical_equivalent <- d$left_spherical_equivalent <- d$left_spherical_power + .5 * d$left_3mm_cylindrical_power


# axis of astigmatism
#5107 3mm steep meridian angle (right) #5100 3mm flat meridian angle (right)
d$right_3mm_steep_meridian_angle<-d$f.5107.0.0
d$right_3mm_flat_meridian_angle<-d$f.5100.0.0
d$right_axis_of_astigmatism <- d$right_3mm_steep_meridian_angle-d$right_3mm_flat_meridian_angle
d$right_axis_of_astigmatism <- d$right_3mm_steep_meridian_angle
#5104 3mm steep meridian angle (left) #5103 3mm flat meridian angle (left)
d$left_3mm_steep_meridian_angle<-d$f.5104.0.0
d$left_3mm_flat_meridian_angle<-d$f.5103.0.0
d$left_axis_of_astigmatism <- d$left_3mm_steep_meridian_angle-d$left_3mm_flat_meridian_angle

d$pos_right_axis_of_astigmatism <- d$right_axis_of_astigmatism>0
d$pos_left_axis_of_astigmatism <- d$left_axis_of_astigmatism>0


# “The astigmatism was then categorized as with-the-rule, against-the rule, or oblique.
# Keratometric and total astigmatism were considered to be with-the-rule when the steepest meridian was between 60° and 119°.
# Those with the steepest meridian of 0° to 29° or 150° to 180° were considered to have against-the-rule astigmatism.
# All others were considered oblique astigmatism. “
d$right_axis <- 'OB'
d$right_axis[which(60<d$right_3mm_steep_meridian_angle&d$right_3mm_steep_meridian_angle<119)] <- 'WTR'
d$right_axis[which((0<d$right_3mm_steep_meridian_angle&d$right_3mm_steep_meridian_angle<29)|(150<d$right_3mm_steep_meridian_angle&d$right_3mm_steep_meridian_angle<180))] <- 'ATR'

d$left_axis <- 'OB'
d$left_axis[which(60<d$left_3mm_steep_meridian_angle&d$left_3mm_steep_meridian_angle<119)] <- 'WTR'
d$left_axis[which((0<d$left_3mm_steep_meridian_angle&d$left_3mm_steep_meridian_angle<29)|(150<d$left_3mm_steep_meridian_angle&d$left_3mm_steep_meridian_angle<180))] <- 'ATR'




