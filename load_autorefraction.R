
Field ID	Description
20052	Reason for skipping refractometry (left)
5193	Duration at which refractometer first shown (left)
20055	Reason for skipping refractometry (right)
5190	Duration at which refractometer first shown (right)
5191	Auto-refraction method (left)
5189	Auto-refraction method (right)
5111	3mm asymmetry angle (left)
5108	3mm asymmetry angle (right)
5141	3mm asymmetry index unreliable (left)
5144	3mm asymmetry index unreliable (right)
5156	3mm asymmetry index (left)
5159	3mm asymmetry index (right)
5155	3mm asymmetry index for irregular astigmatism level (left)
5152	3mm asymmetry index for irregular astigmatism level (right)
5112	3mm cylindrical power angle (left)
5115	3mm cylindrical power angle (right)
5119	3mm cylindrical power (left)
5116	3mm cylindrical power (right)
5292	3mm index of best keratometry results (left)
5237	3mm index of best keratometry results (right)
5136	3mm keratometry result unreliable (left)
5140	3mm keratometry result unreliable (right)
5163	3mm regularity index (left)
5160	3mm regularity index (right)
5148	3mm regularity index unreliable (left)
5145	3mm regularity index unreliable (right)
5149	3mm regularity index for irregular astigmatism level (left)
5164	3mm regularity index for irregular astigmatism level (right)
#5104	3mm strong meridian angle (left)
d$left_3mm_strong_meridian_angle <- rowMeans(d[,grep("f.5104.0.",names(d),value=TRUE)],na.rm=T)
#5107	3mm strong meridian angle (right)
d$right_3mm_strong_meridian_angle <- rowMeans(d[,grep("f.5107.0.",names(d),value=TRUE)],na.rm=T)
#5135	3mm strong meridian (left)
d$left_3mm_strong_meridian <- rowMeans(d[,grep("f.5135.0.",names(d),value=TRUE)],na.rm=T)
d$left_3mm_strong_meridian.2 <- rowMeans(d[,grep("f.5135.1.",names(d),value=TRUE)],na.rm=T)
#5132	3mm strong meridian (right)
d$right_3mm_strong_meridian <- rowMeans(d[,grep("f.5132.0.",names(d),value=TRUE)],na.rm=T)
d$right_3mm_strong_meridian.2 <- rowMeans(d[,grep("f.5132.1.",names(d),value=TRUE)],na.rm=T)
#5103	3mm weak meridian angle (left)
#5100	3mm weak meridian angle (right)
#5096	3mm weak meridian (left)
d$left_3mm_weak_meridian <- rowMeans(d[,grep('f.5096.0.',names(d),value=TRUE)],na.rm=TRUE)
d$left_3mm_weak_meridian.2 <- rowMeans(d[,grep('f.5096.1.',names(d),value=TRUE)],na.rm=TRUE)
#5099	3mm weak meridian (right)
d$right_3mm_weak_meridian <- rowMeans(d[,grep('f.5099.0.',names(d),value=TRUE)],na.rm=TRUE)
d$right_3mm_weak_meridian.2 <- rowMeans(d[,grep('f.5099.1.',names(d),value=TRUE)],na.rm=TRUE)
#5306	6mm index of best keratometry results (left)
#5251	6mm index of best keratometry results (right)
5138	6mm keratometry result unreliable (left)
5139	6mm keratometry result unreliable (right)
5110	6mm asymmetry angle (left)
5109	6mm asymmetry angle (right)
5157	6mm asymmetry index (left)
5158	6mm asymmetry index (right)
5142	6mm asymmetry index unreliable (left)
5143	6mm asymmetry index unreliable (right)
5113	6mm cylindrical power angle (left)
5114	6mm cylindrical power angle (right)
5118	6mm cylindrical power (left)
5117	6mm cylindrical power (right)
5162	6mm regularity index (left)
5161	6mm regularity index (right)
5147	6mm regularity index unreliable (left)
5146	6mm regularity index unreliable (right)
5105	6mm strong meridian angle (left)
5106	6mm strong meridian angle (right)
#5134	6mm strong meridian (left)
d$left_6mm_strong_meridian <- rowMeans(d[,c('f.5134.0.0','f.5134.0.1','f.5134.0.2','f.5134.0.3','f.5134.0.4','f.5134.0.5')],na.rm=TRUE)
#5133	6mm strong meridian (right)
d$right_6mm_strong_meridian <- rowMeans(d[,c('f.5133.0.0','f.5133.0.1','f.5133.0.2','f.5133.0.3','f.5133.0.4','f.5133.0.5')],na.rm=TRUE)
#5102	6mm weak meridian angle (left)
#5101	6mm weak meridian angle (right)
#5097	6mm weak meridian (left)
d$left_6mm_weak_meridian <- rowMeans(d[,c('f.5097.0.0','f.5097.0.1','f.5097.0.2','f.5097.0.3','f.5097.0.4','f.5097.0.5')],na.rm=TRUE)
#5098	6mm weak meridian (right)
d$right_6mm_weak_meridian <- rowMeans(d[,c('f.5098.0.0','f.5098.0.1','f.5098.0.2','f.5098.0.3','f.5098.0.4','f.5098.0.5')],na.rm=TRUE)
#5089	Astigmatism angle (left)
5088	Astigmatism angle (right)
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
5274	Vertex distance (left)
5215	Vertex distance (right)
5276	Index of best refractometry result (left)
5221	Index of best refractometry result (right)
5090	Refractometry result unreliable (left)
5091	Refractometry result unreliable (right)
5273	Auto-refractor device ID (left)
5214	Auto-refractor device ID (right)


# spherical equivalent
d$left_spherical_equivalent <- d$left_spherical_power+.5*d$left_cylindrical_power
d$right_spherical_equivalent <- d$right_spherical_power+.5*d$right_cylindrical_power
d$left_spherical_equivalent.2 <- d$left_spherical_power.2+.5*d$left_cylindrical_power.2
d$right_spherical_equivalent.2 <- d$right_spherical_power.2+.5*d$right_cylindrical_power.2

# corneal 3mm astigmatism  = 3mm strong meridian  -  3mm weak meridian 
d$right_3mm_corneal_astigmatism <- d$right_3mm_astigmatism <- d$right_corneal_astigmatism <- d$right_3mm_strong_meridian - d$right_3mm_weak_meridian
d$right_3mm_corneal_astigmatism.2 <- d$right_3mm_strong_meridian.2-d$right_3mm_weak_meridian.2
d$left_3mm_corneal_astigmatism <- d$left_3mm_astigmatism <- d$left_corneal_astigmatism <- d$left_3mm_strong_meridian - d$left_3mm_weak_meridian
d$left_3mm_corneal_astigmatism.2 <- d$left_3mm_strong_meridian.2-d$left_3mm_weak_meridian.2


# 6mm astigmatism  = 6mm strong meridian  -  6mm weak meridian 
d$right_6mm_corneal_astigmatism <- d$right_6mm_strong_meridian-d$right_6mm_weak_meridian
d$left_6mm_corneal_astigmatism <- d$left_6mm_strong_meridian-d$left_6mm_weak_meridian
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

X <- d[which(d$eye.anisometropia),grep('eye',names(X),value=T)]
