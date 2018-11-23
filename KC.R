

KC.1 <- d[which(d$right_3mm_strong_meridian>48),]
KC.2 <- d[which(d$right_3mm_strong_meridian>49),]
KC.3 <- d[which(d$right_3mm_strong_meridian>50),]

#KC.1[,'f.eid']
#KC.2[,'f.eid']
#KC.3[,'f.eid']

coding <- as.data.frame(fread('/home/pontikos/d/UKBB/coding19.tsv'))

rownames(coding) <- coding$coding

table(rowSums(KC.1[ ,grep('41202.0.',colnames(d))] =='H185',na.rm=TRUE))
table(rowSums(KC.1[ ,grep('41202.0.',colnames(d))] =='H186',na.rm=TRUE))

table(rowSums(apply(KC.1,1,function(x) { grepl('H18',x) })))


disease.codes <- unique(unlist((c(KC.1[ ,grep('41202.0.',colnames(d))]))))

coding[disease.codes , 'meaning']

cornea.disorder <- d[which( rowSums(apply(d[,grep('41202.0.',colnames(d))],2,function(x) { grepl('H18',x) }),na.rm=TRUE)>0 ),]


print(d[which(rowSums(d[ ,grep('41202.0.',colnames(d))] =='H186',na.rm=TRUE)>0),'right_3mm_strong_meridian'])
print(d[which(rowSums(d[ ,grep('41202.0.',colnames(d))] =='H186',na.rm=TRUE)>0),'left_3mm_strong_meridian'])


