
dict<-read('ukb6749.csv')

previous.description <- ''

for (i in 1:nrow(dict)) {
    description <- dict[i,'Description']
    if (description=='') {
        dict[i,'Description']<-previous.description
    } else {
        previous.description<-description
    }
}


dict$UDI <- paste('f',gsub('-','.',dict$UDI),sep='.') 
rownames(dict) <- dict$UDI


#write.csv(dict,file='ukb6749.csv',row.names=FALSE)

