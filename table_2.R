
epsilon <- 1e-20

females <- which(d$gender=='F')
males <- which(d$gender=='M')
age.40 <- which(40<=d$age & d$age<49)
age.50 <- which(50<=d$age & d$age<59)
age.60 <- which(60<=d$age & d$age<69)

left_corneal_astigmatism <- d$left_corneal_astigmatism
right_corneal_astigmatism <- d$right_corneal_astigmatism

f <- function (x) {
    return(sprintf('%.3f (%.3f, %.3f-%.3f)', mean(x), sd(x), quantile(x)[['25%']], quantile(x)[['75%']]))
}

table.1 <- data.frame()

table.1['Men 40-49','right_corneal_astigmatism'] <- f(right_corneal_astigmatism[intersect(males,age.40)])
table.1['Men 40-49','left_corneal_astigmatism'] <- f(left_corneal_astigmatism[intersect(males,age.40)])
#table.1['Men 40-49','left_KC']

table.1['Men 50-59','right_corneal_astigmatism'] <- f(right_corneal_astigmatism[intersect(males,age.50)])
table.1['Men 50-59','left_corneal_astigmatism'] <- f(left_corneal_astigmatism[intersect(males,age.50)])
table.1['Men 60-69','right_corneal_astigmatism'] <- f(right_corneal_astigmatism[intersect(males,age.60)])
table.1['Men 60-69','left_corneal_astigmatism'] <- f(left_corneal_astigmatism[intersect(males,age.60)])

table.1['Women 40-49','right_corneal_astigmatism'] <- f(right_corneal_astigmatism[intersect(females,age.40)])
table.1['Women 40-49','left_corneal_astigmatism'] <- f(left_corneal_astigmatism[intersect(females,age.40)])
table.1['Women 50-59','right_corneal_astigmatism'] <- f(right_corneal_astigmatism[intersect(females,age.50)])
table.1['Women 50-59','left_corneal_astigmatism'] <- f(left_corneal_astigmatism[intersect(females,age.50)])
table.1['Women 60-69', 'right_corneal_astigmatism'] <- f(right_corneal_astigmatism[intersect(females,age.60)])
table.1['Women 60-69','left_corneal_astigmatism'] <- f(left_corneal_astigmatism[intersect(females,age.60)])

table.1['Total 40-49','right_corneal_astigmatism'] <- f(right_corneal_astigmatism[age.40])
table.1['Total 40-49','left_corneal_astigmatism'] <- f(left_corneal_astigmatism[age.40])

table.1['Total 50-59', 'right_corneal_astigmatism'] <- f(right_corneal_astigmatism[age.50])
table.1['Total 50-59', 'left_corneal_astigmatism'] <- f(left_corneal_astigmatism[age.50])

table.1['Total 60-69', 'right_corneal_astigmatism'] <- f(right_corneal_astigmatism[age.60])
table.1['Total 60-69', 'left_corneal_astigmatism'] <- f(left_corneal_astigmatism[age.60])

table.1['All', 'right_corneal_astigmatism'] <- f(right_corneal_astigmatism)
table.1['All', 'left_corneal_astigmatism'] <- f(left_corneal_astigmatism)

x <-right_corneal_astigmatism-left_corneal_astigmatism

bonferroni <- .05/nrow(d)

#s <- t.test(log(right_corneal_astigmatism+epsilon),log(left_corneal_astigmatism+epsilon),paired=TRUE)
s <- t.test(right_corneal_astigmatism,left_corneal_astigmatism,paired=TRUE)

table.1['Difference (right-left)',1] <- sprintf("%.3f (%.3f %.2f), P=%f", s$estimate, s$conf.int[1], s$conf.int[2], s$p.value)
table.1['Difference (right-left)',2] <- ''

#write.table(table.1,file='',sep='\t')
table.1$cohorts <- rownames(table.1)

table.1 <- table.1[,c('cohorts','right_corneal_astigmatism','left_corneal_astigmatism')]

write.table(table.1,file='',sep=',')



