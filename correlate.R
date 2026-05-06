library(readxl)
td <- SL_LSZUIXIN[c(1:19)]

install.packages('corrplot')
library(corrplot)

M<-cor(td,method = "spearman")

testRes<-cor.mtest(td,conf.level = 0.95)
testRes$p

corrplot(M, method = "color", col = COL1("Greys"), 
         tl.col = "black", tl.cex = 0.7, tl.srt = 90,tl.pos = "lt",
         p.mat = testRes$p, diag = T, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.8,
         insig = 'label_sig', pch.col = 'white')

corrplot(M, method = "number",
         number.cex = 0.63,type = "lower",
         tl.col = "n", tl.cex = 0.7, tl.pos = "n",
         col = COL1("Greys"), addCoef.col = "white",
         insig = "blank",
         add = T)


