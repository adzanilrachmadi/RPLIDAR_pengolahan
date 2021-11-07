
library(readxl)

#uji hipotesis pearson
cor.test(datauji2$JRPM,datauji2$Polutan, method = "pearson")
#uji hipotesis spearman
cor.test(datauji$JRPM,datauji$Polutan, method = "spearman")



#uji hipotesis pearson
cor.test(manipulasi$JRPM,manipulasi$Polutan, method = "pearson")
#uji hipotesis spearman
cor.test(manipulasi$JRPM,manipulasi$Polutan, method = "spearman")
