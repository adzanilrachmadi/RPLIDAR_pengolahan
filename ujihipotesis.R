
library(readxl)

#uji hipotesis spearman
  #uji korelasi berdasarkan rpm
  cor.test(dataujirpm$rpm,dataujirpm$Polutan, method = "spearman")
  #uji korelasi berdasarkan jarak
  cor.test(dataujijarak$Jarak,dataujijarak$Polutan, method = "spearman")
  #uji korelasi ganda 
  cor.test(dataujiganda$JRPM,dataujiganda$Polutan, method = "spearman")




#uji hipotesis pearson
cor.test(manipulasi$JRPM,manipulasi$Polutan, method = "pearson")
#uji hipotesis spearman
cor.test(manipulasi$JRPM,manipulasi$Polutan, method = "spearman")
