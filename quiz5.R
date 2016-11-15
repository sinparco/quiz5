library(haven)
library(apaTables)

#Loading Data
my.data <- read.csv("reg_quiz2_data.csv")
View(my.data)

#Cor Table
apa.cor.table(my.data, filename="Table1_APA.doc", table.number=1")

#Testing Self esteem and Academic Success
reg1 <- lm(aSuc ~ selfEsteem, data=my.data);summary(reg1)
apa.reg.table(reg1, filename="Table2_APA.doc", table.number=2) #Con accounts for 23% of the distribution beyond Negative Self Esteem alone (Part Correlation)
) #GMA accounts for 24% of the distribution


#Testing Self esteem and Academic Success above and beyond PAS
reg2 <- lm(aSuc ~ selfEsteem + PAS, data=my.data);summary(reg2)
apa.reg.table(reg2, ) #Con accounts for 22% of the distribution beyond Positive Self Esteem alone (Part Correlation)

#Testing Self esteem and Academic Success above and beyond NAS
reg3 <- lm(aSuc ~ selfEsteem + NAS, data=my.data);summary(reg3)
apa.reg.table(reg3, filename="Table3_APA.doc", table.number=3) #Con accounts for 23% of the distribution beyond Negative Self Esteem alone (Part Correlation)

#Testing Self esteem and Academic Success above and beyond PAS and NAS
block1 <- lm(aSuc ~ NAS + PAS, data=my.data, na.action=na.exclude) 
block2 <- lm(aSuc ~ NAS + PAS + selfEsteem, data=my.data, na.action=na.exclude)
apa.reg.table(block1, block2) #Interactions increase betwen the difference between the R2
