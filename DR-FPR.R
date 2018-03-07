library(ggplot2)
par(mfrow=c(1,2))
data1 <- read.table(
  header=TRUE, text='
    Criteria     Techniques              Percentage
 
1    FPR       DNS-UNSWNB-ensemble            1.38
2    DR        DNS-UNSWNB-ensemble            98.93
5    FPR       DNS-UNSWNB-SVM                 6.13
6    DR        DNS-UNSWNB-SVM                 94.58
3    FPR       DNS-NIMS-ensemble              2.01
4    DR        DNS-NIMS-ensemble              97.38
7    FPR       DNS-NIMS-BN                    7.25
8    DR        DNS-NIMS-BN                    93.17
  ')
	

ggplot(data1,aes(Techniques, Percentage, fill = Criteria)) + 
    geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values=c("green", "red"))


data2 <- read.table(
  header=TRUE, text='
    Criteria     Techniques              Percentage
 

1    FPR       HTTP-UNSWNB-ensemble           2.58
2    DR        HTTP-UNSWNB-ensemble           97.02
3    FPR       HTTP-NIMS-ensemble             2.15
4    DR        HTTP-NIMS-ensemble             97.95
5    FPR       HTTP-UNSWNB-SVM                5.81
6    DR        HTTP-UNSWNB-SVM                95.52
7    FPR       HTTP-NIMS-BN                   7.20
8    DR        HTTP-NIMS-BN                    94.11
  ')


ggplot(data2,aes(Techniques, Percentage, fill = Criteria)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values=c("green", "red"))

  
# number of domains in datasets
H <- c(412,500, 42, 684,2000)
M <- c("UNSW-NB15", "NIMS-Alexa", "NIMS-Zeus", "NIMS-Citadel","NIMS-Conficker")
# Plot the bar chart.
barplot(H,names.arg = M,xlab = "datasets",ylab = "Number of domain names",col = "blue",
        main = "",border = "red")

