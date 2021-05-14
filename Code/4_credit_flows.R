
######################
### SI Figure B.3 ####
######################

rm(list = ls())
gc()

### Install packages if necessary
# install.packages("ggplot2")     # ggplot2_3.3.2
# install.packages("data.table")  # data.table_1.13.2
# install.packages("gridExtra")   # gridExtra_2.3

library(ggplot2)
library(data.table)
library(gridExtra)

# Set working directory
setwd("~/YOURPATH/replication_files")

# Load data
load("Data/credit.RData")

credit$country <- factor(credit$country, levels = credit$country
                         [order(credit$Household_Credit_Share, decreasing=T)])  

credit_hh <- credit[ oecd == 1 , .(country, Household_Credit_Share)]
credit_hh[ , type := "Households"]
setnames(credit_hh, "Household_Credit_Share", "value")

credit_ent <- credit[ oecd == 1 , .(country, Enterprise_Credit_Share)]
credit_ent[ , type := "Business"]
setnames(credit_ent, "Enterprise_Credit_Share", "value")

credit_comb <- rbind(credit_hh, credit_ent)

(plot2 <- (ggplot(credit_comb, aes(y=value*100, x=country, fill=type))
           + geom_bar(stat = "identity", width=.85, show.legend = F)
           + geom_label(x = 22, y = 75, label = "Business sector", show.legend = F, fill="#ef8a62", color="black")
           + geom_label(x = 7, y = 25, label = "Household sector", show.legend = F, fill="#67a9cf", color="black")
           + theme_bw()
           + ylab("Relative shares (%)")
           + xlab("Country")
           + ggtitle("(b) Distribution of Bank Credit by Sector")
           + coord_flip()
           + theme(legend.position="bottom", panel.grid.minor = element_blank(), 
                   plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),
                   panel.border=element_blank())
           + scale_fill_manual("Sector", values=c("#ef8a62","#67a9cf"))
))

credit2 <- credit
credit2$country <- factor(credit2$country, levels = credit2$country
                          [order(credit2$Enterprise_credit_to_GDP, decreasing=T)])  

(plot1 <- (ggplot(credit2[ oecd == 1 ], aes(y=Enterprise_credit_to_GDP*100, x=country))
           + geom_bar(stat = "identity", width=.85, position="dodge", fill="gray60")
           + theme_bw()
           + ylab("Share of GDP (%)")
           + xlab("Country")
           + ggtitle("(a) Business Credit (% of GDP)")
           + coord_flip()
           + theme(legend.position="bottom", panel.grid.minor = element_blank(), 
                   plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),
                   panel.border=element_blank())
           + scale_y_continuous(limits=c(0,110), breaks=seq(0,100,20))
))


pdf("Output/SI_figure_b3.pdf",  width=10, height=5)
grid.arrange(plot1, plot2, ncol=2)
dev.off()



