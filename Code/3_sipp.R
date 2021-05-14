
#################
### SIPP DATA ###
#################

rm(list = ls())
gc()

### Install packages if necessary
# install.packages("ggplot2")     # ggplot2_3.3.2
# install.packages("reshape2")    # reshape2_1.4.4
# install.packages("lmtest")      # lmtest_0.9-38
# install.packages("sandwich")    # sandwich_3.0-0 
# install.packages("MASS")        # MASS_7.3-53
# install.packages("stargazer")   # stargazer_5.2.2
# install.packages("data.table")  # data.table_1.13.2
# install.packages("plm")         # plm_2.2-5
# install.packages("BBmisc")      # BBmisc_1.11
# install.packages("msm")         # msm_1.6.8
# install.packages("sjPlot")      # sjPlot_2.8.6
# install.packages("plyr")        # plyr_1.8.6

library(ggplot2)
library(reshape2)
library(lmtest)
library(sandwich)
library(MASS)
library(stargazer)
library(data.table)
library(plm)
library(BBmisc)
library(msm)
library(sjPlot)
library(plyr)

# Set working directory
setwd("/WholeTale/workspace")

### Load SIPP data
load("Data/sipp_data.RData")

# Employment status
sipp_data[ rmesr %in% c(3,5,6) , empl_stat_layoff := 1 ]
sipp_data[ rmesr == 1 , empl_stat_layoff := 0 ]

# Education
sipp_data[ eeducate %in% c(31:39) , educ := "high school or less" ]
sipp_data[ eeducate %in% c(40) , educ := "some college" ]
sipp_data[ eeducate %in% c(41,42,43) , educ := "college" ]
sipp_data[ eeducate %in% c(44) , educ := "BA" ]
sipp_data[ eeducate %in% c(45,46,47) , educ := "MA" ]
sipp_data[ , educ := factor(educ, levels=c("high school or less","some college","college","BA","MA"))]

# Ownership status
sipp_data[ etenure %in% c(1,3) , ownership := "own" ]
sipp_data[ etenure == 2 , ownership := "rent" ]

# Race
sipp_data[ erace == 1 , erace_v2 := "White"]
sipp_data[ erace == 2 , erace_v2 := "Black"]
sipp_data[ erace == 3 , erace_v2 := "Native American"]
sipp_data[ erace == 4 , erace_v2 := "Asian"]

sipp_data[ , erace_v2 := as.factor(erace_v2) ]
sipp_data[ , erace_v2 := relevel(erace_v2, "White") ]


### Income
# Remove few negative values
sipp_data[ thtotinc_swave < 0 , thtotinc_swave := NA ]

# Own business
sipp_data[ ebiznow1 == 1 , own_business := 1 ]
sipp_data[ ebiznow1 != 1 , own_business := 0 ]

# HH reference person
sipp_data[ epppnum == ehrefper , hh_ref_pers := 1 ]

### Inflation adjust
sipp_data[ , rhhuscbt_cpi10_adj := rhhuscbt / cpi10_index_all ]
sipp_data[ , thtotinc_swave_cpi10_adj := thtotinc_swave / cpi10_index_all ]

### IHS transformations
sipp_data[ , ihs_rhhuscbt_cpi10_adj := log(rhhuscbt_cpi10_adj + sqrt(1+rhhuscbt_cpi10_adj^2)) ]
sipp_data[ , ihs_thtotinc_swave_cpi10_adj := log(thtotinc_swave_cpi10_adj + sqrt(1+thtotinc_swave_cpi10_adj^2)) ]

### Income tertiles
sipp_data[ , thtotinc1 := lapply(.SD, function(x) quantile(x, .33, na.rm=T) ), by=c("year"), 
                  .SDcols= c("thtotinc_swave_cpi10_adj")  ]
sipp_data[ , thtotinc2 := lapply(.SD, function(x) quantile(x, .66, na.rm=T) ), by=c("year"), 
                  .SDcols= c("thtotinc_swave_cpi10_adj")  ]

sipp_data[ thtotinc_swave_cpi10_adj <   thtotinc1 , thtotinc_swave_tert := 1 ]
sipp_data[ thtotinc_swave_cpi10_adj >=  thtotinc1 , thtotinc_swave_tert := 2 ]
sipp_data[ thtotinc_swave_cpi10_adj >=  thtotinc2 , thtotinc_swave_tert := 3 ]

sipp_data[ , thtotinc_swave_tert := as.factor(thtotinc_swave_tert) ]
sipp_data[ , thtotinc_swave_tert := relevel(thtotinc_swave_tert, 2) ]

sipp_data[ , c("thtotinc1","thtotinc2") := NULL ]


#########################################
### Figure 7 -- US Part (right panel) ### 
#########################################

sipp_data[ thtotinc_swave_cpi10_adj > 0 , debt_to_inc := rhhuscbt_cpi10_adj / thtotinc_swave_cpi10_adj ]

# Remove top 1% outlier
sipp_data[ debt_to_inc > quantile(debt_to_inc, .99, na.rm=T) , debt_to_inc := NA ]

summary(sipp_data[ , debt_to_inc ])

debt_avg <- as.data.table(ddply(sipp_data[ empl_stat_layoff == 1 ], .(year,thtotinc_swave_tert), summarise,
                               debt_to_inc_w = weighted.mean(debt_to_inc, whfnwgt, na.rm=T)))

debt_avg[ , year := as.numeric(as.character(year))]

debt_avg[ thtotinc_swave_tert == 1 , inc_tertile := "bottom" ]
debt_avg[ thtotinc_swave_tert == 2 , inc_tertile := "middle" ]
debt_avg[ thtotinc_swave_tert == 3 , inc_tertile := "top" ]

debt_avg[ , country := "USA"]

reds <- c("#fecc5c","#fd8d3c","#e31a1c")

debt_avg_plot <- na.omit(debt_avg[ year >= 1995 ])

pdf("Output/figure_7_usa.pdf", width=4, height=3)
(plot <- (ggplot(data=debt_avg_plot, aes(y=debt_to_inc_w*100, x=year, group=inc_tertile, colour=inc_tertile, shape=inc_tertile, fill=inc_tertile))
          + stat_summary(fun.data = "mean_se", geom="line", size=0.6)
          + stat_summary(data=debt_avg_plot, fun.data = "mean_se", geom="point", size=1.8, colour="gray30")     
          + ylab("Debt-to-income ratio (%)")
          + xlab("Year")
          + theme_bw()
          + facet_wrap(~ country)
          + theme(legend.position="right", panel.grid.minor = element_blank(), 
                  plot.title = element_text(hjust = 0.5), panel.border = element_rect(color="gray"))
          + scale_color_manual(name="Income tertile", values=reds)
          + scale_fill_manual(name="Income tertile", values=reds)
          + scale_shape_manual(name="Income tertile", values=c(21,22,23))
          + coord_cartesian(ylim=c(25,85))
          + scale_x_continuous(breaks=seq(1995,2010,5))

))
dev.off()


####################
### SI Table C.6 ###
####################

# Subset
sipp_data_sub <- sipp_data[ hh_ref_pers == 1 & age %in% c(16:65) & own_business == 0 ]

# Prep data for PLM
sipp_data_sub_plm <- pdata.frame(sipp_data_sub, index = c("pid","year"), drop.index = T)

# Models
mod_plm1 <- plm(ihs_rhhuscbt_cpi10_adj ~ empl_stat_layoff*thtotinc_swave_tert + educ + age + I(age^2) + 
                rfnkids + ownership + erace_v2, effect = "twoways", model = "within", sipp_data_sub_plm)

mod_plm2 <- plm(ihs_rhhuscbt_cpi10_adj ~ empl_stat_layoff*ihs_thtotinc_swave_cpi10_adj + educ + age + I(age^2) + 
                  rfnkids + ownership + erace_v2, effect = "twoways", model = "within", sipp_data_sub_plm)

# Robust standard errors
mod_plm1_rse <- coeftest(mod_plm1, vcov=vcovHC(mod_plm1, type="HC1"))
mod_plm2_rse <- coeftest(mod_plm2, vcov=vcovHC(mod_plm2, type="HC1"))

label.sipp_mod <- c("Layoff", "Income (log)", "Income: 2nd tertile", "Income: top tertile", 
                    "Education: some college","Education: college","Education: BA","Education: MA","Age","Age square",
                    "Number of children","Homeowner","White","Asian","Black",
                    "Layoff $\\times$ Income (log)", "Layoff $\\times$ Income: 2nd tertile", "Layoff $\\times$ Income: top tertile")

stargazer(mod_plm2, mod_plm1,
          se = list(mod_plm2_rse[,2], mod_plm1_rse[,2]),
          omit="factor", digits.extra=0, digits=2, 
          align=T, no.space=T, omit.stat=c("ser","F"), type="text",
          covariate.labels = label.sipp_mod, dep.var.labels = "Total household unsecured debt (log)",
          add.lines = list(c("Mean DV", 
                             round(mean(mod_plm2$model$ihs_rhhuscbt_cpi10_adj),2), 
                             round(mean(mod_plm1$model$ihs_rhhuscbt_cpi10_adj),2)), 
                           c("Household FE","$\\checkmark$","$\\checkmark$"),
                           c("Year FE","$\\checkmark$","$\\checkmark$")),
          out = "Output/SI_table_c6.txt")


#################
### Figure 10 ###
#################

var.level.mod_2 <- seq(4,13, length.out = 25)

# Coefficients
estmean.mod_2 <- mod_plm2$coefficients
# Robust variance covariance matrix
var.mod_2 <- vcovHC(mod_plm2, type="HC1")

marg.eff_mod_2 <- mod_plm2$coefficients[1] + mod_plm2$coefficients[14]*var.level.mod_2

SEs_mod_2 <- rep(NA, length(var.level.mod_2))
for (i in 1:length(var.level.mod_2)){
  j <- var.level.mod_2[i]
  SEs_mod_2[i] <- deltamethod (~  (x1) + (x14)*j , estmean.mod_2, var.mod_2, ses=T)
}

upper.mod_2 <- marg.eff_mod_2 + 1.96*(SEs_mod_2)
lower.mod_2 <- marg.eff_mod_2 - 1.96*(SEs_mod_2)

tab_mod_2 <- as.data.table(cbind(var.level=var.level.mod_2, lower = lower.mod_2, 
                                 coef = marg.eff_mod_2, upper = upper.mod_2))

# Density plot
hist.out <- hist(sipp_data_sub[ , ihs_thtotinc_swave_cpi10_adj ], breaks=80, plot=F)
n.hist <- length(hist.out$mids)
dist <- hist.out$mids[2]-hist.out$mids[1]
hist.max <- max(hist.out$counts)
yrange <- c(tab_mod_2[ , upper], tab_mod_2[ ,lower])
maxdiff<-(max(yrange)-min(yrange))

hist_mod_v1.2 <- data.frame(ymin=rep(min(yrange)-maxdiff/5,n.hist),
                            ymax=hist.out$counts/hist.max*maxdiff/5+min(yrange)-maxdiff/5,
                            xmin=hist.out$mids-dist/2,
                            xmax=hist.out$mids+dist/2)

hist_mod_v1.2 <- as.data.table(hist_mod_v1.2)

# Match marginal effects distribution
hist_mod_v1.2 <- hist_mod_v1.2[ xmax >= min(var.level.mod_2) ]

hist_mod_v1.2[ , ymax := ymax + 0.2 ]
hist_mod_v1.2[ , ymin := ymin + 0.2 ]

pdf("Output/figure_10.pdf", width=4.5, height=3.5)
(plot <- (ggplot() 
          + geom_hline(aes(yintercept=0), colour = "gray50", linetype="dashed", size=.5)
          + geom_rect(data=hist_mod_v1.2, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                      fill="gray90", alpha=0.5, colour="gray70")
          + geom_ribbon(data=tab_mod_2, aes(y=coef, x=var.level, ymin=lower, ymax=upper), 
                        alpha=0.3, color=NA, fill="blue")   
          + geom_line(data=tab_mod_2, aes(y=coef, x=var.level), size=0.9)
          + ylab("Marginal effect of unemployment\non unsecured debt (log)")
          + xlab("Household income (log)")
          + theme_bw()
          + theme(legend.position="bottom", panel.grid.minor=element_blank(), panel.border = element_rect(color="gray"))
          + scale_x_continuous(breaks=seq(4,12,2))
          + scale_y_continuous(breaks=seq(-0.5,1.5,0.5), limits=c(-0.72,1.4))          
))
dev.off()





