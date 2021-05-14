
#####################################
### Credit Regime Permissiveness ####
#####################################

rm(list = ls())
gc()

### Install packages if necessary
# install.packages("ggplot2")     # ggplot2_3.3.2
# install.packages("reshape2")    # reshape2_1.4.4
# install.packages("lmtest")      # lmtest_0.9-38
# install.packages("sandwich")    # sandwich_3.0-0 
# install.packages("MASS")        # MASS_7.3-53
# install.packages("stringr")     # stringr_1.4.0
# install.packages("stargazer")   # stargazer_5.2.2
# install.packages("effects")     # effects_4.2-0
# install.packages("data.table")  # data.table_1.13.2
# install.packages("plm")         # plm_2.2-5
# install.packages("psych")       # psych_2.0.9
# install.packages("BBmisc")      # BBmisc_1.11
# install.packages("ggrepel")     # ggrepel_0.8.2

library(ggplot2)
library(reshape2)
library(lmtest)
library(sandwich)
library(MASS)
library(stringr)
library(stargazer)
library(effects)
library(data.table)
library(plm)
library(psych)
library(BBmisc)
library(ggrepel)


# Set working directory
setwd("~/YOURPATH/replication_files")

### Load data
load("Data/credit_regime_raw_data.RData")

### Plot raw data
d_credit_regime_final_sub_l <- melt(credit_regime_raw_data, id.vars=c("cty_code","cty_name","year"))

d_credit_regime_final_sub_avg <- credit_regime_raw_data[ , lapply(.SD, function(x) mean(x, na.rm=T) ) , by=c("cty_code","cty_name") , .SDcols=4:length(credit_regime_raw_data) ]
d_credit_regime_final_sub_avg_l <- melt(d_credit_regime_final_sub_avg, id.vars=c("cty_code","cty_name"))

d_credit_regime_final_sub_avg_l[ variable == "max_LTV_final", var_name := "Maximum loan-to-\nvalue ratio (LTV, in %)" ]
d_credit_regime_final_sub_avg_l[ variable == "pension_assets_GDP_final", var_name := "Pension assets (% GDP)" ]
d_credit_regime_final_sub_avg_l[ variable == "share_borrowing1_final", var_name := "Lending to household sector\n(% total lending)" ]
d_credit_regime_final_sub_avg_l[ variable == "share_gvt_owned_banks_final", var_name := "Share government-owned banks (%)" ]
d_credit_regime_final_sub_avg_l[ variable == "tax_relief_bin", var_name := "Tax relief for\ndebt-financed homeownership" ]
d_credit_regime_final_sub_avg_l[ variable == "stock_mt_cap_final", var_name := "Stock market\ncapitalization (% GDP)" ]


#####################
### SI Figure B.1 ###
#####################

pdf("Output/SI_figure_b1.pdf", width=8, height=6)
(plot <- (ggplot(data=d_credit_regime_final_sub_avg_l[ !is.na(var_name) ], aes(y=value, x=cty_name))
          + geom_bar(position = "dodge2", stat="identity", fill="gray50")
          + geom_point(colour="gray25")
          + theme_bw()
          + facet_wrap(~ var_name, scales = "free_x", ncol=3)
          + ylab("Average value (2000-2017)")
          + theme(legend.position="bottom", axis.title.y = element_blank(), panel.grid.minor = element_blank())
          + guides(colour = guide_legend(nrow = 1))
          + coord_flip()
))
dev.off()



####################################
### Create permissiveness scores ###
####################################

# Loop through PCA per year
year_list <- unique(sort(credit_regime_raw_data[ , year ]))

var_list1 <- c("cty_code","cty_name","year","max_LTV_final","pension_assets_GDP_final","share_borrowing1_final",
              "share_gvt_owned_banks_final","tax_relief_bin","stock_mt_cap_final")

credit_regime_raw_data1 <- credit_regime_raw_data[ , var_list1 , with = F ]

cty_pca_out <- list()
tab_pca_fin <- list() 

for(i in 1:length(year_list)){

  d <- credit_regime_raw_data1[ year %in% year_list[i] ]
  
  cty_code <- as.character(d[ , cty_code ])
  cty_name <- as.character(d[ , cty_name ])
  
  d[ ,c("cty_name","cty_code","year") := NULL]
  
  pca_out <- principal(d, nfactors = 1, scores=T, missing=T)

  cty_pca_out[[i]] <- data.table(cbind(cty_code, pca_out$scores, year = year_list[i], seq(1,length(cty_code),1)))
  
  # For PCA
  rownames(d) <- cty_name
  res.pca <- prcomp(~., d, center = TRUE, scale = TRUE, na.action = na.omit)
  
  tab_pca_v2 <- data.table(cbind(cty_lab=names(res.pca$x[,1]), pc1=res.pca$x[,1], pc2=res.pca$x[,2], year = year_list[i]))
  
  tab_pca_v2[ , pc1 := as.numeric(pc1)]
  tab_pca_v2[ , pc2 := as.numeric(pc2)]
  
  tab_pca_v2[ , pc1_norm := normalize(pc1, method = "range", range = c(-1, 1))  ]
  tab_pca_v2[ , pc2_norm := normalize(pc2, method = "range", range = c(-1, 1))  ]
  
  res.pca.1 <- data.table(cbind(pc1 = res.pca$rotation[,1], pc2 = res.pca$rotation[,2], year = year_list[i], indicator = var_list1[4:length(var_list1)]))
  res.pca.1[ , pc1 := as.numeric(pc1)]
  res.pca.1[ , pc2 := as.numeric(pc2)]
  
  setnames(res.pca.1, "pc1", "pc1_norm")
  setnames(res.pca.1, "pc2", "pc2_norm")
  
  tab_pca_out <- rbind(tab_pca_v2, res.pca.1, fill=T)
  tab_pca_out[ is.na(pc1), ind2 := paste("R",seq(1,length(indicator),1),sep="")]
  tab_pca_out[ !is.na(pc1), ind2 := cty_lab ]
  
  tab_pca_out[ , dim1 := principal(d, nfactors = 2, scores=T, missing=T)$Vaccounted[2,1]]
  tab_pca_out[ , dim2 := principal(d, nfactors = 2, scores=T, missing=T)$Vaccounted[2,2]]
  
  tab_pca_fin[[i]] <- tab_pca_out
  
}

cty_pca_comb <- do.call("rbind", cty_pca_out)

cty_pca_comb[ , PC1 := as.numeric(PC1)]
cty_pca_comb[ , year := as.numeric(as.character(year))]

# Normalize 
cty_pca_comb[ , PC1_norm2 := normalize(PC1, method = "range", range = c(0, 1))  ]

# Merge country names
cty_list <- unique(credit_regime_raw_data1[ ,.(cty_code,cty_name)])

cty_pca_comb <- merge(cty_pca_comb, cty_list, by="cty_code",all.x=T)


### PCA
tab_pca_fin_all <- do.call("rbind", tab_pca_fin)

tab_pca_fin_cty <- tab_pca_fin_all[ !is.na(cty_lab) ]
tab_pca_fin_ind <- tab_pca_fin_all[ !is.na(indicator) ]

tab_pca_fin_cty_avg <- tab_pca_fin_cty[ , lapply(.SD, function(x) mean(x, na.rm=T) ) , by=c("ind2") , .SDcols=c("pc1_norm","pc2_norm","pc1","pc2") ]
tab_pca_fin_ind_avg <- tab_pca_fin_ind[ , lapply(.SD, function(x) mean(x, na.rm=T) ) , by=c("indicator","ind2") , .SDcols=c("pc1_norm","pc2_norm","pc1","pc2") ]

tab_pca_fin_dim_avg <- tab_pca_fin_all[ , lapply(.SD, function(x) mean(x, na.rm=T) ) , .SDcols=c("dim1","dim2") ]

tab_pca_ALL <- rbind(tab_pca_fin_cty_avg, tab_pca_fin_ind_avg, fill=T)
tab_pca_ALL[ is.na(indicator) , cty := "X" ]

tab_pca_ALL[ ! is.na(cty) , pc1_norm2 := normalize(pc1, method = "range", range = c(-0.9, 0.9)) ]
tab_pca_ALL[ ! is.na(cty) , pc2_norm2 := normalize(pc2, method = "range", range = c(-0.9, 0.9)) ]

tab_pca_ALL[ is.na(cty) , pc1_norm2 := normalize(pc1_norm, method = "range", range = c(-0.7, 0.7)) ]
tab_pca_ALL[ is.na(cty) , pc2_norm2 := normalize(pc2_norm, method = "range", range = c(-0.7, 0.7)) ]

tab_pca_ALL[ is.na(cty), ind3 := paste(ind2, indicator, sep=": ")]

ind_names_full <- c("Maximum loan-to-value ration (LTV, in %)","Pension assets (% GDP)","Lending to household sector (% total lending)","Government-owned banks",
                    "Tax relief for debt-financed homeownership","Stock market capitalization (% GDP)")


#####################
### SI Figure B.2 ###
#####################

pdf("Output/SI_figure_b2.pdf", width=8, height=4)
(plot <- (ggplot(data=tab_pca_ALL, aes(y=pc2_norm2, x=pc1_norm2, colour=ind3))
          + geom_hline(yintercept = 0, linetype="dashed", colour="gray30")
          + geom_vline(xintercept = 0, linetype="dashed", colour="gray30")
          + annotate("segment", x = 0, xend = tab_pca_ALL[ is.na(cty) ]$pc1_norm2, y = 0, yend = tab_pca_ALL[ is.na(cty) ]$pc2_norm2, size=0.5, 
                     colour="gray50", arrow=arrow(length=unit(0.2,"cm"), type = "open"))
          + geom_point(data=tab_pca_ALL[ !is.na(cty) ], size=2, colour="darkgreen")
          + geom_point(data=tab_pca_ALL[ is.na(cty) ], size=2, aes(color=ind3), alpha=0)
          + geom_text_repel(data=tab_pca_ALL[ !is.na(cty) ], aes(label=ind2), size=3, colour="darkgreen")
          + geom_text_repel(data=tab_pca_ALL[ is.na(cty) ], aes(label=ind2), colour="black", size=3, show.legend = F, fontface="bold")
          + theme_bw()
          + theme(legend.position="right", panel.grid.minor = element_blank(), panel.border = element_rect(color="gray"))
          + guides(colour = guide_legend(ncol = 1))
          + scale_color_manual(name="Indicators", values=rep("black",13), labels=paste(paste("R",seq(1,13,1),sep=""), ind_names_full , sep=": "))
          + xlab(paste("Dimension 1 (",100*round(tab_pca_fin_dim_avg$dim1,3),"% explained)",sep=""))
          + ylab(paste("Dimension 2 (",100*round(tab_pca_fin_dim_avg$dim2,3),"% explained)",sep=""))
          + xlim(-1,1)
          + ylim(-1,1)
))
dev.off()



################
### Figure 3 ###
################

credit_scores_cty <- cty_pca_comb[ ,.(cty_code,cty_name,year,PC1_norm2)]

setnames(credit_scores_cty, "PC1_norm2", "credit_scores")

pdf("Output/figure_3.pdf", width=9.5, height=5.5)
(plot <- (ggplot(data=credit_scores_cty, aes(y=credit_scores, x=year))
          + stat_smooth(method=loess, formula=y~x, size=0.75)
          + geom_line(colour="gray25")
          + theme_bw()
          + facet_wrap(~ cty_name, ncol=6)
          + ylab("Credit regime permissiveness score")
          + xlab("Year")
          + theme(legend.position="bottom", panel.grid.minor = element_blank())
          + guides(colour = guide_legend(nrow = 1))
))
dev.off()


################
### Figure 4 ###
################

credit_scores_cty_avg <- credit_scores_cty[ , lapply(.SD, function(x) mean(x, na.rm=T) ) , by=c("cty_name") , .SDcols=c("credit_scores") ]
credit_scores_cty_avg$cty_name <- factor(credit_scores_cty_avg$cty_name, levels = credit_scores_cty_avg$cty_name[order(credit_scores_cty_avg$credit_scores, decreasing=F)]) 

pdf("Output/figure_4.pdf", width=5.5, height=4.5)
(plot <- (ggplot(data=credit_scores_cty_avg, aes(y=credit_scores, x=cty_name))
          + geom_bar(stat="identity", show.legend=F, fill="#43a2ca")
          + annotate(geom="text", x=12.5, y=0.91, label="more permissive", color="gray25", size=2.7)
          + annotate("segment", x = 13, xend = 17, y = 1, yend = 1, size=0.4, colour="gray25", arrow=arrow(length=unit(0.30,"cm"), type = "closed", angle = 20))
          + theme_bw()
          + theme(legend.position="bottom", axis.title.y = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(color="gray"))
          + guides(colour = guide_legend(nrow = 1))
          + ylab("Average credit regime permissiveness scores (2000-2017)")
          + ylim(0,1)
          + coord_flip()
))
dev.off()



#########################################
### Regression models with covariates ###
#########################################

# Load country-level covariates
load("Data/cty_covars.RData")

# Merge
credit_scores_cty_combined <- merge(credit_scores_cty, cty_covars, by=c("cty_code","year"), all.x=T)
credit_scores_cty_combined[ , gdpusd_log := log(gdpusd) ]

# Run models
mod1.1 <- plm(bis_tot_credit_hh_pct_gdp ~ credit_scores*uegen, 
               index = c("cty_code", "year"), model = "within", effect = "twoways", credit_scores_cty_combined)

mod1.2 <- plm(bis_tot_credit_hh_pct_gdp ~ credit_scores*uegen + gdpusd_log + realgdpgr + kaopen + deficit, 
               index = c("cty_code", "year"), model = "within", effect = "twoways", credit_scores_cty_combined)

mod1.3 <- plm(bis_tot_credit_hh_pct_gdp ~ credit_scores*uegen + gdpusd_log + realgdpgr + kaopen + deficit + 
                 share_emp_serv + unemp + ngini + outlays, 
               index = c("cty_code", "year"), model = "within", effect = "twoways", credit_scores_cty_combined)

mod1.4 <- plm(bis_tot_credit_hh_pct_gdp ~ credit_scores*uegen + gdpusd_log + realgdpgr + kaopen + deficit + share_emp_serv + unemp + ngini + 
                 outlays + real_house_price + interest, 
               index = c("cty_code", "year"), model = "within", effect = "twoways", credit_scores_cty_combined)

# Cluster-robust standard errors
mod1.1_cse <- coeftest(mod1.1, vcov=vcovHC(mod1.1,type="HC1",cluster="group"))
mod1.2_cse <- coeftest(mod1.2, vcov=vcovHC(mod1.2,type="HC1",cluster="group"))
mod1.3_cse <- coeftest(mod1.3, vcov=vcovHC(mod1.3,type="HC1",cluster="group"))
mod1.4_cse <- coeftest(mod1.4, vcov=vcovHC(mod1.4,type="HC1",cluster="group"))


####################
### SI Table C.2 ###
####################

labels_mod1x <- c("Credit permissiveness","UI generosity","GDP (log)","Real GDP growth","Capital account openness","Government deficit",
                  "Share service sector employment","Unemployment rate","Gini","Government outlays","Real house prices","Interest rate",
                  "Credit permissiveness $\\times$ UI generosity")

stargazer(mod1.1, mod1.2, mod1.3, mod1.4,
          se = list(mod1.1_cse[,2], mod1.2_cse[,2], mod1.3_cse[,2], mod1.4_cse[,2]),
          omit="factor", digits.extra=0, digits=2, 
          align=T, no.space=T, omit.stat=c("ser","F"), type="text",
          covariate.labels = labels_mod1x, dep.var.labels = "Household debt (share GDP)",
          add.lines = list(c("Mean DV",
                             round(mean(mod1.1$model$bis_tot_credit_hh_pct_gdp),2), 
                             round(mean(mod1.2$model$bis_tot_credit_hh_pct_gdp),2),
                             round(mean(mod1.3$model$bis_tot_credit_hh_pct_gdp),2), 
                             round(mean(mod1.4$model$bis_tot_credit_hh_pct_gdp),2)),
                           c("State FE","$\\checkmark$","$\\checkmark$","$\\checkmark$","$\\checkmark$"),
                           c("Year FE","$\\checkmark$","$\\checkmark$","$\\checkmark$","$\\checkmark$")),
          out="Output/SI_table_c2.txt")


### Run LM model for plot
mod1.4x <- lm(bis_tot_credit_hh_pct_gdp ~ uegen*credit_scores + gdpusd_log + realgdpgr + kaopen + deficit + share_emp_serv + unemp + ngini + 
                outlays + real_house_price + interest + cty_code_fac + year_fac, credit_scores_cty_combined)
summary(mod1.4x)

d_plot_mod1.4x <- as.data.table(predictorEffect("uegen",mod1.4x, xlevels=list(credit_scores=c(0.2,0.8))))
d_plot_mod1.4x[ credit_scores == "0.2" , grp := "Restrictive credit regime" ]
d_plot_mod1.4x[ credit_scores == "0.8" , grp := "Permissive credit regime" ]
d_plot_mod1.4x[ , grp := factor(grp, levels=c("Restrictive credit regime","Permissive credit regime"))]


################
### Figure 6 ###
################

pdf("Output/figure_6.pdf", width=5.75, height=3.25)
(plot <- (ggplot(data=d_plot_mod1.4x, aes(y=fit, x=uegen, group=1))
          + geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3, color=NA, fill="blue")
          + geom_line(size=0.9)
          + theme_bw()
          + geom_rug(sides="b")
          + ylab("Predicted debt levels (% of GDP)")
          + xlab("Unemployment insurance generosity")
          + facet_wrap(~ grp)
          + theme(legend.position="right", panel.grid.minor=element_blank(), panel.border = element_rect(color="gray"))
          + guides(colour = guide_legend(ncol = 1))
))
dev.off()



####################
### SI Table C.4 ###
####################

### Robustness to banking and systemic risk crisis dummies 
mod2.1 <- plm(bis_tot_credit_hh_pct_gdp ~ credit_scores*uegen + gdpusd_log + realgdpgr + kaopen + deficit + share_emp_serv + unemp + ngini + 
                 outlays + real_house_price + interest + banking_crisis_RR,
               index = c("cty_code", "year"), model = "within", effect = "twoways", credit_scores_cty_combined)

mod2.2 <- plm(bis_tot_credit_hh_pct_gdp ~ credit_scores*uegen + gdpusd_log + realgdpgr + kaopen + deficit + share_emp_serv + unemp + ngini + 
                 outlays + real_house_price + interest + systemic_crisis_RR,
               index = c("cty_code", "year"), model = "within", effect = "twoways", credit_scores_cty_combined)

mod2.1_cse <- coeftest(mod2.1, vcov=vcovHC(mod2.1,type="HC1",cluster="group"))
mod2.2_cse <- coeftest(mod2.2, vcov=vcovHC(mod2.2,type="HC1",cluster="group"))

labels_mod2x <- c("Credit permissiveness","UI generosity","GDP (log)","Real GDP growth","Capital account openness","Government deficit",
                     "Share service sector employment","Unemployment rate","Gini","Government outlays","Real house prices","Interest rate",
                     "Banking crisis dummy", "Systemic crisis dummy",
                     "Credit permissiveness $\\times$ UI generosity")

stargazer(mod2.1, mod2.2,
          se = list(mod2.1_cse[,2], mod2.2_cse[,2]),
          omit="factor", digits.extra=0, digits=2, 
          align=T, no.space=T, omit.stat=c("ser","F"), type="text",
          covariate.labels = labels_mod2x, dep.var.labels = "Household debt (share GDP)",
          label="tab:mod1_rob_crises", column.sep.width = "-10pt",
          add.lines = list(c("Mean DV",
                             round(mean(mod2.1$model$bis_tot_credit_hh_pct_gdp),2), 
                             round(mean(mod2.2$model$bis_tot_credit_hh_pct_gdp),2)), 
                           c("State FE","$\\checkmark$","$\\checkmark$","$\\checkmark$"),
                           c("Year FE","$\\checkmark$","$\\checkmark$","$\\checkmark$")),
          out="Output/SI_table_c4.txt")


####################
### SI Table C.3 ###
####################

### Robustness to lagged DV
mod2.3 <- plm(bis_tot_credit_hh_pct_gdp ~ credit_scores*uegen + gdpusd_log + realgdpgr + kaopen + deficit + share_emp_serv + unemp + ngini + outlays + 
                 real_house_price + interest + bis_tot_credit_hh_pct_gdp_lag1,
               index = c("cty_code", "year"), model = "within", effect = "twoways", credit_scores_cty_combined)
summary(mod2.3)

mod2.4 <- plm(bis_tot_credit_hh_pct_gdp ~ credit_scores*uegen + gdpusd_log + realgdpgr + kaopen + deficit + share_emp_serv + unemp + ngini + outlays + 
                 real_house_price + interest + bis_tot_credit_hh_pct_gdp_lag1 + bis_tot_credit_hh_pct_gdp_lag2,
               index = c("cty_code", "year"), model = "within", effect = "twoways", credit_scores_cty_combined) 

summary(mod2.4)

mod2.3_cse <- coeftest(mod2.3, vcov=vcovHC(mod2.3,type="HC1",cluster="group"))
mod2.4_cse <- coeftest(mod2.4, vcov=vcovHC(mod2.4,type="HC1",cluster="group"))

labels_mod2xx <- c("Credit permissiveness","UI generosity","GDP (log)","Real GDP growth","Capital account openness","Government deficit",
                     "Share service sector employment","Unemployment rate","Gini","Government outlays","Real house prices","Interest rate",
                     "Household debt (share GDP), lagged 1 year", "Household debt (share GDP), lagged 2 years",
                     "Credit permissiveness $\\times$ UI generosity")

stargazer(mod2.3, mod2.4,
          se = list(mod2.3_cse[,2], mod2.4_cse[,2]),
          omit="factor", digits.extra=0, digits=2, 
          align=T, no.space=T, omit.stat=c("ser","F"), type="text",
          covariate.labels = labels_mod2xx, dep.var.labels = "Household debt (share GDP)",
          label="tab:mod1_rob_lags", column.sep.width = "-10pt",
          add.lines = list(c("Mean DV",
                             round(mean(mod2.3$model$bis_tot_credit_hh_pct_gdp),2), 
                             round(mean(mod2.4$model$bis_tot_credit_hh_pct_gdp),2)), 
                           c("State FE","$\\checkmark$","$\\checkmark$","$\\checkmark$"),
                           c("Year FE","$\\checkmark$","$\\checkmark$","$\\checkmark$")),
          out="Output/SI_table_c3.txt")


#####################################################
#### Alternative Measure of Credit Permissiveness ###
#####################################################

# Loop through PCA per year
year_list <- unique(sort(credit_regime_raw_data[ , year ]))

# include deposits 
var_list2 <- c("cty_code","cty_name","year","max_LTV_final","pension_assets_GDP_final","share_borrowing1_final",
               "share_gvt_owned_banks_final","tax_relief_bin","stock_mt_cap_final","share_currency_deposits_tot_ass")

credit_regime_raw_data2 <- credit_regime_raw_data[ , var_list2 , with = F ]

cty_pca_rob_out <- list()

for(i in 1:length(year_list)){
  
  d <- credit_regime_raw_data2[ year %in% year_list[i] ]
  
  cty_code <- as.character(d[ , cty_code ])
  cty_name <- as.character(d[ , cty_name ])
  
  d[ ,c("cty_name","cty_code","year") := NULL]
  
  pca_out <- principal(d, nfactors = 1, scores=T, missing=T)
  
  cty_pca_rob_out[[i]] <- data.table(cbind(cty_code, pca_out$scores, year = year_list[i], seq(1,length(cty_code),1)))
  
}

cty_pca_rob_comb <- do.call("rbind", cty_pca_rob_out)

cty_pca_rob_comb[ , PC1 := as.numeric(PC1)]
cty_pca_rob_comb[ , year := as.numeric(as.character(year))]

# Normalize 
cty_pca_rob_comb[ , PC1_norm2 := normalize(PC1, method = "range", range = c(0, 1))  ]

# Merge country names
cty_list <- unique(credit_regime_raw_data2[ ,.(cty_code,cty_name)])

cty_pca_rob_comb <- merge(cty_pca_rob_comb, cty_list, by="cty_code",all.x=T)

cty_pca_rob_comb[ , c("PC1","V4") := NULL ]

setnames(cty_pca_rob_comb, "PC1_norm2", "credit_scores")


#####################
### SI Figure B.4 ###
#####################

pdf("Output/SI_figure_b4.pdf", width=9.5, height=5.5)
(plot <- (ggplot(data=cty_pca_rob_comb, aes(y=credit_scores, x=year))
          + stat_smooth(method=loess, formula=y~x, size=0.75)
          + geom_line(colour="gray25")
          + theme_bw()
          + facet_wrap(~ cty_name, ncol=6)
          + ylab("Credit regime permissiveness score (normalized)")
          + xlab("Year")
          + theme(legend.position="bottom", panel.grid.minor = element_blank())
          + guides(colour = guide_legend(nrow = 1))
))
dev.off()


#####################
### SI Figure B.5 ###
#####################

cty_pca_rob_comb_avg <- cty_pca_rob_comb[ , lapply(.SD, function(x) mean(x, na.rm=T) ) , by=c("cty_name") , .SDcols=c("credit_scores") ]
cty_pca_rob_comb_avg$cty_name <- factor(cty_pca_rob_comb_avg$cty_name, levels = cty_pca_rob_comb_avg$cty_name[order(cty_pca_rob_comb_avg$credit_scores, decreasing=F)]) 

pdf("Output/SI_figure_b5.pdf", width=5.5, height=4.5)
(plot <- (ggplot(data=cty_pca_rob_comb_avg, aes(y=credit_scores, x=cty_name))
          + geom_bar(stat="identity", show.legend=F, fill="#43a2ca")
          + annotate(geom="text", x=12.5, y=0.91, label="more permissive", color="gray25", size=2.7)
          + annotate("segment", x = 13, xend = 17, y = 1, yend = 1, size=0.4, colour="gray25", arrow=arrow(length=unit(0.30,"cm"), type = "closed", angle = 20))
          + theme_bw()
          + theme(legend.position="bottom", axis.title.y = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(color="gray"))
          + guides(colour = guide_legend(nrow = 1))
          + ylab("Average credit regime permissiveness scores (2000-2017)")
          + ylim(0,1)
          + coord_flip()
))
dev.off()




#####################
### SI Figure B.6 ###
#####################

### Load interest rate data
load("Data/oecd_int_rates.RData")

# Select EUROZONE rate
oecd_int_rates_EA19 <- oecd_int_rates[  LOCATION %in% "EA19" ,.(Time,Value) ]

cty_list <- c("AUS", "AUT", "BEL", "CAN", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "GRC", "IRL", "ITA", "NLD", "PRT", "SWE", "USA")

pdf("Output/SI_figure_b6.pdf", width=8, height=7)
(plot <- (ggplot(data=oecd_int_rates[  LOCATION %in% cty_list & Time >= 1995 ], aes(y=Value, x=Time, colour="Country short-term interest rate", linetype="Country short-term interest rate"))
          + geom_line(size=1.25)
          + geom_line(data=oecd_int_rates_EA19[  Time >= 1995 ], aes(colour="Euro area short-term interest rate", linetype="Euro area short-term interest rate"))
          + facet_wrap(~ Country, ncol=5)
          + theme_bw()
          + ylab("Short-term interest rate (%)")
          + xlab("Year")
          + theme(legend.position="bottom", panel.grid.minor=element_blank(), panel.border = element_rect(color="gray"),
                  axis.text.x = element_text(angle = 45, hjust=1))
          + scale_color_manual("", values=c("gray30","blue"))
          + scale_linetype_manual("", values=c("solid","dashed"))
))
dev.off()





