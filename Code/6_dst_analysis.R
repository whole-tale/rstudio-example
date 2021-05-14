

###############################
### Figure 7 (Denmark part) ###
###############################

### Use dataset dst_main from 1_prep_dst_data.R

dst_main[ year <= 1994 , oblgaeld_cpi10_adj := NA ]
dst_main[ year <= 1994 , oblgaeld_hh_cpi10_adj := NA ]
dst_main[ year <= 1994 , ihs_oblgaeld_hh_cpi10_adj := NA ]

### Debt leverage
dst_main[ dispo_hh_v2 > 0 & bankgaeld_hh_cpi10_adj >= 0, hh_cons_debt_lvg_disp := bankgaeld_hh_cpi10_adj / dispo_hh_v2 ]
dst_main[ dispo_hh_v2 > 0 & oblgaeld_hh_cpi10_adj >= 0, hh_mtg_debt_lvg_disp := oblgaeld_hh_cpi10_adj / dispo_hh_v2 ]

dst_main[ dispo_v2 > 0 & bankgaeld_cpi10_adj >= 0, ind_cons_debt_lvg_disp := bankgaeld_cpi10_adj / dispo_v2 ]
dst_main[ dispo_v2 > 0 & oblgaeld_cpi10_adj >= 0, ind_mtg_debt_lvg_disp := oblgaeld_cpi10_adj / dispo_v2 ]

### Income tertile
dst_main[ , dispo_hh_v2_cutoff1x := lapply(.SD, function(x) quantile(x, .33, na.rm=T)), 
          by=c("year"), .SDcols="dispo_hh_v2" ]
dst_main[ , dispo_hh_v2_cutoff2x := lapply(.SD, function(x) quantile(x, .66, na.rm=T)), 
          by=c("year"), .SDcols="dispo_hh_v2" ]

dst_main[ dispo_hh_v2 <  dispo_hh_v2_cutoff1x , hh_dispo_tert := "bottom" ]
dst_main[ dispo_hh_v2 >= dispo_hh_v2_cutoff1x , hh_dispo_tert := "Q2" ]
dst_main[ dispo_hh_v2 >= dispo_hh_v2_cutoff2x , hh_dispo_tert := "top" ]

dst_main[ , hh_dispo_tert := factor(hh_dispo_tert, levels=c("bottom","Q2","top"))]


d_debt <- dst_main[ owner_koejd2 == 0 &  hh_cons_debt_lvg_disp < quantile(hh_cons_debt_lvg_disp,.98,na.rm=T) & !is.na(hh_dispo_tert) , 
                    lapply(.SD, function(x) mean(x,na.rm=T)), by=c("year","ansx_socstil3","hh_dispo_tert"), .SDcols="hh_cons_debt_lvg_disp" ]


(plot <- (ggplot(data=d_debt[ ansx_socstil3 %in% c("employed","unemployed")], 
                 aes(y=hh_cons_debt_lvg_disp*100, x=year, colour=hh_dispo_tert, group=hh_dispo_tert))
          + geom_line(size=0.8)
          + theme_bw()
          + facet_wrap( ~ ansx_socstil3, ncol=5)
          + ylab("Unsecured debt leverage (%)")
          + xlab("Year")
          + theme(legend.position="bottom")
))



#################
### Figure 8a ###
#################

### Use dataset dst_main_head_v4.RData from 1_prep_dst_data

# Model
mod1 <- felm(ihs_bankgaeld_hh_cpi10_adj ~ yr1st_unemp_tally_fac + inc_grp_dispo_pr_unemp + I(age^2) + educ + save_ratio  + arledgr_wks  + koen + fam_stat + antboernf + insur_stat2 
                           + factor(FAMILIE_TYPE) + ihs_arblhumv_hh | year + b_year_all + postnummer_year | 0 | familie_id,
                           dst_main_head_v4[ yr1st_unemp_tally %in% c(-5:5) & owner_koejd2 == 0  ] )


stargazer(mod1, type="text")

# For plot
estmean.mod1 <- mod1$coefficients
var.mod1 <- mod1$clustervcv

rownames(estmean.mod1)

tab.mod1 <- list()
for(j in 1:10){
  marg.eff.mod1 <- mod1$coefficients[j]
  
  SEs.mod1 <- eval(parse(text=paste("deltamethod( ~ (x",j,"), estmean.mod1, var.mod1, ses=T)", sep="")))
  
  tab.mod1[[j]] <- as.data.table(cbind(coef = marg.eff.mod1, SE = SEs.mod1, time = as.numeric(gsub("yr1st_unemp_tally_fac","",rownames(mod1$coefficients)[j]))))
  
}

tab.mod1_all <- do.call("rbind",tab.mod1)

tab.mod1_all[ , 1:2 := lapply(.SD, function(x) as.numeric(x) ), .SDcols=1:2 ]

tab.mod1_all[ , lower := coef - 1.96*SE ]
tab.mod1_all[ , upper := coef + 1.96*SE ]

# Plot
(plot <- (ggplot(data=tab.mod1_all[ time %in% seq(-5,5)], aes(y=coef, x=time, group=1))
          + geom_hline(yintercept = 0, colour="gray50", linetype="dashed")
          + geom_vline(xintercept = 0, colour="black", size=0.5)
          + annotate("text", x = 2.3, y = 1.4, label = "Unemployment shock",  size=3.3)
          + geom_ribbon(data=tab.mod1_all[ time %in% seq(-5,-2,1) ], aes(ymin=lower, ymax=upper), alpha=.25, fill="gray30")
          + geom_ribbon(data=tab.mod1_all[ time %in% seq(0,5,1) ], aes(ymin=lower, ymax=upper), alpha=.25, fill="gray30")
          + geom_line(data=tab.mod1_all[ time %in% seq(-5,-2,1) ], size=0.8)
          + geom_line(data=tab.mod1_all[ time %in% c(-2,-1,0) ], size=0.8, linetype="dashed")
          + geom_line(data=tab.mod1_all[ time %in% seq(0,5,1) ], size=0.8)
          + geom_point(size=2.2)
          + ylab("Marginal effects of unemployment\non household debt (log, relative to t=-1)")
          + xlab("Event time (in years)")
          + theme_bw()
          + theme(legend.position="bottom", panel.grid.minor=element_blank(),
                  plot.title = element_text(hjust = 0.5), panel.border = element_rect(color="gray"),
                  plot.margin=unit(c(5.5, 5.5, 47, 5.5), "points"))
          + scale_x_continuous(breaks=seq(-5,5))
          + scale_y_continuous(limits=c(-0.6,1.5), breaks=seq(-0.5,1.5,0.5))
))




#################
### Figure 8b ###
#################

mod2 <- felm(ihs_bankgaeld_hh_cpi10_adj ~ yr1st_unemp_tally_fac*dispo_hh_v2_vol_yr_unemp_grp + I(age^2) + educ + save_ratio  + arledgr_wks  + koen + factor(FAMILIE_TYPE)  +
                                         antboernf + insur_stat2 + ihs_arblhumv_hh + dispo_hh_quint | year + b_year_all + postnummer_year | 0 | familie_id,
                                       dst_main_head_v4[ yr1st_unemp_tally %in% c(-5:5) & owner_koejd2 == 0  ] )

summary(mod2)

# Plots
estmean.mod2 <- mod2$coefficients
var.mod2 <- mod2$clustervcv

rownames(estmean.mod2)

tab.mod2 <- list()
for(i in seq(1,21,10) ){ 
  tab.1 <- list()
  for(j in 1:10){
    # All other
    marg.eff.mod2 <- mod2$coefficients[j] + mod2$coefficients[33+(i-1)+j]
    
    SEs.mod2 <- eval(parse(text=paste("deltamethod( ~ (x",j,") + (x",33+(i-1)+j,"), estmean.mod2, var.mod2, ses=T)", sep="")))
    
    tab.1[[j]] <- as.data.table(cbind(coef = marg.eff.mod2, SE = SEs.mod2, time = as.numeric(gsub("yr1st_unemp_tally_fac","",rownames(mod2$coefficients)[j]))))
    
  }
  tab.mod2[[i]] <- do.call("rbind",tab.1)
}

tab.mod2x <- as.data.table(do.call("rbind",tab.mod2))
tab.mod2x[ , inc_vol_grp := rep(gsub("dispo_hh_v2_vol_yr_unemp_grp","",
                                                               rownames(mod2$coefficients)[11:13]), each=10) ]


# Baseline
tab.mod2_b <- list()

for(j in 1:10){
  marg.eff.mod2_b <- mod2$coefficients[j]
  
  SEs.mod2_b <- eval(parse(text=paste("deltamethod( ~ (x",j,"), estmean.mod2, var.mod2, ses=T)", sep="")))
  
  tab.mod2_b[[j]] <- as.data.table(cbind(coef = marg.eff.mod2_b, SE = SEs.mod2_b, time = as.numeric(gsub("yr1st_unemp_tally_fac","",rownames(mod2$coefficients)[j]))))
  
}

tab.mod2_bx <- do.call("rbind",tab.mod2_b)
tab.mod2_bx[ , inc_vol_grp := "0" ]

tab.mod2_all <- rbind(tab.mod2x,tab.mod2_bx)

tab.mod2_all[ , 1:2 := lapply(.SD, function(x) as.numeric(x) ), .SDcols=1:2 ]

tab.mod2_all[ , lower := coef - 1.96*SE ]
tab.mod2_all[ , upper := coef + 1.96*SE ]

tab.mod2_all[ inc_vol_grp == "-3" , inc_vol_grp2 := "> 50%" ]
tab.mod2_all[ inc_vol_grp == "-2" , inc_vol_grp2 := "25-50%" ]
tab.mod2_all[ inc_vol_grp == "-1" , inc_vol_grp2 := "0-25%" ]
tab.mod2_all[ inc_vol_grp == "0" , inc_vol_grp2 := "none" ]

tab.mod2_all[ , inc_vol_grp2 := factor(inc_vol_grp2, levels=c("0-25%","25-50%","> 50%","none"))]

my.colour1 <- c("#CC6666","#9999CC","#66CC99","grey")

(plot2_fin <- (ggplot(data=tab.mod2_all[ inc_vol_grp  %in% c(-1,-2) & time %in% seq(-5,5)], aes(y=coef, x=time, group=inc_vol_grp2, colour=inc_vol_grp2))
               + geom_hline(yintercept = 0, colour="gray50", linetype="dashed")
               + geom_vline(xintercept = 0, colour="black", size=0.5)
               + geom_ribbon(data=tab.mod2_all[ inc_vol_grp %in% c(-1,-2) & time %in% seq(-5,-2,1) ], aes(ymin=lower, ymax=upper, fill=inc_vol_grp2), alpha=.2, colour=NA)
               + geom_ribbon(data=tab.mod2_all[ inc_vol_grp  %in% c(-1,-2) & time %in% seq(0,5,1) ], aes(ymin=lower, ymax=upper, fill=inc_vol_grp2), alpha=.2, colour=NA)
               + geom_line(data=tab.mod2_all[ inc_vol_grp  %in% c(-1,-2) & time %in% seq(-5,-2,1) ], size=0.8)
               + geom_line(data=tab.mod2_all[ inc_vol_grp  %in% c(-1,-2) & time %in% c(-2,-1,0) ], size=0.8, linetype="dashed")
               + geom_line(data=tab.mod2_all[ inc_vol_grp  %in% c(-1,-2) & time %in% seq(0,5,1) ], size=0.8)
               + annotate("text", x = 2.3, y = 1.4, label = "Unemployment shock",  size=3.3)
               + geom_point(aes(shape=inc_vol_grp2), size=2.2)
               + ylab("Marginal effects of unemployment\non household debt (log, relative to t=-1)")
               + xlab("Event time (in years)")
               + theme_bw()
               + theme(legend.position="bottom", panel.grid.minor=element_blank(),
                       plot.title = element_text(hjust = 0.5), panel.border = element_rect(color="gray"))
               + guides(colour = guide_legend(nrow = 1))
               + scale_color_manual(name="Net income loss", values=my.colour3)
               + scale_fill_manual(name="Net income loss", values=my.colour3)
               + scale_shape_discrete(name="Net income loss")
               + scale_y_continuous(limits=c(-0.6,1.5), breaks=seq(-0.5,1.5,0.5))
               + scale_x_continuous(breaks=seq(-5,5))
))  





######################################################
### Results for unemployment as 25 weeks or longer ### 
######################################################

### Use dataset created in 1_prep_dst_data.R where unemployement is defined as 25 weeks or longer


######################
### SI Figure C.2a ###
######################

mod3 <- felm(ihs_bankgaeld_hh_cpi10_adj ~ yr1st_unemp_tally_fac + inc_grp_dispo_pr_unemp + I(age^2) + educ + save_ratio  + arledgr_wks  + koen + fam_stat + antboernf + insur_stat2 
                               + factor(FAMILIE_TYPE) + ihs_arblhumv_hh | year + b_year_all + postnummer_year | 0 | familie_id,
                               d_FPx_v8_head_unemp_25_wks[ yr1st_unemp_tally %in% c(-5:5) & owner_koejd2 == 0  ] )

# For plot
estmean.mod3 <- mod3$coefficients
var.mod3 <- mod3$clustervcv

rownames(estmean.mod3)

tab.mod3 <- list()
for(j in 1:10){
  marg.eff.mod3 <- mod3$coefficients[j]
  
  SEs.mod3 <- eval(parse(text=paste("deltamethod( ~ (x",j,"), estmean.mod3, var.mod3, ses=T)", sep="")))
  
  tab.mod3[[j]] <- as.data.table(cbind(coef = marg.eff.mod3, SE = SEs.mod3, time = as.numeric(gsub("yr1st_unemp_tally_fac","",rownames(mod3$coefficients)[j]))))
  
}

tab.mod3_all <- do.call("rbind",tab.mod3)

tab.mod3_all[ , 1:2 := lapply(.SD, function(x) as.numeric(x) ), .SDcols=1:2 ]

tab.mod3_all[ , lower := coef - 1.96*SE ]
tab.mod3_all[ , upper := coef + 1.96*SE ]

(plot <- (ggplot(data=tab.mod3_all[ time %in% seq(-5,5)], aes(y=coef, x=time, group=1))
          + geom_hline(yintercept = 0, colour="gray50", linetype="dashed")
          + geom_vline(xintercept = 0, colour="black", size=0.5)
          + annotate("text", x = 2.3, y = 1.15, label = "Unemployment shock",  size=3.3)
          + geom_ribbon(data=tab.mod3_all[ time %in% seq(-5,-2,1) ], aes(ymin=lower, ymax=upper), alpha=.25, fill="gray30")
          + geom_ribbon(data=tab.mod3_all[ time %in% seq(0,5,1) ], aes(ymin=lower, ymax=upper), alpha=.25, fill="gray30")
          + geom_line(data=tab.mod3_all[ time %in% seq(-5,-2,1) ], size=0.8)
          + geom_line(data=tab.mod3_all[ time %in% c(-2,-1,0) ], size=0.8, linetype="dashed")
          + geom_line(data=tab.mod3_all[ time %in% seq(0,5,1) ], size=0.8)
          + geom_point(size=2.2)
          + ylab("Marginal effects of unemployment\non household debt (log, relative to t=-1)")
          + xlab("Event time (in years)")
          + theme_bw()
          + theme(legend.position="bottom", panel.grid.minor=element_blank(),
                  plot.title = element_text(hjust = 0.5), panel.border = element_rect(color="gray"),
                  plot.margin=unit(c(5.5, 5.5, 47, 5.5), "points"))
          + scale_x_continuous(breaks=seq(-5,5))
          + scale_y_continuous(limits=c(-0.2,1.2), breaks=seq(-0.5,1.5,0.5))
))



######################
### SI Figure C.2b ###
######################

mod4 <- felm(ihs_bankgaeld_hh_cpi10_adj ~ yr1st_unemp_tally_fac*dispo_hh_v2_vol_yr_unemp_grp + I(age^2) + educ + save_ratio  + arledgr_wks  + koen + factor(FAMILIE_TYPE)  +
                                             antboernf + insur_stat2 + ihs_arblhumv_hh + dispo_hh_quint | year + b_year_all + postnummer_year | 0 | familie_id,
                                           d_FPx_v8_head_unemp_25_wks[ yr1st_unemp_tally %in% c(-5:5) & owner_koejd2 == 0  ] )

summary(mod4)

# Plots
estmean.mod4 <- mod4$coefficients
var.mod4 <- mod4$clustervcv

rownames(estmean.mod4)

tab.mod4 <- list()
for(i in seq(1,21,10) ){ 
  tab.1 <- list()
  for(j in 1:10){
    # All other
    marg.eff.mod4 <- mod4$coefficients[j] + mod4$coefficients[33+(i-1)+j]
    
    SEs.mod4 <- eval(parse(text=paste("deltamethod( ~ (x",j,") + (x",33+(i-1)+j,"), 
                                                          estmean.mod4, var.mod4, ses=T)", sep="")))
    
    tab.1[[j]] <- as.data.table(cbind(coef = marg.eff.mod4, SE = SEs.mod4, time = as.numeric(gsub("yr1st_unemp_tally_fac","",rownames(mod4$coefficients)[j]))))
    
  }
  tab.mod4[[i]] <- do.call("rbind",tab.1)
}

tab.mod4x <- as.data.table(do.call("rbind",tab.mod4))
tab.mod4x[ , inc_vol_grp := rep(gsub("dispo_hh_v2_vol_yr_unemp_grp","", rownames(mod4$coefficients)[11:13]), each=10) ]


# Baseline
tab.mod4_b <- list()
for(j in 1:10){
  marg.eff.mod4_b <- mod4$coefficients[j]
  
  SEs.mod4_b <- eval(parse(text=paste("deltamethod( ~ (x",j,"), estmean.mod4, var.mod4, ses=T)", sep="")))
  
  tab.mod4_b[[j]] <- as.data.table(cbind(coef = marg.eff.mod4_b, SE = SEs.mod4_b, time = as.numeric(gsub("yr1st_unemp_tally_fac","",rownames(mod4$coefficients)[j]))))
  
}

tab.mod4_bx <- do.call("rbind",tab.mod4_b)
tab.mod4_bx[ , inc_vol_grp := "0" ]

tab.mod4_all <- rbind(tab.mod4x,tab.mod4_bx)

tab.mod4_all[ , 1:2 := lapply(.SD, function(x) as.numeric(x) ), .SDcols=1:2 ]

tab.mod4_all[ , lower := coef - 1.96*SE ]
tab.mod4_all[ , upper := coef + 1.96*SE ]

tab.mod4_all[ inc_vol_grp == "-2" , inc_vol_grp2 := "25-50%" ]
tab.mod4_all[ inc_vol_grp == "-1" , inc_vol_grp2 := "0-25%" ]

tab.mod4_all2 <- tab.mod4_all[ !is.na(inc_vol_grp2) ]

tab.mod4_all2[ , inc_vol_grp2 := factor(inc_vol_grp2, levels=c("0-25%","25-50%"))]

(plot <- (ggplot(data=tab.mod4_all2[ time %in% seq(-5,5) ], aes(y=coef, x=time, group=inc_vol_grp2, colour=inc_vol_grp2, fill=inc_vol_grp2))
                     + geom_hline(yintercept = 0, colour="gray50", linetype="dashed")
                     + geom_vline(xintercept = 0, colour="black", size=0.5)
                     + geom_ribbon(data=tab.mod4_all2[ time %in% seq(-5,-2,1) ], aes(ymin=lower, ymax=upper, fill=inc_vol_grp2), alpha=.2, colour=NA)
                     + geom_ribbon(data=tab.mod4_all2[ time %in% seq(0,5,1) ], aes(ymin=lower, ymax=upper, fill=inc_vol_grp2), alpha=.2, colour=NA)
                     + geom_line(data=tab.mod4_all2[ time %in% seq(-5,-2,1) ], size=0.8)
                     + geom_line(data=tab.mod4_all2[ time %in% c(-2,-1,0) ], size=0.8, linetype="dashed")
                     + geom_line(data=tab.mod4_all2[ time %in% seq(0,5,1) ], size=0.8)
                     + annotate("text", x = 2.3, y = 1.15, label = "Unemployment shock",  size=3.3)
                     + geom_point(aes(shape=inc_vol_grp2), size=2.3, colour="gray20", fill="white")
                     + ylab("Marginal effects of unemployment\non household debt (log, relative to t=-1)")
                     + xlab("Event time (in years)")
                     + theme_bw()
                     + theme(legend.position="bottom", panel.grid.minor=element_blank(), plot.title = element_text(hjust = 0.5), panel.border = element_rect(color="gray"))
                     + guides(colour = guide_legend(nrow = 1))
                     + scale_color_manual(name="Net income loss", values=c("gray50","black"))
                     + scale_fill_manual(name="Net income loss", values=c("gray50","black"))
                     + scale_shape_manual(name="Net income loss", values=c(21,23))
                     + scale_y_continuous(limits=c(-0.2,1.2), breaks=seq(-0.5,1.5,0.5))
                     + scale_x_continuous(breaks=seq(-5,5))
))



#################################
### Figure 9 and SI Table C.5 ###
#################################

### Use dataset dst_main_head_v4.RData from 1_prep_dst_data

### Income quintile
dst_main_head_v4[ , dispo_hh_v2_cutoff1 := lapply(.SD, function(x) quantile(x, .2, na.rm=T)), 
              by=c("year"), .SDcols="dispo_hh_v2" ]
dst_main_head_v4[ , dispo_hh_v2_cutoff2 := lapply(.SD, function(x) quantile(x, .4, na.rm=T)), 
              by=c("year"), .SDcols="dispo_hh_v2" ]
dst_main_head_v4[ , dispo_hh_v2_cutoff3 := lapply(.SD, function(x) quantile(x, .6, na.rm=T)), 
              by=c("year"), .SDcols="dispo_hh_v2" ]
dst_main_head_v4[ , dispo_hh_v2_cutoff4 := lapply(.SD, function(x) quantile(x, .8, na.rm=T)), 
              by=c("year"), .SDcols="dispo_hh_v2" ]

dst_main_head_v4[ dispo_hh_v2 <  dispo_hh_v2_cutoff1 , hh_dispo_quint := "1" ]
dst_main_head_v4[ dispo_hh_v2 >= dispo_hh_v2_cutoff1 , hh_dispo_quint := "2" ]
dst_main_head_v4[ dispo_hh_v2 >= dispo_hh_v2_cutoff2 , hh_dispo_quint := "3" ]
dst_main_head_v4[ dispo_hh_v2 >= dispo_hh_v2_cutoff3 , hh_dispo_quint := "4" ]
dst_main_head_v4[ dispo_hh_v2 >= dispo_hh_v2_cutoff4 , hh_dispo_quint := "5" ]
dst_main_head_v4[ , hh_dispo_quint := relevel(hh_dispo_quint, "1")]

dst_main_head_v4[ , arledgr_wks := round(arledgr/1000*52,1) ]

dst_main_head_v4[ ansx_socstil3 == "unemployed" , ansx_unemp := 1 ]
dst_main_head_v4[ ansx_socstil3 == "employed" , ansx_unemp := 0 ]

### Model
mod5 <- felm(ihs_bankgaeld_cpi10_adj ~ ansx_unemp*hh_dispo_quint + ihs_indestpi_cpi10_adj + antboernf + educ + I(age^2) + fam_stat + arledgr_wks
                               |  pnr + year + b_year_all | 0 | familie_id, dst_main_head_v4[ owner_koejd2 == 0 ] )
summary(mod5)

# Plot
estmean.mod5 <- mod5$coefficients
var.mod5 <- mod5$clustervcv

rownames(estmean.mod5)

tab.mod5 <- list()
for(j in 1:4){
  marg.eff.mod5 <- mod5$coefficients[1] + mod5$coefficients[15+j]
  
  SEs.mod5 <- eval(parse(text=paste("deltamethod( ~ x1 + (x",15+j,"), estmean.mod5, var.mod5, ses=T)", sep="")))
  
  tab.mod5[[j]] <- as.data.table(cbind(coef = marg.eff.mod5, SE = SEs.mod5, inc_quint = as.numeric(gsub("hh_dispo_quint","",rownames(mod5$coefficients)[1+j]))))
  
}

tab.mod5_all <- do.call("rbind",tab.mod5)

tab.mod5_all[ , 1:2 := lapply(.SD, function(x) as.numeric(x) ), .SDcols=1:2 ]

tab.mod5_all[ , lower := coef - 1.96*SE ]
tab.mod5_all[ , upper := coef + 1.96*SE ]

tab.mod5_all[ inc_quint == 1 , inc_quint2 := "Q1" ]
tab.mod5_all[ inc_quint == 2 , inc_quint2 := "Q2" ]
tab.mod5_all[ inc_quint == 3 , inc_quint2 := "Q3" ]
tab.mod5_all[ inc_quint == 4 , inc_quint2 := "Q4" ]
tab.mod5_all[ inc_quint == 5 , inc_quint2 := "Q5" ]

(plot <- (ggplot(data=tab.mod5_all, aes(y=coef, x=inc_quint_name, group=group))
          + geom_hline(yintercept = 0, colour="gray50", linetype="dashed")
          + geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.75, width=0.1, size=0.75, color="gray30")
          + geom_point(size=2.75)
          + theme_bw()
          + ylab("Marginal effects of unemployment\non unsecured debt (log)")
          + xlab("Income quintile")
          + theme(legend.position="bottom", panel.grid.minor=element_blank(), panel.border = element_rect(color="gray"))
))


