
################################
### LITS: Life in Transition ###
################################

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
# install.packages("BBmisc")      # BBmisc_1.11
# install.packages("ggrepel")     # ggrepel_0.8.2
# install.packages("lme4")        # lme4_1.1-25
# install.packages("sjPlot")      # sjPlot_2.8.6
# install.packages("lemon")       # lemon_0.4.5
# install.packages("glmmTMB")     # glmmTMB_1.0.2.1

library(ggplot2)
library(reshape2)
library(lmtest)
library(sandwich)
library(MASS)
library(stargazer)
library(data.table)
library(BBmisc)
library(ggrepel)
library(lme4)
library(sjPlot)
library(lemon)
library(glmmTMB)


# Set working directory
setwd("~/YOURPATH/replication_files")

### Load LITS wave 2
load("Data/lits_w2_data.RData")

### Prep data
lits_w2_data[ , countryname := trimws(countryname) ]

lits_w2_data[ , homeowner := ifelse(q202 == 2, 1, 0) ]

lits_w2_data[ q227 > 0 , income_decile := q227 ]

lits_w2_data[ , did_work := ifelse(q501 == 1, 1, 0) ]

lits_w2_data[ ,.N, q515 ]
lits_w2_data[ , educ_level := q515 ]
lits_w2_data[ q515 == -1 , educ_level := 0 ]

lits_w2_data[ , educ_level_num := as.numeric(as.character(educ_level)) ]

lits_w2_data[ , married := ifelse(q701 == 2, 1, 0) ]

lits_w2_data[ Select_r == 1 , respondent_male := 1 ]
lits_w2_data[ Select_r == 2 , respondent_male := 0 ]

lits_w2_data[ Head_Of0 == 1 , head_age_grp := "18-24" ]
lits_w2_data[ Head_Of0 == 2 , head_age_grp := "25-34" ]
lits_w2_data[ Head_Of0 == 3 , head_age_grp := "35-44" ]
lits_w2_data[ Head_Of0 == 4 , head_age_grp := "45-54" ]
lits_w2_data[ Head_Of0 == 5 , head_age_grp := "55-64" ]
lits_w2_data[ Head_Of0 == 6 , head_age_grp := "65+" ]

lits_w2_data[ , no_children_hh := as.numeric(as.character(Children)) ]

lits_w2_data[ q804a == 1 | q804b == 1 | q804c == 1 | q804d == 1 | q804e == 1, coping := "Reduce expenses"]
lits_w2_data[ q810_2 == 1 | q810_1 == 1 , coping := "Borrow money from\nfriends/relatives"]
lits_w2_data[ q810_3 == 1 | q810_4 == 1 | q810_6 == 1 , coping := "Borrow money from banks"]
lits_w2_data[ q804n == 1 | q804o == 1 | q804q == 1 , coping := "Delay\npayments"]
lits_w2_data[ q804r == 1 , coping := "Sell\nassets"]
lits_w2_data[ q804s == 1 , coping := "Forced\nto move"]
lits_w2_data[ q804g == 1 | q804i == 1, coping := "Postponed or\nwithdrew education"]

lits_w2_data[ q812_1 == 1 | q812_2 == 1 | q812_3 == 1 | q812_4 == 1 , coping := "Government transfers" ]

lits_w2_data[ , borrow_bank := ifelse(coping == "Borrow money from banks", 1, 0) ]
lits_w2_data[ , reduce_expenses := ifelse(q804a == 1 | q804b == 1 | q804c == 1 | q804d == 1 | q804e == 1, 1, 0) ]
lits_w2_data[ , public_benefits := ifelse(q812_1 == 1 | q812_2 == 1 | q812_3 == 1 | q812_4 == 1 , 1, 0) ]


#####################
### SI Figure C.1 ### 
#####################

# Calculate shares per category
lits_w2_data_shares <- lits_w2_data[ ,.N , by=c("coping","countryname")]
lits_w2_data_shares <- lits_w2_data_shares[ !is.na(coping) & !is.na(countryname) ]

lits_w2_data_shares[ , count := lapply(.SD, function(x) sum(x, na.rm=T) ), by=c("countryname"), .SDcols="N" ]
lits_w2_data_shares[ , share := N/count*100]

lits_w2_data_shares[ , countryname_fac := factor(countryname, levels=c("Sweden","UK","France","Germany","Italy")) ]

lits_w2_data_shares_sub1 <- lits_w2_data_shares[ ! coping %in% c("Borrow money from\nfriends/relatives","Postponed or\nwithdrew education","Forced\nto move","Sell\nassets","Delay\npayments") & 
                                                   countryname %in% c("Germany","Sweden","UK","France","Italy") ]

pdf("Output/SI_figure_c1.pdf", width=9, height=3)
(plot <- ggplot(lits_w2_data_shares_sub1, aes(y=share, x=countryname_fac, fill=countryname_fac, label=round(share,1))) 
  + geom_bar(position = "dodge", stat = "identity", show.legend=F)
  + geom_label(size = 2.5, position = position_stack(vjust = 0.5), show.legend = F)
  + theme_bw()
  + ylab("Population share (%)")
  + facet_rep_wrap(~ coping, repeat.tick.labels = "x")
  + theme(legend.position="right", panel.grid.minor=element_blank(), 
          panel.border = element_rect(colour="gray"), axis.title.x = element_blank())
  + scale_fill_brewer(palette = "Set1")
)
dev.off()




####################
### SI Table C.1 ###
####################

cty_list2 <- c("Germany","Sweden","UK","France","Italy") 

mod5.1 <- glmer(reduce_expenses ~ (1|countryname) + homeowner + income_decile + did_work + educ_level_num + married +
                  respondent_male + head_age_grp + no_children_hh, lits_w2_data[ countryname %in% cty_list2 ], family = "binomial")

mod5.2 <- glmer(borrow_bank ~ (1|countryname) + homeowner + income_decile + did_work + educ_level_num + married +
                  respondent_male + head_age_grp + no_children_hh, lits_w2_data[ countryname %in% cty_list2 ], family = "binomial")

mod5.3 <- glmer(public_benefits ~ (1|countryname) + homeowner + income_decile + did_work + educ_level_num + married +
                  respondent_male + head_age_grp + no_children_hh, lits_w2_data[ countryname %in% cty_list2 ], family = "binomial")

modX_labels <- c("Homeowner","Income (deciles)","Employed","Education level","Married","Respondent male",
                 "Age head 25-34","Age head 25-34","Age head 35-44","Age head 45-54","Age head 55-64","Age head 65+",
                 "Number of children in hh")

### Get random effect intercepts
sigma <- cbind(ranef(mod5.1)$countryname, ranef(mod5.2)$countryname, ranef(mod5.3)$countryname)

sigma_fr <- round(as.numeric(sigma[1,]), 2)
sigma_de <- round(as.numeric(sigma[2,]), 2)
sigma_it <- round(as.numeric(sigma[3,]), 2)
sigma_se <- round(as.numeric(sigma[4,]), 2)
sigma_uk <- round(as.numeric(sigma[5,]), 2)

stargazer(mod5.1, mod5.2, mod5.3,
          dep.var.labels = c("reduce expenses","borrow from bank", "government transfers"),
          covariate.labels = modX_labels,
          type="text", omit.stat = c("f","ser"), align=TRUE, no.space=TRUE, digits=2, digits.extra=0,
          add.lines = list(c("FR", sigma_fr), c("DE", sigma_de), c("IT", sigma_it),
                           c("SE", sigma_se), c("UK", sigma_uk)),
          out="Output/SI_table_c1.txt")


#################
#### Figure 5 ###
#################

### Collect random effect coefficients for plot
d_plot_mod5.1 <- as.data.table(plot_model(mod5.1, type="re")$data)
d_plot_mod5.2 <- as.data.table(plot_model(mod5.2, type="re")$data)
d_plot_mod5.3 <- as.data.table(plot_model(mod5.3, type="re")$data)

d_plot_mod5.1[ , dv := "Reduce expenses" ]
d_plot_mod5.2[ , dv := "Borrow money from banks" ]
d_plot_mod5.3[ , dv := "Government transfers" ]

d_plot_mod5.2$term <- factor(d_plot_mod5.2$term, levels = d_plot_mod5.2$term
                             [order(d_plot_mod5.2$estimate, decreasing=T)]) 

d_plot_mod5_COMB <- rbind(d_plot_mod5.2,d_plot_mod5.3,d_plot_mod5.1)
d_plot_mod5_COMB[ , dv := factor(dv, levels=c("Borrow money from banks","Reduce expenses","Government transfers"))]
d_plot_mod5_COMB <- rbind(d_plot_mod5.2,d_plot_mod5.3,d_plot_mod5.1)
d_plot_mod5_COMB[ , dv := factor(dv, levels=c("Borrow money from banks","Reduce expenses","Government transfers"))]


### Add credit permissiveness scores for 2009 
d_plot_mod5.2[ term == "France" , country_score := "France (0.30)"]
d_plot_mod5.2[ term == "Germany" , country_score := "Germany (0.17)"]
d_plot_mod5.2[ term == "Italy" , country_score := "Italy (0.02)"]
d_plot_mod5.2[ term == "Sweden" , country_score := "Sweden (0.59)"]
d_plot_mod5.2[ term == "UK" , country_score := "United Kingdom (0.84)"]

d_plot_mod5.3[ term == "France" , country_score := "France (0.30)"]
d_plot_mod5.3[ term == "Germany" , country_score := "Germany (0.17)"]
d_plot_mod5.3[ term == "Italy" , country_score := "Italy (0.02)"]
d_plot_mod5.3[ term == "Sweden" , country_score := "Sweden (0.59)"]
d_plot_mod5.3[ term == "UK" , country_score := "United Kingdom (0.84)"]

d_plot_mod5.1[ term == "France" , country_score := "France (0.30)"]
d_plot_mod5.1[ term == "Germany" , country_score := "Germany (0.17)"]
d_plot_mod5.1[ term == "Italy" , country_score := "Italy (0.02)"]
d_plot_mod5.1[ term == "Sweden" , country_score := "Sweden (0.59)"]
d_plot_mod5.1[ term == "UK" , country_score := "United Kingdom (0.84)"]

d_plot_mod5.2$country_score <- factor(d_plot_mod5.2$country_score, levels = d_plot_mod5.2$country_score
                                      [order(d_plot_mod5.2$estimate, decreasing=T)]) 

d_plot_mod5_COMB <- rbind(d_plot_mod5.2,d_plot_mod5.3,d_plot_mod5.1)
d_plot_mod5_COMB[ , dv := factor(dv, levels=c("Borrow money from banks","Reduce expenses","Government transfers"))]
d_plot_mod5_COMB <- rbind(d_plot_mod5.2,d_plot_mod5.3,d_plot_mod5.1)
d_plot_mod5_COMB[ , dv := factor(dv, levels=c("Borrow money from banks","Reduce expenses","Government transfers"))]

pdf("Output/figure_5.pdf", width=7.2, height=2.75)
(plot <- (ggplot(data=d_plot_mod5_COMB[], aes(y=estimate, x=country_score, group=1, label=round(estimate,1)))
          + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.15, size=0.7, colour="gray50")
          + geom_point(size=2)
          + theme_bw()
          + facet_wrap( ~ dv, scales="free_x")
          + theme(legend.position="bottom", panel.grid.minor=element_blank(), 
                  panel.border = element_rect(color="gray"), axis.title.y = element_blank())
          + coord_flip()
          + ylab("Odds ratios for using the coping mechanism")
))
dev.off()





