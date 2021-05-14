

##################################################
### This script draws on extracted variables   ###
### from the registers defined in the codebook ###
##################################################


### Prepare IDAN registry data

# Subset to main categories only: H,M,A,S
idan_sub <- idan[ type %in% c("H","M","A","S") ]

idan_sub2 <- idan_sub

rm(idan_sub)
gc()

# Shift ansxfrem (forward looking) so it reflects the current year status
setkey(idan_sub2, pnr, year)
idan_sub2 <- idan_sub2 %>% group_by(pnr) %>%
  mutate(ansxfrem_lag = lag(ansxfrem, n=1)) %>% as.data.table()

idan_sub2 <- idan_sub2 %>% group_by(pnr) %>%
  mutate(ansxtilb_lead = lead(ansxtilb, n=1)) %>% as.data.table()

setattr(idan_sub2, "class", c("data.table","data.frame"))

setnames(idan_sub2, "ansxtilb","ansx_back")
setnames(idan_sub2, "ansxfrem","ansx_fwd")

setnames(idan_sub2, "ansxtilb_lead","ansx_back_lead")
setnames(idan_sub2, "ansxfrem_lag","ansx_fwd_lag")

idan_sub2[ is.na(ansx_fwd_lag)  , ansx_fwd_lag := "" ]
idan_sub2[ is.na(ansx_back_lead) , ansx_back_lead := "" ]

idan_sub2[ , ansx_current_year := ifelse(ansx_fwd_lag == "", ansx_back_lead, ansx_fwd_lag) ]
idan_sub2[ , ansx_current_year := gsub("T","A", ansx_current_year) ] # Match it to ansxfrem coding

### Merge wage earner indicator
setkey(idan_sub2, pnr, year)
idan_sub2 <- idan_sub2 %>% group_by(pnr) %>%
  mutate(lonm_fwd_lag = lag(lonmfrem, n=1)) %>% as.data.table()

idan_sub2 <- idan_sub2 %>% group_by(pnr) %>%
  mutate(lonm_back_lead = lead(lonmtilb, n=1)) %>% as.data.table()

setattr(idan_sub2, "class", c("data.table","data.frame"))

idan_sub2[ , lonm_current_year := ifelse(is.na(lonm_fwd_lag), lonm_back_lead, lonm_fwd_lag) ]
idan_sub2[ type == "S" , lonm_current_year := 0 ]

### Assign values to labor market status
idan_sub2[ , ansx_current_year2 := ansx_current_year ]
idan_sub2[ type == "S" & ansx_current_year == "A3" ,  ansx_current_year2 := "now self-employed"]
idan_sub2[ type == "H" & ansx_current_year == "A3" ,  ansx_current_year2 := "now new job new firm"]
idan_sub2[ type == "A" & ansx_current_year == "A3" ,  ansx_current_year2 := "now self-employed"]

idan_sub2[ type == "H" & ansx_current_year == "04" ,  ansx_current_year2 := "now job as employee"]

idan_sub2[ ansx_current_year == "A1" ,  ansx_current_year2 := "now new work same firm (A1)"]
idan_sub2[ ansx_current_year == "A2" ,  ansx_current_year2 := "now new work same firm (A2)"]
idan_sub2[ ansx_current_year == "A4" ,  ansx_current_year2 := "now new work due to closure"]
idan_sub2[ ansx_current_year == "A5" ,  ansx_current_year2 := "now unemployed"]
idan_sub2[ ansx_current_year == "A6" ,  ansx_current_year2 := "now outside workforce"]
idan_sub2[ ansx_current_year == "A7" ,  ansx_current_year2 := "now on leave"]

idan_sub2[ ansx_current_year2 == "now new work due to closure" & type == "S",  ansx_current_year2 := "now new work due to closure -- self-empl"]

idan_sub2[ ansx_current_year2 == "U" & tilknyt %in% c(1,11,21,31) , ansx_current_year2 := "full time work" ]
idan_sub2[ ansx_current_year2 == "U" & tilknyt %in% c(2,3,4,12,13,14,22,23,24,32,33,34) , ansx_current_year2 := "part time work" ]
idan_sub2[ ansx_current_year2 == "U" & type == "H" & tilknyt %in% c(5,15,25,35) , ansx_current_year2 := "part time work (<10h)" ]

idan_sub2[ ansx_current_year2 == "U" & type == "S" , ansx_current_year2 := "self-employed" ]
idan_sub2[ ansx_current_year2 == "U" & type == "A" , ansx_current_year2 := "self-employed" ]

idan_sub2[ ansx_current_year2 == "02" & type == "A" , ansx_current_year2 := "now self-employed" ]
idan_sub2[ ansx_current_year2 == "02" & type == "S" , ansx_current_year2 := "now self-employed" ]
idan_sub2[ ansx_current_year2 == "02" & lonm_current_year == 0 & is.na(type),  ansx_current_year2 := "now self-employed"]

# Assisting spouse
idan_sub2[ ansx_current_year2 == "03" & type == "M" , ansx_current_year2 := "now assisting spouse" ]
idan_sub2[ ansx_current_year2 == "A3" & type == "M" , ansx_current_year2 := "now assisting spouse" ]
idan_sub2[ ansx_current_year2 == "U"  & type == "M" , ansx_current_year2 := "assisting spouse" ]

idan_sub2[ ansx_current_year2 == "U" & type == "H" , ansx_current_year2 := "work" ]

idan_sub2[ ansx_current_year2 == "03" & type == "H" & tilknyt %in% c(2,3,4,12,13,14,22,23,24,32,33,34) , 
           ansx_current_year2 := "assisting spouse, pt work" ]

idan_sub2[ ansx_current_year2 == "03" & type == "H" & tilknyt %in% c(1,11,21,31) , 
           ansx_current_year2 := "assisting spouse, ft work" ]

idan_sub2[ ansx_current_year2 == "03" & is.na(type) & is.na(lonm_current_year), ansx_current_year2 := "now assisting spouse" ]

# Remaining A3
idan_sub2[ ansx_current_year2 == "A3" & lonm_current_year == 1 , ansx_current_year2 := "now new job new firm" ]

# Remaining 02
# Work with lonm_current_year tilknyt
idan_sub2[ ansx_current_year2 == "02" & lonm_current_year == 1 & tilknyt %in% c(1,11,21,31) , ansx_current_year2 := "now full time work" ]
idan_sub2[ ansx_current_year2 == "02" & lonm_current_year == 1 & tilknyt %in% c(2,3,4,12,13,14,22,23,24,32,33,34) , ansx_current_year2 := "now part time work" ]
idan_sub2[ ansx_current_year2 == "02" & lonm_current_year == 1 & tilknyt %in% c(5,15,25,35) , ansx_current_year2 := "now part time work (<10h)" ]

idan_sub2[ ansx_current_year2 == "02" & lonm_current_year == 1  , ansx_current_year2 := "now work" ]

# Empty cells ansx_current_year2
idan_sub2[ ansx_current_year2 == "" & type == "H" & tilknyt %in% c(1,11,21,31) , ansx_current_year2 := "full time work" ]
idan_sub2[ ansx_current_year2 == "" & type == "H" & tilknyt %in% c(2,3,4,12,13,14,22,23,24,32,33,34) , ansx_current_year2 := "part time work" ]
idan_sub2[ ansx_current_year2 == "" & type == "H" & tilknyt %in% c(5,15,25,35) , ansx_current_year2 := "part time work (<10h)" ]

idan_sub2[ ansx_current_year2 == "" & type == "H" , ansx_current_year2 := "work" ]

idan_sub2[ ansx_current_year2 == "" & type == "M" & lonm_current_year == 0 , ansx_current_year2 := "assisting spouse" ]
idan_sub2[ ansx_current_year2 == "" & type == "S" & lonm_current_year == 0 , ansx_current_year2 := "self-employed" ]
idan_sub2[ ansx_current_year2 == "" & type == "A" & lonm_current_year == 0 , ansx_current_year2 := "self-employed" ]

idan_sub2[ ansx_current_year2 == "" & type == "M" & is.na(lonm_current_year) , ansx_current_year2 := "assisting spouse" ]
idan_sub2[ ansx_current_year2 == "" & type == "S" & is.na(lonm_current_year) , ansx_current_year2 := "self-employed" ]
idan_sub2[ ansx_current_year2 == "" & type == "A" & is.na(lonm_current_year) , ansx_current_year2 := "self-employed" ]

### Deal with U: some information comes from missing data, so use the backward insteaad
idan_sub2[ ansx_current_year2 == "U" & ansx_back_lead == "T5"  , ansx_current_year2 := "now unemployed" ]
idan_sub2[ ansx_current_year2 == "U" & ansx_back_lead == "T6"  , ansx_current_year2 := "outside workforce" ]
idan_sub2[ ansx_current_year2 == "U" & ansx_back_lead == "T7"  , ansx_current_year2 := "now on leave" ]

setkey(idan_sub2, pnr, year)
idan_sub2_u <- unique(idan_sub2)

### Merge groups
idan_sub2_u[ , ansx_current_year3 := ansx_current_year2 ]
idan_sub2_u[ ansx_current_year3 == "" , ansx_current_year3 := "NA" ]
idan_sub2_u[ ansx_current_year3 == " ." , ansx_current_year3 := "NA" ]

idan_sub2_u[ ansx_current_year2 == "assisting spouse, ft work" , ansx_current_year3 := "assisting spouse" ]
idan_sub2_u[ ansx_current_year2 == "assisting spouse, pt work" , ansx_current_year3 := "assisting spouse" ]
idan_sub2_u[ ansx_current_year2 == "assisting spouse, ft work" , ansx_current_year3 := "assisting spouse" ]

idan_sub2_u[ ansx_current_year2 == "now new work same firm (A1)" , ansx_current_year3 := "now new work same firm" ]
idan_sub2_u[ ansx_current_year2 == "now new work same firm (A2)" , ansx_current_year3 := "now new work same firm" ]

idan_sub2_u[ ansx_current_year2 == "part time work (<10h)" , ansx_current_year3 := "working" ]
idan_sub2_u[ ansx_current_year2 == "part time work" , ansx_current_year3 := "working" ]
idan_sub2_u[ ansx_current_year2 == "full time work" , ansx_current_year3 := "working" ]
idan_sub2_u[ ansx_current_year2 == "work" , ansx_current_year3 := "working" ]

idan_sub3 <- idan_sub2_u
idan_sub3[ , c("lonm_fwd_lag", "lonm_back_lead","ansx_fwd_lag", "ansx_back_lead") := NULL ]

# Merge idan_sub3 with main registries


###########################################################
### dst_main is a (restricted) datafile based on all    ###
### extracted registry variables listed in the codebook ###
###########################################################

### Employment status
dst_main[ SOCSTIL_KODE == "115" , empl_status3 := "self employed"]
dst_main[ SOCSTIL_KODE == "116" , empl_status3 := "self employed"]
dst_main[ SOCSTIL_KODE == "117" , empl_status3 := "self employed"]
dst_main[ SOCSTIL_KODE == "118" , empl_status3 := "self employed"]
dst_main[ SOCSTIL_KODE == "120" , empl_status3 := "assisting spouse"]
dst_main[ SOCSTIL_KODE == "130" , empl_status3 := "employed"]
dst_main[ SOCSTIL_KODE == "131" , empl_status3 := "employed"]
dst_main[ SOCSTIL_KODE == "132" , empl_status3 := "employed"]
dst_main[ SOCSTIL_KODE == "133" , empl_status3 := "employed"]
dst_main[ SOCSTIL_KODE == "134" , empl_status3 := "employed"]
dst_main[ SOCSTIL_KODE == "135" , empl_status3 := "employed"]
dst_main[ SOCSTIL_KODE == "200" , empl_status3 := "unemployed"]
dst_main[ SOCSTIL_KODE == "201" , empl_status3 := "unemployed"]
dst_main[ SOCSTIL_KODE == "310" , empl_status3 := "student"]
dst_main[ SOCSTIL_KODE == "315" , empl_status3 := "early retirement"]
dst_main[ SOCSTIL_KODE == "316" , empl_status3 := "partial unemployed"]
dst_main[ SOCSTIL_KODE == "317" , empl_status3 := "employed without pay"]
dst_main[ SOCSTIL_KODE == "318" , empl_status3 := "leave of absence"]
dst_main[ SOCSTIL_KODE == "319" , empl_status3 := "training"]
dst_main[ SOCSTIL_KODE == "320" , empl_status3 := "activation"]
dst_main[ SOCSTIL_KODE == "321" , empl_status3 := "activation"]
dst_main[ SOCSTIL_KODE == "322" , empl_status3 := "maternity"]
dst_main[ SOCSTIL_KODE == "323" , empl_status3 := "sickness"]
dst_main[ SOCSTIL_KODE == "324" , empl_status3 := "early retirement"]
dst_main[ SOCSTIL_KODE == "325" , empl_status3 := "early retirement"]
dst_main[ SOCSTIL_KODE == "326" , empl_status3 := "unemployed"]
dst_main[ SOCSTIL_KODE == "327" , empl_status3 := "rehab"]
dst_main[ SOCSTIL_KODE == "328" , empl_status3 := "pensioner"]
dst_main[ SOCSTIL_KODE == "329" , empl_status3 := "pensioner"]
dst_main[ SOCSTIL_KODE == "330" , empl_status3 := "other outside labor force"]
dst_main[ SOCSTIL_KODE == "331" , empl_status3 := "pensioner"]
dst_main[ SOCSTIL_KODE == "332" , empl_status3 := "training"]
dst_main[ SOCSTIL_KODE == "333" , empl_status3 := "training"]
dst_main[ SOCSTIL_KODE == "334" , empl_status3 := "unemployed"]
dst_main[ SOCSTIL_KODE == "335" , empl_status3 := "activation"]
dst_main[ SOCSTIL_KODE == "400" , empl_status3 := "child"]

### Combine ansx with socstile_kode for those outside the labor market
dst_main[ , ansx_socstil := ansx_current_year3 ]

dst_main[ is.na(ansx_current_year3) , ansx_socstil := empl_status3 ]
dst_main[ ansx_current_year3 == "NA" , ansx_socstil := empl_status3 ]

dst_main[ ansx_current_year3 == "now outside workforce" , ansx_socstil := paste("now",empl_status3, sep=" ") ]
dst_main[ ansx_current_year3 == "outside workforce" , ansx_socstil := empl_status3 ]

# Other categories
dst_main[ ansx_current_year3 == "not employed" & ansx_current_year2 == "" , ansx_socstil := empl_status3 ]

### Rename and regroup some
dst_main[ ansx_socstil == "now child" , ansx_socstil := "child" ]
dst_main[ ansx_socstil == "working" , ansx_socstil := "employed" ]
dst_main[ ansx_socstil == "now NA" , ansx_socstil := "NA" ]

dst_main[ is.na(ansx_socstil) , ansx_socstil := "NA" ]

### Code employment status
dst_main[ , ansx_socstil2 := ansx_socstil ]
dst_main[ , ansx_socstil2 := gsub("now ", "", ansx_socstil2) ]

dst_main[ ansx_socstil2 == "new work same firm" ,  ansx_socstil2 := "employed" ]
dst_main[ ansx_socstil2 == "self employed" ,  ansx_socstil2 := "self-employed" ]
dst_main[ ansx_socstil2 == "job as employee" ,  ansx_socstil2 := "employed" ]
dst_main[ ansx_socstil2 == "partial unemployed" ,  ansx_socstil2 := "unemployed" ]
dst_main[ ansx_socstil2 == "rehab" ,  ansx_socstil2 := "sickness" ]
dst_main[ ansx_socstil2 == "part time work (<10h)" ,  ansx_socstil2 := "part time work" ]
dst_main[ ansx_socstil2 == "work" ,  ansx_socstil2 := "employed" ]
dst_main[ ansx_socstil2 == "on leave" ,  ansx_socstil2 := "leave of absence" ]
dst_main[ ansx_socstil2 == "new work due to closure -- self-empl" ,  ansx_socstil2 := "self-employed" ]

dst_main[ ansx_socstil2 == "02" ,  ansx_socstil2 := "NA" ]
dst_main[ ansx_socstil2 == "03" ,  ansx_socstil2 := "NA" ]
dst_main[ ansx_socstil2 == "04" ,  ansx_socstil2 := "NA" ]
dst_main[ ansx_socstil2 == "A3" ,  ansx_socstil2 := "NA" ]
dst_main[ ansx_socstil2 == "A8" ,  ansx_socstil2 := "other outside labor force" ]
dst_main[ ansx_socstil2 == "A9" ,  ansx_socstil2 := "NA" ]
dst_main[ ansx_socstil2 == "U"  ,  ansx_socstil2 := "NA" ]

dst_main[ ansx_socstil2 == "part time work" ,  ansx_socstil2 := "employed" ]
dst_main[ ansx_socstil2 == "full time work" ,  ansx_socstil2 := "employed" ]

dst_main[ , ansx_socstil3 := ansx_socstil2 ]
dst_main[ ansx_socstil3 == "new job new firm" , ansx_socstil3 := "employed" ]
dst_main[ ansx_socstil3 == "new work due to closure" , ansx_socstil3 := "employed" ]


### CPI adjustment
dst_main[ , konthj_cpi10_adj := konthj / cpi_index_10 ]
dst_main[ , overforsindk_cpi10_adj := overforsindk / cpi_index_10 ]
dst_main[ , qmidyd_cpi10_adj := qmidyd / cpi_index_10 ]
dst_main[ , korstoett_cpi10_adj := korstoett / cpi_index_10 ]
dst_main[ , korydial_cpi10_adj := korydial / cpi_index_10 ]
dst_main[ , QORLOV2_cpi10_adj := QORLOV2 / cpi_index_10 ]
dst_main[ , SAMLINK_NY_cpi10_adj := SAMLINK_NY / cpi_index_10 ]
dst_main[ , SKATMVIALT_NY_cpi10_adj := SKATMVIALT_NY / cpi_index_10 ]
dst_main[ , andoverforsel_cpi10_adj := andoverforsel / cpi_index_10 ]
dst_main[ , bankgaeld_cpi10_adj := bankgaeld / cpi_index_10 ]
dst_main[ , oblgaeld_cpi10_adj := oblgaeld / cpi_index_10 ]
dst_main[ , loenmv_cpi10_adj := loenmv / cpi_index_10 ]
dst_main[ , bankakt_cpi10_adj := bankakt / cpi_index_10 ]
dst_main[ , indestpi_cpi10_adj := indestpi / cpi_index_10 ]
dst_main[ , kursakt_cpi10_adj := kursakt / cpi_index_10 ]
dst_main[ , perindkialt_cpi10_adj := perindkialt / cpi_index_10 ]
dst_main[ , dispon_ny_cpi10_adj := DISPON_NY / cpi_index_10 ]


### Household level variables
dst_main[ , familie_id_num := as.numeric(familie_id) ]
dst_main[ !is.na(familie_id_num) , perindkialt_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "perindkialt_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , overforsindk_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "overforsindk_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , SAMLINK_NY_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "SAMLINK_NY_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , qmidyd_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "qmidyd_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , andoverforsel_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "andoverforsel_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , bankgaeld_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "bankgaeld_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , oblgaeld_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "oblgaeld_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , dispon_ny_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "dispon_ny_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , korydial_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "korydial_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , loenmv_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "loenmv_cpi10_adj" ]
dst_main[ !is.na(familie_id_num) , indestpi_hh_cpi10_adj := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "indestpi_cpi10_adj" ]

### Remove values where NA at individuals levels gives you 0 at the hh level
dst_main[ is.na(overforsindk_cpi10_adj) & overforsindk_hh_cpi10_adj == 0 , overforsindk_hh_cpi10_adj := NA ]
dst_main[ is.na(SAMLINK_NY_cpi10_adj) & SAMLINK_NY_hh_cpi10_adj == 0 , SAMLINK_NY_hh_cpi10_adj := NA ]
dst_main[ is.na(qmidyd_cpi10_adj) & qmidyd_hh_cpi10_adj == 0 , qmidyd_hh_cpi10_adj := NA ]
dst_main[ is.na(andoverforsel_cpi10_adj) & andoverforsel_hh_cpi10_adj == 0 , andoverforsel_hh_cpi10_adj := NA ]
dst_main[ is.na(bankgaeld_cpi10_adj) & bankgaeld_hh_cpi10_adj == 0 , bankgaeld_hh_cpi10_adj := NA ]
dst_main[ is.na(dispon_ny_cpi10_adj) & dispon_ny_hh_cpi10_adj == 0 , dispon_ny_hh_cpi10_adj := NA ]
dst_main[ is.na(korydial_cpi10_adj) & korydial_hh_cpi10_adj == 0 , korydial_hh_cpi10_adj := NA ]
dst_main[ is.na(loenmv_cpi10_adj) & loenmv_hh_cpi10_adj == 0 , loenmv_hh_cpi10_adj := NA ]
dst_main[ is.na(perindkialt_cpi10_adj) & perindkialt_hh_cpi10_adj == 0 , perindkialt_hh_cpi10_adj := NA ]
dst_main[ is.na(indestpi_cpi10_adj) & indestpi_hh_cpi10_adj == 0 , indestpi_hh_cpi10_adj := NA ]

### Create new dispo income variable
dst_main[ , dispo_v2 := perindkialt_cpi10_adj - SKATMVIALT_NY_cpi10_adj ]
dst_main[ !is.na(familie_id_num) , dispo_hh_v2 := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "dispo_v2" ]
dst_main[ is.na(dispo_v2) & dispo_hh_v2 == 0 , dispo_hh_v2 := NA ]

### IHS transformation
dst_main[ , ihs_bankgaeld_hh_cpi10_adj := log(bankgaeld_hh_cpi10_adj + sqrt(1+bankgaeld_hh_cpi10_adj^2)) ]
dst_main[ , ihs_bankgaeld_cpi10_adj := log(bankgaeld_cpi10_adj + sqrt(1+bankgaeld_cpi10_adj^2)) ]
dst_main[ , ihs_oblgaeld_hh_cpi10_adj := log(oblgaeld_hh_cpi10_adj + sqrt(1+oblgaeld_hh_cpi10_adj^2)) ]
dst_main[ , ihs_overforsindk_cpi10_adj := log(overforsindk_cpi10_adj + sqrt(1+overforsindk_cpi10_adj^2)) ]
dst_main[ , ihs_qmidyd_cpi10_adj := log(qmidyd_cpi10_adj + sqrt(1+qmidyd_cpi10_adj^2)) ]
dst_main[ , ihs_korydial_cpi10_adj := log(korydial_cpi10_adj + sqrt(1+korydial_cpi10_adj^2)) ]
dst_main[ , ihs_loenmv_hh_cpi10_adj := log(loenmv_hh_cpi10_adj + sqrt(1+loenmv_hh_cpi10_adj^2)) ]
dst_main[ , ihs_qmidyd_hh_cpi10_adj := log(qmidyd_hh_cpi10_adj + sqrt(1+qmidyd_hh_cpi10_adj^2)) ]
dst_main[ perindkialt_cpi10_adj >= 0 , ihs_perindkialt_cpi10_adj := log(perindkialt_cpi10_adj + sqrt(1+perindkialt_cpi10_adj^2)) ]
dst_main[ perindkialt_hh_cpi10_adj >= 0 , ihs_perindkialt_hh_cpi10_adj := log(perindkialt_hh_cpi10_adj + sqrt(1+perindkialt_hh_cpi10_adj^2)) ]
dst_main[ dispo_v2 >= 0 , ihs_dispo_v2 := log(dispo_v2 + sqrt(1+dispo_v2^2)) ]
dst_main[ dispo_hh_v2 >= 0, ihs_dispo_hh_v2 := log(dispo_hh_v2 + sqrt(1+dispo_hh_v2^2)) ]

# Head of household
dst_main[ FAMILIE_TYPE %in% c(1,2,3,4,7,8) & koen == 1 & plads == 2, plads_male_hh := 1 ] # for families make male the head
dst_main[ FAMILIE_TYPE %in% c(1,2,3,4,7,8) & koen == 2 & plads == 1, plads_male_hh := 2 ] # for families make female the spouse
dst_main[ FAMILIE_TYPE %in% c(1,2,3,4,7,8) & plads == 3 , plads_male_hh := plads ] # children

dst_main[ FAMILIE_TYPE %in% c(1,2,3,4,7,8) & koen == 1 & plads == 1 , plads_male_hh := plads ] # male is head
dst_main[ FAMILIE_TYPE %in% c(1,2,3,4,7,8) & koen == 2 & plads == 2 , plads_male_hh := plads ] # female is spouse

# For singles leave as is
dst_main[ FAMILIE_TYPE == 5 , plads_male_hh := plads ]

dst_main[ , plads_male_hh := as.integer(as.character(plads_male_hh))]

### Education 
dst_main[ , afsp1e_v2 := as.character(as.numeric(afsp1e)) ]
dst_main[ , afsp1e_code := substring(afsp1e_v2, 1, 2)]

dst_main[ afsp1e_code == 10 , educ := "primary" ]
dst_main[ afsp1e_code == 20 , educ := "secondary" ]
dst_main[ afsp1e_code == 25 , educ := "secondary" ]
dst_main[ afsp1e_code == 35 , educ := "higher" ]
dst_main[ afsp1e_code == 40 , educ := "higher" ]
dst_main[ afsp1e_code == 50 , educ := "higher" ]
dst_main[ afsp1e_code == 60 , educ := "BA" ]
dst_main[ afsp1e_code == 65 , educ := "MA" ]
dst_main[ afsp1e_code == 70 , educ := "PhD / prof. degree" ]

dst_main[ , afsp1e_v2 := NULL ]


################################################
### Subset to male household heads age 16-65 ###
### and remove self-employed                 ###
################################################

dst_main_head <- dst_main[ age %in% c(16:65) & ansx_socstil3 != "self-employed" & plads_male_hh == 1 ]

# IHS transformation for savings
dst_main_head[ , ihs_indestpi_hh_cpi10_adj := log(indestpi_hh_cpi10_adj + sqrt(1+indestpi_hh_cpi10_adj^2)) ]

dst_main_head[ , arledgr_wks := round(arledgr/1000*52,1) ]

dst_main_head[ , min_yr := lapply(.SD, function(x) min(x,na.rm=T) ),  by=c("pnr"), .SDcols= "year" ]
dst_main_head[ , max_yr := lapply(.SD, function(x) max(x,na.rm=T) ),  by=c("pnr"), .SDcols= "year" ]

# Define periods of unemployment based on number of weeks: more than 40 weeks
dst_main_head[ , unemp := ifelse(arledgr_wks >= 40, 1, 0) ]

### For robustness check SI Figure C.2 define unemployment as more than 25 weeks per year and re-run the code
# dst_main_head[ , unemp := ifelse(arledgr_wks >= 25, 1, 0) ]

# Don't count those on leave
dst_main_head[ ansx_socstil3 %in% c("leave of absence","other outside labor force") & unemp == 1 , unemp:= 0 ]

# Define spells and start dates
dst_main_head[ unemp == 1 , year_unemp := year]

dst_main_head[ HELTID_DELTID_KODE == 0 , wrk_stat := "self-employed" ]
dst_main_head[ HELTID_DELTID_KODE == 1 , wrk_stat := "ft_insured" ]
dst_main_head[ HELTID_DELTID_KODE == 2 , wrk_stat := "ft_uninsured" ]
dst_main_head[ HELTID_DELTID_KODE == 3 , wrk_stat := "ft_uninsured" ]
dst_main_head[ HELTID_DELTID_KODE == 4 , wrk_stat := "ft_insured" ]
dst_main_head[ HELTID_DELTID_KODE == 5 , wrk_stat := "pt_insured" ]
dst_main_head[ HELTID_DELTID_KODE == 6 , wrk_stat := "pt_uninsured (18-27h)" ]
dst_main_head[ HELTID_DELTID_KODE == 7 , wrk_stat := "pt_uninsured (9-18h)" ]
dst_main_head[ HELTID_DELTID_KODE == 8 , wrk_stat := "pt_uninsured (1-9h)" ]

dst_main_head[ , wrk_stat := factor(wrk_stat) ]
dst_main_head[ , wrk_stat := relevel(wrk_stat, "ft_insured") ]

dst_main_head[ HELTID_DELTID_KODE %in% c(1,5) , insur_stat2 := "insured" ]
dst_main_head[ HELTID_DELTID_KODE %in% c(2,3,4,6,7,8) , insur_stat2 := "uninsured" ]

dst_main_head[ , insur_stat2 := factor(insur_stat2) ]
dst_main_head[ , insur_stat2 := relevel(insur_stat2, "insured") ]

dst_main_head[ as.numeric(akasse) == 00 , insur_stat := "uninsured"]
dst_main_head[ as.numeric(akasse) != 00 , insur_stat := "insured"]

# Lag
setkey(dst_main_head_sub, pnr, year)
dst_main_head_sub <- dst_main_head_sub %>% group_by(pnr) %>%
  mutate(year_unemp_lag = lag(year_unemp, n=1)) %>% as.data.table()
setattr(dst_main_head_sub, "class", c("data.table","data.frame"))

dst_main_head_sub[ , year_unemp := NULL ]

# Merge back
dst_main_head_v2 <- merge(dst_main_head, dst_main_head_sub, by=c("pnr","year"), all.x=T)

# Get years prior to unemployment
dst_main_head_v2[ unemp == 1 & year_unemp > 0 & is.na(year_unemp_lag) , year_unemp_bg := year_unemp ]
dst_main_head_v2[ unemp == 0 & is.na(year_unemp) & year_unemp_lag > 0 , year_unemp_end := year_unemp_lag ]
dst_main_head_v2[ unemp == 1 & year_unemp == max_yr , year_unemp_end := max_yr ] # for right censored obs

# Subset
dst_main_head_v2_sub2 <- dst_main_head_v2[ ,.(pnr,year,year_unemp_end)]

# Lead
setkey(dst_main_head_v2_sub2, pnr, year)
dst_main_head_v2_sub2 <- dst_main_head_v2_sub2 %>% group_by(pnr) %>%
  mutate(year_unemp_end_lead = lead(year_unemp_end, n=1)) %>% as.data.table()
gc()

setattr(dst_main_head_v2_sub2, "class", c("data.table","data.frame"))

dst_main_head_v2_sub2[ , year_unemp_end := NULL ]

# Merge back
dst_main_head_v2.1 <- merge(dst_main_head_v2,dst_main_head_v2_sub2, by=c("pnr","year"), all.x=T)

dst_main_head_v2.1[ , year_unemp_end := NULL ]
setnames(dst_main_head_v2.1, "year_unemp_end_lead", "year_unemp_end")

dst_main_head_v2.1[ , year_unemp_bg := lapply( .SD, function (x) na.locf(x, na.rm=F) ), by="pnr", .SDcols="year_unemp_bg" ]
dst_main_head_v2.1[ unemp == 0 , year_unemp_bg := NA ]
gc()

dst_main_head_v2.1[ , year_unemp_end := lapply( .SD, function (x) na.locf(x, na.rm=F, fromLast = T) ), by="pnr", .SDcols="year_unemp_end" ]
dst_main_head_v2.1[ unemp == 0 , year_unemp_end := NA ]

dst_main_head_v2.1[  , duration_unemp := year_unemp_end - year_unemp_bg+1 ]
dst_main_head_v2.1[  , count_unemp := year_unemp - year_unemp_bg+1 ]

dst_main_head_v2.1[ unemp == 1 & year_unemp == max_yr , duration_unemp := count_unemp ] # for right censored obs

dst_main_head_v2.1[ unemp == 0 & is.na(year_unemp) , duration_unemp := NA ]

# Get year before unemployment spell
dst_main_head_v2.1_sub3 <- dst_main_head_v2.1[ ,.(pnr,year,year_unemp_bg,duration_unemp)]

setkey(dst_main_head_v2.1_sub3, pnr, year)
dst_main_head_v2.1_sub3 <- dst_main_head_v2.1_sub3 %>% group_by(pnr) %>%
  mutate(year_unemp_bg_lead = lead(year_unemp_bg, n=1)) %>% as.data.table()
gc()

dst_main_head_v2.1_sub3 <- dst_main_head_v2.1_sub3 %>% group_by(pnr) %>%
  mutate(duration_unemp_lead = lead(duration_unemp, n=1)) %>% as.data.table()
gc()

setattr(dst_main_head_v2.1_sub3, "class", c("data.table","data.frame"))

head(dst_main_head_v2.1_sub3)
dst_main_head_v2.1_sub3[ , c("year_unemp_bg","duration_unemp") := NULL ]

# Merge back
dst_main_head_v2.1 <- merge(dst_main_head_v2.1,dst_main_head_v2.1_sub3,by=c("pnr","year"), all.x=T)
gc()

dst_main_head_v2.1[ unemp == 0 & is.na(year_unemp) & year_unemp_bg_lead > 0 , count_unemp := 0 ]
dst_main_head_v2.1[ unemp == 0 & is.na(year_unemp) & year_unemp_bg_lead > 0 , duration_unemp := duration_unemp_lead ]

# Sum weeks per spell and divide by total time of spell
dst_main_head_v2.1[ !is.na(year_unemp_bg) , sum_wks_unemp := lapply( .SD, function (x) sum(x, na.rm=T) ),
                        by=c("pnr","year_unemp_bg"), .SDcols="arledgr_wks" ]

dst_main_head_v2.1[ , sum_wks_unemp_std := sum_wks_unemp / duration_unemp ]

# Select first year of unemployment
dst_main_head_v2.1[ , yr1st_unemp := lapply( .SD, function (x) min(x, na.rm=T) ), by="pnr", .SDcols="year_unemp_bg" ]
dst_main_head_v2.1[ , yr1st_unemp := lapply( .SD, function (x) na.locf(x, na.rm=F) ), by="pnr", .SDcols="yr1st_unemp" ]
dst_main_head_v2.1[ is.infinite(yr1st_unemp) , yr1st_unemp := NA ]

dst_main_head_v2.1[ , yr1st_unemp_tally := year - yr1st_unemp ]
gc()

# Subset to those who were employed two years before
pnr_unemp_y1 <- unique(dst_main_head_v2.1[ yr1st_unemp_tally %in% c(-1) & arledgr_wks <= 2 , pnr ])
pnr_unemp_y2 <- unique(dst_main_head_v2.1[ pnr %in% pnr_unemp_y1 & yr1st_unemp_tally %in% c(-2) & arledgr_wks <= 0 , pnr ])

# Subset
dst_main_head_v3 <- dst_main_head_v2.1[ pnr %in% pnr_unemp_y2 ]

# Lag data
dst_main_head_v3_lag <- dst_main_head_v3[ ,.(pnr,year,loenmv_hh_cpi10_adj,dispo_hh_v2)]
setkey(dst_main_head_v3_lag, pnr, year)

dst_main_head_v3_lag <- dst_main_head_v3_lag %>% group_by(pnr) %>%
  mutate(loenmv_hh_cpi10_adj_lag = lag(loenmv_hh_cpi10_adj, n=1)) %>% as.data.table()
gc()

dst_main_head_v3_lag <- dst_main_head_v3_lag %>% group_by(pnr) %>%
  mutate(dispo_hh_v2_lag = lag(dispo_hh_v2, n=1)) %>% as.data.table()
gc()

setattr(dst_main_head_v3_lag, "class", c("data.table","data.frame"))

dst_main_head_v3_lag[ , c("loenmv_hh_cpi10_adj","dispo_hh_v2") := NULL ]

# Merge
dst_main_head_v3 <- merge(dst_main_head_v3, dst_main_head_v3_lag, by=c("pnr","year"), all.x=T)

# Disposable income volatility
dst_main_head_v3[ dispo_hh_v2 >= 0 & dispo_hh_v2_lag >= 0 ,
               dispo_hh_v2_avg_crt_prv_y := rowSums(cbind(dispo_hh_v2,dispo_hh_v2_lag), na.rm=T)/2 ]

dst_main_head_v3[ dispo_hh_v2 >= 0 & dispo_hh_v2_lag >= 0 ,
               dispo_hh_v2_vol := 100*((dispo_hh_v2 - dispo_hh_v2_lag)/dispo_hh_v2_avg_crt_prv_y) ]

dst_main_head_v3[ dispo_hh_v2 == 0 & dispo_hh_v2_lag == 0, dispo_hh_v2_vol := 0 ]

summary(dst_main_head_v3[ ,.(dispo_hh_v2_vol,loenmv_hh_vol) ])

# Get data the year prior to unemployment
d_sub_unemp_inc <- dst_main_head_v3[ year == yr1st_unemp - 1 , .(pnr,dispo_hh_v2,indestpi_hh_cpi10_adj,bankgaeld_hh_cpi10_adj,wrk_stat,
                                                                 insur_stat,insur_stat2,dispo_hh_v2_vol,loenmv_hh_vol,loenmv_hh_cpi10_adj)]

names(d_sub_unemp_inc)[2:ncol(d_sub_unemp_inc)] <- paste(names(d_sub_unemp_inc)[2:ncol(d_sub_unemp_inc)],"yr_pr_unemp",sep="_")

dst_main_head_v3 <- merge(dst_main_head_v3, d_sub_unemp_inc, by="pnr", all.x=T)

# Average income three years prior
d_sub_unemp_avg_inc <- dst_main_head_v3[ yr1st_unemp_tally %in% c(-3,-2,-1) , .(pnr,year,dispo_hh_v2,indestpi_hh_cpi10_adj, loenmv_hh_cpi10_adj) ]
                                                                 
d_sub_unemp_avg_inc[ , dispo_hh_v2_avg_pre_unemp := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("pnr"), .SDcols= "dispo_hh_v2" ]
d_sub_unemp_avg_inc[ , indestpi_hh_cpi10_adj_avg_pre_unemp := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("pnr"), .SDcols= "indestpi_hh_cpi10_adj" ]
d_sub_unemp_avg_inc[ , loenmv_hh_cpi10_adj_avg_pre_unemp := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("pnr"), .SDcols= "loenmv_hh_cpi10_adj" ]

d_sub_unemp_avg_inc_v2 <- d_sub_unemp_avg_inc[ , .(pnr,dispo_hh_v2_avg_pre_unemp,indestpi_hh_cpi10_adj_avg_pre_unemp,loenmv_hh_cpi10_adj_avg_pre_unemp)]

setkey(d_sub_unemp_avg_inc_v2,pnr)

d_sub_unemp_avg_inc_v2_u <- unique(d_sub_unemp_avg_inc_v2)

dst_main_head_v3 <- merge(dst_main_head_v3, d_sub_unemp_avg_inc_v2_u, by="pnr", all.x=T)

# Quintiles
dst_main_head_v3[ dispo_hh_v2_yr_pr_unemp <  quantile(dispo_hh_v2_yr_pr_unemp, .2, na.rm=T ) , inc_grp_dispo_pr_unemp := "1" ]
dst_main_head_v3[ dispo_hh_v2_yr_pr_unemp >= quantile(dispo_hh_v2_yr_pr_unemp, .2, na.rm=T ) , inc_grp_dispo_pr_unemp := "2" ]
dst_main_head_v3[ dispo_hh_v2_yr_pr_unemp >= quantile(dispo_hh_v2_yr_pr_unemp, .4, na.rm=T ) , inc_grp_dispo_pr_unemp := "3" ]
dst_main_head_v3[ dispo_hh_v2_yr_pr_unemp >= quantile(dispo_hh_v2_yr_pr_unemp, .6, na.rm=T ) , inc_grp_dispo_pr_unemp := "4" ]
dst_main_head_v3[ dispo_hh_v2_yr_pr_unemp >= quantile(dispo_hh_v2_yr_pr_unemp, .8, na.rm=T ) , inc_grp_dispo_pr_unemp := "5" ]

# Create balanced panel
dst_main_head_v3_bal <- dst_main_head_v3[ ,.(pnr,year)]

setkey(dst_main_head_v3_bal, pnr, year)
dst_main_head_v3_bal <- dst_main_head_v3_bal %>% group_by(pnr) %>%
  mutate(year_lag = lag(year, n=1)) %>% as.data.table()
gc()

setattr(dst_main_head_v3_bal, "class", c("data.table","data.frame"))

# Merge
dst_main_head_v3 <- merge(dst_main_head_v3, dst_main_head_v3_bal, by=c("pnr","year"), all.x=T)
gc()

dst_main_head_v3[ , diff_yrs := year - year_lag ]

pnr_fp_to_delete <- dst_main_head_v3[ yr1st_unemp_tally %in% c(-5:5) & diff_yrs >= 2 , pnr ]

pnr_fp_to_delete_u <- unique(pnr_fp_to_delete)

# Balanced panel
dst_main_head_v4 <- dst_main_head_v3[ ! pnr %in% pnr_fp_to_delete_u ]
gc()

dst_main_head_v4[ yr1st_unemp_tally == 1 , pnr ]

rm(dst_main_head_v3,dst_main_head_v3_bal,pnr_fp_to_delete_u,pnr_fp_to_delete)
gc()


# Family status
dst_main_head_v4[ FAMILIE_TYPE %in% c(1,2,3,4,7,8) , fam_stat := "couple" ]
dst_main_head_v4[ FAMILIE_TYPE %in% c(5,9) , fam_stat := "single" ]

# Clean data
dst_main_head_v4[ year <= 1994 , oblgaeld_cpi10_adj := NA ]
dst_main_head_v4[ year <= 1994 , oblgaeld_hh_cpi10_adj := NA ]
dst_main_head_v4[ year <= 1994 , ihs_oblgaeld_hh_cpi10_adj := NA ]

### Insurance status
dst_main_head_v4[ as.numeric(akasse) == 00 , insur_stat := "uninsured"]
dst_main_head_v4[ as.numeric(akasse) != 00 , insur_stat := "insured"]

### Redo ownership code: group owners and those living in coops
dst_main_head_v4[ koejd > 0 , owner_koejd2 := 1 ]
dst_main_head_v4[ koejd == 0 , owner_koejd2 := 0 ]
dst_main_head_v4[ ejerforhold == 41 , owner_koejd2 := 1 ]
dst_main_head_v4[ oblgaeld_cpi10_adj > 0 & owner_koejd2 == 0 , owner_koejd2 := 1 ]

# Disposable income volatility
dst_main_head_v4[ dispo_v2 >= 0 & dispo_v2_lag >= 0 ,
               dispo_v2_avg_crt_prv_y := rowSums(cbind(dispo_v2,dispo_v2_lag), na.rm=T)/2 ]

dst_main_head_v4[ dispo_v2 >= 0 & dispo_v2_lag >= 0 ,
               dispo_v2_vol := 100*((dispo_v2 - dispo_v2_lag)/dispo_v2_avg_crt_prv_y) ]

dst_main_head_v4[ dispo_v2 == 0 & dispo_v2_lag == 0, dispo_v2_vol := 0 ]

# Labor income volatility
dst_main_head_v4[ loenmv_cpi10_adj >= 0 & loenmv_cpi10_adj_lag >= 0 ,
               loenmv_avg_crt_prv_y := rowSums(cbind(loenmv_cpi10_adj,loenmv_cpi10_adj_lag), na.rm=T)/2 ]

dst_main_head_v4[ loenmv_cpi10_adj >= 0 & loenmv_cpi10_adj_lag >= 0 ,
               loenmv_vol := 100*((loenmv_cpi10_adj - loenmv_cpi10_adj_lag)/loenmv_avg_crt_prv_y) ]

dst_main_head_v4[ loenmv_cpi10_adj == 0 & loenmv_cpi10_adj_lag == 0, loenmv_vol := 0 ]


# Event study indicators
dst_main_head_v4[ , yr1st_unemp_tally_fac := as.factor(yr1st_unemp_tally) ]
dst_main_head_v4[ , yr1st_unemp_tally_fac := relevel(yr1st_unemp_tally_fac, "-1") ]

dst_main_head_v4[ , inc_grp_dispo_pr_unemp := as.factor(as.character(inc_grp_dispo_pr_unemp)) ]
dst_main_head_v4[ , inc_grp_dispo_pr_unemp := relevel(inc_grp_dispo_pr_unemp, "3")]

dst_main_head_v4[ , postnummer_year := paste(postnummer,year,sep="_") ]

# Savings rate
dst_main_head_v4[ , save_ratio := NULL ]
dst_main_head_v4[ dispo_hh_v2 > 0 , save_ratio := indestpi_hh_cpi10_adj / (dispo_hh_v2/12) ]
dst_main_head_v4[ save_ratio < 0 , save_ratio := NA ] 
dst_main_head_v4[ save_ratio > quantile(save_ratio, .99, na.rm=T) , save_ratio := NA ] 

dst_main_head_v4[ !is.na(familie_id_num) , arblhumv_hh := lapply(.SD, function(x) sum(x,na.rm=T) ), by=c("familie_id","year"), .SDcols= "arblhumv" ]
dst_main_head_v4[ is.na(arblhumv) & arblhumv_hh == 0 , arblhumv_hh := NA ]

dst_main_head_v4[ , ihs_arblhumv_hh := log(arblhumv_hh + sqrt(1+arblhumv_hh^2)) ]

# For income decline: get data the year of unemployment
d_inc_vol <- dst_main_head_v4[ year == yr1st_unemp , .(pnr,dispo_hh_v2_vol,loenmv_hh_vol,dispo_v2_vol,loenmv_vol)]

names(d_inc_vol)[2:ncol(d_inc_vol)] <- paste(names(d_inc_vol)[2:ncol(d_inc_vol)],"yr_unemp",sep="_")

dst_main_head_v4 <- merge(dst_main_head_v4, d_inc_vol, by="pnr", all.x=T)

dst_main_head_v4[ , c("dispo_hh_v2_vol_yr_pr_unemp_grp") := NULL ]

dst_main_head_v4[ dispo_hh_v2_vol_yr_unemp < -50 , dispo_hh_v2_vol_yr_unemp_grp := -3 ]
dst_main_head_v4[ dispo_hh_v2_vol_yr_unemp >= -50 & dispo_hh_v2_vol_yr_unemp < -25 , dispo_hh_v2_vol_yr_unemp_grp := -2 ]
dst_main_head_v4[ dispo_hh_v2_vol_yr_unemp >= -25 & dispo_hh_v2_vol_yr_unemp < 0 , dispo_hh_v2_vol_yr_unemp_grp := -1 ]
dst_main_head_v4[ dispo_hh_v2_vol_yr_unemp >= 0  , dispo_hh_v2_vol_yr_unemp_grp := 0 ]

dst_main_head_v4[ , dispo_hh_v2_vol_yr_unemp_grp := factor(dispo_hh_v2_vol_yr_unemp_grp) ]
dst_main_head_v4[ , dispo_hh_v2_vol_yr_unemp_grp := relevel(dispo_hh_v2_vol_yr_unemp_grp, "0") ]

