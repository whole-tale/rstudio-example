
ReadME for Replication Files for A Social Policy Theory of Everyday Borrowing. On the Role of Welfare States and Credit Regimes

April 6, 2021

Andreas Wiedemann
Princeton University
awiedemann@princeton.edu

Computing environment
	Operating system: 	macOS Big Sur 10.16 (x86_64-apple-darwin17.0 (64-bit))
	Statistical software: 	R version 4.0.3 (2020-10-10)

R package dependencies: 

	ggplot2_3.3.2
	reshape2_1.4.4
	lmtest_0.9-38
	sandwich_3.0-0 
	MASS_7.3-53
	stringr_1.4.0
	stargazer_5.2.2
	effects_4.2-0
	data.table_1.13.2
	plm_2.2-5
	psych_2.0.9
	BBmisc_1.11
	ggrepel_0.8.2
	lemon_0.4.5
	sjPlot_2.8.6
	msm_1.6.8
	lme4_1.1-25
	gridExtra_2.3
	plyr_1.8.6
	glmmTMB_1.0.2.1


Note: The Danish register data used to produce Figure 7 (Danish plot), Figure 8, Figure 9, and SI Figure C.2 as well as SI Table C.5 are confidential and therefore not publicly available. The data are stored at Statistics Denmark and can only be accessed by researchers affiliated with a Danish research institution using Statistics Denmark remote server system. Replication data can therefore not be made available. Information about the Danish population registries and how to access them can be found at https://www.dst.dk/en/TilSalg/Forskningsservice. 
### List of files:

Folder hierarchy
	Data (contains all data files)
	Code (contains all R scripts)
	Output (holder for all tables and figures produced by the code, as indicated in the note.txt file)
	

### DOCUMENTATION

readme.txt -- This file, which lists all files and compute environment dependencies necessaryto verify the results from this manuscript

codebook.pdf -- combined codebook for all data files


### DATA FILES

credit_regime_raw_data.RData: This file contains variables to construct credit permissiveness scores. 
credit.RData: This file contains variables to construct SI Figure B.3. based on data from Beck (2012).
cty_covars.RData: This file contains variables used together with credit_regime_raw_data.RData for country-level analyses. 
lits_w2_data.RData: This file contains Life in Transition Survey II data used for the analysis for country-level analyses. 
oecd_int_rates.RData: This file contains short-term interest data from the OECD used for SI Figure B.6
sipp_data.RData: This file contains SIPP data used for the US analysis. 


The Danish registry data used in this article (and not included in replication file) appear in the codebook. Variables from the following registries were used: 
	BBR (1981-2012)
	BEF (1980-2013)
	FAM (1980-2013)
	IDAN (1980-2012)
	INDH (1980-2012)
	INDK (1980-2012)
	RAS (1980-2012)
	UDDA (1980-2015)

### CODE

Use the four scripts in the folder "Code" to reproduce all other figures and tables in the main text and appendix. Set your working directory to the "replication_files" folder. The scripts include all relevant packages, including installation commands, to run the code. The scripts draw on the datasets in the "Data" folder and save figures and tables in the "Output" folder as follows:
1_credit_permissiveness.R	: This script produces the following figures and tables:
	Figure 3
	Figure 4
	Figure 6
	SI Figure B.1
	SI Figure B.2
	SI Figure B.4
	SI Figure B.5
	SI Figure B.6
	SI Table C.2
	SI Table C.3
	SI Table C.4

2_lits.R	: This script produces the following figures and tables:
	Figure 5
	SI Figure C.1	SI Table C.13_sipp.R: This script produces the following figures and tables:	Figure 7 (USA part)	Figure 10
	SI Table C.64_credit_flows.R: This script produces the following figure:
	SI Figure B.35_dst_prep_data.R: This script shows how to prepare the registry data for analysis once all the relevant variables have been extracted from the raw registry datasets [data restricted]

6_dst_analysis.R: This script produces the following figures and tables [data is restricted and not included in the replication files]
	Figure 7 (Denmark part) 
	Figure 8a
	Figure 8b
	Figure 9
	SI Figure C.2a 
	SI Figure C.2b
	SI Table C.5


