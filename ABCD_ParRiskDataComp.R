require(tidyverse)
require(data.table)
require(stringi)
require(dplyr)

#set the working directory from which the files will be read from
setwd("C:/Users/mmatt/Desktop/Projects/psychopathology-risk/github/data/ABCDStudyNDA")

#create a list of the files from your target directory
mat <- matrix(NA,1,1)
file_list <- list.files(path="C:/Users/mmatt/Desktop/Projects/psychopathology-risk/github/data/ABCDStudyNDA",pattern=".txt")
struct_list <- rep(list(mat),length(file_list))

#initialize empty list
data_list <- vector(mode = "list", length = length(file_list))

#Add element names of each structure to dataframe in list
for (i in 1:length(file_list)) {
  struct_list[[i]] <- colnames(fread(file_list[i],header=TRUE,nrow=0))
}


###IDENTIFY ELEMENTS OF INTEREST###
#Common to all studies - DO NOT CHANGE THIS ONE OR REPEAT THESE ELSEWHERE
common = c('subjectkey', 'interview_age', 'interview_date', 'eventname', 'sex',
           'rel_family_id')

#demographic
demo = c("race_ethnicity", "demo_comb_income_v2", "demo_prnt_ed_v2", "demo_prtnr_ed_v2","demo_prim")
puberty = c('pds_p_ss_female_category_2', 'pds_p_ss_male_category_2')

#Subcortical volumes 
subcort = c('smri_vol_scs_aal', 'smri_vol_scs_aar', 'smri_vol_scs_amygdalalh', 'smri_vol_scs_amygdalarh', 
           'smri_vol_scs_caudatelh', 'smri_vol_scs_caudaterh', 'smri_vol_scs_hpuslh', 'smri_vol_scs_hpusrh', 
           'smri_vol_scs_pallidumlh', 'smri_vol_scs_pallidumrh', 'smri_vol_scs_putamenlh', 'smri_vol_scs_putamenrh',
           'smri_vol_scs_tplh', 'smri_vol_scs_tprh', 'smri_vol_scs_intracranialv', 'smri_vol_scs_subcorticalgv')

#parental psychopathology 
maternal = c('famhx_ss_moth_prob_dprs_p','famhx_ss_moth_prob_alc_p', 'famhx_ss_moth_prob_dg_p', 
            'famhx_ss_moth_prob_ma_p', 'famhx_ss_moth_prob_nrv_p')
paternal = c('famhx_ss_fath_prob_dprs_p','famhx_ss_fath_prob_alc_p', 'famhx_ss_fath_prob_dg_p', 
            'famhx_ss_fath_prob_ma_p', 'famhx_ss_fath_prob_nrv_p')
momdad = c('famhx_ss_momdad_dprs_p','famhx_ss_momdad_alc_p', 'famhx_ss_momdad_dg_p', 
          'famhx_ss_momdad_ma_p', 'famhx_ss_momdad_nrv_p')

#exclusion criteria
exclude = c("iqc_t1_ok_ser", "fsqc_qc", "mrif_score", "famhx_ss_momdad_vs_p")
ksads = c("ksads_1_843_p", "ksads_1_845_p", "ksads_1_844_p", "ksads_1_840_p", "ksads_1_841_p", 
          "ksads_1_842_p", "ksads_2_837_p", "ksads_2_835_p", "ksads_2_836_p", "ksads_2_831_p", 
           "ksads_2_832_p", "ksads_2_830_p", "ksads_2_833_p", "ksads_2_834_p", "ksads_3_848_p", 
           "ksads_4_851_p", "ksads_4_852_p", "ksads_5_857_p", "ksads_5_858_p", "ksads_6_859_p", 
           "ksads_6_860_p", "ksads_7_861_p", "ksads_7_862_p", "ksads_8_864_p", "ksads_8_863_p", 
           "ksads_10_869_p", "ksads_10_870_p", "ksads_11_917_p", "ksads_11_918_p", "ksads_13_939_p",
           "ksads_13_938_p", "ksads_13_929_p", "ksads_13_934_p", "ksads_13_933_p", "ksads_13_932_p",
           "ksads_13_931_p", "ksads_13_930_p", "ksads_13_936_p", "ksads_13_935_p", "ksads_13_937_p",
           "ksads_13_940_p", "ksads_14_855_p", "ksads_14_853_p", "ksads_14_854_p", "ksads_15_901_p",
           "ksads_15_902_p", "ksads_16_900_p", "ksads_16_897_p", "ksads_16_899_p", "ksads_16_898_p",
           "ksads_19_891_p", "ksads_19_892_p", "ksads_20_874_p", "ksads_20_883_p", "ksads_20_872_p",
           "ksads_20_881_p", "ksads_20_889_p", "ksads_20_890_p", "ksads_20_887_p", "ksads_20_878_p",
           "ksads_20_877_p", "ksads_20_886_p", "ksads_20_875_p", "ksads_20_884_p", "ksads_20_876_p",
           "ksads_20_885_p", "ksads_20_879_p", "ksads_20_888_p", "ksads_20_873_p", "ksads_20_882_p",
           "ksads_20_880_p", "ksads_20_871_p", "ksads_21_921_p", "ksads_21_922_p")

#concatenate elements of interest
elem_int = c(subcort, maternal, paternal, momdad, demo, 
             puberty, exclude, ksads)


###Find structures that contain element###

#This loops through each element of interst, and check if it exists in any of the structures. 
#It stops on the first structure that it exists in, and then moves to the next element. 

struct_log <- matrix(NA,length(elem_int),length(file_list))

for (iE in 1:length(elem_int)) { #For each element...
  for (iS in 1:length(struct_list)) { #For each structure... 
    if (any(grepl(paste0('\\<',elem_int[iE],'\\>'),struct_list[[iS]])) == TRUE){#Does the element exist anywhere in the structure
      struct_log[iE,iS] <- 1
        break } #If yes, mark 1 and move on to next element
    else {
        struct_log[iE,iS] <- 0 } #If no, mark 0 and check next structure
  }
}


#Then, take the max to identify which structures contain element of interest (ie, they have a 1)
struct_log[is.na(struct_log)] <- 0 #Make NA = 0
struct_ind <- as.logical(apply(struct_log,2,max,na.rm = TRUE))
struct_inc <- file_list[struct_ind]


#Create list of just structures of interest + column names
struct_int_list <- rep(list(mat),length(struct_inc))
for (i in 1:length(struct_inc)) {
  struct_int_list[[i]] <- colnames(fread(struct_inc[i],header=TRUE,nrow=0))
}

#Add participant ID to elem_int list
elem_int_id <- c('subjectkey','eventname',elem_int)

#Loop through structures of interest 
#identify which elements of interest they have
#If you have errors/warnings here, you probably need to check the element names you first specified
struct_subset <- rep(list(mat),length(struct_inc))
for (i in 1:length(struct_inc)){
  elem_match <- lapply(elem_int_id,grep,struct_int_list[[i]], value = TRUE)
  elem_match_cond <- sapply(elem_match, paste, collapse = " ")
  elem_match_cond_cond <- elem_match_cond[elem_match_cond != ""]
  struct_subset[[i]] <- fread(struct_inc[i], select = elem_match_cond_cond)
}


#Merge all structures
merged_df <- Reduce(function(...) merge(..., by=c('subjectkey','eventname'), 
                                        all=TRUE), struct_subset) 
#merge common variables
common_elems <- read.delim2("acspsw03.txt", header = TRUE)
common_elems_sub <- common_elems[common]
merged_df <- merge(merged_df,common_elems_sub,by = c('subjectkey','eventname'),all=TRUE)
#merge ACS weighting
acsw <- common_elems[c('subjectkey','eventname','acs_raked_propensity_score')]
merged_df <- merge(merged_df,acsw,by = c('subjectkey','eventname'),all=TRUE)
#merge site
site_id <- read.delim2("abcd_lt01.txt", header = TRUE)
site_id <- site_id[c('subjectkey','eventname','site_id_l')]
merged_df <- merge(merged_df,site_id,by = c('subjectkey','eventname'))

###YOU NOW HAVE YOUR VARIABLES OF INTEREST IN merged_df###

#OPTIONAL: Make instances of 555,777,888,999 = NA
merged_df[merged_df == 555] <- NA
merged_df[merged_df == 777] <- NA
merged_df[merged_df == 888] <- NA
merged_df[merged_df == 999] <- NA

#OPTIONAL: Subset by timepoint of interest. Change to whatever you're interest in. 
merged_df_t1 <- subset(merged_df,eventname == 'baseline_year_1_arm_1')

#Optional: Apply Exclusion Criteria
#Can count changes in observations to determine # excluded for methods
merged_df_t1_incl <- subset(merged_df_t1, iqc_t1_ok_ser > 0)
merged_df_t1_incl <- subset(merged_df_t1_incl, fsqc_qc > 0)
merged_df_t1_incl <- subset(merged_df_t1_incl, mrif_score > 0)
merged_df_t1_incl <- subset(merged_df_t1_incl, famhx_ss_momdad_vs_p == 0)
#Excluding KSADS dx..
#ksads_cols <- merged_df_t1_incl[ , grep("ksad", colnames(merged_df_t1_incl))]#Find colnames that contain 'ksad'
merged_df_t1_incl[, "ksads_max"] <- apply(merged_df_t1_incl[,19:92], 1, max,na.rm=TRUE) #If any dx, max = 1
merged_df_t1_incl <- subset(merged_df_t1_incl, ksads_max == 0) #Exclude any dx


#OPTIONAL: Clean up df by removing exclusion variables after using them
clean_df <- merged_df_t1_incl
clean_df[,exclude] <- list(NULL)
clean_df$ksads_max <- list(NULL)
clean_df[,ksads] <- list(NULL)

#OPTIONAL: Remove missing data
#Remove if missing ICV
clean_df <- drop_na(clean_df,smri_vol_scs_intracranialv) #1 individual
#Remove if missing 4+/5 parental histories of interest
famhx_cases <- clean_df[,c('subjectkey','famhx_ss_momdad_dprs_p','famhx_ss_momdad_alc_p',
                                                  'famhx_ss_momdad_dg_p','famhx_ss_momdad_ma_p',
                                                  'famhx_ss_momdad_nrv_p')]
famhx_cases[famhx_cases == ""] <- NA 
famhx_NA <- apply(famhx_cases, MARGIN = 1, function(x) sum(is.na(x)))
table(famhx_NA)
#get logical if sum NA across famhx vars < 4
famhx_NA_g4 <- famhx_NA < 4
#exclude 
clean_df <- clean_df[famhx_NA_g4,]

#Data cleaned and ready to use!!
write.csv(clean_df,'C:/Users/mmatt/Desktop/Projects/psychopathology-risk/PsyRisk/PsyRiskLME/ABCD_ParRiskData.csv')
