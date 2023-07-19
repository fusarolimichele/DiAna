## Set up packages-------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","data.table","janitor","foreach","xml2","rvest","readxl")

## Download FAERS--------------------------------------------------------------
options(timeout=500)#increase if it times out because of low wifi power
FAERS_url <- "https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html"
pg <- read_html(FAERS_url)
links_to_QD <- html_attr(html_nodes(pg, "a"), "href")
links_to_QD <- links_to_QD[grepl(".zip",links_to_QD) & grepl("ascii",links_to_QD)]
dir.create("Raw_FAERS_QD")
for (file in links_to_QD){
  zip_name <- file.path("Raw_FAERS_QD", basename(file))
  download.file(file, zip_name)
  folder_name <- gsub(".zip","",zip_name)
  dir.create(folder_name)
  unzip(zip_name,exdir=folder_name)
  file.remove(zip_name)
}
file.rename("Raw_FAERS_QD//faers_ascii_2018q1/ascii/DEMO18Q1_new.txt",
            "Raw_FAERS_QD//faers_ascii_2018q1/ascii/DEMO18Q1.txt")

## Better storage--------------------------------------------------------------
faers_list <- list.files(path="Raw_FAERS_QD/",
                         recursive=T,pattern=".TXT",
                         ignore.case = T, full.names = T)
faers_list <- faers_list[!grepl("STAT|SIZE",faers_list)]
dir.create("Clean Data")
write.csv2(faers_list, "Clean Data/faers_list.csv")

## correct for missing newlines
correct_problematic_file <- function(file_path, old_line) {
  lines <- readLines(file(file_path,open = "r"))
  lines <- unlist(strsplit(gsub(old_line,
                                gsub("([0-9]+)$","SePaRaToR\\1",old_line),
                                lines,fixed=T),
                           "SePaRaToR"))
  writeLines(lines, con = file_path)
}

correct_problematic_file("Raw_FAERS_QD//aers_ascii_2011q2/ascii/DRUG11Q2.txt",
                         "$$$$$$7475791")
correct_problematic_file("Raw_FAERS_QD//aers_ascii_2011q3/ascii/DRUG11Q3.txt",
                         "$$$$$$7652730")
correct_problematic_file("Raw_FAERS_QD//aers_ascii_2011q4/ascii/DRUG11Q4.txt",
                         "021487$7941354")

## store to rds
store_to_rds <-  function(f){
  name <- gsub(".TXT",".rds",f, ignore.case = T)
  print (name)
  cn<- unlist(strsplit(readLines(file(f),n=1),split = "\\$"))
  x <- read.table(f,skip=1,sep="$", comment.char = "",quote="",row.names = NULL)
  colnames(x) <- cn
  saveRDS(x,file=name)
  closeAllConnections()
}

invisible(lapply(faers_list, store_to_rds))


## Merge quarters--------------------------------------------------------------

faers_list <- read.csv2("Clean Data/faers_list.csv")$x

unify_data <- function(files_list, namekey, column_subset,
                       duplicated_cols_x,duplicated_cols_y) {
  i <- 0
  foreach (f = files_list) %do%
    {name <- gsub(".TXT", ".rds", f, ignore.case = TRUE)
    print(name)
    x <- readRDS(name)
    x <- x[!is.na(names(x))]
    quart <- substr(name, nchar(name) - 7, nchar(name) - 4)
    x <- setDT(x)[, quarter := quart]
    names(x) <- namekey[names(x)]
    if (i > 0) {
      y <- rbindlist(list(y, x), fill = TRUE)
    } else {
      y <- x
    }
    i <- i + 1
    }
  if (sum(!is.na(duplicated_cols_x))>0){
    for (n in 1:length(duplicated_cols_x)){
      y[is.na(get(duplicated_cols_x[n])),
        (duplicated_cols_x[n]) := get(duplicated_cols_y[n])]
    }
  }
  removed_cols <- setdiff(colnames(y),column_subset)
  cols <- colnames(y)[colnames(y) %in% column_subset]
  y <- y[, ..cols]
  y[y == ""] <- NA
  y <- y %>% distinct()
  print(paste0("The following columns were lost in the cleaning: ",
               paste0(removed_cols,collapse = "; ")))
  return(y)
}

DEMO <- unify_data(files_list = faers_list[str_detect(faers_list,
                                                      regex("demo",
                                                            ignore_case = T))],
                   namekey = c(ISR="primaryid",CASE="caseid",
                               FOLL_SEQ="caseversion",I_F_COD="i_f_cod",
                               EVENT_DT="event_dt",MFR_DT="mfr_dt",
                               FDA_DT="fda_dt",REPT_COD="rept_cod",
                               MFR_NUM="mfr_num",MFR_SNDR="mfr_sndr",
                               AGE="age",AGE_COD="age_cod",GNDR_COD="sex",
                               E_SUB="e_sub",WT="wt",WT_COD="wt_cod",
                               REPT_DT="rept_dt", OCCP_COD="occp_cod",
                               TO_MFR="to_mfr",
                               REPORTER_COUNTRY="reporter_country",
                               quarter="quarter",i_f_code="i_f_cod"),
                   column_subset = c("primaryid","caseid","caseversion",
                                     "i_f_cod","sex","age","age_cod","age_grp",
                                     "wt","wt_cod","reporter_country",
                                     "occr_country","event_dt","rept_dt",
                                     "mfr_dt","init_fda_dt","fda_dt","rept_cod",
                                     "occp_cod","mfr_num","mfr_sndr","to_mfr",
                                     "e_sub","quarter","auth_num","lit_ref"),
                   duplicated_cols_x = c("rept_dt" , "sex"),
                   duplicated_cols_y = c(" rept_dt", "gndr_cod"))
saveRDS(DEMO,"Clean Data/DEMO.rds")
rm(DEMO)

DRUG <- unify_data(files_list = faers_list[str_detect(faers_list,regex("drug",ignore_case = T))],
                   namekey = c(ISR="primaryid",DRUG_SEQ="drug_seq",ROLE_COD="role_cod",
                     DRUGNAME="drugname",VAL_VBM="val_vbm",ROUTE="route",
                     DOSE_VBM="dose_vbm",DECHAL="dechal",
                     RECHAL="rechal",LOT_NUM="lot_num",NDA_NUM="nda_num",
                     EXP_DT="exp_dt"),
                   column_subset = c("primaryid","drug_seq","role_cod","drugname","prod_ai"),
                   duplicated_cols_x = NA,
                   duplicated_cols_y = NA)
saveRDS(DRUG,"Clean Data/DRUG.rds")
rm(DRUG)

DRUG_INFO <- unify_data(files_list = faers_list[str_detect(faers_list,regex("drug",ignore_case = T))],
                        namekey = c(ISR="primaryid",DRUG_SEQ="drug_seq",ROLE_COD="role_cod",
                          DRUGNAME="drugname",VAL_VBM="val_vbm",ROUTE="route",
                          DOSE_VBM="dose_vbm",DECHAL="dechal",
                          RECHAL="rechal",LOT_NUM="lot_num",NDA_NUM="nda_num",
                          EXP_DT="exp_dt"),
                        column_subset = c("primaryid","drug_seq","val_vbm","nda_num","lot_num",
                          "route","dose_form","dose_freq","exp_dt",
                          "dose_vbm","cum_dose_unit","cum_dose_chr","dose_amt",
                          "dose_unit","dechal","rechal"),
                        duplicated_cols_x = c("lot_num"),
                        duplicated_cols_y = c("lot_nbr"))
saveRDS(DRUG_INFO,"Clean Data/DRUG_INFO.rds")
rm(DRUG_INFO)

INDΙ <- unify_data(faers_list[str_detect(faers_list,regex("indi",ignore_case = T))],
                   c(ISR="primaryid",DRUG_SEQ="drug_seq",
                     indi_drug_seq="drug_seq",INDI_PT="indi_pt"),
                   c("primaryid","drug_seq","indi_pt"),
                   NA,
                   NA)
INDΙ <- INDΙ[!is.na(indi_pt)]
saveRDS(INDΙ,"Clean Data/INDI.rds")
rm(INDΙ)

OUTC <- unify_data(faers_list[str_detect(faers_list,regex("outc",ignore_case = T))],
                   c(ISR="primaryid",OUTC_COD="outc_cod"),
                   c("primaryid","outc_cod"),
                   c("outc_cod"),
                   c("outc_code"))
OUTC <- OUTC[!is.na(outc_cod)]
saveRDS(OUTC,"Clean Data/OUTC.rds")
rm(OUTC)

REAC <- unify_data(faers_list[str_detect(faers_list,regex("reac",ignore_case = T))],
                   c(ISR="primaryid", PT="pt"),
                   c("primaryid","pt","drug_rec_act"),
                   NA,
                   NA)
REAC <- REAC[!is.na(pt)]
saveRDS(REAC,"Clean Data/REAC.rds")
rm(REAC)

RPSR <- unify_data(faers_list[str_detect(faers_list,regex("rpsr",ignore_case = T))],
                   c(ISR="primaryid",RPSR_COD="rpsr_cod"),
                   c("primaryid", "rpsr_cod"),
                   NA,
                   NA)
saveRDS(RPSR,"Clean Data/RPSR.rds")
rm(RPSR)

THER <- unify_data(faers_list[str_detect(faers_list,regex("ther",ignore_case = T))],
                   c(ISR="primaryid",dsg_drug_seq="drug_seq",
                     DRUG_SEQ="drug_seq",START_DT="start_dt",END_DT="end_dt",
                     DUR="dur",DUR_COD="dur_cod"),
                   c("primaryid","drug_seq","start_dt",
                     "end_dt","dur","dur_cod"),
                   NA,
                   NA)
saveRDS(THER,"Clean Data/THER.rds")
rm(THER)

##MedDRA standardization-------------------------------------------------------
# Importing MedDRA
soc <- read.csv2("External Sources/Dictionaries/MedDRA/MedAscii/soc.asc",
                 sep = "$", header=F) %>% select(1:3)
colnames(soc) <- c("soc_cod","soc","def")
soc_hlgt <- read.csv2("External Sources/Dictionaries/MedDRA/MedAscii/soc_hlgt.asc",
                      sep = "$", header=F) %>% select(1:2)
colnames(soc_hlgt) <- c("soc_cod","hlgt_cod")
hlgt <- read.csv2("External Sources/Dictionaries/MedDRA/MedAscii/hlgt.asc",
                  sep = "$", header=F) %>% select(1:2)
colnames(hlgt) <- c("hlgt_cod","hlgt")
hlgt_hlt <- read.csv2("External Sources/Dictionaries/MedDRA/MedAscii/hlgt_hlt.asc",
                      sep = "$", header=F) %>% select(1:2)
colnames(hlgt_hlt) <- c("hlgt_cod","hlt_cod")
hlt <- read.csv2("External Sources/Dictionaries/MedDRA/MedAscii/hlt.asc",
                 sep = "$", header=F) %>% select(1:2)
colnames(hlt) <- c("hlt_cod","hlt")
hlt_pt <- read.csv2("External Sources/Dictionaries/MedDRA/MedAscii/hlt_pt.asc",
                    sep = "$", header=F)  %>% select(1:2)
colnames(hlt_pt) <- c("hlt_cod","pt_cod")
pts <- read.csv2("External Sources/Dictionaries/MedDRA/MedAscii/pt.asc",
                 sep = "$", header=F) %>% select(1:2,4)
colnames(pts) <- c("pt_cod","pt","primary_soc_cod")
llt <- read.csv2("External Sources/Dictionaries/MedDRA/MedAscii/llt.asc",
                 sep = "$", header=F) %>% select(1:3)
colnames(llt) <- c("llt_cod","llt","pt_cod")
# Merge the data
meddra <- setDT(merge(merge(merge(merge(merge(merge(merge(
  soc, soc_hlgt,all = TRUE),hlgt, all = TRUE),hlgt_hlt,all = TRUE),
  hlt, all = TRUE), hlt_pt, all = TRUE), pts, all = TRUE), llt,all=TRUE))
# Convert to lowercase
meddra[,(colnames(meddra)):=lapply(.SD, tolower),]
# Write the data to CSV files
write.csv2(distinct(meddra[,.(def,soc, hlgt,hlt,pt,llt)]),
           "External Sources/Dictionaries/MedDRA/meddra.csv")
write.csv2(distinct(meddra[soc_cod==primary_soc_cod][,.(def,soc, hlgt,hlt,pt)]),
           "External Sources/Dictionaries/MedDRA/meddra_primary.csv")


# Read PT file and extract unique lowercase PT values
pt_list <- unique(tolower(trimws(
  setDT(read.csv2("External Sources/Dictionaries/MedDRA/meddra.csv"))$pt)))
manual_fix_file <- "External Sources/Manual_fix/pt_fixed.csv"
standardize_PT <- function(data_file, pt_variable) {
  # Read data file
  data <- setDT(readRDS(data_file))
  
  # Extract PTs from data and calculate frequencies
  pt_freq <- data[, .(pt = tolower(trimws(get(pt_variable))))][
    !is.na(pt)][, .N, by = "pt"][order(-N)]
  
  # Check if PTs are standardized or not
  pt_freq[, standard_pt := ifelse(pt %in% pt_list, pt, NA)]
  pt_freq[, freq := round(N/sum(N) * 100, 2)]
  
  # Get unstandardized PTs
  not_pts <- pt_freq[is.na(standard_pt)][, .(pt, N, freq)]
  print(paste0("The portion of non standardized PTs at the beginning is ",
               round(sum(not_pts$N)*100/nrow(data[!is.na(get(pt_variable))]),3)))
  
  # Try to translate unstandardized PTs through LLTs
  llts <- left_join(not_pts, setDT(read.csv2("External Sources/Dictionaries/MedDRA/meddra.csv"))[
    , .(standard_pt = pt, pt = llt)])
  not_llts <- llts[is.na(standard_pt)] %>% select(-standard_pt)
  
  # If still untraslated, use manual integration
  pt_fixed <- setDT(read.csv2(manual_fix_file))[, .(pt, standard_pt)]
  manual <- left_join(not_llts, pt_fixed)[is.na(standard_pt),][, .(pt, standard_pt)]
  
  # Combine PTs from LLTs, manual integration, and already standardized PTs
  pt_fixed <- distinct(rbindlist(list(pt_fixed, manual, llts[!is.na(standard_pt), .(pt, standard_pt)])))
  unstandardized_pts <- pt_fixed[is.na(standard_pt)]
  # Write updated manual fix file
  write.csv2(pt_fixed, manual_fix_file)
  print(paste0(nrow(unstandardized_pts),
               " pts are not standardized using LLTs or previously proposed manual fix: ",
               paste0(unstandardized_pts$pt,collapse = "; "),
               ". Consider updating the pt_fixed.csv file."))
  pt_fixed <- setDT(read.csv2(manual_fix_file))[, .(pt_temp=pt, standard_pt)]
  # Update PTs in the data file
  data <- pt_fixed[data[, pt_temp := tolower(trimws(get(pt_variable)))], on = "pt_temp"][
    ,pt_temp := ifelse(is.na(standard_pt), pt_temp, standard_pt)] %>% select(-standard_pt)
  data <- data %>% select(-all_of(pt_variable))
  # Calculate the portion of standardized PTs
  standardized_percentage <- round(nrow(data[pt_temp %in% pt_list]) * 100 / nrow(data[!is.na(pt_temp)]), 3)
  print(paste0("The portion of standardized PTs at the end is ",
               standardized_percentage))
  setnames(data,old="pt_temp",new=pt_variable)
  
  # Return the standardized data and standardized percentage
  return(data)
}

Reac <- standardize_PT("Clean Data/Reac.rds","pt")
#consider updating the pt_fixed file
saveRDS(Reac,"Clean Data/Reac.rds")
Reac <- standardize_PT("Clean Data/Reac.rds","drug_rec_act")
#consider updating the pt_fixed file
saveRDS(Reac,"Clean Data/Reac.rds")
Indi <- standardize_PT("Clean Data/Indi.rds","indi_pt")
#consider updating the pt_fixed file
saveRDS(Indi,"Clean Data/Indi.rds")

##Drug standardization-------------------------------------------------------
Drug <- setDT(readRDS("Clean Data/Drug.rds"))
old_DIANA_dictionary <- setDT(read.csv2("External Sources/Dictionaries/DiAna_dictionary/drugnames_standardized.csv"))[
  ,.(drugname,Substance,Checked,OpenRefine)][Substance!="na"][!is.na(Substance)]

Drug <-Drug[,drugname:=gsub("\\s+"," ",trimws(gsub("\\.$","",trimws(tolower(drugname)))))]
Drug <- Drug[,drugname:=trimws(gsub("[^)[:^punct:]]+$","",drugname,perl=TRUE))]
Drug <- Drug[,drugname:=trimws(gsub("^[^([:^punct:]]+","",drugname,perl=TRUE))]
Drug <- Drug[,drugname:=trimws(gsub("[^)[:^punct:]]+$","",drugname,perl=TRUE))]
Drug <- Drug[,drugname:=trimws(gsub("^[^([:^punct:]]+","",drugname,perl=TRUE))]
Drug <- Drug[,drugname:=gsub("\\( ","\\(",drugname)]
Drug <- Drug[,drugname:=gsub(" \\)","\\)",drugname)]

temp <- Drug[,.N,by="drugname"]
temp <- temp[,freq:=100*N/sum(temp$N, na.rm = T)][order(-N)]
temp <- old_DIANA_dictionary[temp,on="drugname"]
write.csv2(temp, "External Sources/Dictionaries/DIANA_dictionary/drugnames_standardized.csv")
# consider extending the standardization, inside the drugnames_standardized,
# before resetting the DIANA_dictionary.
# Our scope is to at least grant a translation for terms occurring more than
# 200 times. For drugs of interest, particularly, the newly marketed ones,
# a more extended translation may be needed for the best case retrieval.


DIANA_dictionary <- setDT(read.csv2("External Sources/Dictionaries/DiAna_dictionary/drugnames_standardized.csv"))[
  ,.(drugname,Substance)][Substance!="na"][!is.na(Substance)]

Drug <- DIANA_dictionary[Drug,on="drugname"]
Drug_multi <- Drug[grepl(";",Substance)] 
cn <- c("primaryid","drug_seq","Substance","role_cod","drugname","prod_ai")

Drug_multi <- Drug_multi[, strsplit(Substance, ";", fixed=TRUE), by = cn]
Drug_multi <- Drug_multi[,.(primaryid,drug_seq,Substance=V1,role_cod,drugname,prod_ai)]

Drug_one <- Drug[!grepl(";",Substance)] 
Drug_one <- Drug_one[,.(primaryid,drug_seq,Substance,role_cod,drugname,prod_ai)]

Drug <- rbindlist(list(Drug_multi,Drug_one))
Drug <- Drug[,trial:=grepl(", trial",Substance)]
nrow(Drug[trial==TRUE])
Drug <- Drug[,Substance:=gsub(", trial","",Substance)]
Drug$drugname <- as.factor(Drug$drugname)
Drug$prod_ai <- as.factor(Drug$prod_ai)
Drug$Substance <- as.factor(Drug$Substance)
saveRDS(Drug,"Clean Data/Drug.rds")

## Sex standardization-----------------------------------------------------
Demo <- setDT(readRDS("Clean Data/Demo.rds"))
Demo[,.N,by="sex"][order(-N)]
Demo[!sex %in% c("F","M")]$sex<- NA

## Age standardization-----------------------------------------------------
Demo[,.N,by="age_cod"][order(-N)]

Demo[,age_corrector:=ifelse(age_cod=="DEC",3650,
                            ifelse(age_cod=="YR"|is.na(age_cod),365,
                                   ifelse(age_cod=="MON",30.41667,
                                          ifelse(age_cod=="WK",7,
                                                 ifelse(age_cod=="DY",1,
                                                        ifelse(age_cod=="HR",0.04166667,
                                                               ifelse(age_cod=="SEC",
                                                                      1.157407e-05,NA)))))))]

Demo <- Demo[,age_in_days:=round(abs(as.numeric(age))*age_corrector)]
summary(Demo$age_in_days)
Demo[,age_in_days:= ifelse(age_in_days<=122*365,age_in_days,
                           ifelse (age_cod=="DEC",age_in_days/age_corrector,
                                   NA))]#plausible compilation error
Demo <- Demo[,age_in_years:=round(age_in_days/365)]
hist(Demo$age_in_years, xlab = "Age (years old)")
summary(Demo$age_in_years)
Demo <- Demo %>% select(-age_corrector,-age,-age_cod)

temp <- Demo[!is.na(age_grp) & !is.na(age_in_years),.N,by=c("age_in_years","age_grp")]
temp[age_grp%in%c("N","I")]$age_grp <- "N&I"
temp$age_grp <- factor(temp$age_grp,levels = c("N&I","C","T","A","E"))

Age_thresholds <- tribble(
  ~age_group , ~age_threshold,
  #----------|----------------|
  "C"        , 2,
  "T"        , 12,
  "A"        , 18,
  "E"        , 65
)

ggplot(data=temp) +
  geom_point(mapping=aes(x=age_in_years,y=N,group=age_grp,fill=age_grp,color=age_grp), show.legend = FALSE) +
  geom_density(mapping=aes(x=age_in_years,y=N,group=age_grp,fill=age_grp,color=age_grp),stat="identity",alpha=0.5) +
  geom_point(data = Age_thresholds,mapping=aes(x=age_threshold,y=0),show.legend = FALSE)+
  geom_text(data = Age_thresholds,mapping=aes(x=age_threshold,y=0,label=age_threshold), nudge_y = -500,angle=45,size=3,show.legend = FALSE)+
  xlab("Age (yr)") +
  ylab("Freq") +
  theme()+
  guides(color="none")+
  scale_fill_discrete(name = "Age Group", labels = c("Neonate&Infant",
                                                     "Child","Teenager",
                                                     "Adult","Elderly"))
Demo$age_grp_st <- as.character(NA)
Demo[!is.na(age_in_years)]$age_grp_st <- "E"
Demo[age_in_years < 65]$age_grp_st <- "A"
Demo[age_in_years < 18]$age_grp_st <- "T"
Demo[age_in_years < 12]$age_grp_st <- "C"
Demo[age_in_years < 2]$age_grp_st <- "I"
Demo[age_in_days <28]$age_grp_st <- "N"
Demo[,.N,by="age_grp_st"][order(-N)][,perc:=round(N/sum(N)*100),]
Demo <- Demo %>% select(-age_grp) %>% rename (age_grp=age_grp_st)
saveRDS(Demo,"Clean Data/DEMO.rds")

## Weight standardization---------------------------------------------------
Demo[,.N,by="wt_cod"][order(-N)]
Demo$wt_corrector  <-  as.numeric(NA)
Demo[wt_cod=="LBS"]$wt_corrector   <-  0.453592
Demo[wt_cod=="KG"]$wt_corrector  <-  1
Demo[wt_cod=="GMS"]$wt_corrector  <-  0.001
Demo[is.na(wt_cod)]$wt_corrector  <-  1

Demo <- Demo[,wt_in_kgs:=round(abs(as.numeric(wt))*wt_corrector)]
Demo[wt_in_kgs>635]$wt_in_kgs <- NA

temp <- Demo[,.N,by="wt_in_kgs"][order(-N)]
ggplot(data=temp) +
  geom_col(aes(x=wt_in_kgs,y=N))
Demo <- Demo %>% select(-wt_corrector,-wt,-wt_cod)
saveRDS(Demo,"Clean Data/DEMO.rds")

## Country standardization--------------------------------------------
Countries <- setDT(read_delim("External Sources/Manual_fix/countries.csv",";", escape_double = FALSE, trim_ws = TRUE))
Countries[is.na(Countries$country)]$country <- "NA" #to avoid losing Namibia
Countries[union(Demo$occr_country,Demo$reporter_country),on="country"][is.na(Country_Name)] #check if new not translated
Demo <- Countries[,.(country,occr_country=Country_Name)][Demo[,country:=occr_country],on="country"] 
Demo <- Countries[,.(country,reporter_country=Country_Name)][Demo[,country:=reporter_country],on="country"] 
Demo[,.N,by="country"][order(-N)]
Demo <- Demo %>% select(-country) %>% droplevels()
saveRDS(Demo,"Clean Data/DEMO.rds")

## Occupation standardization------------------------------------------------
Demo[,.N,by="occp_cod"][order(-N)]
Demo[!occp_cod%in%c("MD","CN","OT","PH","HP","LW","RN")]$occp_cod <- NA
Demo <- Demo %>% droplevels()
saveRDS(Demo,"Clean Data/DEMO.rds")
rm(list=ls())
