## Set up packages-------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","data.table","janitor","foreach","xml2","rvest","readxl","distr")

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

Reac <- standardize_PT("Clean Data/REAC.rds","pt")
#consider updating the pt_fixed file
saveRDS(Reac,"Clean Data/REAC.rds")
Reac <- standardize_PT("Clean Data/REAC.rds","drug_rec_act")
#consider updating the pt_fixed file
saveRDS(Reac,"Clean Data/REAC.rds")
Indi <- standardize_PT("Clean Data/INDI.rds","indi_pt")
#consider updating the pt_fixed file
saveRDS(Indi,"Clean Data/INDI.rds")

##Drug standardization-------------------------------------------------------
Drug <- setDT(readRDS("Clean Data/DRUG.rds"))
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
saveRDS(Drug,"Clean Data/DRUG.rds")

## Sex standardization-----------------------------------------------------
Demo <- setDT(readRDS("Clean Data/DEMO.rds"))
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
Demo <- Countries[,.(country,occr_country=Country_Name)][Demo[,country:=occr_country] %>% select(-occr_country),on="country"] 
Demo <- Countries[,.(country,reporter_country=Country_Name)][Demo[,country:=reporter_country]%>% select(-reporter_country),on="country"] 
Demo <- Demo %>% select(-country) %>% droplevels()
saveRDS(Demo,"Clean Data/DEMO.rds")

## Occupation standardization------------------------------------------------
Demo[,.N,by="occp_cod"][order(-N)]
Demo[!occp_cod%in%c("MD","CN","OT","PH","HP","LW","RN")]$occp_cod <- NA
Demo <- Demo %>% droplevels()
saveRDS(Demo,"Clean Data/DEMO.rds")
rm(list=ls())

## Dates and duration standardization ---------------------------------------
#please change according to the last quarter
max_date <- 20230331

Demo <- setDT(readRDS("Clean Data/DEMO.rds"))

check_date <- function(dt) {
  n <- nchar(dt)
  invalid_dates <- (n == 4 & (dt <1985|dt > as.numeric(substr(max_date, 0,4)))) |
    (n == 6 & (dt <198500|dt > as.numeric(substr(max_date, 0,6)))) |
    (n == 8 & (dt <19850000|dt > as.numeric(substr(max_date, 0,8)))) |
    (!n %in% c(4, 6, 8))
  dt[invalid_dates] <- NA
  return(dt)
}

date_columns <- c("fda_dt", "rept_dt", "mfr_dt", "init_fda_dt", "event_dt")
for (col in date_columns) {
  Demo[, n := nchar(.SD[[col]])]
  Demo[, (col) := check_date(.SD[[col]])]
}
Demo <- Demo %>% select(-n)
saveRDS(Demo, "Clean Data/DEMO.rds")

Ther <- setDT(readRDS("Clean Data/THER.rds"))

for (col in c("start_dt", "end_dt")) {
  Ther[, n := nchar(.SD[[col]])]
  Ther[, (col) := check_date(.SD[[col]])]
}


Ther$dur <- as.numeric(Ther$dur)
Ther$dur_corrector <- as.numeric(NA)
Ther[dur_cod == "YR", dur_corrector := 365]
Ther[is.na(dur_cod), dur_corrector := NA]
Ther[dur_cod == "MON", dur_corrector := 30.41667]
Ther[dur_cod == "WK", dur_corrector := 7]
Ther[dur_cod == "DAY", dur_corrector := 1]
Ther[dur_cod == "HR", dur_corrector := 0.04166667]
Ther[dur_cod == "MIN", dur_corrector := 0.0006944444]
Ther[dur_cod == "SEC", dur_corrector := 1.157407e-05]

Ther <- Ther[, dur_in_days := abs(dur) * dur_corrector][, dur_in_days := ifelse(dur_in_days > 50*365, NA, dur_in_days)]

Ther <- Ther[, dur_std := ifelse(nchar(end_dt) == 8, ymd(end_dt), NA) - ifelse(nchar(start_dt) == 8, ymd(start_dt), NA) + 1]
Ther <- Ther[, dur_std := ifelse(dur_std < 0, NA, dur_std)][, dur_std := ifelse(is.na(dur_std), dur_in_days, dur_std)]
Ther <- Ther[,start_dt:=ifelse(!is.na(start_dt),
                               start_dt,
                               ifelse(!is.na(end_dt)&!is.na(dur_std),
                                      as.numeric(gsub("-","",as.character(ymd(end_dt)-dur_std+1))),
                                      NA))]
Ther <- Ther[,end_dt:=ifelse(!is.na(end_dt),
                             end_dt,
                             ifelse(!is.na(start_dt)&!is.na(dur_std),
                                    as.numeric(gsub("-","",as.character(ymd(start_dt)+dur_std-1))),
                                    NA))]

Ther <- Ther[,dur_in_days:=dur_std] %>% select(-n,-dur_std, -dur_corrector, -dur, -dur_cod)
saveRDS(Ther, "Clean Data/THER.rds")

## time to onset calculation ------------------------------------------------
Ther <- Demo[, .(primaryid, event_dt)][!is.na(event_dt)][Ther, on = "primaryid"]
Ther <- Ther[, time_to_onset := ifelse(nchar(event_dt) == 8, ymd(event_dt), NA) - ifelse(nchar(start_dt) ==8, ymd(start_dt), NA) + 1]
Ther <- Ther[, time_to_onset := ifelse(is.na(time_to_onset) | ( time_to_onset<=0 & event_dt<=20121231), NA, time_to_onset)]
saveRDS(Ther, "Clean Data/THER.rds")
rm(list = ls())

## DRUG_INFO standardization------------------------------------------------
DRUG_INFO <- setDT(readRDS("Clean Data/DRUG_INFO.rds"))
DRUG_INFO <- DRUG_INFO[,route:=tolower(trimws(route)),]
route_st <- setDT(read_delim("External Sources/Manual_fix/route_st.csv",";",
                             escape_double = FALSE, trim_ws = TRUE))[
                               ,.(route,route_st)] %>% distinct()
DRUG_INFO <- route_st[DRUG_INFO,on="route"]
route_st <- DRUG_INFO[,.N,by=c("route","route_st")][order(-N)]
write.csv2(route_st,
           "External Sources/Manual_fix/route_st.csv")
print(paste0("The following terms are translated to NA: ",paste0(route_st[
  is.na(route_st)]$route,collapse="; "), ". Integrate NAs and repeat."))


DRUG_INFO$dechal[!DRUG_INFO$dechal %in% c("Y","N","D")] <- NA
DRUG_INFO$dechal <- as.factor(DRUG_INFO$dechal)
DRUG_INFO$rechal[!DRUG_INFO$rechal %in% c("Y","N","D")] <- NA
DRUG_INFO$rechal <- as.factor(DRUG_INFO$rechal)

DRUG_INFO$dose_form <- tolower(trimws(DRUG_INFO$dose_form))
dose_form_st <- setDT(read_delim("External Sources/Manual_fix/dose_form_st.csv",
                                 ";", escape_double = FALSE, trim_ws = TRUE))[
                                   ,.(dose_form,dose_form_st)]
DRUG_INFO <- dose_form_st[DRUG_INFO,on="dose_form"]
DRUG_INFO$dose_form_st <- as.factor(DRUG_INFO$dose_form_st)
dose_form_st <- DRUG_INFO[,.N,by=c("dose_form","dose_form_st")][order(-N)]
write.csv2(dose_form_st,
           "External Sources/Manual_fix/dose_form_st.csv")
print(paste0("The following terms are translated to NA: ",paste0(dose_form_st[
  is.na(dose_form_st)]$dose_form,collapse="; "), ". Integrate NAs and repeat."))


dose_freq <- setDT(read_delim("External Sources/Manual_fix/dose_freq_st.csv",
                              ";", escape_double = FALSE, trim_ws = TRUE))[
                                ,.(dose_freq,dose_freq_st)][!is.na(dose_freq_st)] %>%
  distinct()
DRUG_INFO <-  DRUG_INFO %>% select(-route,-dose_form)
DRUG_INFO <- dose_freq[DRUG_INFO,on="dose_freq"]
DRUG_INFO$dose_freq_st <- as.factor(DRUG_INFO$dose_freq_st)
dose_freq_st <- DRUG_INFO[,.N,by=c("dose_freq","dose_freq_st")][order(-N)]
write.csv2(dose_freq_st,
           "External Sources/Manual_fix/dose_freq_st.csv")
print(paste0("The following terms are translated to NA: ",paste0(dose_freq_st[
  is.na(dose_freq_st)]$dose_freq,collapse="; "), ". Integrate NAs and repeat."))


route_form_st <- setDT(read_delim("External Sources/Manual_fix/route_form_st.csv",";",
                               escape_double = FALSE, trim_ws = TRUE))[
                                 ,.(dose_form_st,route_plus)] %>% distinct()
DRUG_INFO <- route_form_st[DRUG_INFO,on="dose_form_st"]
route_form_st <- DRUG_INFO[,.N,by=c("dose_form_st","route_st","route_plus")][order(-N)]
write.csv2(route_form_st,
           "External Sources/Manual_fix/route_form_st.csv")


DRUG_INFO$route_st <- ifelse(is.na(DRUG_INFO$route_st)|DRUG_INFO$route_st=="unknown",
                             DRUG_INFO$route_plus,DRUG_INFO$route_st)
DRUG_INFO$route_st <- as.factor(DRUG_INFO$route_st)


DRUG_INFO <- DRUG_INFO[,.(primaryid,drug_seq,val_vbm,route=route_st,dose_vbm,cum_dose_unit,cum_dose_chr, dose_amt,dose_unit,dose_form=dose_form_st,dose_freq=dose_freq_st,dechal,rechal,lot_num,nda_num,exp_dt)]

check_date <- function(dt) {
  n <- nchar(dt)
  invalid_dates <- (n == 4 & (dt <1985|dt > as.numeric(substr(max_date, 0,4)))) |
    (n == 6 & (dt <198500|dt > as.numeric(substr(max_date, 0,6)))) |
    (n == 8 & (dt <19850000|dt > as.numeric(substr(max_date, 0,8)))) |
    (!n %in% c(4, 6, 8))
  dt[invalid_dates] <- NA
  return(dt)
}

max_date <- 20500101
for (col in c("exp_dt")) {
  DRUG_INFO[, n := nchar(.SD[[col]])]
  DRUG_INFO[, (col) := check_date(.SD[[col]])]
}

saveRDS(DRUG_INFO,"Clean Data/DRUG_INFO.rds")
rm(list=ls())

## Remove nullified reports ------------------------------------------------
faers_list <- read.csv2("Clean Data/faers_list.csv")$x
Demo <- setDT(readRDS("Clean Data/DEMO.rds"))

Deleted <- faers_list[str_detect(faers_list,regex("deleted",ignore_case = T))]
i <- 0
for (f in Deleted){
  name <- gsub(".txt",".rds",f)
  x <- read.table(f,skip=1,sep="$",header = F, comment.char = "",quote="",
                  row.names = NULL)
  colnames(x) <- "caseid"
  if(i>0){DELETED <- rbindlist(list(DELETED,x))}else{DELETED <- x}
  i <- i+1
}
DELETED <- DELETED %>%  distinct()
Demo <- Demo[!caseid %in%DELETED$caseid]
saveRDS(Demo,"Clean Data/DEMO.rds")

## Remove duplicated ids----------------------------------------------------
#remove duplicated primaryid
Demo <- Demo[Demo[,.I[quarter==last(quarter)],by=primaryid]$V1]

#flatten case version by caseid --------------------------------------------
Demo <- Demo[Demo[,.I%in%c(Demo[,.I[.N],by="caseid"]$V1)]]
cols <- c("caseversion","sex","quarter","i_f_cod","rept_cod",
          "occp_cod","e_sub","age_grp","occr_country",
          "reporter_country")
Demo[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
saveRDS(Demo,"Clean Data/DEMO.rds")

## Remove duplicated manufacturer ids ---------------------------------
Demo <- Demo[order(fda_dt)]
Demo <- Demo[Demo[,.I%in%c(Demo[,.I[.N],by=c("mfr_num","mfr_sndr")]$V1,
                           Demo[,which(is.na(mfr_num))],
                           Demo[,which(is.na(mfr_sndr))])]]

## Remove reports with no drug or reaction ---------------------------------
Drug <- readRDS("Clean Data/DRUG.rds")
Reac <- readRDS("Clean Data/REAC.rds")

Drug <- Drug[!Substance%in%c("no medication","unspecified")]
Reac <- Reac[!pt%in%c("no adverse event")]

no_drugs <- setdiff(unique(Demo$primaryid),unique(Drug$primaryid))
no_event <- setdiff(unique(Demo$primaryid),unique(Reac$primaryid))
not_complete <- union(no_drugs,no_event)
Demo <- Demo[!primaryid %in% not_complete]
saveRDS(Demo,"Clean Data/DEMO.rds")

## Identify pre-marketing and literature ------------------------------------
Demo[,premarketing:=primaryid%in%Drug[trial==TRUE]$primaryid]
Demo[,literature:=!is.na(lit_ref)]
saveRDS(Demo,"Clean Data/DEMO.rds")

## Clean datasets from excluded primaryids ----------------------------------

# replace the name of the directory
# according to the last quarter downloaded

data_directory <- "data23Q1"

dir.create(data_directory)
Demo_Supp <-  Demo[,.(primaryid,caseid,caseversion,i_f_cod,auth_num,e_sub,
                      lit_ref,rept_dt,to_mfr,mfr_sndr,mfr_num,mfr_dt,quarter)]
saveRDS(Demo_Supp,paste0(data_directory,"/DEMO_SUPP.rds"))
Demo <- Demo[,.(primaryid,sex,age_in_days,wt_in_kgs,occr_country,event_dt,
                occp_cod,reporter_country,rept_cod,init_fda_dt,fda_dt,
                premarketing,literature)]
saveRDS(Demo,paste0(data_directory,"/DEMO.rds"))

Drug <- Drug[primaryid %in% Demo$primaryid]
Drug_Name <- Drug[,.(primaryid,drug_seq,drugname,prod_ai)] %>% distinct()
saveRDS(Drug_Name,paste0(data_directory,"/DRUG_NAME.rds"))
rm(Drug_Name)

Drug <- Drug[,.(primaryid,drug_seq,substance=Substance,role_cod)] %>% distinct()
Drug$role_cod <- factor(Drug$role_cod,levels=c("C","I","SS","PS"),
                        ordered = TRUE)
saveRDS(Drug,paste0(data_directory,"/DRUG.rds"))
rm(Drug)

Reac <- Reac[primaryid %in% Demo$primaryid]
meddra_primary <- setDT(read_csv2("External Sources/Dictionaries/MedDRA/meddra_primary.csv"))
setorderv(meddra_primary,c("soc","hlgt","hlt","pt"))

Reac <- Reac[,.(primaryid,
                pt=factor(pt,
                          levels=meddra_primary$pt,
                          ordered=TRUE),
                drug_rec_act = factor(drug_rec_act,
                                      levels=meddra_primary$pt,
                                      ordered=TRUE))]
saveRDS(Reac,paste0(data_directory,"/REAC.rds"))
rm(Reac)

Outc <- setDT(readRDS("Clean Data/Outc.rds"))
Outc <- Outc[primaryid %in% Demo$primaryid][
  ,.(primaryid,outc_cod=factor(outc_cod,levels=c("OT", "CA", "HO", "RI",
                                                 "DS", "LT", "DE"),
                               ordered = TRUE))] %>% distinct()
saveRDS(Outc,paste0(data_directory,"/OUTC.rds"))
rm(Outc)

Indi <- setDT(readRDS("Clean Data/Indi.rds"))
Indi <- Indi[primaryid %in% Demo$primaryid]
Indi <- Indi[,.(primaryid,drug_seq,
                indi_pt= factor(indi_pt,
                                levels=meddra_primary$pt,
                                ordered=TRUE))] %>% distinct()
saveRDS(Indi,paste0(data_directory,"/INDI.rds"))
rm(Indi)

Ther <- setDT(readRDS("Clean Data/Ther.rds"))
Ther <- Ther[primaryid%in%Demo$primaryid][,.(primaryid,drug_seq,start_dt,
                                             dur_in_days,end_dt,time_to_onset,
                                             event_dt)] %>% distinct()
saveRDS(Ther,paste0(data_directory,"/THER.rds"))
rm(Ther)

Drug_Info <- setDT(readRDS("Clean Data/Drug_info.rds"))
Drug_Info <- Drug_Info[primaryid%in%Demo$primaryid]
Doses <-  Drug_Info[,.(primaryid,drug_seq,dose_vbm,cum_dose_unit,cum_dose_chr,
                       dose_amt,dose_unit,dose_freq)] %>% distinct()
saveRDS(Doses,paste0(data_directory,"/DOSES.rds"))
rm(Doses)
Drug_Supp <- Drug_Info[,.(primaryid,drug_seq,route,dose_form,dechal,rechal,
                          lot_num,exp_dt)] %>% distinct()
Drug_Supp <- Drug_Supp[,dose_form:=factor(dose_form)]
saveRDS(Drug_Supp,paste0(data_directory,"/DRUG_SUPP.rds"))
rm(Drug_Supp)

Drug_Name <- setDT(readRDS(paste0(data_directory,"/DRUG_NAME.rds")))
Drug_Name <- Drug_Info[,.(primaryid,drug_seq,val_vbm,nda_num)][
  Drug_Name,on=c("primaryid","drug_seq")]
saveRDS(Drug_Name,paste0(data_directory,"/DRUG_NAME.rds"))
rm(Drug_Name)
rm(Drug_Info)

Rpsr <- setDT(readRDS("Clean Data/Rpsr.rds"))
Demo_Supp <- Rpsr[,.(primaryid,rpsr_cod=factor(rpsr_cod))][Demo_Supp,
                                                           on="primaryid"]
saveRDS(Demo_Supp,paste0(data_directory,"/DEMO_SUPP.rds"))
rm(list = ls())


## Rule-based deduplication --------------------------------------------------
# replace the name of the directory
# according to the last quarter downloaded

data_directory <- "data23Q1"

Reac <- setDT(readRDS(paste0(data_directory,"/REAC.rds")))
Demo <- setDT(readRDS(paste0(data_directory,"/DEMO.rds")))
Drug <- setDT(readRDS(paste0(data_directory,"/DRUG.rds")))

complete_duplicates <- c("event_dt","sex","reporter_country","age_in_days","wt_in_kgs","pt","PS","SS","IC")
temp_reac <- Reac[order(pt)][,.(pt=paste0(pt,collapse="; ")),by="primaryid"] %>%
  distinct()
temp_drug_PS <- Drug[order(substance)][role_cod=="PS"][
  ,.(PS=paste0(substance,collapse="; ")),by="primaryid"] %>% distinct()
temp_drug_SS <- Drug[order(substance)][role_cod=="SS"][
  ,.(SS=paste0(substance,collapse="; ")),by="primaryid"] %>% distinct()
temp_drug_IC <- Drug[order(substance)][role_cod%in%c("I","C")][
  ,.(IC=paste0(substance,collapse="; ")),by="primaryid"] %>% distinct()
temp <- temp_reac[temp_drug_IC[temp_drug_SS[temp_drug_PS[Demo,
                                                         on="primaryid"],
                                            on="primaryid"],
                               on="primaryid"],
                  on="primaryid"]
rm(temp_reac)
rm(temp_drug_PS)
rm(temp_drug_SS)
rm(temp_drug_IC)


temp <- temp[order(fda_dt)]
temp_grouped <- temp[,DUP_ID:=.GRP,by=complete_duplicates]
singlets_pids <- temp_grouped[DUP_ID %in% temp_grouped[,.N,by="DUP_ID"][N==1]$DUP_ID]$primaryid
duplicates <- temp_grouped[!primaryid %in% singlets_pids]
duplicates_pids <- duplicates[duplicates[,.I[primaryid==last(primaryid)],by="DUP_ID"]$V1]$primaryid

Demo[,RB_duplicates:=!primaryid%in% c(singlets_pids,duplicates_pids)]
saveRDS(Demo,paste0(data_directory,"/DEMO.rds"))

##From now on draft in progress.
#+ solo PS e SS + colonna

## Probabilistic deduplication ----------------------------------------------
Demo <- setDT(readRDS("~/Desktop/2707/DEMO.rds"))
Outc <- setDT(readRDS("~/Desktop/2707/OUTC.rds"))
Drug <- setDT(readRDS("~/Desktop/2707/DRUG.rds"))[,.(primaryid,substance)] %>% distinct()
Reac <- setDT(readRDS("~/Desktop/2707/REAC.rds"))[,.(primaryid,pt)] %>% distinct()

sample <- Demo[sample(.N, 1000), ]
df_weights <- data.frame(var="any",value=c("W_NA"),weight=as.numeric(0))

vars <- c("sex","reporter_country")
a_values <- c(0.051,0.036)
for (v in 1:length(vars)){
a <- a_values[v]
var <- vars[v]
b <- sum(is.na(Demo[[var]]))/nrow(Demo)
c <- a*(2-a-2*b)
df_weights <- rbind(df_weights,c(var,"W_disc",log2(c-2*log2(1-b))))
not_NA <- sum(!is.na(Demo[[var]]))
for (value in setdiff(unique(Demo[[var]]),NA)){
  Beta <- nrow(Demo[get(var)==value])/not_NA
  Weight <- log2(1-c*(1-Beta)*(1-b)^(-2))-log2(Beta) 
  df_weights <- rbind(df_weights,c(var,value,Weight))
}
}
var <- "outc_cod"
a <- 0.101
b <- length(unique(Outc$primaryid))/nrow(Demo)
c <- a*(2-a-2*b)
df_weights <- rbind(df_weights,c(var,"W_disc",log2(c-2*log2(1-b))))
not_NA <- nrow(Demo)-length(unique(Outc$primaryid))
for (value in setdiff(unique(Outc[[var]]),NA)){
  Beta <- nrow(Outc[get(var)==value])/not_NA
  Weight <- log2(1-c*(1-Beta)*(1-b)^(-2))-log2(Beta) 
  df_weights <- rbind(df_weights,c(var,value,Weight))
}

var <- "substance"
a <- 0.107
b <- 0
c <- a*(2-a-2*b)
not_NA <- nrow(Demo)
frequencies <- Drug[,.N,by="substance"][,freq:=N/not_NA]
df_weights <- rbind(df_weights,c(var,"W_disc",log2(c-2*log2(1-b))))
for (value in setdiff(unique(Drug[[var]]),NA)){
  Beta <- frequencies[substance==value]$freq
  Weight <- log2(1-c*(1-Beta)*(1-b)^(-2))-log2(Beta) 
  df_weights <- rbind(df_weights,c(var,value,Weight))
}

var <- "pt"
a <- 0.387
b <- 0
c <- a*(2-a-2*b)
not_NA <- nrow(Demo)
frequencies <- Reac[,.N,by="pt"][,freq:=N/not_NA]
df_weights <- rbind(df_weights,c(var,"W_disc",log2(c-2*log2(1-b))))
for (value in setdiff(unique(Reac[[var]]),NA)){
  Beta <- frequencies[pt==value]$freq
  Weight <- log2(1-c*(1-Beta)*(1-b)^(-2))-log2(Beta) 
  df_weights <- rbind(df_weights,c(var,value,Weight))
}

setDT(df_weights)[,weight:=as.numeric(weight)]

var <- "age_in_years"
Demo <- Demo[,age_in_years:=age_in_days/365]
a1_VigiMatch <- 0.036
a2_VigiMatch <- 0.010
b_VigiMatch <- 0.186
# we calculate the parameters according to our frequency of blanks and keeping
# the proportion betweeen Hit, Miss, and Deviation proposed by VigiMatch
b_age <- sum(is.na(Demo[[var]]))/nrow(Demo)
a1_age <- a1_VigiMatch*b_age/b_VigiMatch
a2_age <- a2_VigiMatch*b_age/b_VigiMatch
s1_age <- 2.1
diff_age <- sample(Demo[!is.na(age_in_years)]$age_in_years,
                   10000) - sample(Demo[!is.na(age_in_years)]$age_in_years,
                                   10000)

var <- "event_dt"
Demo <- Demo[,date_lower:=ifelse(nchar(event_dt==8),as.numeric(ymd(event_dt)-1),
                                 ifelse(nchar(event_dt==6),as.numeric(ym(event_dt)),
                                        as.numeric(ymd(event_dt, truncated = 2L))))]
Demo <- Demo[,date_upper:=ifelse(nchar(event_dt==8),as.numeric(ymd(event_dt)+1),
                                 ifelse(nchar(event_dt==6),as.numeric(ceiling_date(ym(event_dt),"month")-days(1)),
                                        as.numeric(ceiling_date(ymd(event_dt, truncated = 2L),"year")-days(1))))]
a1_VigiMatch <- 0.051
a2_VigiMatch <- 0.01
b_VigiMatch <- 0.229
# we calculate the parameters according to our frequency of blanks and keeping
# the proportion betweeen Hit, Miss, and Deviation proposed by VigiMatch
b_date <- sum(is.na(Demo[[var]]))/nrow(Demo)
a1_date <- a1_VigiMatch*b_date/b_VigiMatch
a2_date <- a2_VigiMatch*b_date/b_VigiMatch
s1_date <- 50.2
diff_date <- sample(Demo[!is.na(date_lower)]$date_lower,
                    10000,replace=TRUE) - sample(Demo[!is.na(date_lower)]$date_lower,
                                                 10000,replace=TRUE)

df_scores <- data.frame(pid1=as.numeric(NA),pid2=as.numeric(NA),
                        sex=as.numeric(NA),country=as.numeric(NA),#outcome=as.numeric(NA),
                        substance=as.numeric(NA),
                        pt=as.numeric(NA),age=as.numeric(NA),date=as.numeric(NA))


combinations <- combn(sample$primaryid, 2, simplify = FALSE)
calculate_pu_age <- function(d){
  pu <- ((1-b_age)^2)*dnorm(d,mean(diff_age,na.rm = T),sd(diff_age,na.rm = T))
  return(pu)
}
calculate_pr_age <- function(d){
  pr <- ((1-a1_age-a2_age-b_age)^2) *  d(distr::Dirac(0))(d) +
    a2_age*(2-a2_age-2*b_age) * ((1-b_age)^2)*dnorm(d,mean(diff_age,na.rm = T),sd(diff_age,na.rm = T)) +
    2*a1_age*(1-a1_age-a2_age-b_age) * dnorm(d,0,s1_age) +
    (a1_age^2)* dnorm(d,0,2*s1_age)
  return(pr)
}
calculate_pu_date <- function(d){
  pu <- ((1-b_date)^2)*dnorm(d,mean(diff_date,na.rm = T),sd(diff_date,na.rm = T))
  return(pu)
}
calculate_pr_date <- function(d){
  pr <- ((1-a1_date-a2_date-b_date)^2) *  d(distr::Dirac(0))(d) +
    a2_date*(2-a2_date-2*b_date) * ((1-b_date)^2)*dnorm(d,mean(diff_date,na.rm = T),sd(diff_date,na.rm = T)) +
    2*a1_date*(1-a1_date-a2_date-b_date) * dnorm(d,0,s1_date) +
    (a1_date^2)* dnorm(d,0,2*s1_date)
  return(pr)
}

for (i in 1:length(combinations)){
  pid_i <- combinations[[i]][[1]]
  pid_j <- combinations[[i]][[2]]
  scores <- c()
  for (var in vars){
    x <- as.character(sample[primaryid==pid_i,get(var)])
    y <- as.character(sample[primaryid==pid_j,get(var)])
    score <- ifelse(is.na(x)|is.na(y), 0,
                    ifelse(x!=y, df_weights[var==var&value=="W_disc"]$weight,
                           df_weights[var==var&value==x]$weight))
    scores <- c(scores,score)
  }
  ##outc
  # outc_i <- as.character(Outc[primaryid==pid_i]$outc_cod)
  # outc_j <- as.character(Outc[primaryid==pid_j]$outc_cod)
  # concordance <- intersect(outc_i,outc_j)
  # discordance <- unique(c(setdiff(outc_i,outc_j), setdiff(outc_j,outc_i)))
  # w_discordance <- length(discordance)*df_weights[var=="outc_cod"&value=="W_disc"]$weight
  # score <-w_discordance
  # for (outc in concordance){score <- score + df_weights[var=="outc_cod"&value==outc]$weight}
  # scores <- c(scores,score)
  ##drug
  drug_i <- setdiff(as.character(Drug[primaryid==pid_i]$substance),NA)
  drug_j <- setdiff(as.character(Drug[primaryid==pid_j]$substance),NA)
  concordance <- intersect(drug_i,drug_j)
  discordance <- unique(c(setdiff(drug_i,drug_j), setdiff(drug_j,drug_i)))
  w_discordance <- length(discordance)*df_weights[var=="substance"&value=="W_disc"]$weight
  score <-w_discordance
  for (drug in concordance){score <- score + df_weights[var=="substance"&value==drug]$weight}
  scores <- c(scores,score)
  ##pt
  reac_i <- as.character(Reac[primaryid==pid_i]$pt)
  reac_j <- as.character(Reac[primaryid==pid_j]$pt)
  concordance <- intersect(reac_i,reac_j)
  discordance <- unique(c(setdiff(reac_i,reac_j), setdiff(reac_j,reac_i)))
  w_discordance <- length(discordance)*df_weights[var=="pt"&value=="W_disc"]$weight
  score <- w_discordance
  for (reac in concordance){score <- score + df_weights[var=="pt"&value==reac]$weight}
  scores <- c(scores,score)
  #age_in_years
  d=Demo[primaryid==pid_i]$age_in_years - Demo[primaryid==pid_j]$age_in_years
  if (is.na(d)){score=0} else {
    int_pu <- integrate(calculate_pu_age, lower = d-1, upper = d+1)$value
    int_pr <- integrate(calculate_pr_age, lower = d-1, upper = d+1)$value
    score <- log2(int_pr/int_pu)
  }
  scores <- c(scores,score)
  #event_dt
  di_l <-  Demo[primaryid==pid_i]$date_lower
  di_u <- Demo[primaryid==pid_i]$date_upper
  dj_l <-  Demo[primaryid==pid_j]$date_lower
  dj_u <- Demo[primaryid==pid_j]$date_upper
  difference <- abs(c(di_l-dj_l,di_l-dj_u,di_u-dj_l,di_u-dj_u))
  if (anyNA(difference)){score=0} else {
    int_pu <- integrate(calculate_pu_date, lower = min(difference), upper = max(difference))$value
    int_pr <- integrate(calculate_pr_date, lower = min(difference), upper = max(difference))$value
    score <- log2(int_pr/int_pu)
  }
  scores <- c(scores,score)
  df_scores <- rbind(c(pid_i,pid_j,scores),df_scores)
}

df_scores <- setDT(df_scores)[,total:=sex+country+outcome+substance+pt+age+date]

#event_dt
#remove outcome
