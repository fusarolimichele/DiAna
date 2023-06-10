## Set up packages-------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","data.table","janitor","foreach","xml2","rvest")

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

DEMO <- unify_data(faers_list[str_detect(faers_list,regex("demo",ignore_case = T))],
                   c(ISR="primaryid",CASE="caseid",FOLL_SEQ="caseversion",
                     I_F_COD="i_f_cod",EVENT_DT="event_dt",
                     MFR_DT="mfr_dt",FDA_DT="fda_dt",REPT_COD="rept_cod",
                     MFR_NUM="mfr_num",MFR_SNDR="mfr_sndr",
                     AGE="age",AGE_COD="age_cod",GNDR_COD="sex",E_SUB="e_sub",
                     WT="wt",WT_COD="wt_cod",
                     REPT_DT="rept_dt", OCCP_COD="occp_cod",TO_MFR="to_mfr",
                     REPORTER_COUNTRY="reporter_country",quarter="quarter",
                     i_f_code="i_f_cod"),
                   c("primaryid","caseid","caseversion","i_f_cod",
                     "sex","age","age_cod","age_grp","wt","wt_cod",
                     "reporter_country","occr_country",
                     "event_dt","rept_dt","mfr_dt","init_fda_dt","fda_dt",
                     "rept_cod","occp_cod","mfr_num","mfr_sndr","to_mfr",
                     "e_sub","quarter","auth_num","lit_ref"),
                   c("rept_dt" , "sex"),
                   c(" rept_dt", "gndr_cod"))
saveRDS(DEMO,"Clean Data/DEMO.rds")
rm(DEMO)

DRUG <- unify_data(faers_list[str_detect(faers_list,regex("drug",ignore_case = T))],
                   c(ISR="primaryid",DRUG_SEQ="drug_seq",ROLE_COD="role_cod",
                     DRUGNAME="drugname",VAL_VBM="val_vbm",ROUTE="route",
                     DOSE_VBM="dose_vbm",DECHAL="dechal",
                     RECHAL="rechal",LOT_NUM="lot_num",NDA_NUM="nda_num",
                     EXP_DT="exp_dt"),
                   c("primaryid","drug_seq","role_cod","drugname","prod_ai"),
                   NA,
                   NA)
saveRDS(DEMO,"Clean Data/DEMO.rds")
rm(DRUG)

DRUG_INFO <- unify_data(faers_list[str_detect(faers_list,regex("drug_info",ignore_case = T))],
                        c(ISR="primaryid",DRUG_SEQ="drug_seq",ROLE_COD="role_cod",
                          DRUGNAME="drugname",VAL_VBM="val_vbm",ROUTE="route",
                          DOSE_VBM="dose_vbm",DECHAL="dechal",
                          RECHAL="rechal",LOT_NUM="lot_num",NDA_NUM="nda_num",
                          EXP_DT="exp_dt"),
                        c("primaryid","drug_seq","val_vbm","nda_num","lot_num",
                          "route","dose_form","dose_freq","exp_dt",
                          "dose_vbm","cum_dose_unit","cum_dose_chr","dose_amt",
                          "dose_unit","dechal","rechal"),
                        c("lot_num"),
                        c("lot_nbr"))
saveRDS(DRUG_INFO,"Clean Data/DRUG_INFO.rds")
rm(DRUG_INFO)

INDΙ <- unify_data(faers_list[str_detect(faers_list,regex("indi",ignore_case = T))],
                   c(ISR="primaryid",DRUG_SEQ="drug_seq",
                     indi_drug_seq="drug_seq",INDI_PT="indi_pt"),
                   c("primaryid","drug_seq","indi_pt"),
                   c("lot_num"),
                   c("lot_nbr"))
INDI <- INDI[!is.na(indi_pt)]
saveRDS(INDI,"Clean Data/INDI.rds")
rm(INDI)

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

