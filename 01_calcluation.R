#' Organisation of saccade data
#' ============================
   rm(list = ls())
   closeAllConnections()

#' Libraries
#' ---------
   library(tidyverse)
   library(rstudioapi)
   library(dplyr)
   library(progress)
   source("source/utility.R")

#' Determine the working directory
#' -------------------------------
   current_path = getActiveDocumentContext()$path 
   rootdir = setwd(dirname(current_path))

#' Select the list type: 
#' ---------------------  
   Exp =   c("prosaccade",      # 1
             "word_processing"  # 2
   )[2] # CHANGE THIS NUMBER AND RUN THE CODE

#' Load the data & select some columns: 
#' ------------------------------------  
   df.left  <- read.table(paste0("data/raw/",Exp,"/","left",".txt"), 
                          sep="\t", header=TRUE, encoding = "UTF-8",na.strings = c(".", "NA"))
   df.right <- read.table(paste0("data/raw/",Exp,"/","right",".txt"), 
                          sep="\t", header=TRUE, encoding = "UTF-8",na.strings = c(".", "NA"))

   str(df.left)
   
#' Mark columns as right (r) and left (l) in right and left eye data
#' -----------------------------------------------------------------
   colnames(df.left) <- paste0(colnames(df.left), "_l")
   colnames(df.left)[which(names(df.left) == "CURRENT_SAC_START_TIME_l")] <- "CURRENT_SAC_START_TIME"
   colnames(df.right) <- paste0(colnames(df.right), "_r")
   colnames(df.right)[which(names(df.right) == "CURRENT_SAC_START_TIME_r")] <- "CURRENT_SAC_START_TIME"

#' Calculate the difference in gaze position between the end of one saccade and 
#' the start of the next saccade
#' First we remove the NA values. Then we subtract the end position of the current 
#' saccade (pixels or resolution) from the start position of the next saccade 
#' (pixels or resolution). 
#' ----------------------------------------------------------------------------
   df.left  <- df.left %>% drop_na(CURRENT_SAC_END_X_l) %>% drop_na(NEXT_SAC_START_X_l)
   df.right <- df.right %>% drop_na(CURRENT_SAC_END_X_r) %>% drop_na(NEXT_SAC_START_X_r)
   df.left$drift.px_l  <- abs(df.left$CURRENT_SAC_END_X_l - df.left$NEXT_SAC_START_X_l)
   df.right$drift.px_r  <- abs(df.right$CURRENT_SAC_END_X_r - df.right$NEXT_SAC_START_X_r)

   df.left  <- df.left %>% drop_na(CURRENT_SAC_END_X_RESOLUTION_l) %>% drop_na(NEXT_SAC_START_X_RESOLUTION_l)
   df.right <- df.right %>% drop_na(CURRENT_SAC_END_X_RESOLUTION_r) %>% drop_na(NEXT_SAC_START_X_RESOLUTION_r)
   df.left$drift.res_l. <- abs(df.left$CURRENT_SAC_END_X_RESOLUTION_l - df.left$NEXT_SAC_START_X_RESOLUTION_l)
   df.right$drift.res_r <- abs(df.right$CURRENT_SAC_END_X_RESOLUTION_r - df.right$NEXT_SAC_START_X_RESOLUTION_r)
   
#' Merging right and left eye data 
#' Firstly, we create the id_trial variable consisting of the combination of 
#' participants and trials and create the common_id variable that will enter the 
#' loop.Then we perform a fuzzy join using the fuzzyjoin::distance_join() function. 
#' We use CURRENT_SAC_START_TIME as a reference for the join. The reason for 
#' using fuzzy is that the saccade start times are not exactly the same for both 
#' eyes. Here we set max_dist = 2 ms    
#' -----------------------------------------------------------------------------
   
   #common id
   df.left$id_trial  <- paste0(df.left$RECORDING_SESSION_LABEL_l,".",df.left$TRIAL_INDEX_l)
   df.right$id_trial <- paste0(df.right$RECORDING_SESSION_LABEL_r,".",df.right$TRIAL_INDEX_r)
   common_id <- intersect(df.left$id_trial, df.right$id_trial)

   #control
   nlevels(as.factor(df.right$RECORDING_SESSION_LABEL_r))
   nlevels(as.factor(df.left$RECORDING_SESSION_LABEL_l))
   nlevels(as.factor(df.left$TRIAL_INDEX_l))
   nlevels(as.factor(df.right$TRIAL_INDEX_r))
   nlevels(as.factor(df.left$id_trial))
   nlevels(as.factor(df.right$id_trial))
   nlevels(as.factor(common_id))

   #progress bar
   pb <- progress_bar$new(total = length(common_id))

   #loop
   df.merged = list() 
   for (i in common_id){
     df.merged[[i]] <- fuzzyjoin::distance_join(df.left[df.left$id_trial==i,], 
                                                df.right[df.right$id_trial==i,], 
                                                by = c("CURRENT_SAC_START_TIME"),
                                                max_dist =2, distance_col = "distance")
     pb$tick()
     #Sys.sleep(1/length(common_id))
   }
   df.merged = do.call(rbind, df.merged)
   df.merged$id_trial.x <-NULL; df.merged$id_trial.y <-NULL
   
   #Control
   head(df.merged)
   (totalrow=nrow(df.merged))
   (Na.L=sum(is.na(df.merged$CURRENT_SAC_AMPLITUDE_l)))
   (Na.R=sum(is.na(df.merged$CURRENT_SAC_AMPLITUDE_r)))
   (df.merged$MissingData.L= (Na.L*100)/totalrow) 
   (df.merged$MissingData.R= (Na.R*100)/totalrow) 
   
   
#' For saccade disconjugacy we calculate the difference between right and left 
#' eye data. For postsaccadic drift we calculate the difference between right 
#' and left eye data.    
#' -----------------------------------------------------------------------------
   #minus <- function(x,y) sum(x,na.rm=T) - sum(y,na.rm=T)
   #minus(2,NA)
   #df.merged$saccade_disconjugacy = minus(df.merged$CURRENT_SAC_AMPLITUDE.x - df.merged$CURRENT_SAC_AMPLITUDE.y)
   df.merged  <- df.merged %>% 
     drop_na(CURRENT_SAC_AMPLITUDE_l) %>% 
     drop_na(CURRENT_SAC_AMPLITUDE_r)

   df.merged$saccade_disconjugacy   <- abs(df.merged$CURRENT_SAC_AMPLITUDE_l - df.merged$CURRENT_SAC_AMPLITUDE_r)
   df.merged$saccade_version         <- (abs(df.merged$CURRENT_SAC_AMPLITUDE_l + df.merged$CURRENT_SAC_AMPLITUDE_r))/2
   #df.merged$saccade_vesion2         <- (df.merged$CURRENT_SAC_AMPLITUDE_l + df.merged$CURRENT_SAC_AMPLITUDE_r)/2
   df.merged$postsaccadic_drift.px  <- abs(df.merged$drift.px_l - df.merged$drift.px_r)
   df.merged$postsaccadic_drift.res <- abs(df.merged$drift.res_l - df.merged$drift.res_r)

   # control
   min(df.merged$postsaccadic_drift.res); max(df.merged$postsaccadic_drift.res)
   min(df.merged$drift.res_r); max(df.merged$drift.res_r)
   
#' Rename the columns and remove 
#' -----------------------------  
   df.merged=reNameCols(df.merged)
   colnames(df.merged)

#' Merge with Duygu's other data
#' -----------------------------  
   df.list=read.csv('data/list/list_ekleme.csv', sep=';',na.strings = c("NA", "-",''))
   unique(df.list$id)
   unique(df.merged$ID)
   nlevels(as.factor(df.list$id))
   nlevels(as.factor(df.merged$ID))
   
   df.merged$ID[!(df.merged$ID %in% df.list$id)]
   df.list$id[!(df.list$id %in% df.merged$ID)]
   df.merged=merge(df.merged,df.list, by.x = "ID", by.y = "id")
   head(df.merged)

  
   df.sum = df.merged %>%
     select('ID','Group',"CURRENT_SAC_AMPLITUDE_r","CURRENT_SAC_DURATION_r",
            "CURRENT_SAC_AVG_VELOCITY_r","CURRENT_SAC_PEAK_VELOCITY_r","saccade_disconjugacy",
            "postsaccadic_drift.res",'saccade_version',"WISC_VCI","WISC_WMI","SOBAT_total_standard",
            "PAS.total","RAN.mean","MOYA_parent_total") %>%  
     group_by(ID,Group) %>%
     transmute(AMPLITUDE = mean(CURRENT_SAC_AMPLITUDE_r, na.rm=T),
            DURATION = mean(CURRENT_SAC_DURATION_r, na.rm=T),
            AVG_VELOCITY = mean(CURRENT_SAC_AVG_VELOCITY_r, na.rm=T),
            PEAK_VELOCITY = mean(CURRENT_SAC_PEAK_VELOCITY_r, na.rm=T),
            saccade_disconjugacy = mean(saccade_disconjugacy, na.rm=T),
            postsaccadic_drift.res = mean(postsaccadic_drift.res, na.rm=T),
            saccade_version = mean(saccade_version, na.rm=T),
            WISC_VCI=WISC_VCI,WISC_WMI=WISC_WMI,SOBAT_total_standard=SOBAT_total_standard,
            PAS.total=PAS.total,RAN.mean=RAN.mean,MOYA_parent_total=MOYA_parent_total) %>%
     distinct() 
   write_csv(df.sum, paste0('data/sum/sum_data', Exp,'.csv'))

#' Write the merge data frame in to a file: 
#' ----------------------------------------  
   write.table(df.merged, paste0("data/merged/",Exp,"/","left_right.txt"), 
               sep="\t", quote=F, row.names=F)

   