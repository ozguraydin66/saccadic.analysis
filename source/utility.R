SaccadeDet <- function(df) {
    table = ddply(df, .(), summarise, 
                   N=length(CURRENT_SAC_AMPLITUDE),
                   N.0_1= round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE<1])/N),digits=2),
                   N.1_2=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=1  & CURRENT_SAC_AMPLITUDE<=2])/N),digits=2),
                   N.2_3=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=2  & CURRENT_SAC_AMPLITUDE<=3])/N),digits=2),
                   N.3_4=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=3  & CURRENT_SAC_AMPLITUDE<=4])/N),digits=2),
                   N.4_5=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=4  & CURRENT_SAC_AMPLITUDE<=5])/N),digits=2),
                   N.5_6=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=5  & CURRENT_SAC_AMPLITUDE<=6])/N),digits=2),
                   N.6_7=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=6  & CURRENT_SAC_AMPLITUDE<=7])/N),digits=2),
                   N.7_8=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=7  & CURRENT_SAC_AMPLITUDE<=8])/N),digits=2),
                   N.8_9=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=8  & CURRENT_SAC_AMPLITUDE<=9])/N),digits=2),
                   N.9_10=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=9  & CURRENT_SAC_AMPLITUDE<=10])/N),digits=2),
                   N.10_20=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=10 & CURRENT_SAC_AMPLITUDE<=20])/N),digits=2),
                   N.20_30=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=20 & CURRENT_SAC_AMPLITUDE<=30])/N),digits=2),
                   N.30_40=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=30 & CURRENT_SAC_AMPLITUDE<=40])/N),digits=2),
                   N.40_50=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=40 & CURRENT_SAC_AMPLITUDE<=50])/N),digits=2),
                   N.50=round((100*length(CURRENT_SAC_AMPLITUDE[CURRENT_SAC_AMPLITUDE>=50])/N),digits=2)
    )
    return(table)
}


selectCols <- function(Exp) {
  if(Exp=="prosaccade"){
    colsN = c("RECORDING_SESSION_LABEL","TRIAL_INDEX", 
              "CURRENT_SAC_AMPLITUDE",
              "CURRENT_SAC_AVG_VELOCITY",
              "CURRENT_SAC_PEAK_VELOCITY",
              "CURRENT_SAC_DURATION",
              "CURRENT_SAC_START_TIME", 
              "CURRENT_SAC_START_INTEREST_AREA_ID",              
              "CURRENT_SAC_START_INTEREST_AREA_INDEX",
              "CURRENT_SAC_END_X",
              "CURRENT_SAC_DIRECTION",
              "NEXT_SAC_START_X",
              'NEXT_FIX_DURATION',
              "CURRENT_SAC_END_X_RESOLUTION", 
              "NEXT_SAC_START_X_RESOLUTION",
              "CURRENT_SAC_END_INTEREST_AREA_LABEL",
              "CURRENT_SAC_START_INTEREST_AREA_LABEL",
              "amplitude","correctLocation", "direction","fixDuration","instance",                                        
              "targetPhysicalLocation","wrongLocation")
  }
  if(Exp=="word_processing"){
    colsN = c("RECORDING_SESSION_LABEL","TRIAL_INDEX", 
              "CURRENT_SAC_AMPLITUDE",
              "CURRENT_SAC_AVG_VELOCITY",
              "CURRENT_SAC_PEAK_VELOCITY",
              "CURRENT_SAC_DURATION",
              "CURRENT_SAC_START_TIME", "CURRENT_SAC_START_INTEREST_AREA_ID",              
              "CURRENT_SAC_START_INTEREST_AREA_INDEX",
              "CURRENT_SAC_END_X","NEXT_SAC_START_X",
              "CURRENT_SAC_END_X_RESOLUTION", "NEXT_SAC_START_X_RESOLUTION",
              "CURRENT_SAC_END_INTEREST_AREA_LABEL",
              "CURRENT_SAC_START_INTEREST_AREA_LABEL",
              "CURRENT_SAC_DIRECTION",
              'NEXT_FIX_DURATION',
              "KEY_PRESSED","RT","bigram","exp_type","expected_response",
              "freq","on","top_string","trial_number","trigram","word_length","word_type") 
  }
  if(Exp=="text_reading"){
    colsN = c("RECORDING_SESSION_LABEL","TRIAL_INDEX", 
              "CURRENT_SAC_AMPLITUDE",
              "CURRENT_SAC_AVG_VELOCITY",
              "CURRENT_SAC_PEAK_VELOCITY",
              "CURRENT_SAC_DURATION",
              "CURRENT_SAC_DIRECTION",
              "CURRENT_SAC_START_TIME", 
              "CURRENT_SAC_START_INTEREST_AREA_ID",              
              "CURRENT_SAC_START_INTEREST_AREA_INDEX",
              "CURRENT_SAC_START_INTEREST_AREA_LABEL",
              "CURRENT_SAC_END_INTEREST_AREA_LABEL",
              "CURRENT_SAC_END_X","NEXT_SAC_START_X",
              "CURRENT_SAC_END_INTEREST_AREA_LABEL",
              "CURRENT_SAC_START_INTEREST_AREA_LABEL",
              "CURRENT_SAC_END_X_RESOLUTION", 
              "CURRENT_SAC_DIRECTION",
              'NEXT_FIX_DURATION',
              "NEXT_SAC_START_X_RESOLUTION",
              "QUESTION_ACCURACY",
              "QUESTION_KEY_PRESSED",
              "QUESTION_RT",
              "SENTENCE_RT",                                     
              "condition","expected_key","identifier","item")
  }
  if(Exp=="fixation"){
  colsN= c(
    "item",
    "RECORDING_SESSION_LABEL",
    "CURRENT_FIX_START",  
    "CURRENT_FIX_DURATION", 
    "CURRENT_FIX_END", 
    "CURRENT_FIX_X", 
    "CURRENT_FIX_Y",
    "CURRENT_FIX_INTEREST_AREA_LABEL",
    "condition",
    "CURRENT_FIX_INTEREST_AREA_INDEX"
  )
  }
  return(colsN)
}

reNameCols <- function(dat) {
  if(Exp=="prosaccade"){
    dat <- dat %>%
      dplyr::rename(RECORDING_SESSION_LABEL = RECORDING_SESSION_LABEL_l,
                    TRIAL_INDEX=TRIAL_INDEX_l,
                    Group=Group_l,
                    ID=ID_l,
                    direction=direction_l,
                    amplitude=amplitude_l,
                    correctLocation=correctLocation_l,
                    fixDuration=fixDuration_l,
                    instance=instance_l,
                    targetPhysicalLocation=targetPhysicalLocation_l,
                    wrongLocation=wrongLocation_l,
                    CURRENT_SAC_START_TIME.l=CURRENT_SAC_START_TIME.x,
                    CURRENT_SAC_START_TIME.r=CURRENT_SAC_START_TIME.y) %>%
      dplyr::select(!c(RECORDING_SESSION_LABEL_r,
                    TRIAL_INDEX_r,
                    Group_r,
                    ID_r,
                    direction_r,
                    amplitude_r,
                    correctLocation_r,
                    fixDuration_r,
                    instance_r,
                    targetPhysicalLocation_r,
                    wrongLocation_r))
  }
  if(Exp=="word_processing"){
    dat <- dat %>%
      dplyr::rename(RECORDING_SESSION_LABEL = RECORDING_SESSION_LABEL_l,
                    TRIAL_INDEX=TRIAL_INDEX_l,
                    Group=Group_l,
                    ID=ID_l,
                    KEY_PRESSED=KEY_PRESSED_l,
                    RT=RT_l,
                    freq=freq_l,
                    bigram=bigram_l,
                    expected_response=expected_response_l,
                    on=on_l,
                    top_string=top_string_l,
                    trial_number=trial_number_l,
                    trigram=trigram_l,
                    word_length=word_length_l,
                    word_type=word_type_l,
                    exp_type=exp_type_l,
                    top_string=top_string_l,
                    CURRENT_SAC_START_TIME.l=CURRENT_SAC_START_TIME.x,
                    CURRENT_SAC_START_TIME.r=CURRENT_SAC_START_TIME.y) %>%
      dplyr::select(!c(RECORDING_SESSION_LABEL_r,
                TRIAL_INDEX_r,
                Group_r,
                ID_r,
                KEY_PRESSED_r,
                RT=RT_r,
                freq_r,
                bigram_r,
                exp_type_r,
                expected_response_r,
                on_r,
                top_string_r,
                trial_number_r,
                trigram_r,
                word_length_r,
                word_type_r))
  }
  
  if(Exp=="text_reading"){
    dat <- dat %>%
      dplyr::rename(RECORDING_SESSION_LABEL = RECORDING_SESSION_LABEL_l,
                    TRIAL_INDEX=TRIAL_INDEX_l,
                    CURRENT_SAC_DIRECTION=CURRENT_SAC_DIRECTION_l,
                    CURRENT_SAC_START_INTEREST_AREA_LABEL=CURRENT_SAC_START_INTEREST_AREA_LABEL_l,
                    CURRENT_SAC_END_INTEREST_AREA_LABEL=CURRENT_SAC_END_INTEREST_AREA_LABEL_l,
                    CURRENT_SAC_START_INTEREST_AREA_ID=CURRENT_SAC_START_INTEREST_AREA_ID_l,    
                    CURRENT_SAC_START_INTEREST_AREA_INDEX=CURRENT_SAC_START_INTEREST_AREA_INDEX_l,
                    Group=Group_l,
                    Text=text_l,
                    ID=ID_l,
                    QUESTION_ACCURACY=QUESTION_ACCURACY_l,
                    QUESTION_KEY_PRESSED=QUESTION_KEY_PRESSED_l,
                    QUESTION_RT=QUESTION_RT_l, 
                    SENTENCE_RT=SENTENCE_RT_l,
                    Condition=condition_l,
                    expected_key=expected_key_l,
                    identifier=identifier_l,
                    item=item_l,
                    CURRENT_SAC_START_TIME.l=CURRENT_SAC_START_TIME.x,
                    CURRENT_SAC_START_TIME.r=CURRENT_SAC_START_TIME.y) %>%
      dplyr::select(!c(RECORDING_SESSION_LABEL_r,
                TRIAL_INDEX_r,
                CURRENT_SAC_DIRECTION_r,
                CURRENT_SAC_START_INTEREST_AREA_LABEL_r,
                CURRENT_SAC_END_INTEREST_AREA_LABEL_r,
                CURRENT_SAC_START_INTEREST_AREA_ID_r,
                CURRENT_SAC_START_INTEREST_AREA_INDEX_r,
                Group_r,
                text_r,
                ID_r,
                QUESTION_ACCURACY_r,
                QUESTION_KEY_PRESSED_r,
                QUESTION_RT_r, 
                SENTENCE_RT_r,
                condition_r,
                expected_key_r,
                identifier_r,
                item_r))
  }
  
  return(dat)
}

#https://statsandr.com/blog/outliers-detection-in-r/#additional-remarks
rm.outliers <- function(dat=dat, method="percentile", measure=measure) {
  N.row= nrow(dat)
  par(mfrow=c(2,2))
  boxplot(dat$measure,ylab = measure)
  hist(dat$measure)
  if(method=="iqr"){
    Q1 <- quantile(dat$measure, .25)
    Q3 <- quantile(dat$measure, .75)
    IQR <- IQR(dat$measure)
    #outlier_ind <- subset(dat, dat$measure<(Q1-1.5*IQR)|dat$measure>(Q3+1.5*IQR))
    outlier_ind <- which(dat$measure<(Q1-1.5*IQR)|dat$measure>(Q3+1.5*IQR))
  }
  
  if(method=="percentile"){
    lower_bound <- quantile(dat$measure, 0.01)
    upper_bound <- quantile(dat$measure, 0.99)
    outlier_ind <- which(dat$measure < lower_bound | dat$measure > upper_bound)
  }
  if(method== "zscore"){
    dat$z_measure <- scale(dat$measure)
    outlier_ind <- which(dat$z_measure > 3.29)
  }
  if(method== "hampel"){ 
    lower_bound <- median(dat$measure) - 3 * mad(dat$measure, constant = 1)
    upper_bound <- median(dat$measure) + 3 * mad(dat$measure, constant = 1)
    outlier_ind <- which(dat$measure < lower_bound | dat$measure > upper_bound)
  }
  dat <- dat[-outlier_ind, ]
  (outlierpercent <- 100-((nrow(dat)*100)/N.row))
  print(paste0('data missing(%)= ', outlierpercent))
  boxplot(dat$measure,ylab = measure)
  hist(dat$measure)
  return_list <- list(dat,outlierpercent)
  return(return_list)
}  

#' Creates tables showing descriptive results
#' ------------------------------------------
#' @param data       Your original data.  
#' @param cond1      Column name for the first factor
#' @param cond2      Column name for the second factor
#' @param cond3      Column name for the third factor   
#' 

desc1 <- function (data,cond1,measure) {
  colnames(data)[which(names(data) == measure)] <- "measure"
  condN1= cond1
  colnames(data)[which(names(data) == cond1)] <- "cond1"
  table = ddply(data, .(cond1), summarise, N=length(measure),
                Mean=mean(measure, na.rm=TRUE), 
                SD=sd(measure, na.rm=TRUE),SE=SD/sqrt(N),
                lower.bound=Mean-((qt(p=0.05/2, df=N-1,lower.tail=F)) * SE),
                upper.bound=Mean+((qt(p=0.05/2, df=N-1,lower.tail=F)) * SE),
                CI=SE*qt(.975, N-1),
                CI_min= Mean-1.96*SD/sqrt(N),
                CI_max= Mean+1.96*SD/sqrt(N),
                Value.min= Mean+CI, Value.max= Mean-CI)
  colnames(table)[which(names(table) == "cond1")] <- condN1
  return(table) 
}

desc2 <- function (data,cond1,cond2, measure) {
  colnames(data)[which(names(data) == measure)] <- "measure"
  condN1= cond1; condN2= cond2
  colnames(data)[which(names(data) == cond1)] <- "cond1"
  colnames(data)[which(names(data) == cond2)] <- "cond2"
  table = ddply(data, .(cond1, cond2), summarise, N=length(measure),
                Mean=mean(measure, na.rm=TRUE), 
                SD=sd(measure, na.rm=TRUE),SE=SD/sqrt(N),
                lower.bound=Mean-((qt(p=0.05/2, df=N-1,lower.tail=F)) * SE),
                upper.bound=Mean+((qt(p=0.05/2, df=N-1,lower.tail=F)) * SE),
                CI=SE*qt(.975, N-1),
                CI_min= Mean-1.96*SD/sqrt(N),
                CI_max= Mean+1.96*SD/sqrt(N),
                Value.min= Mean+CI, Value.max= Mean-CI)
  colnames(table)[which(names(table) == "cond1")] <- condN1
  colnames(table)[which(names(table) == "cond2")] <- condN2
  return(table) 
}

desc3 <- function (data,cond1,cond2,cond3, measure) {
  colnames(data)[which(names(data) == measure)] <- "measure"
  condN1= cond1; condN2= cond2; condN3= cond3
  colnames(data)[which(names(data) == cond1)] <- "cond1"
  colnames(data)[which(names(data) == cond2)] <- "cond2"
  colnames(data)[which(names(data) == cond3)] <- "cond3"
  table = ddply(data, .(cond1, cond2,cond3), summarise, N=length(measure),
                Mean=mean(measure, na.rm=TRUE), 
                SD=sd(measure, na.rm=TRUE),SE=SD/sqrt(N),
                lower.bound=Mean-((qt(p=0.05/2, df=N-1,lower.tail=F)) * SE),
                upper.bound=Mean+((qt(p=0.05/2, df=N-1,lower.tail=F)) * SE),
                CI=SE*qt(.975, N-1),
                CI_min= Mean-1.96*SD/sqrt(N),
                CI_max= Mean+1.96*SD/sqrt(N),
                Value.min= Mean+CI, Value.max= Mean-CI)
  colnames(table)[which(names(table) == "cond1")] <- condN1
  colnames(table)[which(names(table) == "cond2")] <- condN2
  colnames(table)[which(names(table) == "cond3")] <- condN3
  return(table) 
}

#' Drawing a line plot based on descriptive results
#' ------------------------------------------------
#' see function decriptive() for descriptive results
#' Comparing two different tables (ie. data1 and data2)
#' 
#' @param data1      first descriptive table  
#' @param data2      first descriptive table
#' @param header1    header of first plot
#' @param header2    header of second plot
#' @param lev1       first level name of the factor 
#' @param lev2       second level name of the factor
#' @param measure    the name of y axis
#' 

barchart1 <- function(data1= table.1,lev1,lev2, yLab) {
  MinList <- list(a= c(data1$Value.max[1], data1$Value.max[2]))
  MaxList <- list(a= c(data1$Value.min[1], data1$Value.min[2]))
  maxvalue <- lapply(MaxList, function(x) x[which.max(x)])
  minvalue <- lapply(MinList, function(x) x[which.min(x)])
  #maxvalue <- lapply(MaxList, function(x) x[which.max(abs(x))])
  #minvalue <- lapply(MinList, function(x) x[which.min(abs(x))])
  maxvalue <- as.numeric(maxvalue)
  minvalue <- as.numeric(minvalue)
  d1 <- colnames(data1)
  names(data1)[names(data1) == d1[1]] <- 'cond1' 
  data1$cond1 = as.factor(data1$cond1)
  levels(data1$cond1) <- c(lev1, lev2)
  library(ggplot2)
  plot.1 <- ggplot(data1, aes(x = cond1, y = Mean, color=cond1)) +
    #facet_wrap(~cond2)+
    geom_errorbar(aes(ymin = CI_min, ymax = CI_max), width = 0.15, size = 1) +
    geom_point(shape = 15,size  = 4) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(minvalue, maxvalue),
                       labels = scales::number_format(accuracy = 0.001)) +
    #scale_y_continuous(limits = c(0, 1)) +
    theme_light() + theme(legend.position = "none")  + 
    xlab("") + 
    ylab(yLab) +
    theme(plot.title = element_text(size=12))
  return(plot.1)
}

barchart2 <- function(data1= table.1,lev1,lev2,lev3,lev4, yLab) {
  MinList <- list(a= c(data1$Value.max[1], data1$Value.max[2],
                       data1$Value.max[3], data1$Value.max[4]))
  MaxList <- list(a= c(data1$Value.min[1], data1$Value.min[2],
                       data1$Value.min[3], data1$Value.min[4]))
  maxvalue <- lapply(MaxList, function(x) x[which.max(x)])
  minvalue <- lapply(MinList, function(x) x[which.min(x)])
  #maxvalue <- lapply(MaxList, function(x) x[which.max(abs(x))])
  #minvalue <- lapply(MinList, function(x) x[which.min(abs(x))])
  maxvalue <- as.numeric(maxvalue)
  minvalue <- as.numeric(minvalue)
  d1 <- colnames(data1)
  names(data1)[names(data1) == d1[1]] <- 'cond1' 
  names(data1)[names(data1) == d1[2]] <- 'cond2' 
  data1$cond1 = as.factor(data1$cond1)
  data1$cond2 = as.factor(data1$cond2)
  levels(data1$cond1) <- c(lev1, lev2)
  levels(data1$cond2) <- c(lev3, lev4)
  library(ggplot2)
  plot.1 <- ggplot(data1, aes(x = cond1, y = Mean, color=cond1)) +
    facet_wrap(~cond2)+
    geom_errorbar(aes(ymin = CI_min, ymax = CI_max), width = 0.15, size = 1) +
    geom_point(shape = 15,size  = 4) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(minvalue, maxvalue),
                       labels = scales::number_format(accuracy = 0.001)) +
    #scale_y_continuous(limits = c(0, 1)) +
    theme_light() + theme(legend.position = "none")  + 
    xlab("") + 
    ylab(yLab) +
    theme(plot.title = element_text(size=12))
  return(plot.1)
}


barchart3 <- function(data1= table.1,lev1,lev2,lev3,lev4,lev5,lev6,yLab) {
  MinList <- list(a= c(data1$Value.max[1], data1$Value.max[2],
                       data1$Value.max[3], data1$Value.max[4],
                       data1$Value.max[5], data1$Value.max[6],
                       data1$Value.max[7], data1$Value.max[8]))
  MaxList <- list(a= c(data1$Value.min[1], data1$Value.min[2],
                       data1$Value.min[3], data1$Value.min[4],
                       data1$Value.min[5], data1$Value.min[6],
                       data1$Value.min[7], data1$Value.min[8]))
  maxvalue <- lapply(MaxList, function(x) x[which.max(x)])
  minvalue <- lapply(MinList, function(x) x[which.min(x)])
  #maxvalue <- lapply(MaxList, function(x) x[which.max(abs(x))])
  #minvalue <- lapply(MinList, function(x) x[which.min(abs(x))])
  maxvalue <- as.numeric(maxvalue)
  minvalue <- as.numeric(minvalue)
  d1 <- colnames(data1)
  names(data1)[names(data1) == d1[1]] <- 'cond1' 
  names(data1)[names(data1) == d1[2]] <- 'cond3' 
  names(data1)[names(data1) == d1[3]] <- 'cond2' 
  data1$cond1 = as.factor(data1$cond1)
  data1$cond2 = as.factor(data1$cond2)
  data1$cond3 = as.factor(data1$cond3)
  
  levels(data1$cond1) <- c(lev1, lev2)
  levels(data1$cond2) <- c(lev3, lev4)
  levels(data1$cond3) <- c(lev5, lev6)
  library(ggplot2)
  plot.1 <- ggplot(data1, aes(x = cond1, y = Mean, color=cond1)) +
    facet_grid(cond3~cond2)+ 
    geom_errorbar(aes(ymin = CI_min, ymax = CI_max), width = 0.15, size = 1) +
    geom_point(shape = 15,size  = 4) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(minvalue, maxvalue),
                       labels = scales::number_format(accuracy = 0.001)) +
    #scale_y_continuous(limits = c(0, 1)) +
    theme_light() + theme(legend.position = "none") + 
    xlab("") + ylab(yLab) +
    theme(plot.title = element_text(size=12))
  return(plot.1)
}


emmipFig2 <- function(ModelName, leg.Value,x.Value,
                       leg.Name,x.Name) {
  if(x.Value=='Group.sl'){val3='Dyslexia';val4='TD'}
  if(x.Value=='direction.sl'){val3='Right';val4='Left'}
  if(leg.Value=='Group.sl'){val5='Dyslexia';val6='TD'}
  if(leg.Value=='direction.sl'){val5='Right';val6='Left'}
  
  Frml= as.formula(paste0(leg.Value,"~",x.Value))
  a= emmip(ModelName,Frml, CIs=TRUE) + 
    xlab(x.Name) +
    #ylab("Doğrusal Tahmin") +
    guides(color = guide_legend(title = leg.Name)) +
    scale_x_continuous(breaks=c(-0.5,0.5), labels = c(val3, val4)) +
    theme_classic() +
    theme(legend.position="bottom")+
    scale_color_hue(labels = c(val5, val6)) 
}

emmipFig3 <- function(ModelName, leg.Value,x.Value,facet.Value,
                       leg.Name,x.Name,facet.Name) {
  if(facet.Value=='Group.sl'){val1='Dyslexia';val2='TD'}
  if(facet.Value=='OLD20.sl'){val1='Low';val2='High'}
  if(facet.Value=='Frequency.sl'){val1='Low';val2='High'}
  if(facet.Value=='direction.sl'){val1='Right';val2='Left'}
  
  if(x.Value=='Group.sl'){val3='Dyslexia';val4='TD'}
  if(x.Value=='OLD20.sl'){val3='Low';val4='High'}
  if(x.Value=='Frequency.sl'){val3='Low';val4='High'}
  if(x.Value=='direction.sl'){val3='Right';val4='Left'}
  
  if(leg.Value=='Group.sl'){val5='Dyslexia';val6='TD'}
  if(leg.Value=='OLD20.sl'){val5='Low';val6='High'}
  if(leg.Value=='Frequency.sl'){val5='Low';val6='High'}
  if(leg.Value=='direction.sl'){val5='Right';val6='Left'}
  
  custom_labels <- as_labeller(function(x){
    return(paste0(facet.Name,": ", c(val1, val2)))})
  Frml2= as.formula(paste0("~",facet.Value))
  Frml1= as.formula(paste0(leg.Value,"~",x.Value,"|",facet.Value))
  a= emmip(ModelName,Frml1,CIs=TRUE) + 
    xlab(x.Name) +
    #ylab("Doğrusal Tahmin") +
    guides(color = guide_legend(title = leg.Name)) +
    scale_x_continuous(breaks=c(-0.5, 0.5), labels = c(val3, val4)) +
    facet_wrap(Frml2, labeller= custom_labels, strip.position = "top", ncol = 2) +
    theme_classic() +
    theme(legend.position="bottom") +
    scale_color_hue(labels = c(val5, val6)) 
  return(a)
}


emmeansFig <- function(fit,facet.Value,x.Value,facet.Name,x.Name){
  if(facet.Value=='Group.sl'){val1='Dyslexia';val2='TD'}
  if(facet.Value=='OLD20.sl'){val1='Low';val2='High'}
  if(facet.Value=='Frequency.sl'){val1='Low';val2='High'}
  if(facet.Value=='direction.sl'){val1='Right';val2='Left'}
  Frml= as.formula(paste0("~",facet.Value))
  custom_label <- as_labeller(function(x){
    return(paste0(facet.Name, ": ", c(val1, val2)))})
  a = plot(fit, comparisons = TRUE) + 
    ylab(x.Name) + 
    facet_wrap(Frml, labeller= custom_label, strip.position = "right", ncol = 1) +
    theme_bw()
  return(a)
}
