#' Organisation of saccade data
#' ============================
   rm(list = ls())
   closeAllConnections()

#' Libraries
#' ---------
   library(plyr)
   library(tidyverse)
   library(lme4)
   library(lmerTest)
   library(AICcmodavg)
   library(effectsize)
   library(bestNormalize)
   library(emmeans)
   library(rstudioapi)
   source("source/utility.R")

# 'Select the direction:
#' --------------------  
   direction =   c( "horizontal",    # 1 
                    "vertical"       # 2 
  )[1] # CHANGE THIS NUMBER AND RUN THE CODE
   
# 'Select the measure type:
#' -----------------------  
   measure =   c( "saccade_disconjugacy",         # 1 
                  "postsaccadic_drift.res",       # 2
                  "CURRENT_SAC_AMPLITUDE_r",      # 3
                  "CURRENT_SAC_DURATION_r",       # 4
                  "CURRENT_SAC_AVG_VELOCITY_r",   # 5
                  "CURRENT_SAC_PEAK_VELOCITY_r",  # 6
                  "saccade_version"               # 7
  )[5] # CHANGE THIS NUMBER AND RUN THE CODE
   
   if(measure== "saccade_disconjugacy") {measure.label="Saccadic Disconjugacy (deg)"}
   if(measure== "postsaccadic_drift.res"){measure.label="Postsaccadic Drift (ppd)"}
   if(measure== "CURRENT_SAC_AMPLITUDE_r"){measure.label="Amplitude (deg)"}
   if(measure== "CURRENT_SAC_DURATION_r"){measure.label="Time (ms)"}
   if(measure== "CURRENT_SAC_AVG_VELOCITY_r"){measure.label="Avr. Velocity Hız (deg/sec)"}    
   if(measure== "CURRENT_SAC_PEAK_VELOCITY_r"){measure.label="Peak Velocity (deg/sec)"} 
   if(measure== "saccade_version"){measure.label="Saccade Version (deg)"} 
   
#' Determine the working directory
#' -------------------------------
   current_path = getActiveDocumentContext()$path 
   rootdir = setwd(dirname(current_path))
   result_file = file.path(rootdir, paste0("results/prosaccade/",measure,"_",direction,".txt"))
   FigFile = function(FigN) {
     fig_file = paste0(rootdir, "/plots/prosaccade/",measure,"_",direction,"_",FigN,".jpg")
     return(fig_file)}

#' Load the data 
#' -------------  
   df  <- read.table("data/merged/prosaccade/left_right.txt", 
                          sep="\t", header=TRUE, encoding = "UTF-8",na.strings = c(".", "NA"))
   head(df)
   str(df)

#' Transform to factor
#' -------------------   
   df <- df %>% dplyr::mutate_at(c('Group','ID','direction','TRIAL_INDEX'), as.factor)

#' Data elimination according to the direction you choose
#' -------------------------------------------------------  
   unique(df$CURRENT_SAC_DIRECTION_r)
   if(direction=="horizontal"){
     df <- df[(df$CURRENT_SAC_DIRECTION_r %in% c("RIGHT","LEFT")), ]}
   if(direction=="vertical"){
     df <- df[(df$CURRENT_SAC_DIRECTION_r %in% c("UP","DOWN")), ]}
   df[] <- lapply(df, function(x) if(is.factor(x)) factor(x) else x)
   unique(df$CURRENT_SAC_DIRECTION_r)
   str(df)
   
#' Plots of saccade_disconjugacy and postsaccadic_drift.res
#' --------------------------------------------------------
   df.alt=df[df$postsaccadic_drift.res<6,]
   library("ggpubr")
   fig.A=ggscatter(df.alt, x='saccade_disconjugacy', y="postsaccadic_drift.res", 
                   point = F,
                   color = "Group" ,shape = 1, size = 1.5,
                   add.params = list(color = "Group", fill = "lightgray"), 
                   add = "reg.line", conf.int = T, cor.coef = F, 
                   #cor.method = "pearson", #"spearman"
                   xlab = 'Sakkad Uyumsuzluğu (derece)', ylab = "Sakkad-sonrası Sürüklenme (ppd)")
 
#' Select the column
#' -----------------
   colnames(df)[which(names(df) == measure)] <- "measure"
   
#' Descriptive results
#' -------------------
   source("source/utility.R")
   (table = desc2(df,'Group', 'direction', measure ))
   jpeg(file=FigFile("2"),  units="cm", width=12, height=7, res=300)
     barchart2(table,'TD','Dyslexia','Left target','Right target', measure.label)
   dev.off()

#' Normalization transformation
#' ----------------------------
   # The bestNormalize package contains a suite of transformation-estimating 
   # functions that can be used to normalize data. The function of the same name 
   # attempts to find and execute the best of all of these potential 
   # normalizing transformations.
   # see https://cran.r-project.org/web/packages/bestNormalize/
   normalized = df
   (BNobject <- bestNormalize(normalized$measure))
   df$measure  <- orderNorm(normalized$measure)$x.t

#' LME analyzing
#' -------------
#' Applying contrast coding in order to more closely match the  inferences drawn from ANOVA. This was done 
#' by converting each predictor variable into a numeric variable with the values of –0.5 and 0.5. 
   df$Group.sl    <- ifelse(df$Group == "C", 0.5, -0.5)
   df$direction.sl <- ifelse(df$direction == "left", 0.5, -0.5)
   
#' Formula of Linear Mixed Effect analysis (general)
   #install.packages('buildmer', type="source")
   library(buildmer)
   max_mod <- buildmer(measure~Group.sl*direction.sl 
                       + WISC_VCI
                       + WISC_WMI 
                       + Conners_T_ADHD_index
                       + (1 + Group.sl*direction.sl|ID) 
                       + (1 + Group.sl*direction.sl|TRIAL_INDEX)
                       , data=df
                       , buildmerControl=buildmerControl(include=~ Group.sl*direction.sl, calc.anova = TRUE, ddf = "Satterthwaite"))
   (originalGenModel=summary(max_mod))
   (f <- formula(max_mod@model))
  
   fit.mod.gen_A <- lmer(measure ~ 1 + Group.sl + direction.sl + Group.sl:direction.sl 
                       + WISC_WMI 
                       + Conners_T_ADHD_index 
                       + (1 | ID) 
                       , data= df)
   (LmerTestSumGen_A=summary(fit.mod.gen_A))   
   
   fit.mod.gen_B <- lmer(measure ~ 1 + Group.sl + direction.sl + Group.sl:direction.sl 
                         + WISC_VCI 
                         + Conners_T_ADHD_index 
                         + (1 | ID) 
                         , data= df)
   (LmerTestSumGen_B=summary(fit.mod.gen_B))   
   
   fit.mod.gen_C <- lmer(measure ~ 1 
                         + log(CURRENT_SAC_AMPLITUDE_r)+Group.sl+direction.sl 
                         + log(CURRENT_SAC_AMPLITUDE_r):Group.sl 
                         + log(CURRENT_SAC_AMPLITUDE_r):direction.sl
                         + WISC_VCI
                         + WISC_WMI 
                         + Conners_T_ADHD_index
                         + (1 | ID) 
                         , data= df)
   (LmerTestSumGen_C=summary(fit.mod.gen_C))   

#' Plots of Dislexia Group
#' ------------------------
   library("ggpubr")
   fig.1=ggscatter(df[df$Group=="D",], x = "measure", y =  "SOBAT_total_standard", 
                   point = F,
                   #color = 'lightgray' ,shape = 1, size = 1.5,
                   add.params = list(color = "direction", fill = "lightgray"), 
                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                   cor.method = "pearson", #"spearman"
                   xlab = measure, ylab = "SOBAT_total_standard")
   fig.2=ggscatter(df[df$Group=="D",], x = "measure", y =  "PAS.total", 
                   point = F,
                   #color = 'lightgray' ,shape = 1, size = 1.5,
                   add.params = list(color = "direction", fill = "lightgray"), 
                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                   cor.method = "pearson", #"spearman",
                   xlab = measure, ylab = "PAS.total")
   fig.3=ggscatter(df[df$Group=="D",], x = "measure", y =  "RAN.mean", 
                   point = F,
                   #color = 'lightgray' ,shape = 1, size = 1.5,
                   add.params = list(color = "direction", fill = "lightgray"), 
                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                   cor.method = "pearson", # "spearman"
                   xlab = measure, ylab = "RAN.mean")
   fig.4=ggscatter(df[df$Group=="D",], x = "measure", y =  "MOYA_parent_total", 
                   point = F,
                   #color = 'lightgray' ,shape = 1, size = 1.5,
                   add.params = list(color = "direction", fill = "lightgray"), 
                   add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
                   cor.method = "pearson", #"spearman"
                   xlab = measure, ylab = "MOYA_parent_total")
   fig <- ggpubr::ggarrange(fig.1,fig.2,fig.3,fig.4,common.legend=TRUE,
                                legend="bottom",ncol = 2, nrow = 2)
   jpeg(file=FigFile("3"),  units="cm", width=14, height=14, res=300)
     fig
   dev.off()

   fig.5=ggscatter(df[df$direction=="right",], x = "measure", y =  "CURRENT_SAC_AMPLITUDE_r", 
                   point = F,
                   title='Right',
                   color = 'lightgray' ,shape = 1, size = 1.5,
                   add.params = list(color = "Group", fill = "lightgray"), 
                   add = "reg.line", conf.int = TRUE, cor.coef = F, 
                   cor.method = "pearson", #"spearman"
                   xlab = measure.label, ylab = "Amplitude (deg)")
   fig.6=ggscatter(df[df$direction=="left",], x = "measure", y =  "CURRENT_SAC_AMPLITUDE_r", 
                   point = F,
                   title='Left',
                   color = 'lightgray' ,shape = 1, size = 1.5,
                   add.params = list(color = "Group", fill = "lightgray"), 
                   add = "reg.line", conf.int = TRUE, cor.coef = F, 
                   cor.method = "pearson", #"spearman"
                   xlab = measure.label, ylab = "Amplitude (deg)")
   fig <- ggpubr::ggarrange(fig.5,fig.6,common.legend=TRUE,
                            legend="bottom",ncol = 2, nrow = 1)
   
   
   jpeg(file=FigFile("3b"),  units="cm", width=15, height=7, res=300)
   fig
   dev.off()
   
   
#' Formula of Linear Mixed Effect analysis (only Dislexia Group)
#' -------------------------------------------------------------
   max_mod <- buildmer(measure~direction.sl 
                       + SOBAT_total_standard
                       + PAS.total 
                       + RAN.mean
                       + MOYA_parent_total
                       + (1 + direction.sl|ID) 
                       + (1 + direction.sl|TRIAL_INDEX)
                       , data=df[df$Group=="D",]
                       , buildmerControl=buildmerControl(include=~ direction.sl, calc.anova = TRUE, ddf = "Satterthwaite"))
   (originalDisModel=summary(max_mod))
   (f <- formula(max_mod@model))

   fit.mod.dis <- lmer(measure ~ 1 + direction.sl 
                       + PAS.total + RAN.mean + MOYA_teacher_total + SOBAT_total_standard
                       + (1 | ID) 
                      , data=df[df$Group=="D",])
   (LmerTestSumDis=summary(fit.mod.dis))   

#' Compute the effect sizes
#' ------------------------
   fit.mod.gen_A_eff1 <- eta_squared(fit.mod.gen_A,partial=TRUE)
   fit.mod.gen_A_eff2 <- epsilon_squared(fit.mod.gen_A,partial=TRUE)
   fit.mod.gen_B_eff1 <- eta_squared(fit.mod.gen_B,partial=TRUE)
   fit.mod.gen_B_eff2 <- epsilon_squared(fit.mod.gen_B,partial=TRUE)
   fit.mod.dis_eff1 <- eta_squared(max_mod,partial=TRUE)
   fit.mod.dis_eff2 <- epsilon_squared(max_mod,partial=TRUE)

#' Analyzing normally distributed data
#' -----------------------------------
   jpeg(file=FigFile("1_A"),  units="cm", width=12, height=10, res=300)
     par(mfrow=c(1,2))
     plot(fitted(fit.mod.gen_A),residuals(fit.mod.gen_B))
     qqnorm(residuals(fit.mod.gen_A)) 
     qqline(residuals(fit.mod.gen_A), col = "steelblue", lwd = 2)
   dev.off()
   
   jpeg(file=FigFile("1_B"),  units="cm", width=12, height=10, res=300)
     par(mfrow=c(1,2))
     plot(fitted(fit.mod.gen_B),residuals(fit.mod.gen_B))
     qqnorm(residuals(fit.mod.gen_B)) 
     qqline(residuals(fit.mod.gen_B), col = "steelblue", lwd = 2)
   dev.off()

#' Post-hoc analysis
#' ----------------- 
   #f= emmeans(fit.model, ~Group.sl*direction.sl, lmer.df = "asymp")
   #(pairwise = pairs(f))
   fitA1 <- emmeans(fit.mod.gen_A, "Group.sl", by = "direction.sl")
   fitA2 <- emmeans(fit.mod.gen_A, "direction.sl", by = "Group.sl")
   (pairwise.A1 = pairs(fitA1))
   (pairwise.A2 = pairs(fitA2))
   
   fitB1 <- emmeans(fit.mod.gen_B, "Group.sl", by = "direction.sl")
   fitB2 <- emmeans(fit.mod.gen_B, "direction.sl", by = "Group.sl")
   (pairwise.B1 = pairs(fitB1))
   (pairwise.B2 = pairs(fitB2))


#' Plotting of the post-hoc analysis
#' ---------------------------------- 
   # ModelName, leg.Value,x.Value,facet.Value,
   #            leg.Name,x.Name,facet.Name
   # 0.5= C (Control), -0.5= D (dislexia)
   # 0.5= left,        -0.5= right
   
   source("source/utility.R")
   Fig1=emmipFig2(fitA1, 'Group.sl','direction.sl','Group','Target Direction')
   Fig2=emmipFig2(fitA2, 'direction.sl','Group.sl','Target Direction','Group')
   jpeg(file=FigFile("4_A"),  units="cm", width=7.5, height=7.5, res=300)
      Fig1
   dev.off()
   jpeg(file=FigFile("5_A"),  units="cm", width=7.5, height=7.5, res=300)
      Fig2
   dev.off()
   
   Fig3=emmipFig2(fitB1, 'Group.sl','direction.sl','Group','Target Direction')
   Fig4=emmipFig2(fitB2, 'direction.sl','Group.sl','Target Direction','Group')
   jpeg(file=FigFile("4_B"),  units="cm", width=7.5, height=7.5, res=300)
      Fig3
   dev.off()
   jpeg(file=FigFile("5_B"),  units="cm", width=7.5, height=7.5, res=300)
      Fig4
   dev.off()

#' Writing  the informations to an output file
#' ------------------------------------------
   sink(result_file)
   
   cat("\n\n", "~~~~~~~ Notes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")
   cat("\n", "direction.sl-0.5 : right")
   cat("\n", "direction.sl 0.5 : left")
   cat("\n", "Group.sl-0.5 : dislexia")
   cat("\n", "Group.sl 0.5 : typic")
   cat("\n\n", "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")

   cat("\n\n", "~~~~~~~ Features ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")
   cat("\n", "Measure Type :", measure)
   cat("\n\n", "~~~~~~~ Normalization transformation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   BNobject
   cat("\n\n", "~~~~~~~ Descriptive results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   knitr::kable(table, digits = 2)
   
   cat("\n\n", "~~~~~~~ Original LME results (all groups) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")
   cat("\n\n", "Bu model 'buildmer' paketinin grup karsilastirmasi icin urettgi modelidir ~~~~~~","\n\n")
   originalGenModel
   
   cat("\n\n", "~~~~~~~ LME results (all groups, WISC_WMI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   LmerTestSumGen_A
   cat("\n\n", "~~~~~~~ LME results (all groups, WISC_VCI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   LmerTestSumGen_B
   cat("\n\n", "~~~~~~~ LME results (all groups, ampitude) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   LmerTestSumGen_C

   cat("\n\n", "~~~~~~~ Effect size (eta2, all groups, WISC_WMI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   fit.mod.gen_A_eff1
   cat("\n\n", "~~~~~~~ Effect size (epsilon, all groups, WISC_WMI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   fit.mod.gen_A_eff2
  
   cat("\n\n", "~~~~~~~ Effect size (eta2, all groups, WISC_VCI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   fit.mod.gen_B_eff1
   cat("\n\n", "~~~~~~~ Effect size (epsilon, all groups, WISC_VCI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   fit.mod.gen_B_eff2
   
   cat("\n\n", "~~~~~~~ Pairwise (all groups, WISC_WMI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   knitr::kable(pairwise.A1, digits = 2)
   knitr::kable(pairwise.A2, digits = 2)
   
   cat("\n\n", "~~~~~~~ Pairwise (all groups, WISC_VCI) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   knitr::kable(pairwise.B1, digits = 2)
   knitr::kable(pairwise.B2, digits = 2)
   
   cat("\n\n", "~~~~~~~ Original LME results (Dislexia) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")
   cat("\n\n", "Bu model 'buildmer' paketinin Ds'leks' grubu icin urettgi modelidir ~~~~~~~~~~~~","\n\n")
   originalDisModel
   
   cat("\n\n", "~~~~~~~ LME results (Dislexia) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   LmerTestSumDis

   cat("\n\n", "~~~~~~~ Effect size (eta2, dislexia) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   fit.mod.dis_eff1
   cat("\n\n", "~~~~~~~ Effect size (epsilon, dislexia) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n\n")
   fit.mod.dis_eff2

   sink()
   
   
   