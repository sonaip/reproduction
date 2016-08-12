# redo the figure without color
# R code for combining analysis and manuscript together, chunkread with 'Reproduction_manuscript.rmd'
# DATA: two parts--one from major experiment of four experiments 'data4exps.mat'; one from additional experiment of comparison 'data_combined'.
# ? table display problem from knit 
# 


## ---- loadData ----
# load necessary packages
source('loadPackages.R')

#clean workspace
# rm(list=ls()) 

# read matlab data and add variable names
d = readMat("data4exps.mat")
dat = as.data.table(d$ds) # read out the matrix
setnames(dat,c('V3','V5','V7','V8','V9','V10','V11','V12','V13'),
         c('duration','production','reproduction','exp','sub','durn1_diff'
           ,'rep_err','prod_err','outlier'))
# turn sub to unique sub
dat$sub <- dat$sub + dat$exp*20
dat$sub <- factor(dat$sub)
dat$exp <- factor(dat$exp, labels = c('Vis/Mix','Vis/Block','Aud/Mix','Aud/Block'))
dat$prod_err[dat$prod_err<0 | dat$prod_err >1] <- NA 
# those negative or more than 1s production errors mean subject release keys before ending ('lazy' trials)
# or lapse of attention
dat <- within(dat, trialn1 <- factor(sign(durn1_diff),labels=c('Longer','Equal','Shorter')))
# recalculate outlier ???
# dat <- within(dat, outlier <- abs(rep_err)/duration > 2)
# original matlab outlier: reproduction is greater than double of duration or less than half.
# vdat = dat[dat$outlier == 0,]  # valid data
# vdat$sub <- factor(vdat$sub)

# control legends etc. theme
lpos <-function(l) {
  theme( plot.title = element_text(face="bold",size = 10),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = l,
         legend.title = element_blank(),
         legend.text = element_text(size=8),
         legend.key = element_blank() # switch off the rectangle around symbols
         
  )
  
}

# outliers 
# examine the proportion of outliers, and print prop. errors
msOutlier <- dat %>% group_by(exp, sub) %>% summarise(p = mean(outlier))
msOutlier$sub <- factor(msOutlier$sub)
outlier_threshold = 0.2

dat <- left_join(dat,msOutlier, key='sub')
## remove outlier participants
dat$outlier <- (dat$outlier | dat$p>outlier_threshold)

# keep valid data into 'vdat'

vdat = dat[dat$outlier==0]
vdat$sub <-droplevels(vdat$sub)

# change data into ms scale, for further logarithmic calculation
vdat$duration <- vdat$duration*1000
vdat$reproduction <- vdat$reproduction*1000
vdat$rep_err <- vdat$rep_err*1000

## ---- outliers_figure ----
 ggplot(data=msOutlier, aes(x=exp, y=mp,  width=0.5)) +
  geom_errorbar(aes(ymin = mp - sp/sqrt(n), ymax = mp + sp/sqrt(n)), width=0.2, 
                position=position_dodge(width=0.8)) +
  geom_bar(stat="identity", position="dodge", width=0.8) +
  theme_bw() +lpos(c(0.2,0.8)) + labs(x="Experiments",y="Prop. Error Trials")
 ggsave('outliers.pdf')

 
## ---- mean_reproduction ----
# Choose color and lightness for modalities and conditions
# subject-wise

 msRepr <- vdat  %>% 
   group_by(exp, duration,sub) %>% 
   summarise(mRepr = mean(reproduction), sdRepr = sd(reproduction), 
             mPrErr = mean(prod_err,na.rm=TRUE), sdProd = sd(prod_err,na.rm=TRUE), 
             mRepErr = mean(rep_err), sdRepErr = sd(rep_err))  %>% 
   arrange(exp, duration,sub)
 
 msRepr$cv = msRepr$sdRepr/msRepr$mRepr
 msRepr$dur <- factor(msRepr$duration,
                      labels = c('0.3', '0.5', '0.8', '1.3', '2.2', '3.6', '5.9', '9.7', '16'))

 
 # grand average (collapse subjects)
 mRepr <- msRepr %>% group_by(exp, duration) %>% 
   summarise(mRepr = mean(mRepr),  mCV = mean(cv), msdProd = mean(sdRepr), 
             mPrErr = mean(mPrErr),
             mRepErr = mean(mRepErr))  
 
 # Change colours for basic plot 
 library(RColorBrewer)
 myPairs <- brewer.pal(9, "Paired")[c(6,5,2,1)]
 
 # put basic figure against time into 'lineFig'
 
lineFig <- ggplot(mRepr, aes(x=duration, y = mRepr, colour = exp, group = exp, shape = exp, linetype = exp)) + 
geom_point(size =3) +
  scale_colour_manual(values = c("black","black","#969696","#969696")) + # darker grey
  scale_x_continuous('Durations (ms)', trans = "log2") +
  scale_shape_manual(values = c(15,0,17,2))+
  scale_linetype_manual(values = c(1,12,1,12))+
 #  scale_y_continuous('Mean reproduction (s)') +
   theme_classic() +
   lpos(c(0.2,0.8))

# reproduction error figure, but for further use...
lineRepError <- lineFig + aes(x=duration, y=mRepErr, group = exp) +
  geom_abline(aes(yintercept = 0,slope = 0), linetype = 9) +
  lpos(c(0.2,0.2)) + 
  labs(x="Durations (s)",y="Mean Reproduction Errors (s)")



## ---- mean_reproduction_figure ----
lineFig + #geom_line()+
  geom_abline(aes(yintercept = 0, slope = 1),linetype = 9,xlim = c(min(mRepr$duration),16)) +
  scale_y_continuous('Mean reproduction (ms)',trans='log10') 
ggsave('mean_reproduction2.pdf')

## ---- SD_figure ----
# standard deviation of production error (appr. motor noise)
lineProd <- lineFig + aes(x=duration, y = msdProd, group = exp) +sub.
  lpos(c(0.20,0.85)) + labs(x="Durations (ms)",y="SD of Reproduction")+
  scale_y_continuous('Mean standard deviation (ms)', trans = 'log2')+
  geom_smooth(method = "lm",se = FALSE) 
print(lineProd)
ggsave('meanSD_Production2.pdf')
     

## ---- mean_reproduction_error_figure ----

lineRepError+
  geom_smooth(se = FALSE)
ggsave('mean_Repr_Err2.pdf') 


## ---- RRE_figure ----

library(scales)
lineRepError + 
  scale_y_continuous(labels = percent) +
  aes(x=duration, y = mRepErr/duration, group = exp) + 
 # geom_point() +
  geom_abline(aes(yintercept = 0,slope = 0), linetype = 9) +
 # geom_smooth(method = 'lm',se = FALSE) +
  lpos(c(0.8,0.8))  + labs(x="Durations (s)",y="Relative Reproduction Errors")
ggsave('mean_Rel_Repr_Err2.pdf')
 

## ---- CV_figure ----

lineCV <- lineFig + aes(x=duration, y=mCV, group = exp) +
  lpos(c(0.85,0.85)) + labs(x="Durations (s)",y="Mean CV")

lineCV  + geom_smooth(method = 'lm', se = FALSE) + ylim(0.15,0.30)
ggsave('mean_CV2.pdf')



# slope_analysis ----
# 1. ANOVA for CV
## ---- ANOVA_CV ----
#  (When generating table of ANOVA analysis, code 'summary' got wrong captions for each raw.)
  msRepr$log_dur = log10(msRepr$duration)
  sub.cv.slopes <- msRepr %>% group_by(exp, sub) %>% 
    do(tidy(glm(cv ~ log_dur, data=.))) %>% 
    filter(., term == 'log_dur') %>%
    select(., estimate) %>%
    rename(., cv.estimate = estimate)
 
  sub.cv.slopes$modality  <-  factor(as.numeric(sub.cv.slopes$exp) < 3, labels = c('A','V')) 
  sub.cv.slopes$cond  <-  factor(as.numeric(sub.cv.slopes$exp) %% 2, labels = c('Block','Mix')) 
  
  # table for CV
aov(cv.estimate ~ modality*cond  , data = sub.cv.slopes) %>% 
  tidy(.) %>%
  xtable(.,caption = 'ANOVA of Coefficient of Variance')
  
  
# 2. ANOVA for SD
## ---- ANOVA_SD ----
  sub.sd.slopes <- msRepr %>% group_by(exp,sub) %>%
   do(tidy(lm(sdRepr ~ log_dur, data =.))) %>%
    filter(term == 'log_dur') %>%
    select(.,estimate) %>%
    rename(., sd.estimate = estimate) %>%# notice there is a difference between plyr and dplyr of rename
  full_join(.,sub.cv.slopes)
  
  # table for sd
  aov(sd.estimate ~ modality*cond, data = sub.sd.slopes ) %>% tidy(.) %>%
    xtable(.,caption = 'ANOVA of Standard Deviation')

# 3. ANOVA for relative reproduction error
## ---- ANOVA_RRE ----
 mRelSlope <-  msRepr %>% group_by(sub, exp) %>% do(tidy(lm(mRepErr/mRepr ~ duration ))) %>%
    filter(term=='duration') 
  mRelSlope$modality <- factor(as.numeric(mRelSlope$exp) < 3, labels = c('A','V')) 
  mRelSlope$cond <- factor(as.numeric(mRelSlope$exp) %% 2, labels = c('Block','Mix')) 
  
#tidy( aov(estimate ~ exp + Error(sub/exp), data=mRelSlope)) %>%
# xtable(., caption = 'ANOVA of Relative Reproduction Error ')
 
 tidy( aov(estimate ~ modality*cond + Error(sub/(modality*cond)), data=mRelSlope)) %>%
   xtable(., caption = 'ANOVA of Relative Reproduction Error ')
 
 
## ---- Bar figure of the average of slopes of RRE ----   
 mRelSlope %>% group_by(exp) %>% summarize(slope = mean(estimate)) %>% 
    ggplot(., aes(exp, slope )) + geom_bar(stat='identity', position='dodge')
 

 ## ---- mean values table ----
 mean_table <- sub.cv.slopes %>% group_by(exp) %>% summarize(slope = mean(cv.estimate)) %>%
   ggplot(., aes(exp,slope)) + geom_bar(stat = 'identity', plsition = 'dodge')

# y4 <- sub.cv.slopes$cv.estimate[sub.cv.slopes$exp =='Aud/Block']
 y1 <- sub.cv.slopes$cv.estimate[sub.cv.slopes$exp =='Vis/Mix']
 y2 <- sub.cv.slopes$cv.estimate[sub.cv.slopes$exp =='Vis/Block']
 y3 <- sub.cv.slopes$cv.estimate[sub.cv.slopes$exp =='Aud/Mix']
 y4 <- sub.cv.slopes$cv.estimate[sub.cv.slopes$exp =='Aud/Block']
  
  