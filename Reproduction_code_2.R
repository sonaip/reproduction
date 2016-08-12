# R code for combining analysis and manuscript together, chunkread with 'Reproduction_manuscript.rmd'
# DATA: two parts--one from major experiment of four experiments 'data4exps.mat'; one from additional experiment of comparison 'data_combined'.
# ? table display problem from knit 
#  To do:
# 1. within-subject between-block and within-block analysis of CV (based on physical duration)
# 2. develop a MLE model (similar to Jazeryari and Cicchinii papers)
# 3. separate sub-seconds and super-seconds, and explain influences of motor noise 
#   based on the comparison between the production sd and reprodution sd. 
#  in MLE model, sig_rep^2 - sig_motor^2 = sig_likelihood^2 * sig_prior^2 / (sig_l^2 + sig_p^2)

## ---- loadData ----
# load necessary packages
source('loadPackages.R')
# Change colours for basic plot 
myPairs <- brewer.pal(9, "Paired")[c(6,5,2,1)]

# read matlab data and add variable names
d = readMat("data4exps.mat")
dat = as.data.table(d$ds) # read out the matrix
setnames(dat,c('V2','V3','V5','V7','V8','V9','V10','V11','V12','V13'),
         c('block','duration','production','reproduction','exp','sub','durn1_diff'
           ,'rep_err','prod_err','outlier'))
# turn sub to unique sub
dat$sub <- dat$sub + dat$exp*20
dat$sub <- factor(dat$sub)
dat$exp <- factor(dat$exp, labels = c('Vis/Mix','Vis/Block','Aud/Mix','Aud/Block'))

# those negative or more than 1s production errors mean subject release keys before ending ('lazy' trials)
# or lapse of attention
dat$prod_err[dat$prod_err<0 | dat$prod_err >1] <- NA 
dat <- within(dat, trialn1 <- factor(sign(durn1_diff),labels=c('Longer','Equal','Shorter')))

# set outliers 
# original matlab outlier: reproduction is greater than double of duration or less than half.
# examine the proportion of outliers for each participants, and print prop. errors
msOutlier <- dat %>% group_by(exp, sub) %>% summarise(p = mean(outlier))
dat <- left_join(dat,msOutlier, key='sub') %>% 
  mutate(outlier = outlier | p>0.3)

fig_outlier <-  ggplot(msOutlier, aes(x=exp, y=p, group=sub)) +  
 geom_bar(width = 0.3, stat="identity",position=position_dodge(width=0.8)) 
# fig_outlier
# mOutlier <- msOutlier %>% group_by(exp) %>% summarise(mp = mean(p),sp = sd(p),n=n())

# remove outlier participants and outliers
vdat <- dat %>% filter(outlier ==0) %>%
  select(.,one_of(c('block','duration','reproduction','exp','sub','rep_err','prod_err','trialn1')))
vdat$sub <- droplevels(vdat$sub) # remove factors of excluded participants



# prepare mean data
# average means subject-wise
msRepr <- vdat %>% group_by(exp, duration,sub) %>% 
   summarise(mRepr = mean(reproduction), sdRepr = sd(reproduction), 
             mPrErr = mean(prod_err,na.rm=TRUE), sdProd = sd(prod_err,na.rm=TRUE), 
             mRepErr = mean(rep_err), sdRepErr = sd(rep_err))  %>% 
  mutate(cv = sdRepr/duration, log_dur = log2(duration)) %>% # cv based on physical duration
   arrange(exp, duration,sub) 

# grand average (collapse subjects)
 mRepr <- msRepr %>% group_by(exp, duration) %>% 
   summarise(mRepr = mean(mRepr),  mCV = mean(cv),
             msdProd = mean(sdProd), msdRepr = mean(sdRepr),
             mPrErr = mean(mPrErr), mRepErr = mean(mRepErr))  
 
 # estimate inter-trial effect
 msInterRepr <- vdat %>% group_by(exp,sub, trialn1) %>%
   summarise(rel_err = mean(rep_err/duration), rel_sdErr = sd(rep_err/duration))
 
 mInterRepr <- msInterRepr %>% group_by(exp, trialn1) %>%
   summarise(rel_err = mean(rel_err), rel_sdErr = mean(rel_sdErr))
 
 # create a template plot 'fig1'
fig1 <- ggplot(mRepr, aes(x=duration, y = mRepr, colour = exp,  shape = exp)) + 
  geom_point(size =1) +
  scale_colour_manual(values = myPairs) +
  scale_shape_manual(values = c(15,17,0,2))+
  xlab('Durations (ms)') +
  theme_classic() +
  legend_pos(c(0.2,0.8)) 

# individual figure
fig_ind <- fig1 %+% msRepr + facet_wrap(~sub)  + 
  scale_x_continuous( trans = "log10") +  legend_pos(c(0.9,0.1)) 

fig_rep <- fig1 + aes(y = mRepErr/duration)  +
  scale_x_continuous(trans = "log10") +  legend_pos(c(0.9,0.8))  + ylab("Relative Reproduction Errors")

# mean_reproduction_figure 
#fig1 + geom_line()+
#  geom_abline(aes(yintercept = 0, slope = 1),linetype = 9,xlim = c(min(mRepr$duration),16)) +
#  scale_x_continuous('Durations (s)', trans = "log2") +
#  scale_y_continuous('Mean reproduction (s)',trans='log2') 
##ggsave('mean_reproduction.pdf')

## ---- outliers_figure ----
#ggplot(data=mOutlier, aes(x=exp, y=mp,  width=0.5)) +
#  geom_errorbar(aes(ymin = mp - sp/sqrt(n), ymax = mp + sp/sqrt(n)), width=0.2, 
#                position=position_dodge(width=0.8)) +
#  geom_bar(stat="identity", position="dodge", width=0.8) +
#  theme_bw() +lpos(c(0.2,0.8)) + labs(x="Experiments",y="Prop. Error Trials")
#ggsave('outliers.pdf')

## ---- some plots to check individuals ----

fig_ind # individual reproduction
fig_ind + aes(y=mRepErr) # show reproduction error
fig_ind + aes(y=mRepErr/mRepr) # show relative reproduction error
fig_ind + aes(y=mPrErr) # production error (key release approx. 0.5 seconds later)
fig_ind + aes(y=sdProd) # variability of production (approx. 0.1)
fig_ind + aes(y=sdRepr) + scale_y_continuous(trans='log2') # scalar property maintained!
fig_ind + aes(y=cv)

fig_inter <- ggplot(msInterRepr, aes(x=trialn1,y=rel_err, fill = exp)) +
  geom_bar(stat='identity',position='dodge')
fig_inter + facet_wrap(~sub) # inter-trial tendency effect
fig_inter + aes(y=rel_sdErr) + facet_wrap(~sub)# no clear pattern
fig_mInter <- fig_inter %+% mInterRepr + aes(x=exp,fill=trialn1) # clear strong inter-trial influence. 

fig_mInter
fig_mInter + aes(y=rel_sdErr) # increase variablity after long trials?

# ---- slope_analysis ----
#  (When generating table of ANOVA analysis, code 'summary' got wrong captions for each raw.)
slope_estimation <- function(df,lm_formula) {
  df %>% group_by(exp, sub) %>% 
    do(tidy(lm(as.formula(lm_formula), data=.))) %>% 
    filter(., term != '(Intercept)') %>%
#    select(., estimate) %>%
    rename(., slope = estimate) %>%
    separate(.,exp,c('modality','design'),sep='/', remove = FALSE)
}

sub.cv.slopes <- slope_estimation(msRepr, 'cv ~ log_dur') # cv slope: testing 2009 Mial's result
sub.cv.linear_slopes<- slope_estimation(msRepr, 'cv ~ duration') # cv slope with linear duration
sub.sd.slopes <- slope_estimation(msRepr, 'sdRepr ~ duration') # scalar slope
sub.re.slopes <- slope_estimation(msRepr, 'mRepErr/mRepr ~ duration') # central tendency slope

## t-tests individual mean slope against 0
t_test <- function(df, u=0) {
  tmp <- t.test(df$slope,mu=u)
  stats <-  c(tmp$parameter,tmp$statistic,tmp$estimate,tmp$p.value)
  names(stats) <- c('df','t','mean','p')
  data.frame (as.list(stats))
}

sub.cv.slopes %>% group_by(exp) %>%  do( t_test(.))
sub.cv.linear_slopes%>% group_by(exp) %>%  do( t_test(.))
sub.sd.slopes%>% group_by(exp) %>%  do( t_test(.)) # against 0, meaningless, should use within-subject ANOVA
sub.re.slopes%>% group_by(exp) %>%  do( t_test(.)) # within-subject

# ---- ANOVAs ----
#  for CV
anova.cv <-  aov(slope ~ modality*design + Error(sub), data = sub.cv.slopes) %>% tidy(.) 
anova.cv_linear <-  aov(slope ~ modality*design + Error(sub), data = sub.cv.linear_slopes) %>% tidy(.) 
anova.sd <- aov(slope ~ modality*design + Error(sub), data = sub.sd.slopes ) %>% tidy(.) 
anova.re <- aov(slope ~ modality*design + Error(sub), data = sub.re.slopes ) %>% tidy(.) # stronger slopes for mixed block


## ---- Block Exps Analysis ----
# only data from 2 exps of block conditions were analyzed, within subject in each experiment
msRepr$duration <- factor(msRepr$duration)

msRepr_block <- vdat %>% 
  filter(.,exp == "Vis/Block"| exp =="Aud/Block") %>%
  group_by(exp, sub, block, duration) %>%
  summarise(mRepr = mean(reproduction), sdRepr = sd(reproduction), 
            mPrErr = mean(prod_err,na.rm=TRUE), sdProd = sd(prod_err,na.rm=TRUE), 
            mRepErr = mean(rep_err), sdRepErr = sd(rep_err)) %>%
  mutate(cv = sdRepr/duration, log_dur = log2(duration)) %>% # cv based on physical duration
  arrange(exp, duration,sub) 

mRepr_block <- msRepr_block %>% group_by(exp, duration, block) %>% 
  summarise(mRepr = mean(mRepr),  mCV = mean(cv),
            msdProd = mean(sdProd), msdRepr = mean(sdRepr),
            mPrErr = mean(mPrErr), mRepErr = mean(mRepErr)) 

# ================ anova for the mean values ===============
# msRepr
msRepr_block_mcv <- msRepr_block %>% 
  group_by(exp,sub,block) %>% 
  summarise(mcv = mean(cv), mratio = mean(sdProd/sdRepr)) %>%
  arrange(exp,sub,block)

 anova.mcv.vis <- aov(mcv ~ block + Error(sub/block), data = filter(msRepr_block_mcv, exp =='Vis/Block') ) %>% tidy(.)
 anova.mcv.aud <- aov(mcv ~ block + Error(sub/block), data = filter(msRepr_block_mcv, exp =='Aud/Block') ) %>% tidy(.)
 
 temp <- msRepr_block_mcv %>% filter(., exp =='Vis/Block') %>% group_by()
 anova.mratio.vis <- aov(mratio ~ block + Error(sub/block), data = filter(msRepr_block_mcv, exp =='Vis/Block') ) %>% tidy(.)
 anova.mratio.aud <- aov(mratio ~ block + Error(sub/block), data = filter(msRepr_block_mcv, exp =='Aud/Block') ) %>% tidy(.)
 

fig_block <- fig1 %+% mRepr_block + 
  aes(y=mCV) +
  geom_smooth(method=lm, se=FALSE)+
  scale_colour_manual(values = c(2,20))+ 
  scale_x_continuous(trans='log2')+
  legend_pos(c(0.9,0.9)) 

# multiple linear regressions in one figure?
 sub.cv.slopes_block <- msRepr_block %>% 
   group_by(exp,sub,block) %>% 
   do(tidy(lm(cv ~ log_dur,.))) %>%
   filter(., term !='(Intercept)') %>%
   rename(., slope = estimate)
 
 sub.cv.slopes_block$block <- factor(sub.cv.slopes_block$block)
 

# ===============anova_block_experiment_independent===================
# visual for the slopes
 
 data_vis_block <- sub.cv.slopes_block %>% filter(., exp == "Vis/Block")
 anova.cv_vis_block <-  aov(slope ~ block + Error(sub/block), data = data_vis_block) %>% tidy(.) 
 
# auditory for the slopes
 data_aud_block <- sub.cv.slopes_block %>% filter(., exp == "Aud/Block")
 anova.cv_aud_block <-  aov(slope ~ block + Error(sub/block), data = data_aud_block) %>% tidy(.) 
 
 
 # figure for the slope value 
 sub.cv.slopes$block <- 4
 sub.cv.slopes$block <- factor(sub.cv.slopes$block)
 
 sub.cv.slopes_join <- filter(sub.cv.slopes, exp == "Vis/Block"| exp =="Aud/Block") %>%
   group_by(exp, sub, block)
 sub.cv.slopes_join$modality <- NULL
 sub.cv.slopes_join$design <- NULL
 
 sub.cv.slopes_block <- full_join(sub.cv.slopes_block,sub.cv.slopes_join, by =c("exp","sub","block","slope"))
 # (finally im gonna cryTT) now do the anova, within subject, experiment independent
 # visual *significant 
 anova.cv_vis_block <- aov(slope ~ block + Error(sub/block), data = filter(sub.cv.slopes_block, exp == "Vis/Block"))%>%tidy(.)
 # auditory *significant
 anova.cv_aud_block <- aov(slope ~ block + Error(sub/block), data = filter(sub.cv.slopes_block, exp == "Aud/Block"))%>%tidy(.)

 
 
 # add the overall regression into the separate one
 
 
# figure for data  
 mRepr_block$block <- factor(mRepr_block$block)
 mRepr_block %>% ggplot(., aes(x = duration, y = mCV, color = exp, shape = exp)) +
   geom_point(size = 2) + 
   scale_x_continuous(trans='log2')+
   legend_pos(c(0.9,0.9)) 
   

##  Figures in the manuscript 
 
# ---- figure1 ----
 # mean estimation + mean relative reproduction error in one figure
 
 fig_mEstimation <- fig1 +aes(y = mRepr) +
   geom_abline(aes(intercept = 0, slope = 1),linetype = 2) +
   scale_x_continuous(trans = "log10")+
   scale_y_continuous(trans = "log10") +
   ylab("Mean Estimate(s)")
   
 fig_mRre <-  fig1 + aes(y = mRepErr/duration)  +
   geom_abline(aes(intercept = 0, slope = 0),linetype = 2) +   
   scale_x_continuous(trans = "log10") +  
   legend_pos(c(0.9,0.8))  + 
   ylim(-0.3,0.4) +
   ylab("Relative Reproduction Errors")
 
 
 library("Rmisc")
  multiplot(fig_mEstimation, fig_mRre, cols = 1)
 

 
 
# ---- figure_mean_cv ----
 fig_mcv <- fig1 + aes(y=mCV) +
    geom_smooth(method = "lm", se = FALSE, linetype = 1, size = 0.4) +
    scale_x_continuous(trans = "log10") +
    ylab("Coefficient of Variance") + 
    ylim(0.1,0.45) +
    legend_pos(c(0.8,0.8))
 fig_mcv
 
 
 
 

 
  