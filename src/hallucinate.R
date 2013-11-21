require(visreg)
load('../data/NHANES.0910.demo.and.hiq.rda')
names(NHANES.0910.demographics.df)
# data description at: http://www.cdc.gov/nchs/nhanes/nhanes2009-2010/DEMO_F.htm#SEQN
# and http://www.cdc.gov/nchs/nhanes/nhanes2009-2010/HIQ_F.htm

DemoKeepVars <- c( 
    "SEQN" ,                 # unique person identifier (merge variable)
    "WTINT2YR" ,         # the two-year interview weight
    "INDFMPIR" ,        # ratio of family income to poverty
    "SDMVPSU" ,         # primary sampling unit varaible, used in complex design
    "SDMVSTRA" ,         # strata variable, used in complex design
    "RIDAGEYR" ,        # person age
    "RIAGENDR" ,             # gender 
    )

QuestionnaireKeepVars <- c( 
    "SEQN" ,                 # unique person identifier (merge variable)
    "HIQ011"                 # Covered by health insurance, from
    # http://www.cdc.gov/nchs/nhanes/nhanes2009-2010/HIQ_F.htm#HIQ011
  )

nhanes.demog <-
  NHANES.0910.demographics.df[ , DemoKeepVars ]

nhanes.hiq <-
  NHANES.0910.HIQ_F.df[ , QuestionnaireKeepVars ]

nki <- read.csv('../data/nki.csv')
nki.img <- read.csv('../data/labelresultsN.csv')
names(nki.img)[1] <- 'Subject'
nki.merge <- merge(nki, nki.img, by='Subject')
age.mean <- mean(c(nki.merge$Age, nhanes.demog$RIDAGEYR))
age.sd <- sd(c(nki.merge$Age, nhanes.demog$RIDAGEYR))
weight.mean <- mean(c(nki.merge$Weight, nhanes.demog$WTINT2YR))
weight.sd <- sd(c(nki.merge$Weight, nhanes.demog$WTINT2YR))
nhanes.demog <- cbind(nhanes.demog, 
  data.frame(AgeScaled=((nhanes.demog$RIDAGEYR-age.mean) / age.sd), 
              WeightScaled=((nhanes.demog$WTINT2YR-age.mean)/age.sd)))
nki.merge <-cbind(nki.merge, 
  data.frame(AgeScaled=((nki.merge$Age - age.mean)/age.sd), 
             WeightScaled=((nki.merge$Weight-weight.mean)/weight.sd)))
mydist <- abs(outer(nki.merge$AgeScaled, nhanes.demog$AgeScaled, '-')) + 
  abs(outer(nki.merge$AgeScaled, nhanes.demog$AgeScaled))
rownames(mydist) <- nki.merge$Subject
colnames(mydist) <- nhanes.demog$Subject
nki.imgcols <- (1:dim(nki.merge)[2])[grepl("LABEL", colnames(nki.merge))]
nhanes.img <- matrix(rep(NA, nrow(nhanes.demog) * length(nki.imgcols)), 
                     nrow=nrow(nhanes.demog))
for(i in 1:dim(nhanes.demog)[1]){
  nkisubj <- rownames(mydist)[which(mydist[, i] == min(mydist[, i]))][1] # take 1st match
  nhanes.img[i, ] <- as.numeric(nki.merge[which(nki.merge$Subject == nkisubj), nki.imgcols])
}
colnames(nhanes.img) = colnames(nki.merge)[nki.imgcols]
nhanes.merge <- cbind(nhanes.demog, nhanes.img, nhanes.hiq)


### visualize outcomes of label values vs family income (multiples of poverty line)
mylm <- lm(LABEL_19 ~ INDFMPIR, data=nhanes.merge)
visreg(mylm) 
summary(mylm)

mylm <- lm(LABEL_32 ~ INDFMPIR, data=nhanes.merge)
visreg(mylm)
summary(mylm)
