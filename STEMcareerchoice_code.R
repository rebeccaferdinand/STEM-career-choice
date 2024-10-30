#####STEM career choice

##library

library (lavaan)
library (psych)
library (psy)
library (car)
library (semTools)
library(ggplot2)
library (reshape2)
library (GGally)
library(apaTables)
library (psych)
library(foreign)
library(haven)

###Data prep#####


#make data single entered
stemavoidance.data <- stemavoidance[ stemavoidance$random == 1, ]

#exclude those with serious medical and genetic illnesses

stemavoidance.data <- stemavoidance[ stemavoidance$random == 1 & stemavoidance$medexcl1 == 0 & stemavoidance$exclude1 == 0, ]

# n = 7908

#Create binary STEM choice variable
#First create data frame of participants with degree or apprenticeship information present

stemavoidance.21 <- stemavoidance.data[!is.na(stemavoidance.data$u1cdegr21 | stemavoidance.data$u1capptyp1),]

#n = 2254

## Convert u1cdegr21 and u1capptyp1 to numeric:
stemavoidance.21$u1cdegr21 <- as.numeric(stemavoidance.21$u1cdegr21)
stemavoidance.21$u1capptyp1 <- as.numeric(stemavoidance.21$u1capptyp1)

#STEM degree/apprenticeship

stemavoidance.21$stemcareer <- ifelse(stemavoidance.21$u1cdegr21 %in% c(1, 2, 3, 4, 5, 6) | stemavoidance.21$u1capptyp1 %in% c(4), 1, 0)





stemavoidance.data$stemcareer <- ifelse(
  stemavoidance.data$u1cdegr21 %in% c(1, 2, 3, 4, 5, 6) | stemavoidance.data$u1capptyp1 %in% c(4),
  1,
  ifelse(is.na(stemavoidance.data$u1cdegr21) & is.na(stemavoidance.data$u1capptyp1), NA, 0)
)

#Create general anxiety variable (standardised, residualised for age and sex)

stemavoidance.data$generalanxiety.as <-rstandard(lm(stemavoidance.data$rcbspgnxm1 ~ stemavoidance.data$rcbage1 + stemavoidance.data$sex1 , na.action=na.exclude))


#Create maths anxiety variable (standardised, residualised for age and sex)

stemavoidance.data$mathsanxiety.as <-rstandard(lm(stemavoidance.data$rcbspmnxm1 ~ stemavoidance.data$rcbage1 + stemavoidance.data$sex1 , na.action=na.exclude))

#Create maths anxiety variable, independent of general anxiety (standardised, also residualised for age and sex)

stemavoidance.data$mathsanxiety.ga.as <-rstandard(lm(stemavoidance.data$rcbspmnxm1 ~ stemavoidance.data$rcbage1 + stemavoidance.data$sex1 + stemavoidance.data$rcbspgnxm1 , na.action=na.exclude))

#Create maths achievement variable (standardised, residualised for age and sex)

stemavoidance.data$mathsgcse.as <-rstandard(lm(stemavoidance.data$pcexgcsematgrdm1 ~ stemavoidance.data$pcexgcseage1 + stemavoidance.data$sex1 , na.action=na.exclude))


#Residualise maths motivation composite for age and sex

stemavoidance.data$mathsmotivation.as <-rstandard(lm(stemavoidance.data$mathsmotivation ~ stemavoidance.data$pcwebage1 + stemavoidance.data$sex1 , na.action=na.exclude))

stemavoidance.21$mathsmotivation.as <-rstandard(lm(stemavoidance.21$mathsmotivation ~ stemavoidance.21$pcwebage1 + stemavoidance.21$sex1 , na.action=na.exclude))

#Residualise maths interest composite for age and sex

"pcmaset1", "pcmainm1"

stemavoidance.data$mathsinterest.as <-rstandard(lm(stemavoidance.data$pcmainm1 ~ stemavoidance.data$pcwebage1 + stemavoidance.data$sex1 , na.action=na.exclude))

stemavoidance.data$mathsse.as <-rstandard(lm(stemavoidance.data$pcmaset1 ~ stemavoidance.data$pcwebage1 + stemavoidance.data$sex1 , na.action=na.exclude))

#create general anxiety female variable (standardised, residualised for age)

stemavoidance.data_female$generalanxiety.a <-rstandard(lm(stemavoidance.data_female$rcbspgnxm1 ~ stemavoidance.data_female$rcbage1, na.action=na.exclude))

#create maths anxiety female variable (standardised, residualised for age)

stemavoidance.data_female$mathsanxiety.a <-rstandard(lm(stemavoidance.data_female$rcbspmnxm1 ~ stemavoidance.data_female$rcbage1, na.action=na.exclude))

#Create maths anxiety female variable (independent of general anxiety, residualised for age, standardised)

stemavoidance.data_female$mathsanxiety.ga.a <-rstandard(lm(stemavoidance.data_female$rcbspmnxm1 ~ stemavoidance.data_female$rcbage1 + stemavoidance.data_female$rcbspgnxm1 , na.action=na.exclude))

#Create maths achievement female variable (standardised, residualised for age)

stemavoidance.data_female$mathsgcse.a <-rstandard(lm(stemavoidance.data_female$pcexgcsematgrdm1 ~ stemavoidance.data_female$pcexgcseage1, na.action=na.exclude))

#Residualise maths motivation female composite for age

stemavoidance.data_female$mathsse.a <-rstandard(lm(stemavoidance.data_female$pcmaset1 ~ stemavoidance.data_female$pcwebage1, na.action=na.exclude))
stemavoidance.data_female$mathsinterest.a <-rstandard(lm(stemavoidance.data_female$pcmainm1 ~ stemavoidance.data_female$pcwebage1, na.action=na.exclude))

#create general anxiety male variable (standardised, residualised for age)

stemavoidance.data_male$generalanxiety.a <-rstandard(lm(stemavoidance.data_male$rcbspgnxm1 ~ stemavoidance.data_male$rcbage1, na.action=na.exclude))

#create maths anxiety male variable (standardised, residualised for age)

stemavoidance.data_male$mathsanxiety.a <-rstandard(lm(stemavoidance.data_male$rcbspmnxm1 ~ stemavoidance.data_male$rcbage1, na.action=na.exclude))

#Create maths anxiety male variable (independent of general anxiety, residualised for age, standardised)

stemavoidance.data_male$mathsanxiety.ga.a <-rstandard(lm(stemavoidance.data_male$rcbspmnxm1 ~ stemavoidance.data_male$rcbage1 + stemavoidance.data_male$rcbspgnxm1 , na.action=na.exclude))

#Create maths achievement male variable (standardised, residualised for age)

stemavoidance.data_male$mathsgcse.a <-rstandard(lm(stemavoidance.data_male$pcexgcsematgrdm1 ~ stemavoidance.data_male$pcexgcseage1 , na.action=na.exclude))

#Residualise maths motivation male composite for age

stemavoidance.data_male$mathsse.a <-rstandard(lm(stemavoidance.data_male$pcmaset1 ~ stemavoidance.data_male$pcwebage1, na.action=na.exclude))
stemavoidance.data_male$mathsinterest.a <-rstandard(lm(stemavoidance.data_male$pcmainm1 ~ stemavoidance.data_male$pcwebage1, na.action=na.exclude))

###Descriptive statistics#####

library(psych)

# Descriptive statistics for maths anxiety


describe(stemavoidance.data$rcbspmnxm1  )

#Descriptive for general anxiety

describe(stemavoidance.data$rcbspgnxm1)

#Descriptive statistics for mathematics GCSE

describe(stemavoidance.data$pcexgcsematgrdm1)

#Descriptive statististics for mathematics motivation

describe(stemavoidance.data$mathsmotivation)


#Descriptive statististics for SES

describe(stemavoidance.data$ases)

#Descriptive statistics for STEM career choice

table(stemavoidance.21$stemcareer)

describe(stemavoidance.data$pcmaset1)



#===Correlations/heatmaps ====#


install.packages("corrplot")
library(corrplot)

stemavoidance.wholesample <- data.frame(stemavoidance.data$ases, stemavoidance.data$mathsanxiety.as, stemavoidance.data$mathsanxiety.ga.as, stemavoidance.data$mathsse.as, stemavoidance.data$mathsinterest.as,  stemavoidance.data$generalanxiety.as, stemavoidance.data$mathsgcse.as, stemavoidance.data$stemcareer)



##===========WHOLE SAMPLE =========#
#load OCD data
OCD.data <- read.csv("Participants_OCD.csv", header= TRUE)
str(OCD.data)

#run correlation
stemavoidance.corr <- cor(stemavoidance.wholesample, method = "pearson", use = "pairwise.complete.obs")



colnames(stemavoidance.corr) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                  "GCSE", "STEM Career Choice")
rownames(stemavoidance.corr) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                  "GCSE", "STEM Career Choice" )

#matrix of correlation p values
p.mat.stemavoidance <- cor.mtest(stemavoidance.wholesample)$p

colnames(p.mat.stemavoidance) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                   "GCSE", "STEM Career Choice" )
rownames(p.mat.stemavoidance) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                   "GCSE", "STEM Career Choice" )



#create correlation matrix

corrplot(stemavoidance.corr, # Correlation matrix
         method = "color", # Correlation plot method
         type = "lower",    # Correlation plot style (also "upper" and "lower")
         diag = FALSE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         title = "", # Main title
         addCoef.col = "black", # Add correlation coefficient
         tl.cex = 0.9, #label size
         number.cex = 1, #corr coefficient size
         sig.level = c(0.001, 0.01, 0.05), #significance level
         pch.cex = 1,
         insig = "label_sig", #leave insig p value
         pch.col = 'grey20',
         p.mat = p.mat.stemavoidance, #matrix of p value
         tl.srt = 40, #rotation of text labels 
) 


trace(corrplot, edit=TRUE)
#Then replace on line 443

place_points = function(sig.locs, point) {
  text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
       labels = point, col = pch.col, cex = pch.cex, 
       lwd = 2)
  #with:
  
  # adjust text(X,Y ...) according to your needs, here +0.25 is added to the Y-position    
  place_points = function(sig.locs, point) {
    text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
         labels = point, col = pch.col, cex = pch.cex, 
         lwd = 2)
    
    # Open a TIFF device with desired DPI
    tiff("high_res_corrplot.tiff", width = 8, height = 6, units = "in", res = 300)
    
    # Generate the plot within the TIFF device
    corrplot(stemavoidance.corr, 
             method = "color", 
             type = "lower",    
             diag = FALSE,      
             tl.col = "black", 
             title = "", 
             addCoef.col = "black", 
             tl.cex = 0.9, 
             number.cex = 1, 
             sig.level = c(0.001, 0.01, 0.05), 
             pch.cex = 1,
             insig = "label_sig", 
             pch.col = 'grey20',
             p.mat = p.mat.stemavoidance, 
             tl.srt = 40 
    )
    
    # Close the TIFF device to save the file
    dev.off()
    
    
    
    
    #sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
    #insig = 'label_sig', pch.col = 'grey20', order = 'AOE'
    
    corrplot
    
    #==========FEMALE SAMPLE ================#
    
    #load data
    
    stemavoidance.femalesample$mathsinterest.a <- stemavoidance.data_female$mathsinterest.a
    stemavoidance.femalesample <- data.frame(stemavoidance.data_female$ases, stemavoidance.data_female$mathsanxiety.a, stemavoidance.data_female$mathsanxiety.ga.a, stemavoidance.data_female$mathsse.a, stemavoidance.data_female$mathsinterest.a,  stemavoidance.data_female$generalanxiety.a, stemavoidance.data_female$mathsgcse.a, stemavoidance.data_female$stemcareer)
    
    df_reordered <- df[, c("ases", "mathsanxiety.a", "", "C")]
    stemavoidance.femalesample <- stemavoidance.femalesample[, c("ases", "mathsanxiety.a", "mathsanxiety.ga.a", "mathsse.a", "mathsinterest.a", "generalanxiety.a", "mathsgcse.a", "stemcareer" )]
    
    #run correlation
    stemavoidance.corr.female <- cor(stemavoidance.femalesample, method = "pearson", use = "pairwise.complete.obs")
    ​
    colnames(stemavoidance.corr.female) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                             "GCSE", "STEM Career Choice")
    rownames(stemavoidance.corr.female) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                             "GCSE", "STEM Career Choice" )
    ​
    #matrix of correlation p values
    p.mat.stemavoidance.female <- cor.mtest(stemavoidance.femalesample)$p
    ​
    colnames(p.mat.stemavoidance.female) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                              "GCSE", "STEM Career Choice" )
    rownames(p.mat.stemavoidance.female) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                              "GCSE", "STEM Career Choice" )
    
    
    
    #create correlation matrix
    
    corrplot(stemavoidance.corr.female, # Correlation matrix
             method = "color", # Correlation plot method
             type = "lower",    # Correlation plot style (also "upper" and "lower")
             diag = FALSE,      # If TRUE (default), adds the diagonal
             tl.col = "black", # Labels color
             title = "", # Main title
             addCoef.col = "black", # Add correlation coefficient
             tl.cex = 0.9, #label size
             number.cex = 1.0, #corr coefficient size
             sig.level = c(0.001, 0.01, 0.05), #significance level
             pch.cex = 1,
             insig = "label_sig", #leave insig p value
             pch.col = 'black',
             p.mat = p.mat.stemavoidance.female, #matrix of p value
             tl.srt = 40, #rotation of text labels 
    ) 
    
    
    
    trace(corrplot, edit=TRUE)
    
    
    #Then replace on line 443
    
    place_points = function(sig.locs, point) {
      text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
           labels = point, col = pch.col, cex = pch.cex, 
           lwd = 2)
      #with:
      
      # adjust text(X,Y ...) according to your needs, here +0.25 is added to the Y-position    
      place_points = function(sig.locs, point) {
        text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
             labels = point, col = pch.col, cex = pch.cex, 
             lwd = 2)
        
        
        
        
        
        #==========MALE SAMPLE ================#
        
        #load data
        
        stemavoidance.malesample$mathsse.a <- stemavoidance.data_male$mathsse.a
        
        stemavoidance.malesample <- stemavoidance.data_male[, c("ases", "mathsanxiety.a", "mathsanxiety.ga.a", "mathsse.a", "mathsinterest.a", "generalanxiety.a", "mathsgcse.a", "stemcareer" )]
        stemavoidance.malesample <- stemavoidance.malesample[, c("ases", "mathsanxiety.a", "mathsanxiety.ga.a", "mathsse.a", "mathsinterest.a", "generalanxiety.a", "mathsgcse.a", "stemcareer" )]
        #run correlation
        stemavoidance.corr.male <- cor(stemavoidance.malesample, method = "pearson", use = "pairwise.complete.obs")
        ​
        colnames(stemavoidance.corr.male) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                               "GCSE", "STEM Career Choice")
        rownames(stemavoidance.corr.male) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                               "GCSE", "STEM Career Choice" )
        ​
        #matrix of correlation p values
        p.mat.stemavoidance.male <- cor.mtest(stemavoidance.malesample)$p
        ​
        colnames(p.mat.stemavoidance.male) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                                "GCSE", "STEM Career Choice" )
        rownames(p.mat.stemavoidance.male) <- c("SES", "MA", "MA.GA", "MSE", "MI", "GA",
                                                "GCSE", "STEM Career Choice" )
        
        > pwr.r.test(sig.level = 0.05, power = 0.80, r = 0.12)
        
        
        #create correlation matrix
        
        corrplot(stemavoidance.corr.male, # Correlation matrix
                 method = "color", # Correlation plot method
                 type = "lower",    # Correlation plot style (also "upper" and "lower")
                 diag = FALSE,      # If TRUE (default), adds the diagonal
                 tl.col = "black", # Labels color
                 title = "", # Main title
                 addCoef.col = "black", # Add correlation coefficient
                 tl.cex = 0.9, #label size
                 number.cex = 1.0, #corr coefficient size
                 sig.level = c(0.001, 0.01, 0.05), #significance level
                 pch.cex = 1,
                 insig = "label_sig", #leave insig p value
                 pch.col = 'black',
                 p.mat = p.mat.stemavoidance.male, #matrix of p value
                 tl.srt = 40, #rotation of text labels 
        ) 
        
        # Save as a high-resolution TIFF file
        tiff("combined_corrplot.tiff", width = 12, height = 6, units = "in", res = 300)
        
        # Combine and plot as above
        par(mfrow = c(1, 2))
        
        # Female plot
        corrplot(stemavoidance.corr.female, 
                 method = "color", 
                 type = "lower",    
                 diag = FALSE,      
                 tl.col = "black", 
                 addCoef.col = "black", 
                 tl.cex = 0.9, 
                 number.cex = 1.0, 
                 sig.level = c(0.001, 0.01, 0.05), 
                 pch.cex = 1,
                 insig = "label_sig", 
                 pch.col = 'black',
                 p.mat = p.mat.stemavoidance.female, 
                 tl.srt = 40 
        )
        
        # Male plot
        corrplot(stemavoidance.corr.male, 
                 method = "color", 
                 type = "lower",    
                 diag = FALSE,      
                 tl.col = "black", 
                 addCoef.col = "black", 
                 tl.cex = 0.9, 
                 number.cex = 1.0, 
                 sig.level = c(0.001, 0.01, 0.05), 
                 pch.cex = 1,
                 insig = "label_sig", 
                 pch.col = 'black',
                 p.mat = p.mat.stemavoidance.male, 
                 tl.srt = 40 
        )
        
        # Close the TIFF device
        dev.off()
        
##regressions#####
#Whole sample logistic regression
        
#maths motivation and maths anxiety
        
        model3 <- glm(stemcareer ~ mathsanxiety.as + mathsse.as+ mathsinterest.as, data = stemavoidance.data, family = binomial)
        
        model3 <- glm(stemcareer ~ mathsanxiety.as + mathsse.as+ mathsinterest.as, data =clean_data, family = binomial)
        
        
        summary(model3)
        
        vif(model6)
        
        # Install and load the DescTools package
        install.packages("DescTools")
        library(DescTools)
        
        # Fit the null model
        null_model <- glm(stemcareer ~ 1, data = stemavoidance.data, family = binomial)
        
        # Fit the full model
        full_model <- glm(stemcareer ~ mathsanxiety.as + mathsse.as + mathsinterest.as, data = stemavoidance.data, family = binomial)
        
        # Calculate Nagelkerke R^2
        NagelkerkeR2 <- PseudoR2(full_model, which = "Nagelkerke")
        print(NagelkerkeR2)
        
        pR2(model3)
        
        
        # Coefficient estimates
        coef_estimates <- coef(model3)
        
        # Standard errors
        se <- summary(model3)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- 
          
          exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        summary(model3)
        
        #maths motivation and maths anxiety and maths achievement (GCSE)
        
        model4 <- glm(stemcareer ~ mathsanxiety.as + mathsse.as+ mathsinterest.as + mathsgcse.as, data = stemavoidance.data, family = binomial)
        
        summary(model4)
        
        pR2(model4)
        
        # Fit the null model
        null_model <- glm(stemcareer ~ 1, data = stemavoidance.data, family = binomial)
        
        # Fit the full model
        full_model <- glm(stemcareer ~ mathsanxiety.as + mathsse.as + mathsinterest.as + mathsgcse.as, data = stemavoidance.data, family = binomial)
        
        # Calculate Nagelkerke R^2
        NagelkerkeR2 <- PseudoR2(full_model, which = "Nagelkerke")
        print(NagelkerkeR2)
        
        clean_data <- na.omit(stemavoidance.data)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(model4)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Coefficient estimates
        coef_estimates <- coef(model4)
        
        # Standard errors
        se <- summary(model4)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        #maths motivation and maths anxiety and SES
        
        model5 <- glm(stemcareer ~ mathsanxiety.as + mathsmotivation.as + ases, data = stemavoidance.data, family = binomial)
        
        summary(model5)
        
        pR2(model5)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(model5)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Coefficient estimates
        coef_estimates <- coef(model5)
        
        # Standard errors
        se <- summary(model5)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        
        
        
        exp(coef(model5))
        
        #maths motivation and maths anxiety and SES and Maths achievement (GCSE)
        
        model6 <- glm(stemcareer ~ mathsanxiety.as + mathsse.as + mathsinterest.as + mathsgcse.as + ases, data = stemavoidance.data, family = binomial)
        
        vif(model6)
        
        summary(model6)
        
        pR2(model6)
        
        exp(coef(model6))
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(model6)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        # Fit the null model
        null_model <- glm(stemcareer ~ 1, data = stemavoidance.data, family = binomial)
        
        # Fit the full model
        full_model <- glm(stemcareer ~ mathsanxiety.as + mathsse.as + mathsinterest.as + mathsgcse.as +ases, data = stemavoidance.data, family = binomial)
        
        # Calculate Nagelkerke R^2
        NagelkerkeR2 <- PseudoR2(full_model, which = "Nagelkerke")
        print(NagelkerkeR2)
        
        # Coefficient estimates
        coef_estimates <- coef(model6)
        
        # Standard errors
        se <- summary(model6)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )

#Female sample logistic regression
        
#maths motivation and maths anxiety
        
        femalemodel3 <- glm(stemcareer ~ mathsanxiety.a + mathsse.a+ mathsinterest.a, data = stemavoidance.femalesample, family = binomial)
        
        vif(femalemodel3)
        
        summary(femalemodel3)
        
        pR2(femalemodel3)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(femalemodel3)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Install and load the DescTools package
        install.packages("DescTools")
        library(DescTools)
        
        # Fit the null model
        null_model <- glm(stemcareer ~ 1, data = stemavoidance.femalesample, family = binomial)
        
        # Fit the full model
        full_model <- glm(stemcareer ~ mathsanxiety.a + mathsse.a + mathsinterest.a + ases + mathsgcse.a, data = stemavoidance.femalesample, family = binomial)
        
        # Calculate Nagelkerke R^2
        NagelkerkeR2 <- PseudoR2(full_model, which = "Nagelkerke")
        print(NagelkerkeR2)
        
        # Coefficient estimates
        coef_estimates <- coef(femalemodel3)
        
        # Standard errors
        se <- summary(femalemodel3)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        #maths motivation and maths anxiety and maths achievement (GCSE)
        
        femalemodel4 <- glm(stemcareer ~ mathsanxiety.a + mathsse.a + mathsinterest.a + mathsgcse.a, data = stemavoidance.femalesample, family = binomial)
        
        vif(femalemodel4)
        
        summary(femalemodel4)
        
        pR2(femalemodel4)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(femalemodel4)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        
        # Coefficient estimates
        coef_estimates <- coef(femalemodel4)
        
        # Standard errors
        se <- summary(femalemodel4)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        #maths motivation and maths anxiety and SES
        
        femalemodel5 <- glm(stemcareer ~ mathsanxiety.a + mathsmotivation.a + ases, data = stemavoidance.data_female, family = binomial)
        
        vif(femalemodel5)
        
        summary(femalemodel5)
        
        pR2(femalemodel5)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(femalemodel5)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Coefficient estimates
        coef_estimates <- coef(femalemodel5)
        
        # Standard errors
        se <- summary(femalemodel5)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        
        #maths motivation and maths anxiety and SES and Maths achievement (GCSE)
        
        femalemodel6 <- glm(stemcareer ~ mathsanxiety.a + mathsse.a + mathsinterest.a + mathsgcse.a + ases, data = stemavoidance.femalesample, family = binomial)
        
        vif(femalemodel6)
        
        summary(femalemodel6)
        
        pR2(femalemodel6)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(femalemodel6)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Coefficient estimates
        coef_estimates <- coef(femalemodel6)
        
        # Standard errors
        se <- summary(femalemodel6)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)       
        

#Male sample logistic regression
        
#maths motivation and maths anxiety
        
        malemodel3 <- glm(stemcareer ~ mathsanxiety.a + mathsse.a + mathsinterest.a, data = stemavoidance.malesample, family = binomial)
        
        vif(malemodel3)
        
        summary(malemodel3)
        
        pR2(malemodel3)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(malemodel3)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Coefficient estimates
        coef_estimates <- coef(malemodel3)
        
        # Standard errors
        se <- summary(malemodel3)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        #maths motivation and maths anxiety and maths achievement (GCSE)
        
        malemodel4 <- glm(stemcareer ~ mathsanxiety.a + mathsse.a + mathsinterest.a + mathsgcse.a, data = stemavoidance.malesample, family = binomial)
        
        vif(malemodel4)
        
        summary(malemodel4)
        
        pR2(malemodel4)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(malemodel4)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Coefficient estimates
        coef_estimates <- coef(malemodel4)
        
        # Standard errors
        se <- summary(malemodel4)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        
        
        #maths motivation and maths anxiety and SES
        
        malemodel5 <- glm(stemcareer ~ mathsanxiety.a + mathsmotivation.a + ases, data = stemavoidance.data_male, family = binomial)
        
        vif(malemodel5)
        
        summary(malemodel5)
        
        pR2(malemodel5)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(malemodel5)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Coefficient estimates
        coef_estimates <- coef(malemodel5)
        
        # Standard errors
        se <- summary(malemodel5)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
        #maths motivation and maths anxiety and SES and Maths achievement (GCSE)
        
        malemodel6 <- glm(stemcareer ~ mathsanxiety.a + mathsse.a + mathsinterest.a + mathsgcse.a + ases, data = stemavoidance.malesample, family = binomial)
        
        vif(malemodel6)
        
        pR2(malemodel6)
        
        summary(malemodel6)
        
        L0 <- logLik(glm(stemcareer ~ 1, family = binomial, data = stemavoidance.data))
        L <- logLik(malemodel6)
        Nagelkerke_Rsquared <- (L0 - L) / L0
        Nagelkerke_Rsquared
        
        # Coefficient estimates
        coef_estimates <- coef(malemodel6)
        
        # Standard errors
        se <- summary(malemodel6)$coefficients[, "Std. Error"]
        
        # Calculate odds ratios
        odds_ratios <- exp(coef_estimates)
        
        # Calculate lower and upper confidence intervals
        lower_ci <- exp(coef_estimates - (1.96 * se))
        upper_ci <- exp(coef_estimates + (1.96 * se))
        
        # Create a data frame for odds ratios and confidence intervals
        odds_ci <- data.frame(
          OddsRatio = odds_ratios,
          LowerCI = lower_ci,
          UpperCI = upper_ci
        )
        
        # Print the odds ratios and confidence intervals
        print(odds_ci)
        
##### Sensitivity analyses ####
        
        # Print the odds ratios and confidence intervals
        install.packages("effsize")
        library(effsize)
        library(lsr)
        
        
        
        
        #SES sensitivity analyses in maths anxiety variable 
        
        table(stemavoidance.data$mathsanxiety.as.ses)
        
        stemavoidance.data$mathsanxiety.as.ses <- ifelse(is.na(stemavoidance.data$mathsanxiety.as), 0 | !is.na(stemavoidance.data$mathsanxiety.as), 1)
        
        table(stemavoidance.data$mathsanxiety.as.ses)
        
        t_test_result <- t.test(stemavoidance.data$ases ~ stemavoidance.data$mathsanxiety.as.ses)
        
        
        # Calculate Cohen's d using the 'cohens_d' function
        cohensD(stemavoidance.data$ases ~ stemavoidance.data$mathsanxiety.as.ses)
        
        # Print the t-test result and Cohen's d
        print(t_test_result)
        print(cohen_d)
        
        
        
        
        #SES sensitivity analyses in general anxiety variable 
        
        stemavoidance.data$generalanxiety.as.ses <- ifelse(is.na(stemavoidance.data$generalanxiety.as), 0 | !is.na(stemavoidance.data$generalanxiety.as), 1)
        
        table(stemavoidance.data$generalanxiety.as.ses)
        
        t.test(stemavoidance.data$ases ~ stemavoidance.data$generalanxiety.as.ses)
        
        cohensD(stemavoidance.data$ases ~ stemavoidance.data$generalanxiety.as.ses)
        
        
        
        #SES sensitivity analyses in maths motivation variable 
        
        stemavoidance.data$mathsmotivation.as.ses <- ifelse(is.na(stemavoidance.data$mathsmotivation.as), 0 | !is.na(stemavoidance.data$mathsmotivation.as), 1)
        
        table(stemavoidance.data$mathsmotivation.as.ses)
        
        t.test(stemavoidance.data$ases ~ stemavoidance.data$mathsmotivation.as.ses)
        
        cohensD(stemavoidance.data$ases ~ stemavoidance.data$mathsmotivation.as.ses)
        
        #SES sensitivity analyses in maths gcse variable 
        
        stemavoidance.data$stemcareer.as.ses <- ifelse(is.na(stemavoidance.data$stemcareer), 0 | !is.na(stemavoidance.data$stemcareer), 1)
        
        table(stemavoidance.data$stemcareer.as.ses)
        
        t.test(stemavoidance.data$ases ~ stemavoidance.data$stemcareer.as.ses)
        
        cohensD(stemavoidance.data$ases ~ stemavoidance.data$stemcareer.as.ses)
        
        
        
        stemavoidance.data$mathsgcse.as.ses <- ifelse(is.na(stemavoidance.data$mathsgcse.as), 0 | !is.na(stemavoidance.data$mathsgcse.as), 1)
        
        cohensD(stemavoidance.data$ases ~ stemavoidance.data$mathsgcse.as.ses)
        
        t.test(stemavoidance.data$ases ~ stemavoidance.data$mathsgcse.as.ses)
        
