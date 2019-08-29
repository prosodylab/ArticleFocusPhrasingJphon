#
# Models
#
library(texreg)

##
##
## look at raw measures by syllable

modelDuration1=lmer(data=filter(dd1,Position=='B'),
               syllable_duration~
                 (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                              Decl.vs.Inter*Left.vs.Right+
                    ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                       Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                    ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                       Decl.vs.Inter+Left.vs.Right||participant),
               )
summary(modelDuration1)
print(modelDuration1,correlations=T) 

modelDuration2=lmer(data=filter(dd2,Position=='B'),
                    syllable_duration~
                      (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                      Decl.vs.Inter*Left.vs.Right+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelDuration2) 

sink("../Paper/Models/modelsDuration.tex", append=FALSE, split=FALSE)
texreg(list(modelDuration1,modelDuration2),
       label="modelDuration",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the duration of word B (estimates in sec, SE in parentheses)",
       use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelDuration1)$coefficients[,'Pr(>|t|)'],summary(modelDuration2)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
       )
sink()

#
# intensity model
#

modelIntensity1=lmer(data=filter(dd1,Position=='B'),
                 Mean_Intensity~
                   (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                   Decl.vs.Inter*Left.vs.Right+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelIntensity1) 

modelIntensity2=lmer(data=filter(dd2,Position=='B'),
                 Mean_Intensity~
                   (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                   Decl.vs.Inter*Left.vs.Right+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelIntensity2) 

sink("../Paper/Models/modelsIntensity.tex", append=FALSE, split=FALSE)
texreg(list(modelIntensity1,modelIntensity2),
       label="modelIntensity",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the mean intensity of word B (estimate in dB, SE in parentheses).",use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelIntensity1)$coefficients[,'Pr(>|t|)'],summary(modelIntensity2)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
)
sink()

#
# pitch model
#

modelPitch1=lmer(data=filter(dd1,Position=='B'),
                    Max_F0~
                      (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                      Decl.vs.Inter*Left.vs.Right+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelPitch1) 

modelPitch2=lmer(data=filter(dd2,Position=='B'),
                    Max_F0~
                      (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                      Decl.vs.Inter*Left.vs.Right+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelPitch2) 

sink("../Paper/Models/modelsPitch.tex", append=FALSE, split=FALSE)
texreg(list(modelPitch1,modelPitch2),
       label="modelPitch",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the Max F$_0$ of word B (estimate in Hz, SE in parentheses).",use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelPitch1)$coefficients[,'Pr(>|t|)'],summary(modelPitch2)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
)
sink()


##
## models for word A

modelDuration1A=lmer(data=filter(dd1,Position=='A'),
                    syllable_duration~
                      (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                      Decl.vs.Inter*Left.vs.Right+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelDuration1A) 

modelDuration2A=lmer(data=filter(dd2,Position=='A'),
                    syllable_duration~
                      (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                      Decl.vs.Inter*Left.vs.Right+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelDuration2A) 

sink("../Paper/Models/modelsDurationA.tex", append=FALSE, split=FALSE)
texreg(list(modelDuration1A,modelDuration2A),
       label="modelDurationA",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the duration of word A (estimate in sec, SE in parentheses).",
       use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelDuration1A)$coefficients[,'Pr(>|t|)'],summary(modelDuration2A)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
)
sink()

#
# intensity model
#

modelIntensity1A=lmer(data=filter(dd1,Position=='A'),
                     Mean_Intensity~
                       (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                       Decl.vs.Inter*Left.vs.Right+
                       ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                          Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                       ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                          Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelIntensity1A) 

modelIntensity2A=lmer(data=filter(dd2,Position=='A'),
                     Mean_Intensity~
                       (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                       Decl.vs.Inter*Left.vs.Right+
                       ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                          Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                       ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                          Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelIntensity2A) 

sink("../Paper/Models/modelsIntensityA.tex", append=FALSE, split=FALSE)
texreg(list(modelIntensity1A,modelIntensity2A),
       label="modelIntensityA",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the mean intensity of word A (estimate in dB, SE in parentheses).",use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelIntensity1A)$coefficients[,'Pr(>|t|)'],summary(modelIntensity2A)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
)
sink()

#
# pitch model
#

modelPitch1A=lmer(data=filter(dd1,Position=='A'),
                 Max_F0~
                   (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                   Decl.vs.Inter*Left.vs.Right+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelPitch1) 

modelPitch2A=lmer(data=filter(dd2,Position=='A'),
                 Max_F0~
                   (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                   Decl.vs.Inter*Left.vs.Right+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelPitch2A) 

sink("../Paper/Models/modelsPitchA.tex", append=FALSE, split=FALSE)
texreg(list(modelPitch1A,modelPitch2A),
       label="modelPitchA",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the Max F$_0$ of word A (estimate in Hz, SE in parentheses).",use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelPitch1A)$coefficients[,'Pr(>|t|)'],summary(modelPitch2A)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
)
sink()


#
# models for word C
#

modelDuration1C=lmer(data=filter(dd1,Position=='C'),
                    syllable_duration~
                      (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                      Decl.vs.Inter*Left.vs.Right+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelDuration1C) 

modelDuration2C=lmer(data=filter(dd2,Position=='C'),
                    syllable_duration~
                      (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                      Decl.vs.Inter*Left.vs.Right+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                      ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                         Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelDuration2C) 

sink("../Paper/Models/modelsDurationC.tex", append=FALSE, split=FALSE)
texreg(list(modelDuration1C,modelDuration2C),
       label="modelDurationC",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the duration of word C (estimate in sec, SE in parentheses).",
       use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelDuration1C)$coefficients[,'Pr(>|t|)'],summary(modelDuration2C)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
)
sink()

#
# intensity model
#

modelIntensity1C=lmer(data=filter(dd1,Position=='C'),
                     Mean_Intensity~
                       (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                       Decl.vs.Inter*Left.vs.Right+
                       ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                          Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                       ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                          Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelIntensity1C) 

modelIntensity2C=lmer(data=filter(dd2,Position=='C'),
                     Mean_Intensity~
                       (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                       Decl.vs.Inter*Left.vs.Right+
                       ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                          Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                       ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                          Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelIntensity2C) 

sink("../Paper/Models/modelsIntensityC.tex", append=FALSE, split=FALSE)
texreg(list(modelIntensity1C,modelIntensity2C),
       label="modelIntensityC",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the mean intensity of word C (estimate in dB, SE in parentheses).",use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelIntensity1C)$coefficients[,'Pr(>|t|)'],summary(modelIntensity2C)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
)
sink()

#
# pitch model
#

modelPitch1C=lmer(data=filter(dd1,Position=='C'),
                 Max_F0~
                   (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                   Decl.vs.Inter*Left.vs.Right+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelPitch1C) 

modelPitch2C=lmer(data=filter(dd2,Position=='C'),
                 Max_F0~
                   (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)*
                   Decl.vs.Inter*Left.vs.Right+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||itemOriginal)+
                   ((Broad.vs.Narrow+First.vs.Late+Second.vs.Third)+
                      Decl.vs.Inter+Left.vs.Right||participant),
)
summary(modelPitch2C) 

sink("../Paper/Models/modelsPitchC.tex", append=FALSE, split=FALSE)
texreg(list(modelPitch1C,modelPitch2C),
       label="modelPitchC",
       custom.model.names=c("Initial","Final"),
       naive=TRUE,single.row = T,
       include.aic=F,
       include.deviance=F,
       include.bic=F, 
       include.loglik=F,
       include.variance=F,
       dcolumn=T, 
       include.nobs=F, 
       include.groups=F,
       caption = "Mixed Effects Regression Models for the mean F$_0$ of word C (estimate in Hz, SE in parentheses).",use.packages=F,float.pos="h!",fontsize = "footnotesize",
       # base stars on lmertest Sattersthwaite p-values:
       override.pval=c(list(summary(modelPitch1)$coefficients[,'Pr(>|t|)'],summary(modelPitch2)$coefficients[,'Pr(>|t|)']))
       # (warning for SE can be ignored--SEs in lmertest are identical)
)
sink()



