
checkIDunique <- function(df,idvar){
  require(dplyr)
  df %>% group_by_at(idvar) %>%
  dplyr::summarise(count=n()) %>% 
  filter(count!=1)
}     

whichColumnsVary <- function(df,idvar,timevar){
  #
  require(dplyr)
  #
  # Check whether idvar+timevar uniquely identify rows
  #
  countDuplicates=df %>% group_by_at(union(idvar,timevar)) %>%
    dplyr::summarise(count=n()) %>% 
    filter(count!=1)
    if (nrow(countDuplicates)!=0){print('idvar+timevar do not unique identify rows') }
  #
  # Check which columns vary and return those column names
  otherColumns=setdiff(names(df),union(idvar,timevar))
  varyingColumns = df %>% group_by_at(idvar) %>%
    dplyr::mutate_at(otherColumns,n_distinct) %>% 
    as.data.frame %>%
    dplyr::select(-one_of(union(idvar,timevar))) %>%
    dplyr::select(which(colMeans(.) > 1)) %>%
    names()
  return(varyingColumns)
}   


both=rbind(dd1,dd2)

selectedColumns=c("recordedFile", "participant", "experiment", "item", "condition", "bracketing","itemOriginal", "Item", "Syllable", "Focus", "Intonation","Constituency", "Broad.vs.Narrow", "First.vs.Late", "Second.vs.Third","Intonation", "Decl.vs.Inter", "Constituency","ConstituencyCorrect", "ProminenceCorrect", "Left.vs.Right","ExpectedIntonation", "IntonationCorrect", "ProminenceCorrect","ConstituencyCorrect","ProminenceA1","ProminenceA2","Position","woi","syllable_label", "syllable_duration", "syllable_position_in_word","Max_F0", "syllable_begin", "syllable_end", "Mean_Intensity","syllable_relativized_duration_by_speaker","Mean_F0_relativized","Mean_Intensity_relativized")
#
both=as.data.frame(dplyr::select(both,selectedColumns))

idvariable=c("recordedFile","Syllable")
timevariable=c("Position")

# 
both.wide=stats::reshape(both,
                idvar=idvariable,
                timevar=timevariable,
                v.names=whichColumnsVary(both,idvariable,timevariable),
                direction="wide")

idvariable=c("recordedFile")
timevariable=c("Syllable")

both.wide=stats::reshape(both.wide,
                 idvar=idvariable,
                 timevar=timevariable,
                 v.names=whichColumnsVary(both.wide,idvariable,timevariable),
                 direction="wide")
summary(both.wide)


predictors=c("syllable_duration.A.First","Max_F0.A.First", "Mean_Intensity.A.First", "syllable_duration.B.First", "Max_F0.B.First", "Mean_Intensity.B.First", "syllable_duration.C.First", "Max_F0.C.First","Mean_Intensity.C.First", "syllable_duration.A.Last", "Max_F0.A.Last", "Mean_Intensity.A.Last",  "syllable_duration.B.Last", "Max_F0.B.Last",  "Mean_Intensity.B.Last",  "syllable_duration.C.Last", "Max_F0.C.Last", "Mean_Intensity.C.Last")

predictorsRelative=c("syllable_relativized_duration_by_speaker.A.First","Mean_F0_relativized.A.First", "Mean_Intensity_relativized.A.First", "syllable_relativized_duration_by_speaker.B.First", "Mean_F0_relativized.B.First", "Mean_Intensity_relativized.B.First", "syllable_relativized_duration_by_speaker.C.First", "Mean_F0_relativized.C.First","Mean_Intensity_relativized.C.First", "syllable_relativized_duration_by_speaker.A.Last", "Mean_F0_relativized.A.Last", "Mean_Intensity_relativized.A.Last",  "syllable_relativized_duration_by_speaker.B.Last", "Mean_F0_relativized.B.Last",  "Mean_Intensity_relativized.B.Last",  "syllable_relativized_duration_by_speaker.C.Last", "Mean_F0_relativized.C.Last", "Mean_Intensity_relativized.C.Last")


#
# Random forest
#
#

# set up control parameters
data.controls = cforest_unbiased(ntree=1000, mtry=4)
set.seed(1745)

subs=both.wide
subsBroad=filter(both.wide,Focus=="Broad")
subsFirst=filter(both.wide,Focus=="First")
subsFirstCorrect=filter(both.wide,Focus=="First"&ProminenceCorrect)

# grow forest for constituency
forest = cforest(Constituency ~ ., subs[,union(predictors,"Constituency")], controls=data.controls)
# predict
predictions = predict(forest)
sum(subs$Constituency==predictions)/nrow(subs)
#
# out-of-bag prediction
predictionsOOB = predict(forest, OOB=TRUE)
sum(subs$Constituency==predictionsOOB)/nrow(subs)
#
subs.varimp = varimp(forest)
subs.varimp = sort(subs.varimp)
print(subs.varimp)
#
# store predictions in actual data frame:
both.wide$PredicOOB=predictionsOOB
both.wide$CorrectPredictionOOB=(both.wide$Constituency==both.wide$PredicOOB)


# grow forest for constituency, Broad focus
forestBroad = cforest(Constituency ~ ., subsBroad[,union(predictors,"Constituency")], controls=data.controls)
# predict
predictionsBroad = predict(forestBroad)
sum(subsBroad$Constituency==predictionsBroad)/nrow(subsBroad)
#
# out-of-bag prediction
predictionsOOBBroad = predict(forestBroad, OOB=TRUE)
predictionsOOBBroadCorrect=(subsBroad$Constituency==predictionsOOBBroad)
sum(predictionsOOBBroadCorrect)/nrow(subsBroad)
#
subsBroad.varimp = varimp(forestBroad)
subsBroad.varimp = sort(subsBroad.varimp)
print(subsBroad.varimp)
#
# store predictions in general data frame
both.wide$predictionsOOBBroad[both.wide$Focus=='Broad']=predictionsOOBBroad
both.wide$predictionsOOBBroadCorrect[both.wide$Focus=='Broad']=predictionsOOBBroadCorrect


# grow forest for constituency, first focus
forestFirst = cforest(Constituency ~ ., subsFirst[,union(predictors,"Constituency")], controls=data.controls)
# predict
predictionsFirst = predict(forestFirst)
sum(subsFirst$Constituency==predictionsFirst)/nrow(subsFirst)
#
# out-of-bag prediction
predictionsOOBFirst = predict(forestFirst, OOB=TRUE)
predictionsOOBFirstCorrect=(subsFirst$Constituency==predictionsOOBFirst)
sum(predictionsOOBFirstCorrect)/nrow(subsFirst)
#
subsFirst.varimp = varimp(forestFirst)
subsFirst.varimp = sort(subsFirst.varimp)
print(subsFirst.varimp)

# store predictions in general data frame
both.wide$predictionsOOBFirst[both.wide$Focus=='First']=predictionsOOBFirst
both.wide$predictionsOOBFirstCorrect[both.wide$Focus=='First']=predictionsOOBFirstCorrect


#annotationFirstCorrect=(as.character(subsFirst$Constituency)==as.character(subsFirst$ConstituencyA2))&!is.na(subsFirst$ConstituencyA2)


# grow forest for constituency, first focus, correctly perceived
forestFirstC = cforest(Constituency ~ ., subsFirstCorrect[,union(predictors,"Constituency")], controls=data.controls)
# predict
predictionsFirstC = predict(forestFirstC)
sum(subsFirstCorrect$Constituency==predictionsFirstC)/nrow(subsFirstCorrect)
#
# out-of-bag prediction
predictionsOOBFirstC = predict(forestFirstC, OOB=TRUE)
predictionsOOBFirstCCorrect=(subsFirstCorrect$Constituency==predictionsOOBFirstC)
sum(predictionsOOBFirstCCorrect)/nrow(subsFirstCorrect)
#
subsFirstC.varimp = varimp(forestFirstC)
subsFirstC.varimp = sort(subsFirstC.varimp)
print(subsFirstC.varimp)




#
# Plot ranking of variables:

#
pdf("../Paper/Figures/phrasingVarimp.pdf")
dotchart(subs.varimp, xlim=c(-0.005, 0.08))
abline(v=0, col="black")
abline(v=abs(min(subs.varimp)), col="black", lt=2)
dev.off()
#
pdf("../Paper/Figures/phrasingBroadVarimp.pdf")
dotchart(subsBroad.varimp, xlim=c(-0.005, 0.08))
abline(v=0, col="black")
abline(v=abs(min(subsBroad.varimp)), col="black", lt=2)
dev.off()
#
pdf("../Paper/Figures/phrasingFirstVarimp.pdf")
dotchart(subsFirst.varimp, xlim=c(-0.005, 0.08))
abline(v=0, col="black")
abline(v=abs(min(subsFirst.varimp)), col="black", lt=2)
dev.off()
# first focus, correctly annotated by annotator:
pdf("../Paper/Figures/phrasingFirstCVarimp.pdf")
dotchart(subsFirstC.varimp, xlim=c(-0.005, 0.1))
abline(v=0, col="black")
abline(v=abs(min(subsFirstC.varimp)), col="black", lt=2)
dev.off()



# #
# TablePredicOOB=both.wide %>% group_by(Constituency) %>%
#   mutate(CorrectPredictionOOB=(Constituency==PredicOOB)) %>%
#   summarise(Annotation.all=sum(ConstituencyCorrect)/n(),
#             R.F.All=sum(CorrectPredictionOOB)/n(),
#             R.F.Broad=sum(predictionsOOBBroadCorrect)/nrow(subsBroad),
#             R.F.First=sum(predictionsOOBFirstCorrect)/nrow(subsFirst)      
#   )


#
both.wide$broadConstituencyCorrect=both.wide$ConstituencyCorrect
both.wide$broadConstituencyCorrect[both.wide$Focus!='Broad']=NA
#
both.wide$firstConstituencyCorrect=both.wide$ConstituencyCorrect
both.wide$firstConstituencyCorrect[both.wide$Focus!='First']=NA
#
both.wide$broadProminenceCorrect=both.wide$ProminenceCorrect
both.wide$broadProminenceCorrect[both.wide$Focus!='Broad']=NA
#
both.wide$firstProminenceCorrect=both.wide$ProminenceCorrect
both.wide$firstProminenceCorrect[both.wide$Focus!='First']=NA
  

TablePredicOOB=both.wide %>% group_by(Constituency) %>%
  summarise(
    'R.F./Annotator--All'=paste0(round(sum(CorrectPredictionOOB)/n(),2),
            '/',
            round(sum(ConstituencyCorrect)/n(),2)),
    'R.F./Annotator--Broad'= paste0(round(sum(predictionsOOBBroadCorrect,na.rm=T)/sum(Focus=='Broad'),2),
             '/',
             round(sum(broadConstituencyCorrect,na.rm=T)/sum(Focus=='Broad'),2)),
    'R.F./Annotator--First'= paste0(round(sum(predictionsOOBFirstCorrect,na.rm=T)/sum(Focus=='First'),2),
             '/',
             round(sum(firstConstituencyCorrect,na.rm=T)/sum(Focus=='First'),2)))

sink("../Paper/Models/phrasingForest.tex", append=FALSE, split=FALSE)
print(xtable(TablePredicOOB,caption='Proportion of accurate classification for all data, broad focus data, and first focus data. For each data set, both the accuracy of the random forest classification (R.F.) and the human annotation is given.',label='phrasingForestTable'),comment = FALSE,size="\\footnotesize")
sink()
#


# forest for focus
#
subs=both.wide
subsDec=filter(both.wide,Intonation=="Declarative")
subsInt=filter(both.wide,Intonation=="Interrogative")
#
# grow forest for focus
forestFocus = cforest(Focus ~ ., subs[,union(predictors,"Focus")], controls=data.controls)
# predict
predictionsFocus = predict(forestFocus)
sum(subs$Focus==predictionsFocus)/nrow(subs)
#
# out-of-bag prediction
predictionsOOBFocus = predict(forestFocus, OOB=TRUE)
predictionsOOBFocusCorrect=subs$Focus==predictionsOOBFocus
sum(predictionsOOBFocusCorrect)/nrow(subs)
#
subsFocus.varimp = varimp(forestFocus)
subsFocus.varimp = sort(subsFocus.varimp)
print(subsFocus.varimp)
#
# store predictions in general data frame
both.wide$predictionsOOBFocus=predictionsOOBFocus
both.wide$predictionsOOBFocusCorrect=predictionsOOBFocusCorrect

# focus forest for declarative utterances
#
forestFocusDec = cforest(Focus ~ ., subsDec[,union(predictors,"Focus")], controls=data.controls)
# predict
predictionsFocusDec = predict(forestFocusDec)
sum(subsDec$Focus==predictionsFocusDec)/nrow(subsDec)
#
# out-of-bag prediction
predictionsOOBFocusDec = predict(forestFocusDec, OOB=TRUE)
predictionsOOBDecCorrect=subsDec$Focus==predictionsOOBFocusDec
sum(predictionsOOBDecCorrect)/nrow(subsDec)
#
subsFocusDec.varimp = varimp(forestFocusDec)
subsFocusDec.varimp = sort(subsFocusDec.varimp)
print(subsFocusDec.varimp)
#
# store predictions in general data frame
both.wide$predictionsOOBFocusDec[both.wide$Intonation=='Declarative']=predictionsOOBFocusDec
both.wide$predictionsOOBDecCorrect[both.wide$Intonation=='Declarative']=predictionsOOBDecCorrect


## focus forest for interrogative utterances
#
forestFocusInt = cforest(Focus ~ ., subsInt[,union(predictors,"Focus")], controls=data.controls)
# predict
predictionsFocusInt = predict(forestFocusInt)
sum(subsInt$Focus==predictionsFocusInt)/nrow(subsInt)
#
# out-of-bag prediction
predictionsOOBFocusInt = predict(forestFocusInt, OOB=TRUE)
predictionsOOBIntCorrect=subsInt$Focus==predictionsOOBFocusInt
sum(predictionsOOBIntCorrect)/nrow(subsInt)
#
subsFocusInt.varimp = varimp(forestFocusInt)
subsFocusInt.varimp = sort(subsFocusInt.varimp)
print(subsFocusInt.varimp)
#
# store predictions in general data frame
both.wide$predictionsOOBFocusInt[both.wide$Intonation=='Interrogative']=predictionsOOBFocusInt
both.wide$predictionsOOBIntCorrect[both.wide$Intonation=='Interrogative']=predictionsOOBIntCorrect


# plot varimp 
#
pdf("../Paper/Figures/FocusVarimp.pdf")
dotchart(subsFocus.varimp, xlim=c(-0.005, 0.06))
abline(v=0, col="black")
abline(v=abs(min(subsFocus.varimp)), col="black", lt=2)
dev.off()
#
pdf("../Paper/Figures/FocusVarimpDec.pdf")
dotchart(subsFocusDec.varimp, xlim=c(-0.005, 0.06))
abline(v=0, col="black")
abline(v=abs(min(subsFocusDec.varimp)), col="black", lt=2)
dev.off()
#
pdf("../Paper/Figures/FocusVarimpInt.pdf")
dotchart(subsFocusInt.varimp, xlim=c(-0.005, 0.06))
abline(v=0, col="black")
abline(v=abs(min(subsFocusInt.varimp)), col="black", lt=2)
dev.off()

# Plot accuracy table for focus
#

#
both.wide$decProminenceCorrect=both.wide$ProminenceCorrect
both.wide$decProminenceCorrect[both.wide$Intonation!='Declarative']=NA
#
both.wide$intProminenceCorrect=both.wide$ProminenceCorrect
both.wide$intProminenceCorrect[both.wide$Intonation!='Interrogative']=NA


TablePredicOOBFocus = 
  both.wide %>% group_by(Focus) %>%
  summarise(
    'R.F./Annotator--All'=paste0(round(sum(predictionsOOBFocusCorrect)/n(),2),
                             '/',
                             round(sum(ProminenceCorrect)/n(),2)),
    'R.F./Annotator--Dec'= paste0(round(sum(predictionsOOBDecCorrect,na.rm=T)/sum(Intonation=='Declarative'),2),
                               '/',
                               round(sum(decProminenceCorrect,na.rm=T)/sum(Intonation=='Declarative'),2)),
    'R.F./Annotator--Int'= paste0(round(sum(predictionsOOBIntCorrect,na.rm=T)/sum(Intonation=='Declarative'),2),
                                '/',
                                round(sum(intProminenceCorrect,na.rm=T)/sum(Intonation=='Declarative'),2)))



sink("../Paper/Models/focusForest.tex", append=FALSE, split=FALSE)
print(xtable(TablePredicOOBFocus,caption='Proportion of accurate classification for all data, declarative  data (Dec), and interrogative data (Int). For each data set, both the accuracy of the random forest classification (R.F.), as well as of the human annotation is given.',label="focusForestTable"),comment = FALSE,size="\\footnotesize")
sink()

confusionTableBroad = both.wide %>% 
  filter(!is.na(ProminenceA1)&Focus=='Broad') %>%
  count(Constituency,ProminenceA1) %>%
  spread(ProminenceA1,n,fill = 0)

sink("../Paper/Models/confusionBroad.tex", append=FALSE, split=FALSE) 
#
print(xtable(confusionTableBroad,
      caption='Confusion matrix for the annotation of Broad Focus utterances depending on phrasing',label='confusionTable'),
      comment = FALSE,
      size="\\footnotesize")
  #
sink()      



# # CART model
# library(rpart)
# library(rpart.plot)
# 
# subs=both.wide
# subsWide=filter(both.wide,Focus=="Wide")
# subsFirst=filter(both.wide,Focus=="First")
# 
# tree = rpart(Constituency~., subs[,predictorsTree])
# rpart.plot(tree)
# 
# conf.matrix = table(predict(tree, type="class"), subs$Constituency)
# 
# sum(subs$Constituency==predict(tree, type="class"))/nrow(subs)
# 
# plotcp(tree)
