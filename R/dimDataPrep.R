
## Data prep for dimensions paper
dannot=read.csv("data/all_responses.txt",sep='\t')

# load acoustic measures
dd.pitch = read.csv("data/dimensions_syllable_pitch_export.csv")
dd.intensity = read.csv("data/dimensions_syllable_intensity_export.csv")
dd <- merge(dd.pitch, dd.intensity)

#
dd$participant=dd$speaker
dd$experiment=factor(unlist(lapply(strsplit(as.character(dd$sound_file_name), "\\_"), "[", 1)))
dd$recordedFile=paste0(dd$sound_file_name,'.wav')

participants=read.csv("data/participants.txt",sep='\t')

# merge acoustics with responses file
dd=merge(dd,dannot,all.x=TRUE,by=c('recordedFile'),suffixes=c("",".other"))

dd$itemOriginal=dd$item_original
dd$Item=factor(dd$itemOriginal)

numericCols=c("syllable_begin", "syllable_duration", "syllable_end", "syllable_position_in_word", "syllable_relativized_duration_by_speaker", "word_begin", "word_duration", "word_end", "word_position_in_utterance", "word_relativized_duration_by_speaker", "Mean_F0", "Mean_F0_relativized", "Stdev_F0", "Stdev_F0_relativized", "Median_F0", "Median_F0_relativized", "Max_F0", "Max_F0_relativized", "Min_F0", "Min_F0_relativized", "Mean_Intensity", "Mean_Intensity_relativized", "Stdev_Intensity", "Stdev_Intensity_relativized", "Median_Intensity", "Median_Intensity_relativized", "Max_Intensity", "Max_Intensity_relativized", "Min_Intensity", "Min_Intensity_relativized")

#
# a look at an example item set:
dd %>%
  filter(item=='4') %>%
  group_by(condition,text) %>%
  dplyr::summarise (n = n())  %>%
  as.data.frame


# adjust woi annotation:
dd$word_position_in_utterance[dd$condition%in%c(4)&dd$word_position_in_utterance!=1]=dd$word_position_in_utterance[dd$condition%in%c(4)&dd$word_position_in_utterance!=1]+2

nColumns = ncol(dd)
# convert to numeric column, otherwise treat as factor:
for (i in 1:nColumns) {
  if (colnames(dd)[i] %in% numericCols) {
    dd[, i] <- as.numeric(as.character(dd[, i]))
  } else {
    dd[, i] <- as.factor(as.character(dd[, i]))
  }
}

dd %>% group_by(experiment) %>% dplyr::summarise(length(unique(participant)))
#
participantsN=unique(dd$participant[dd$experiment=='suborn'])
participantsQ=unique(dd$participant[dd$experiment=='suborq'])
participantsBoth=participantsN[participantsN%in%participantsQ]
#
# only consider at participants that were in both studies
dd=subset(dd,participant%in%participantsBoth)
#
# 26 participated in both experiments:
dd %>% group_by(experiment) %>% dplyr::summarise(length(unique(participant)))

# First and final syllable:
dd$Syllable=NA
dd$Syllable[dd$syllable_position_in_word=='1']='First'
dd$Syllable[dd$syllable_position_in_word=='3']='Last'
dd$Syllable[dd$Item=='1'&dd$word_position_in_utterance%in%c('5','6')&dd$syllable_position_in_word=='2']='Last'
dd$Syllable[dd$Item=='2'&dd$syllable_position_in_word=='2']='Last'
dd$Syllable[dd$Item=='4'&dd$word_position_in_utterance%in%c('5','6')&dd$syllable_position_in_word=='2']='Last'
dd$Syllable=factor(dd$Syllable)
# summary(factor(dd$Syllable[dd$woi%in%c('6','8','10')]))

# Focus coding:
dd$Focus=factor(dd$sound_file_focus,levels=c("Wide","First", "Second", "Third"))
#
# Recode 'wide' as 'broad'
dd$Focus=dplyr::recode_factor(dd$Focus,"Wide"="Broad")
#
contrasts(dd$Focus)=cbind("Broad.vs.Narrow"=c(-3/4,1/4,1/4,1/4),"First.vs.Late"=c(0,-2/3,1/3,1/3),"Second.vs.Third"=c(0,0,-1/2,1/2))
dd$Broad.vs.Narrow=model.matrix(~ Focus,dd)[,2]
dd$First.vs.Late=model.matrix(~ Focus,dd)[,3]
dd$Second.vs.Third=model.matrix(~ Focus,dd)[,4]
# (Broad.vs.Narrow+First.vs.Late+Second.vs.Third)


#Intonation coding:
dd$Intonation=factor(dd$sound_file_intonation)
contrasts(dd$Intonation)=cbind("Decl.vs.Inter"=c(-0.5,0.5))
dd$Decl.vs.Inter=model.matrix(~ Intonation,dd)[,2]

# Coding of phrasing:
dd$Constituency=factor(dd$sound_file_structure)
#
contrasts(dd$Constituency)=cbind("Left.vs.Right"=c(-0.5,0.5))
dd$Left.vs.Right=model.matrix(~ Constituency,dd)[,2]

# In Broad condition, renumber words for consistency
dd$woi=as.numeric(as.character(dd$word_position_in_utterance))
dd$woi=factor(dd$woi)


# RA annotations
#
length(unique(dd$recordedFile))

# Problematic soundfiles: 86, 6.9% of the data
length(unique(dd$recordedFile[dd$Thea_problematic=='1'|dd$David_problematic=='1'|dd$Erin2_problematic=='1']))

# exclude files marked as problematic:
dd=subset(dd,Thea_problematic!='1'&Thea_intonation!=4&Erin2_problematic!='1'&dd$David_problematic!='1')

# remove level 4='problematic' from Thea's intonation annotation:
dd$Thea_intonation=factor(dd$Thea_intonation)

length(unique(dd$recordedFile))
# remaining files: 1166/1253

# beginPause("What kind of response?")
# comment ("Where does the main prominence fall?")
# anno = choice ("intonation",3)
# option ("Falling")
# option ("Rising")
# option ("unclear")
# option ("problematic")
# anno = choice ("focus",5)
# option ("Wide Focus")
# option ("First")
# option ("Second")
# option ("Third")
# option ("unclear")
# anno = choice ("branching",3)
# option ("(a b) c")
# option ("a (b c)")
# option ("unclear")
# anno = boolean("problematic",0)
# clicked = endPause("Continue",1)

# "Cohen suggested the Kappa result be interpreted as follows: values ≤ 0 as indicating no agreement and 0.01–0.20 as none to slight, 0.21–0.40 as fair, 0.41– 0.60 as moderate, 0.61–0.80 as substantial, and 0.81–1.00 as almost perfect agreement."

# Intonation
dd$IntonationA1=recode_factor(dd$Thea_intonation,"1"="Falling","2"="Rising","3"="Unclear",.default=NA_character_)
dd$IntonationA2=recode_factor(dd$Erin2_intonation,"1"="Falling","2"="Rising","3"="Unclear",.default=NA_character_)
#
dd$IntonationTest=dd$IntonationA2

# test for inter-annotator agreement (not sure why warning message)
cohen.kappa(x=cbind(dd$IntonationA1,dd$IntonationA2))

dd$ExpectedIntonation=recode_factor(dd$Intonation,"Declarative"="Falling","Interrogative"="Rising")
levels(dd$ExpectedIntonation)=c("Falling","Rising","Unclear")
#
dd = dd %>% group_by(recordedFile) %>%
  mutate(IntonationCorrect=(ExpectedIntonation==IntonationTest&!is.na(IntonationTest))) %>% ungroup 
         
# overall proportion of intonation annotation as expected based on annotation by A1: 0.963
nrow(dd[dd$IntonationCorrect,])/nrow(dd)


# Prominence:
dd$ProminenceA1=recode_factor(dd$Thea_prominence,"1"="Broad","2"="First","3"="Second","4"="Third",.default=NA_character_)
levels(dd$ProminenceA1)=c("Broad","First","Second","Third")

dd$ProminenceA2=recode_factor(dd$Erin2_prominence,"1"="Broad","2"="First","3"="Second","4"="Third",.default=NA_character_)
levels(dd$ProminenceA1)=c("Broad","First","Second","Third")
  
# Prominence: agreement 0.66 'substantial'
cohen.kappa(x=cbind(dd$ProminenceA1,dd$ProminenceA2))
#
# Correct whether expected placement of prominence:
dd = dd %>% group_by(recordedFile) %>%
  mutate(ProminenceCorrect=(Focus==ProminenceA2&!is.na(ProminenceA2))) %>% ungroup 

#
# overall proportion of prominence as expected, based on annotation by A1: 0.388
nrow(dd[dd$ProminenceCorrect,])/nrow(dd)

#
# Breakdown of proportions:
dd %>% filter(!is.na(ProminenceA2)) %>%
  group_by(Focus,ProminenceA2) %>%
  dplyr::summarise (n = n()) %>%
  mutate(Proportion = round(n / sum(n),2))


# Branching:
dd$ConstituencyA1=recode_factor(dd$Thea_branching,"1"="(AB)C","2"="A(BC)","3"="Unclear",.default=NA_character_)
#
dd$ConstituencyA2=recode_factor(dd$Erin2_branching,"1"="(AB)C","2"="A(BC)","3"="Unclear",.default=NA_character_)

# Branching: interrater reliability: 0.73 (substantial)
cohen.kappa(x=cbind(dd$ConstituencyA1,dd$ConstituencyA2))

dd = dd %>% group_by(recordedFile) %>%
  mutate(ConstituencyCorrect=(as.character(Constituency)==as.character(ConstituencyA2)&!is.na(ConstituencyA2))) %>% ungroup 

#
# overall proportion of constituency as expected based on annotation by A1: 0.616
#
nrow(dd[dd$ConstituencyCorrect,])/nrow(dd)

# breakdown of correct/incorrect by levels:
dd %>% filter(!is.na(ProminenceA2)) %>%
  group_by(Constituency,ConstituencyA2) %>%
  dplyr::summarise (n = n()) %>%
  mutate(Proportion = round(n / sum(n),2))

# old woi annotation (now connectors have numbers too)
# I THOUGHT THEY SAID MARION_1 OR MARVIN_2 AND SARAH_3 ARRIVED BUT IN FACT THEY SAID THAT MARION_4 OR MARVIN_5 AND NOLAN_6 ARRIVED

# Manipulation in the Experiment:
# 1: Intonation: Declarative vs. Interrogative (i.e., polar question)
# 2: Focus: Which word is focused? Broad focus on entire coordinate structure vs. First (=woi 9), Second (=woi 11), or Third (=woi12)
# 3: Constituency: Do first two conjuncts form a constituent or the second two conjuncts?

# Manipulation 1 (intonation) was done between 2 sub-experiments; 2 (Focus) & 3 (Constituency) were within a single experiment

# There were 4 item sets
length(unique(dd$itemOriginal))




# here, we only look at the three NPs (Names) in the second clause:
dd2=subset(dd,word_position_in_utterance%in%c('4','5','6'))


# compute energy and PitchPulses
dd2$Energy=dd2$syllable_duration*dd2$Max_Intensity
dd2$PitchPulses=dd2$syllable_duration*dd2$Mean_F0


dd2$Position=recode_factor(dd2$word_position_in_utterance,'4'='A','5'='B','6'='C')
contrasts(dd2$Position)=cbind("First.vs.Late"=c(-2/3,1/3,1/3),"Second.vs.Third"=c(0,-0.5,0.5))
dd2$PosFirst.vs.Late=model.matrix(~ Position,dd2)[,2]
dd2$PosSecond.vs.Third=model.matrix(~ Position,dd2)[,3]
# (PosFirst.vs.Late+PosSecond.vs.Third)
dd1=subset(dd2,Syllable=='First')
dd2=subset(dd2,Syllable=='Last')

# create 'wide' data frame with one row per utterance
# dput(names(dd2))
# columns in dd2 that vary depending on woi
varyColumns=c("syllable_label", "syllable_begin", "syllable_duration", "syllable_end", "syllable_position_in_word", "syllable_relativized_duration_by_speaker", "word_label", "word_begin", "word_duration", "word_end", "word_position_in_utterance", "word_relativized_duration_by_speaker", "Mean_F0", "Mean_F0_relativized", "Stdev_F0", "Stdev_F0_relativized", "Median_F0", "Median_F0_relativized", "Max_F0", "Max_F0_relativized", "Min_F0", "Min_F0_relativized", "Mean_Intensity", "Mean_Intensity_relativized", "Stdev_Intensity", "Stdev_Intensity_relativized", "Median_Intensity", "Median_Intensity_relativized", "Max_Intensity", "Max_Intensity_relativized", "Min_Intensity", "Min_Intensity_relativized","Position", "PosFirst.vs.Late", "PosSecond.vs.Third")

# number of observations in each cell:
numberObservations=dd1 %>% 
  filter(Position=='B') %>%
  group_by(Intonation, Constituency, Focus) %>%
  summarise(n = n())

sink("../Paper/Models/numberObservations.tex", append=FALSE, split=FALSE) 
#
print(xtable(numberObservations,
             caption='Number of observations considered in acoustic analysis for each cell',label='numberObservations'),
      comment = FALSE,
      size="\\footnotesize")
#
sink()      




# # 
# dd1.wide=reshape(dd1,idvar=c("experiment", "participant","item","condition"),
#                  v.names=varyColumns,
#                  timevar=c("woi"),direction="wide")
# dd2.wide=reshape(dd2,idvar=c("experiment", "participant","item","condition"),
#                  v.names=varyColumns,
#                  timevar=c("woi"),direction="wide")


