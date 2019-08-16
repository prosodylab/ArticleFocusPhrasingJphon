
plotCol=c("syllable_duration","Max_F0", "Mean_Intensity","PitchPulses","Energy")
plotYLabel=c("Duration","Max F0", "Mean Intensity","PitchPulses","Energy")
plotYLowLimit=c(0.12,165,55,20,7)
plotYHighLimit=c(0.26,240,65,52,20)
names(plotYLabel)=plotCol
names(plotYLowLimit)=plotCol
names(plotYHighLimit)=plotCol

# EDA plots

for (i in plotCol) {
  
  # Syllable 1
  Syl1.plot <- ggplot(dd1, aes_string(x='Position', y=i,shape='Focus')) + stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + facet_grid(~ Constituency + Intonation, scales = "fixed") + theme_bw(base_size=10) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Initial Syllable') + ylab(plotYLabel[i]) + coord_cartesian(ylim=c(plotYLowLimit[i],plotYHighLimit[i]))
  Syl1.plot
  
  # Syllable 2
  Syl2.plot <- ggplot(dd2, aes_string(x='Position', y=i,shape='Focus'))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + facet_grid(~ Constituency + Intonation, scales = "fixed") + theme_bw(base_size=10) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Final Syllable')  + ylab(plotYLabel[i]) + coord_cartesian(ylim=c(plotYLowLimit[i],plotYHighLimit[i]))
  Syl2.plot 
  
  pdf(paste0("../Paper/Figures/",i,".pdf"),width=8,height=3, onefile=FALSE)
    grid_arrange_shared_legend(Syl1.plot,Syl2.plot, ncol = 2, nrow = 1)
  dev.off()
  
}


## Additional plots looking at Canadians vs. Americans

# # Data of Canadians
# 
# dd1Can=filter(dd1,Country=='Canada')
# dd2Can=filter(dd2,Country=='Canada')
# 
# 
# for (i in plotCol) {
#   
#   # Syllable 1
#   Syl1.plot <- ggplot(dd1Can, aes_string(x='Position', y=i,shape='Focus')) + stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + facet_grid(~ Constituency + Intonation, scales = "fixed") + theme_bw(base_size=10) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Initial Syllable') + ylab(plotYLabel[i]) + coord_cartesian(ylim=c(plotYLowLimit[i],plotYHighLimit[i]))
#   Syl1.plot
#   
#   # Syllable 2
#   Syl2.plot <- ggplot(dd2Can, aes_string(x='Position', y=i,shape='Focus'))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + facet_grid(~ Constituency + Intonation, scales = "fixed") + theme_bw(base_size=10) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Final Syllable')  + ylab(plotYLabel[i]) + coord_cartesian(ylim=c(plotYLowLimit[i],plotYHighLimit[i]))
#   Syl2.plot 
#   
#   pdf(paste0("../Paper/Figures/",i,"_Can.pdf"),width=8,height=3, onefile=FALSE)
#   grid_arrange_shared_legend(Syl1.plot,Syl2.plot, ncol = 2, nrow = 1)
#   dev.off()
#   
# }
# 
# 
# # Data of Americans
# 
# dd1US=filter(dd1,Country=='US')
# dd2US=filter(dd2,Country=='US')
# 
# 
# for (i in plotCol) {
#   
#   # Syllable 1
#   Syl1.plot <- ggplot(dd1US, aes_string(x='Position', y=i,shape='Focus')) + stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + facet_grid(~ Constituency + Intonation, scales = "fixed") + theme_bw(base_size=10) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Initial Syllable') + ylab(plotYLabel[i]) + coord_cartesian(ylim=c(plotYLowLimit[i],plotYHighLimit[i]))
#   Syl1.plot
#   
#   # Syllable 2
#   Syl2.plot <- ggplot(dd2US, aes_string(x='Position', y=i,shape='Focus'))+ stat_summary(fun.y=mean, geom="point")  + stat_summary(fun.y = "mean", geom="line", aes(group=Focus)) + facet_grid(~ Constituency + Intonation, scales = "fixed") + theme_bw(base_size=10) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Final Syllable')  + ylab(plotYLabel[i]) + coord_cartesian(ylim=c(plotYLowLimit[i],plotYHighLimit[i]))
#   Syl2.plot 
#   
#   pdf(paste0("../Paper/Figures/",i,"_US.pdf"),width=8,height=3, onefile=FALSE)
#   grid_arrange_shared_legend(Syl1.plot,Syl2.plot, ncol = 2, nrow = 1)
#   dev.off()
#   
# }