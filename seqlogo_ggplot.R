rm(list = ls())
#set workdesk
setwd("~/Desktop/bioinformatics/smallpro")
#load the paackage
require(ggplot2)
require(reshape2)
# remove the first line (idline) in BASH by the following code: 
# 'tail -n +2 MA0007.1.pfm >> MA0007.pfm'
# loads pfm matrix
data<-scan("MA0007.pfm")
mat <- matrix(data,byrow=TRUE,nrow=4,dimnames=list(c('A','C','G','T')))
#transfer matrix to datafarme
freqdf <- as.data.frame(t(mat))
#convert the frequency data into percentage data
freqdf$total<-rowSums(freqdf)
options(digits = 1)
freqdf$heiA<-freqdf$A/freqdf$total
freqdf$heiC<-freqdf$C/freqdf$total
freqdf$heiG<-freqdf$G/freqdf$total
freqdf$heiT<-freqdf$T/freqdf$total
#add the position column into dataframe
freqdf$pos = as.numeric(as.character(rownames(freqdf)))
#calculate the height for each letter in probalibilty
logopr <- data.frame(A=freqdf$heiA, C=freqdf$heiC,
                     G=freqdf$heiG, T=freqdf$heiT, 
                     pos=freqdf$pos)
#add##calculate and define the height for each letter in bits
freqdf$height <- apply(freqdf[,c('heiA', 'heiC','heiG','heiT')], MARGIN=1,
                       FUN=function(x){2+sum(log(x^x, base = 2))})
logobit <- data.frame(A=freqdf$heiA*freqdf$height, C=freqdf$heiC*freqdf$height,
                     G=freqdf$heiG*freqdf$height, T=freqdf$heiT*freqdf$height, 
                     pos=freqdf$pos)
#Convert an object into a molten data frame.
mf_pr <- melt(logopr, id.var='pos')
mf_bit <- melt(logobit, id.var='pos')

#plot tp the screen(alternative)
quartz(height=3, width=8)
#plot the seqlogo in probability
ggplot(data=mf_pr, aes(x=as.numeric(as.character(pos)), y=value))  +
  geom_bar(aes(fill=variable,order=value), position='stack', 
           stat='identity', alpha=0.6) +
  xlab('position')+
  ylab('probability')+
  theme_bw()
#plot the seqlogo in bits
ggplot(data=mf_bit, aes(x=as.numeric(as.character(pos)), y=value))  +
  geom_bar(aes(fill=variable,order=value), position='stack', 
           stat='identity', alpha=0.6) +
  xlab('position')+
  ylab('bit')+
  theme_bw()
#save output
quartz.save('StackOverflow_5438474.png', type='png')
