df<-read.csv('News.csv',stringsAsFactors = FALSE,strip.white = TRUE)
colnames(df)[1]<-'IDLink'
sample<-df[c(21,33,58,70,88),]
write.csv(sample,file='sample.csv',row.names = FALSE)
summary(df)
is.na(df)<-df==''
df$Topic<-as.factor(df$Topic)
summary(df$Topic)
a<-c('Facebook','GooglePlus','LinkedIn')
is.na(df[,a])<-df[,a]==-1
df<-df[complete.cases(df[,a]),]
summary(df)
b<-summary(df$Topic)
stats<-data.frame(total=b,row.names = names(b))
stats
# top 90 percentile of each social media
b<-apply(df[,a],2,quantile,probs=0.9)
stats$top90<-NA

stats[,'top10%']<-summary(df[df[,a[1]]>=b[1]|df[,a[2]]>=b[2]|df[,a[3]]>=b[3],'Topic'])
for(i in a){
  stats[,i]<-summary(df[df[,i]>=b[i],"Topic"])
}
stats$top<-round(stats[,'top10%']/stats$total*100,0)
stats$below<-stats$total-stats[,'top10%']
write.csv(stats[,c('top','below','top10%')],file='topics.csv',row.names = TRUE)

stats[,c('numFacebook','numGooglePlus','numLinkedIn')]<-stats[,a]
stats[,a]<-round(stats[,a]/stats$total*100,0)
c<-t(stats[,a])
write.csv(c,file='smtopics.csv',row.names = TRUE)

# top mass media
c<-sort(table(df$Source),decreasing = TRUE)
stats<-data.frame(types=c('Top10','Others'),count=NA,percent=NA)
stats[1,'count']<-sum(c[1:10])
stats[2,'count']<-nrow(df)-stats$count[1]
stats$percent<-round(stats$count/sum(stats$count)*100,0)
stats$mcount<-c(10,length(c)-10)
stats$mpercent<-round(stats$mcount/sum(stats$mcount)*100,2)
write.csv(stats,file='newsource.csv',row.names = FALSE)
