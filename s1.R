# each group: 10 docs, 17 groups
totaldocs<-c(1:170)
group<-split(totaldocs,cut(totaldocs,17))
s1_training<-rbind(dtm_training[g[[1]][1:10],],dtm_training[g[[2]][1:10],])
for(groupidx in 3:length(g)){
  s1_training<-rbind(s1_training,dtm_training[g[[groupidx]][1:10],])
}

s1_test<-rbind(dtm_test[g[[1]][1:10],],dtm_test[g[[2]][1:10],])
for(groupidx in 3:length(g)){
  s1_test<-rbind(s1_test,dtm_test[g[[groupidx]][1:10],])
}

k<-15; # number of topics
seed<-2000;

gibbs_s1<-LDA(s1_training, k=k, method="Gibbs", control=list(seed=seed, burnin=1000, thin=100, iter=2000));

slda_s1<-list() # the results of the gibbs sampling 

# set the number of topics for each week is 13
for(t in 1:length(g)){
  #slda[[t]]<-LDA(dtm[g[[t]],], k=30, method="Gibbs", control=list(seed=as.integer(Sys.time()), burnin=2000, thin=100, iter=2000));
  slda_s1[[t]]<-LDA(s1_training[group[[t]],], k=k, method="Gibbs", control=list(seed=seed, burnin=2000, thin=100, iter=2000));
}


per_sldas1<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_sldas1[per_i]<-perplexity(slda_s1[[per_i]],s1_test[group[[per_i]],])
}

per_ldas1<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_ldas1[per_i]<-perplexity(gibbs_s1,s1_test[group[[per_i]],])
}

per_ldas1<-cbind(per_ldas1,c(1:17),rep("lda",17))
per_ldas1<-data.frame(per_ldas1,stringsAsFactors=FALSE)
names(per_ldas1)<-c("perplexity","time","type")
per_sldas1<-cbind(per_sldas1,c(1:17),rep("slda",17))
per_sldas1<-data.frame(per_sldas1,stringsAsFactors=FALSE)
names(per_sldas1)<-c("perplexity","time","type")
pers1<-rbind(per_ldas1,per_sldas1)
perplexity_plots1<-ggplot(data=pers1, aes(x=time,y=perplexity, group=type, colour=type))+geom_line()+geom_point()
