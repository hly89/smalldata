# each group: 50 docs, 17 groups
totaldocs<-c(1:850)
group_s6<-split(totaldocs,cut(totaldocs,17))
s6_training<-rbind(dtm_training[g[[1]][1:50],],dtm_training[g[[2]][1:50],])
for(groupidx in 3:length(g)){
  s6_training<-rbind(s6_training,dtm_training[g[[groupidx]][1:50],])
}

s6_test<-rbind(dtm_test[g[[1]][1:50],],dtm_test[g[[2]][1:50],])
for(groupidx in 3:length(g)){
  s6_test<-rbind(s6_test,dtm_test[g[[groupidx]][1:50],])
}

k<-15; # number of topics
seed<-2000;

gibbs_s6<-LDA(s6_training, k=k, method="Gibbs", control=list(seed=seed, burnin=1000, thin=100, iter=2000));

slda_s6<-list() # the results of the gibbs sampling 

# set the number of topics for each week is 13
for(t in 1:length(g)){
  #slda[[t]]<-LDA(dtm[g[[t]],], k=30, method="Gibbs", control=list(seed=as.integer(Sys.time()), burnin=2000, thin=100, iter=2000));
  slda_s6[[t]]<-LDA(s6_training[group_s6[[t]],], k=k, method="Gibbs", control=list(seed=seed, burnin=2000, thin=100, iter=2000));
}

per_sldas6<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_sldas6[per_i]<-perplexity(slda_s6[[per_i]],s6_test[group_s6[[per_i]],])
}

per_ldas6<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_ldas6[per_i]<-perplexity(gibbs_s6,s6_test[group_s6[[per_i]],])
}