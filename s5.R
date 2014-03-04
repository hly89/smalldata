# each group: 30 docs, 17 groups
totaldocs<-c(1:510)
group_s5<-split(totaldocs,cut(totaldocs,17))
s5_training<-rbind(dtm_training[g[[1]][1:30],],dtm_training[g[[2]][1:30],])
for(groupidx in 3:length(g)){
  s5_training<-rbind(s5_training,dtm_training[g[[groupidx]][1:30],])
}

s5_test<-rbind(dtm_test[g[[1]][1:30],],dtm_test[g[[2]][1:30],])
for(groupidx in 3:length(g)){
  s5_test<-rbind(s5_test,dtm_test[g[[groupidx]][1:30],])
}

k<-15; # number of topics
seed<-2000;

gibbs_s5<-LDA(s5_training, k=k, method="Gibbs", control=list(seed=seed, burnin=1000, thin=100, iter=2000));

slda_s5<-list() # the results of the gibbs sampling 

# set the number of topics for each week is 13
for(t in 1:length(g)){
  #slda[[t]]<-LDA(dtm[g[[t]],], k=30, method="Gibbs", control=list(seed=as.integer(Sys.time()), burnin=2000, thin=100, iter=2000));
  slda_s5[[t]]<-LDA(s5_training[group_s5[[t]],], k=k, method="Gibbs", control=list(seed=seed, burnin=2000, thin=100, iter=2000));
}

per_sldas5<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_sldas5[per_i]<-perplexity(slda_s5[[per_i]],s5_test[group_s5[[per_i]],])
}

per_ldas5<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_ldas5[per_i]<-perplexity(gibbs_s5,s5_test[group_s5[[per_i]],])
}