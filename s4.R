# each group: 20 docs, 17 groups
totaldocs<-c(1:340)
group_s4<-split(totaldocs,cut(totaldocs,17))
s4_training<-rbind(dtm_training[g[[1]][1:20],],dtm_training[g[[2]][1:20],])
for(groupidx in 3:length(g)){
  s4_training<-rbind(s4_training,dtm_training[g[[groupidx]][1:20],])
}

s4_test<-rbind(dtm_test[g[[1]][1:20],],dtm_test[g[[2]][1:20],])
for(groupidx in 3:length(g)){
  s4_test<-rbind(s4_test,dtm_test[g[[groupidx]][1:20],])
}

k<-15; # number of topics
seed<-2000;

gibbs_s4<-LDA(s4_training, k=k, method="Gibbs", control=list(seed=seed, burnin=1000, thin=100, iter=2000));

slda_s4<-list() # the results of the gibbs sampling 

# set the number of topics for each week is 13
for(t in 1:length(g)){
  #slda[[t]]<-LDA(dtm[g[[t]],], k=30, method="Gibbs", control=list(seed=as.integer(Sys.time()), burnin=2000, thin=100, iter=2000));
  slda_s4[[t]]<-LDA(s4_training[group_s4[[t]],], k=k, method="Gibbs", control=list(seed=seed, burnin=2000, thin=100, iter=2000));
}

per_sldas4<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_sldas4[per_i]<-perplexity(slda_s4[[per_i]],s4_test[group_s4[[per_i]],])
}

per_ldas4<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_ldas4[per_i]<-perplexity(gibbs_s4,s4_test[group_s4[[per_i]],])
}