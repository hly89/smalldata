# each group: 15 docs, 17 groups
totaldocs<-c(1:255)
group_s2<-split(totaldocs,cut(totaldocs,17))
s2_training<-rbind(dtm_training[g[[1]][1:15],],dtm_training[g[[2]][1:15],])
for(groupidx in 3:length(g)){
  s2_training<-rbind(s2_training,dtm_training[g[[groupidx]][1:15],])
}

s2_test<-rbind(dtm_test[g[[1]][1:15],],dtm_test[g[[2]][1:15],])
for(groupidx in 3:length(g)){
  s2_test<-rbind(s2_test,dtm_test[g[[groupidx]][1:15],])
}

k<-15; # number of topics
seed<-2000;

gibbs_s2<-LDA(s2_training, k=k, method="Gibbs", control=list(seed=seed, burnin=1000, thin=100, iter=2000));

slda_s2<-list() # the results of the gibbs sampling 

# set the number of topics for each week is 13
for(t in 1:length(g)){
  #slda[[t]]<-LDA(dtm[g[[t]],], k=30, method="Gibbs", control=list(seed=as.integer(Sys.time()), burnin=2000, thin=100, iter=2000));
  slda_s2[[t]]<-LDA(s2_training[group_s2[[t]],], k=k, method="Gibbs", control=list(seed=seed, burnin=2000, thin=100, iter=2000));
}