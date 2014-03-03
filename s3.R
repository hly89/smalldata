# each group: 5 docs, 17 groups
totaldocs<-c(1:85)
group_s3<-split(totaldocs,cut(totaldocs,17))
s3_training<-rbind(dtm_training[g[[1]][1:5],],dtm_training[g[[2]][1:5],])
for(groupidx in 3:length(g)){
  s3_training<-rbind(s3_training,dtm_training[g[[groupidx]][1:5],])
}

s3_test<-rbind(dtm_test[g[[1]][1:5],],dtm_test[g[[2]][1:5],])
for(groupidx in 3:length(g)){
  s3_test<-rbind(s3_test,dtm_test[g[[groupidx]][1:5],])
}

k<-15; # number of topics
seed<-2000;

gibbs_s3<-LDA(s3_training, k=k, method="Gibbs", control=list(seed=seed, burnin=1000, thin=100, iter=2000));

slda_s3<-list() # the results of the gibbs sampling 

# set the number of topics for each week is 13
for(t in 1:length(g)){
  #slda[[t]]<-LDA(dtm[g[[t]],], k=30, method="Gibbs", control=list(seed=as.integer(Sys.time()), burnin=2000, thin=100, iter=2000));
  slda_s3[[t]]<-LDA(s3_training[group_s3[[t]],], k=k, method="Gibbs", control=list(seed=seed, burnin=2000, thin=100, iter=2000));
}

per_sldas3<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_sldas3[per_i]<-perplexity(slda_s3[[per_i]],s3_test[group_s3[[per_i]],])
}

per_ldas3<-vector("numeric",length(g))
for(per_i in 1:length(g)){
  per_ldas3[per_i]<-perplexity(gibbs_s3,s3_test[group_s3[[per_i]],])
}