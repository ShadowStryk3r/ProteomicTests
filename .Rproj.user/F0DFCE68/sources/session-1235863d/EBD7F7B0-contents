hc_means <- list()
ac_means <- list()
fc <- list()
pvals <- list()
for(protein in 1:1317){
  thing <- c()
  temp <- 0
  for(hc in 1:8){
    temp <- temp + raw.data[protein,hc]
    thing <- append(thing, raw.data[protein,hc])
  }
  temp<-temp/8
  hc_means[length(hc_means)+1]=temp
  temp2 <- 0
  thing2<-c()
  for(ac in 9:26){
    temp2 <- temp2 + raw.data[protein,ac]
    thing2 <- append(thing2, raw.data[protein,ac])
  }
  temp2<-temp2/18
  ac_means[length(ac_means)+1]=temp2
  fc[length(fc)+1] = temp2/temp
  pvals[length(pvals)+1] = wilcox.test(thing, thing2)$p.value
}
