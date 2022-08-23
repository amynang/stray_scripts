library(ggplot2)

pred = c(.6,0,.3,.1)

mmm = as.data.frame(LaplacesDemon::rdirichlet(1e3, pred*1e2))
colnames(mmm) = c("prey1",
                  "prey2",
                  "prey3",
                  "prey4")
mm = reshape2::melt(mmm)

ggplot(mm[mm$variable!="prey2",], aes(value, color = variable)) + 
  geom_density() +
  scale_x_continuous(breaks=c(.1,.3,.6)) +
  theme_classic() +
  theme(legend.position = c(.9,.75))

mean(mmm$prey1)
mean(mmm$prey2)
mean(mmm$prey3)
mean(mmm$prey4)
