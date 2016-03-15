library(ggplot2)

d <- readRDS("data/UkDelayTimes.rds") %>% droplevels

d$time <- d$time + d$ntime
# histogram of delay times in the uk
pdf("delay_analysis/delayTimes_UK.pdf", 6, 4)
ggplot(d, aes(x = ntime/60)) +
  geom_histogram(fill = "red", alpha = 0.75, binwidth = 1) +
  xlab("Delay Time")
dev.off()

# which time of day is the worst?
pdf("delay_analysis/delayTimes_TimeOfDay.pdf", 6, 4)
ggplot(filter(d, time > "2014-07-01 05:00:00 BST" & 
                time < "2014-07-01 22:00:00 BST"), 
       aes(x = time, y = (ntime/60))) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(col = "grey") +
  ylab("Delay Time") +
  xlab("Time of Day")
dev.off()

# which airports are the worst offenders?
pdf("delay_analysis/delayTimes_Airports.pdf", 6, 4)
ggplot(d, aes(y = (ntime/60), x = reorder(AirportName, ntime, median, order=TRUE))) +
  geom_violin(bw = 1, colour = NA, fill = "grey", alpha = 0.5) +
  stat_summary(fun.y="median", geom="point", col = "red") +
  ylim(0, 30) +
  ylab("Delay Time") +
  xlab("") +
  coord_flip()
dev.off()

# # which airlines are the worst offenders?
# pdf("delay_analysis/delayTimes_Airports.pdf", 6, 4)
# ggplot(d, aes(y = (ntime/60), x = reorder(AirlineName, -ntime, median, order=TRUE))) +
#   geom_violin(bw = 1, colour = NA, fill = "grey", alpha = 0.5) +
#   stat_summary(fun.y="median", geom="point", col = "red") +
#   ylim(0, 30) +
#   ylab("Delay Time") +
#   xlab("") +
#   theme(axis.text.x = element_text(angle = 315, hjust = 0))
# dev.off()

mVn <- d %>%
  group_by(AirportName) %>%
  summarise(mDelay = median(ntime/60),
            n = n())

library(ggrepel)

corr <- cor.test(as.numeric(mVn$n), as.numeric(mVn$mDelay))


pdf("delay_analysis/delayTimes_vsCount.pdf", 6, 4)
ggplot(mVn, aes(x = n, y = mDelay)) +
  geom_point(col = "red") +
  geom_text_repel(aes(label = AirportName), 
                  segment.size = 0) +
  scale_x_log10() +
  ggtitle(label = paste0("rsq = ", 
                         signif(corr$estimate, 3),
                         ", p = ", 
                         signif(corr$p.value, 3)))+
  ylab("Delay Time") +
  xlab("Number of Flights") +
  theme(plot.title = element_text(hjust = 0))
dev.off()


