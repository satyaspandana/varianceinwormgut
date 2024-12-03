# SI - GFP bead data

# environment
pacman::p_load(tidyverse, cowplot, grid, gridExtra)
xTextSize<-14

# Read in cleaned and labeled data
# Filtered to remove debris & show only adult worms
# and to remove buffer-only wells (used to separate conditions)
# Labeled wells with experimental conditions
GreenBeadFeed1219<-read.csv("GreenBeadFeed1219.csv")

#What does GFP look like over time?
# grab some values from controls to represent thresholds of detection

v_plain_19<-GreenBeadFeed1219 %>% # wells with beads only
  filter(SourceColumn==1) %>%
  pull(Green) %>%
  quantile(probs=0.9)

v_HK_19<-GreenBeadFeed1219 %>% # wells with heat-killed OP50 as inert food
  filter(SourceColumn==5) %>%
  pull(Green) %>%
  quantile(probs=0.9)

GreenBeadFeed1219 %>%
  ggplot(aes(x=factor(Time), y=Green, color=factor(Time)))+
  geom_boxplot()+
  geom_jitter(width=0.1)+
  geom_hline(yintercept=v_plain_19, lty="dashed")+
  geom_hline(yintercept=v_HK_19, lty="dashed", color="red")+
  scale_y_log10()+
  facet_grid(rows=vars(Bact), cols=vars(Beads))+
  #theme(legend.title = element_blank())+
  labs(x="Time (min)", y="GFP", color="Time (min)")

# Summary statistics
summarizeGreenBeadFeed1219<-GreenBeadFeed1219 %>%
  group_by(Bact, Beads, Time) %>%
  summarize(count=n(),
            meanGFP=mean(Green),
            q10GFP=quantile(Green, probs=0.1),
            medianGFP=median(Green),
            q90GFP=quantile(Green, probs=0.9),
            sdGFP=sd(Green),
  )
summarizeGreenBeadFeed1219<-summarizeGreenBeadFeed1219 %>%
  mutate(cvGFP=sdGFP/meanGFP)

view(summarizeGreenBeadFeed1219)

knitr::kable(summarizeGreenBeadFeed1219)