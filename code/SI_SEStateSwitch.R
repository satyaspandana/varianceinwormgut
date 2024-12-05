# Code for SI figure

WormShedCFU<-read.csv("WormShedCFU.csv", header=TRUE)

# Plot - drift in CFU over time in n2 worms colonized with Salmonella enterica
pSEStateSwitch<-WormShedCFU %>%
  dplyr::filter(Bact=="SE" & Run==4) %>%
  ggplot(aes(y=logCFU, x=factor(Time), color=factor(Bin)))+
  geom_jitter(width=0.05) +
  geom_violin(fill=NA)+
  theme_classic() +
  scale_color_viridis_d(option="magma", direction=-1, begin=0.2, end=0.8)+
  theme(text=element_text(size=xTextSize),
        plot.title = element_text(hjust=0.5),
        legend.position = "none")+
  facet_wrap(~Bin)+
  labs(y=expression(log[10](CFU/Worm)), x="Time (h)", color = "GFP"
       #title="State switching, S. enterica-GFP"
  )
pSEStateSwitch
ggsave("FigBonus_SEStateSwitch.png", width=6, height=4, units="in", dpi=300)
