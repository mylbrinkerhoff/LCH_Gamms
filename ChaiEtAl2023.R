
# From Chai, et al 2023 
m.soe <- gam(soe ~ phonation + s(time, k = 9, by = phonation), data = df.9p)


preds <- get_gam_predictions(m.soe, time)


preds$phonation = factor(preds$phonation, levels = c("M","R","C"))


figure.soe = preds %>%
  ggplot(aes(x=time, y=soe,color = phonation, fill = phonation,group = phonation,linetype=phonation))+
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.1,color = NA) +
  geom_line(size = 1)+
  scale_colour_manual(values=c("#525252","#969696","#cccccc"))+
  scale_fill_manual(values=c("#525252","#969696","#cccccc"))+
  scale_linetype_manual(values=c(1,2,4)) +  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("Time (normalized %)")+ 
  ylab("SoE")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0, 100,25)) +
  #scale_y_continuous(limits = c(90,150), breaks = seq(90, 150, 10))+
  theme(plot.title = element_text(size = 5, hjust = 0.5))+
  theme(axis.title.x = element_text(size=10))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.text.y = element_text(size =10))+
  theme(strip.text.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 10))+
  theme(plot.caption = element_text(hjust=0, size=rel(2.3)))+
  labs(color = NULL,
       linetype = NULL,
       tag = "(a)") +
  guides(color=guide_legend(override.aes=list(fill=NA)),
         fill = "none")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  theme(text=element_text(family="serif"),
        legend.position="top",
        plot.tag.position = 'bottom')
  


modal_reart = get_smooths_difference(m.soe, time, list(phonation = c("M","R"))) %>%
  mutate(contrast = "M-R")
modal_check = get_smooths_difference(m.soe, time, list(phonation = c("M","C"))) %>%
  mutate(contrast = "M-C")
check_reart = get_smooths_difference(m.soe, time, list(phonation = c("R","C"))) %>%
  mutate(contrast = "R-C")

diff = rbind(modal_reart,modal_check,check_reart)
diff$contrast = factor(diff$contrast, levels = c("M-R","M-C","R-C"))

diff$sig_diff = factor(diff$sig_diff, levels = c("TRUE","FALSE"))


diff.soe = diff %>%
  ggplot(aes(time, difference, group = group)) +
  geom_hline(aes(yintercept = 0), colour = "#8f5f3f") +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = sig_diff), alpha = 0.3) +
  geom_line(aes(colour = sig_diff), size = 1) +
  scale_colour_manual(values = c("#6f849c","#e35760")) +
  scale_fill_manual(values = c("#6f849c", "#e35760")) +
  labs(colour = "Significant", fill = "Significant") +
  scale_x_continuous(breaks = seq(0, 100,25)) +
  xlab("Time (normalized %)") +
  ylab("\U0394SoE") +
  facet_wrap(.~contrast) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 5, hjust = 0.5))+
  theme(axis.title.x = element_text(size=10))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.text.y = element_text(size =10))+
  theme(strip.text.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 10))+
  theme(plot.caption = element_text(hjust=0, size=rel(2.3)))+
  labs(color = "significant",
       fill = "significant",
       tag = "(b)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  theme(text=element_text(family="serif"),
        legend.position="top",
        plot.tag.position = "bottom")

soe.combine = ggarrange(figure.soe, diff.soe,
          ncol = 2, nrow = 1,
          widths = c(1,1.3))

ggsave(soe.combine, file = "D:/Research/Yatee fieldwork/ICPhS/figure/soe_combine.svg", width = 5, height = 2.4, units = "in",bg = "transparent")