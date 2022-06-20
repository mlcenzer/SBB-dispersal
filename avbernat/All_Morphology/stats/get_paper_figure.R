# figure code prep and output

p40 = p40 + theme(legend.position=c(0.85, 0.18)) +
  coord_cartesian(ylim=c(0.30,1.01)) +
  annotate(geom="text", x=5.5, y=0.718, label=pvalue45, parse=TRUE, size=font_size)
p50 = p50 + theme(legend.position=c(0.85, 0.18)) +
  coord_cartesian(ylim=c(0.30,1.01))  +
  annotate(geom="text", x=6.5, y=0.85, label=pvalue45, parse=TRUE, size=font_size)
p90 = p90 + ylab(" ") + ggtitle("C") + theme(legend.position=c(0.95, 0.21))+
  coord_cartesian(ylim=c(0.30,1.01)) +
  annotate(geom="text", x=xlab_yrs[4], y=0.59, label=pvalue90, parse=TRUE, size=font_size)


p100 = p100 + ggtitle("D") + theme(legend.position="none")  +
  coord_cartesian(ylim=c(0.709,0.744)) +
  annotate(geom="text", x=5, y=0.715, label=pvalue111, parse=TRUE, size=font_size)

p110 = p110 + ggtitle("E") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0.709,0.744)) +
  annotate(geom="text", x=5, y=0.715, label=pvalue111, parse=TRUE, size=font_size)

p120 = p120 + ggtitle("F") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0.709,0.744)) +
  annotate(geom="text", x=date120, y=0.716, label=pvalue120, parse=TRUE, size=font_size)


#```{r figure-paper, eval=FALSE, echo=FALSE, fig.height=9.5, fig.width=6.8, message=FALSE, warning=FALSE}

options(scipen=0)
p100t = p100 + theme(axis.text = element_text(size = 17))
p110t = p110 + theme(axis.text = element_text(size = 17))
p120t = p120 + theme(axis.text = element_text(size = 17))  +
  theme(axis.title.x = element_blank())
p40t = p40 + theme(axis.text = element_text(size = 17))
p50t = p50 + theme(axis.text = element_text(size = 17))
p90t = p90 + theme(axis.text = element_text(size = 17)) +
  theme(axis.title.x = element_blank())

paper_figure3 = ggdraw() +
  draw_plot(p100t, x=0.01, y=0.25+0.02, width=0.49, height=0.23) + # D
  draw_plot(p110t, x=0.51, y=0.25+0.02, width=0.49, height=0.23) + # E
  draw_plot(p120t, x=0.01, y=0+0.02, width=0.99, height=0.23) + # F
  draw_plot(p40t, x=0.01, y=0.75, width=0.49, height=0.25) + # A
  draw_plot(p50t, x=0.51, y=0.75, width=0.49, height=0.25) + # B
  draw_plot(p90t, x=0.01, y=0.50+0.02, width=0.99, height=0.23) + # C
  draw_plot_label("Month", x=0.48, y=0.75+0.02, fontface="plain", size = 18) +
  draw_plot_label("Month", x=0.49, y=0.25+0.03, fontface="plain", size = 18) +
  draw_plot_label("Year", x=0.49, y=0.50+0.03, fontface="plain", size = 18) +
  draw_plot_label("Year", x=0.49, y=0+0.03, fontface="plain", size = 19) +
  draw_plot_label("Long-Wing Morph Frequency", x=0, y=0.62, fontface="plain", angle=90, size = 18) +
  draw_plot_label("Wing-to-Body Ratio", x=0, y=0.15, fontface="plain", angle=90, size = 19)
paper_figure3

full_path = paste0( "~/Desktop/git_repositories/SBB-dispersal/avbernat/",
                    "Dispersal/Winter_2020/stats/images/paper_figure3S.pdf")
ggsave(full_path, paper_figure3)