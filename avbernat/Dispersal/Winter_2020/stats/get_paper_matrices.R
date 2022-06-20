library(gridExtra)
cm1.A = cm1 + ggtitle("A") + theme(axis.text=element_text(size=10),
                                   title=element_text(size=16))
cm2.B = cm2 + ggtitle("B") + theme(axis.text=element_text(size=10),
                                   title=element_text(size=16))
set = grid.arrange(cm1.A,cm2.B, nrow=1)

# ggsave("images/model-perf.pdf", set)