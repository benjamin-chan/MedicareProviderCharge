Violin <- function(d, title) {
  drg <- d$DRGnum[d$OHSU]
  d.subset <- subset(d, DRGnum %in% drg, select=c(DRGnum, Average.Covered.Charges, OHSU))
  ggplot(d.subset, aes(x=as.character(DRGnum), y=Average.Covered.Charges)) +
#     geom_boxplot(alpha=1/2, outlier.size=0) +
    geom_violin(alpha=1/2, fill="grey") +
    geom_point(data=d.subset[d.subset$OHSU,]) + 
    geom_point(data=d.subset[d.subset$OHSU,]) + 
    scale_y_log10(breaks=c(1E4, 2E4, 4E4, 8E4, 16E4, 32E4), labels=c("$10", "$20", "$40", "$80", "$160", "$320")) +
    labs(title=paste(title, "Nationwide", sep="\n"), x="DRG", y="Average Covered Charges ($1,000s)") +
    theme_bw() +
    theme(panel.grid.major.x=NULL, panel.grid.major.y=element_line(color="grey"), legend.position="bottom")
}
Violin(dfMDC05S, "MDC 05 Diseases & disorders of the circulatory system")
