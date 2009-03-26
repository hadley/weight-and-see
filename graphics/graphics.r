library(ggplot2)

# Figure 1 -----------------------
# UCB berkeley admissions

berkeley <- as.data.frame(UCBAdmissions)
qplot(Dept, data = berkeley, xlab = "Department", weight = Freq)
ggsave("berkeley.pdf", width = 6, height = 4)


# Figure 2 -----------------------
# histograms of % black, weighted by nothing, county population, black population, and white population

midwest <- read.csv("midwest.csv")
base <- ggplot(midwest, aes(percblack)) + scale_x_continuous("Percent black")

base + geom_histogram(binwidth = 1) +
  scale_y_continuous("Counties")
ggsave("perc-black-count.pdf", width = 6, height = 4)
base + geom_histogram(aes(weight = poptotal / 1e6), binwidth = 1) + 
  scale_y_continuous("Population (millions)")
ggsave("perc-black-pop.pdf", width = 6, height = 4)
base + geom_histogram(aes(weight = popblack / 1e6), binwidth = 1) + 
  scale_y_continuous("Black population (millions)")
ggsave("perc-black-pop-black.pdf", width = 6, height = 4)
base + geom_histogram(aes(weight = popwhite / 1e6), binwidth = 1) + 
  scale_y_continuous("White population (millions)")
ggsave("perc-black-pop-white.pdf", width = 6, height = 4)


qplot(percblack, data=midwest, geom="histogram", binwidth=1, weight=white.population / 1e6) + ylab("White population (millions)") + xlab

# Figure 3 -------------------------
# scatterplot of % change vs % share, + size proportional to population

florida <- read.table("florida-election.txt", header=T)
base <- ggplot(florida, aes(b00pc, b_change)) + 
  xlab("Percent of vote (2000)") + 
  ylab("Change in percent of vote")
base + geom_point()  
ggsave("florida.pdf", width = 4, height = 4)

base + geom_point(aes(size = population / 1e6)) +    
  scale_area("Population\n(millions)", 
    to = c(0.5,10), breaks = c(0.5, 1, 1.5, 2))
ggsave("florida-weighted.pdf", width = 5, height = 4)

# Figure 4 & 5 -------------------------
# histogram of weeks + brushed

keywords <- read.table("keywords-day.txt", header=TRUE, sep="\t")
keywords$day <- as.Date(strptime(keywords$day, "%Y-%m-%d"))
keywords$colour <- factor(ifelse(keywords$keyword == "AnzeigenKeyword9", "red", "grey60"), levels=c("red", "grey60"))
keywords$company <- factor(ifelse(keywords$keyword == "bigX", "red", "grey60"), levels=c("red", "grey60"))


keyplot <- ggplot(keywords, aes(day)) + xlab(NULL) + 
  geom_histogram(binwidth = 7)

keyplot + aes(weight = pis / 1e3) + 
  scale_y_continuous("Page impressions (thousands)")
ggsave("web-words.pdf", width = 6, height = 4)

last_plot() + aes(fill = colour) + scale_fill_identity()
ggsave("web-words-brushed.pdf", width = 6, height = 4)

keyplot + aes(weight = clicks, fill = company) + 
  scale_y_continuous("Clicks") + 
  scale_fill_identity()
ggsave("web-clicks.pdf", width = 6, height = 4)

stackplot <- plot_clone(last_plot())
stackplot$layers[[1]]$position <- position_fill()
stackplot + scale_y_continuous("Proportion of clicks")
ggsave("web-clicks-stacked.pdf", width = 6, height = 4)

# Figure 6 -------------------------
# fluctuation diagram, + weighted by total assets


# Figure 7 -------------------------
# self-weighted histogram

models <- read.table("liver-models.txt", header=T)
qplot(Posteriors, data=models, geom="histogram", binwidth=0.01, weight=Posteriors, xlab="Posterior probability", xlim=c(0, 0.2)) + scale_y_continuous("Posterior probability")
ggsave("post-prob.pdf", width = 8, height = 3)

# Figure 8 -------------------------
# fluctuation diagram + histogram of bin sizes

# Figure 9 -------------------------
# self-weighted histogram for meta-analysis

meta <- read.table("meta-analysis.txt", header=TRUE)
qplot(w, data=meta, geom="histogram", binwidth=0.1) + xlab("Weight") + 
  scale_y_continuous("Count")
ggsave("meta.pdf", width = 6, height = 4)
last_plot() + aes(weight = w) + 
    scale_y_continuous("Weight")
ggsave("meta-weighted.pdf", width = 6, height = 4)


# Figure 9 -------------------------
# traffic flows



