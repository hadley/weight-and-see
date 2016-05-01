library(ggplot2)
library(dplyr)
library(readr)
theme_set(theme_bw())

# Figure 1 -----------------------
# UCB berkeley admissions

berkeley <- as.data.frame(UCBAdmissions)
ggplot(berkeley, aes(Dept, weight = Freq)) +
  geom_bar() +
  xlab("Department")
ggsave("graphics/berkeley.pdf", width = 6, height = 4)


# Figure 2 -----------------------
# histograms of % black, weighted by nothing, county population, black population, and white population

midwest <- read_csv("graphics/midwest.csv")
base <- ggplot(midwest, aes(percblack)) + scale_x_continuous("Percent black")

label <- function(text, y) {
  list(
    scale_y_continuous(text),
    annotate("text", 40, y, label = text, hjust = 1)
  )
}

base +
  geom_histogram(binwidth = 2) +
  scale_y_continuous("No. of Counties") +
  annotate("text", 45, 300, label = "Unweighted", hjust = 1)
ggsave("graphics/perc-black-count.pdf", width = 6, height = 4)

base +
  geom_histogram(aes(weight = poptotal / 1e6), binwidth = 2) +
  scale_y_continuous("Population (millions)") +
  annotate("text", 45, 13, label = "Population", hjust = 1)
ggsave("graphics/perc-black-pop.pdf", width = 6, height = 4)

base +
  geom_histogram(aes(weight = popblack / 1e6), binwidth = 2) +
  scale_y_continuous("Black population (millions)") +
  annotate("text", 45, 1.8, label = "Black population", hjust = 1)
ggsave("graphics/perc-black-pop-black.pdf", width = 6, height = 4)

base +
  geom_histogram(aes(weight = popwhite / 1e6), binwidth = 2) +
  scale_y_continuous("White population (millions)") +
  annotate("text", 45, 13, label = "White population", hjust = 1)
ggsave("graphics/perc-black-pop-white.pdf", width = 6, height = 4)

# Figure 3 -------------------------
# scatterplot of % change vs % share, + size proportional to population

florida <- read_tsv("graphics/florida-election.txt")
base <- ggplot(florida, aes(b00pc, b_change)) +
  xlab("Percent of vote (2000)") +
  ylab("Change in percent of vote") +
  xlim(0.275, 0.8)
base + geom_point()
ggsave("graphics/florida.pdf", width = 4, height = 4)

base +
  geom_point(aes(size = population / 1e6)) +
  scale_size_area(
    "Population\n(millions)",
    max_size = 10,
    breaks = c(0.5, 1, 1.5, 2)
  )
ggsave("graphics/florida-weighted.pdf", width = 5, height = 4)

# Figure 4 & 5 -------------------------
# histogram of weeks + brushed

keywords <- read_tsv("graphics/keywords-day.txt", col_types = cols(
  umsatz = "d",
  cpo = "d",
  scr = "d",
  ka = "d"
))
keywords <- keywords %>% mutate(
  colour = factor(
    ifelse(keyword == "AnzeigenKeyword9", "red", "grey20"),
    levels = c("red", "grey20")
  ),
  company = factor(
    ifelse(keyword == "bigX", "red", "grey20"),
    levels = c("red", "grey20")
  )
)

ggplot(keywords, aes(day, weight = pis / 1e3)) +
  geom_histogram(binwidth = 7) +
  xlab(NULL) +
  ylab("Page impressions (thousands)")
ggsave("graphics/web-words.pdf", width = 6, height = 4)

ggplot(keywords, aes(day, weight = pis / 1e3, fill = colour)) +
  geom_histogram(binwidth = 7) +
  xlab(NULL) +
  ylab("Page impressions (thousands)") +
  scale_fill_identity()
ggsave("graphics/web-words-brushed.pdf", width = 6, height = 4)

ggplot(keywords, aes(day, weight = clicks)) +
  geom_histogram(binwidth = 7) +
  xlab(NULL) +
  ylab("Clicks")
ggsave("graphics/web-clicks.pdf", width = 6, height = 4)

ggplot(keywords, aes(day, weight = clicks, fill = company)) +
  geom_histogram(binwidth = 7, position = "fill") +
  xlab(NULL) +
  ylab("Proportion of clicks") +
  scale_fill_identity()
ggsave("graphics/web-clicks-stacked.pdf", width = 6, height = 4)

# Figure 6 -------------------------
# fluctuation diagram, + weighted by total assets


# Figure 7 -------------------------
# self-weighted histogram

models <- read_tsv("graphics/liver-models.txt")

ggplot(models, aes(Posteriors, weight = Posteriors, fill = ModelID == 6)) +
  geom_histogram(binwidth = 0.01) +
  scale_fill_manual(values = c("grey20", "red"), guide = "none") +
  xlab("Posterior probability") +
  ylab("Posterior probability")
ggsave("graphics/post-prob.pdf", width = 8, height = 3)

# Figure 8 -------------------------
# fluctuation diagram + histogram of bin sizes

# Figure 9 -------------------------
# self-weighted histogram for meta-analysis

meta <- read_tsv("graphics/meta-analysis.txt")

ggplot(meta, aes(w)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Weight") +
  ylab("Count")
ggsave("graphics/meta.pdf", width = 4, height = 3)

ggplot(meta, aes(w, weight = w)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Weight") +
  ylab("Weight")
ggsave("graphics/meta-weighted.pdf", width = 4, height = 3)


# Figure 9 -------------------------
# traffic flows



