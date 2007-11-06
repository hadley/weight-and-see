library(ggplot2)

# Figure 1 -----------------------
# histograms of % black, weighted by nothing, county population, black population, and white population

midwest <- read.table("midwest.txt", header=TRUE, sep="\t")
names(midwest) <- c("id", "county", "state", "county", "population", 
"density", "white.population", "black.population", 
"amerindian.population", "asian.population", 
"other.race.population", "white.percent", "black.percent", "X..American.Ind.Esk.Aleut", 
"X..Asian.Pacific", "X..other.race", "Total.population.aged.25.", 
"X..25..with.HSD", "X..25..college", "X..25..higher.prof", "Total.pop.poverty.status", 
"X..poverty.status", "X..below.poverty.level", "X..aged.0.17.below.PL", 
"X..aged.18.59.below.PL", "X..aged.60..below.PL", "County.in.Metropolitan.Area", 
"Category", "X", "Y", "PovertyLs", "HighSchools", "total.population.1", 
"Cities", "Total.white.population.1")

ylab <- function(name) scale_y_continuous(name)
xlab <- scale_x_continuous("Percent black")

qplot(black.percent, data=midwest, geom="histogram", binwidth=1) + ylab("Count") + xlab
qplot(black.percent, data=midwest, geom="histogram", binwidth=1, weight=population / 1e6) + ylab("Population (millions)") + xlab
qplot(black.percent, data=midwest, geom="histogram", binwidth=1, weight=black.population / 1e6) + ylab("Black population (millions)") + xlab
qplot(black.percent, data=midwest, geom="histogram", binwidth=1, weight=white.population / 1e6) + ylab("White population (millions)") + xlab

# Figure 2 -------------------------
# scatterplot of % change vs % share, + size proportional to population

florida <- read.table("florida-election.txt", header=T)
qplot(b00pc, b_change, data=florida, xlab="Percent of vote (2000)", ylab="Change in percent of vote")
qplot(b00pc, b_change, data=florida, size=population / 1e6, xlab="Percent of vote (2000)", ylab="Change in percent of vote") + scale_size("Population\n(millions)", to=c(1,10))

# Figure 3 -------------------------
# histogram of weeks + brushed

keywords <- read.table("keywords-day.txt", header=TRUE, sep="\t")
keywords$day <- as.Date(strptime(keywords$day, "%Y-%m-%d"))

colour <- factor(ifelse(keywords$keyword == "AnzeigenKeyword9", "red", "grey60"), levels=c("red", "grey60"))
qplot(day, data=keywords, geom="histogram", binwidth=7, weight=pis, xlab="Date", fill=colour) + ylab("Page impressions") + scale_fill_identity()

# Figure 4 -------------------------
# spinogram of weeks + brushed

qplot(day, data=keywords, geom="histogram", binwidth=7, weight=umsatz, xlab="Date") + ylab("Clicks")

# Figure 5 -------------------------
# fluctuation diagram, + weighted by total assets


# Figure 6 -------------------------
# self-weighted histogram

models <- read.table("liver-models.txt", header=T)
qplot(Posteriors, data=models, geom="histogram", binwidth=0.01, weight=Posteriors, xlab="Posterior probability", xlim=c(0, 0.2)) + ylab("Posterior probability")

# Figure 7 -------------------------
# fluctuation diagram + histogram of bin sizes

# Figure 8 -------------------------
# self-weighted histogram for meta-analysis

meta <- read.table("meta-analysis.txt", header=TRUE)
qplot(w, data=meta, geom="histogram", binwidth=0.1)
qplot(w, data=meta, geom="histogram", binwidth=0.1, weight=w)


# Figure 9 -------------------------
# traffic flows



