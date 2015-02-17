## load hearings data 

hearingsDAT <- read.csv("IssueDefsDat.csv")
names(hearingsDAT)

## Descriptive stats 

install.packages("reporttools", dependencies=TRUE, repos='http://cran.us.r-project.org')
library(reporttools)
dims.dat <- data.frame(hearingsDAT$program, hearingsDAT$safe, hearingsDAT$yucca, hearingsDAT$problem, hearingsDAT$sci, hearingsDAT$storage, hearingsDAT$trans)
stats <- list("mean","s","min", "median", "max")
tableContinuous(vars = dims.dat, stats=stats, prec=3,
                font.size="normal", longtable=FALSE, cap="Descriptive Statistics of UNF Dimensions", lab="tab:desc") 

## Aggregate mean dimension salience by year

progTS <- aggregate(hearingsDAT$program, by=list(hearingsDAT$Year), FUN=mean, na.rm=TRUE)
progTS <- progTS[,2]

safeTS <- aggregate(hearingsDAT$safe, by=list(hearingsDAT$Year), FUN=mean, na.rm=TRUE)
safeTS <- safeTS[,2]

yuccaTS <- aggregate(hearingsDAT$yucca, by=list(hearingsDAT$Year), FUN=mean, na.rm=TRUE)
yuccaTS <- yuccaTS[,2]

problemTS <- aggregate(hearingsDAT$problem, by=list(hearingsDAT$Year), FUN=mean, na.rm=TRUE)
problemTS <- problemTS[,2]

sciTS <- aggregate(hearingsDAT$sci, by=list(hearingsDAT$Year), FUN=mean, na.rm=TRUE)
sciTS <- sciTS[,2]

storTS <- aggregate(hearingsDAT$storage, by=list(hearingsDAT$Year), FUN=mean, na.rm=TRUE)
storTS <- storTS[,2]

transTS <- aggregate(hearingsDAT$trans, by=list(hearingsDAT$Year), FUN=mean, na.rm=TRUE)
transTS <- transTS[,2]

YearTS <- unique(hearingsDAT$Year)

nwpa <- ifelse(YearTS >= 1983 & YearTS <= 1987,1,0)

nwpaa <- ifelse(YearTS >= 1988,1,0)

### OLS MODELS 

olsTS1 <- lm(progTS ~ nwpa+nwpaa)
summary(olsTS1)

olsTS2 <- lm(safeTS ~ nwpa+nwpaa)
summary(olsTS2)

olsTS3 <- lm(yuccaTS ~ nwpa+nwpaa)
summary(olsTS3)

olsTS4 <- lm(problemTS ~ nwpa+nwpaa)
summary(olsTS4)

olsTS5 <- lm(sciTS ~ nwpa+nwpaa)
summary(olsTS5)

olsTS6 <- lm(storTS ~ nwpa+nwpaa)
summary(olsTS6)

olsTS7 <- lm(transTS ~ nwpa+nwpaa)
summary(olsTS7)

library(apsrtable)
apsrtable(olsTS1,olsTS2,olsTS3,olsTS4,olsTS5,olsTS6,olsTS7, stars="default", model.names=c("Program","Safety","Yucca","Site","Sci/Tech","Storage","Trans"), coef.names=c("Intercept","NWPA", "NWPAA"), caption="Salience of the Dimensions following Policy Change", label="tab:dimsal")