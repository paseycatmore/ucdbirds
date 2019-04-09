setwd("C:/Users/casey/Desktop/Birds")

library(SpadeR)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(gdata)
library(ggplot2)
library(vegan)
library(car)
library(coin)
library(knitr)
library(kableExtra)
library(sjPlot)
library(stargazer)
library(lme4)
library(MuMIn)
library(nlme)

#1 data table

rm(list=ls())

#using 1973 as a jumping off point template
all_ledgers <- read.csv("ledgers/ledger_named_1973.csv")

#append all other years next
ledger_years <- c(1974:2018)

for (year in ledger_years){
  filename = paste("ledgers/ledger_named_", year, ".csv", sep="")
  if (file.exists(filename)){
    ledger <- read.csv(filename)
    all_ledgers <- rbind.fill(all_ledgers, ledger)
    print(filename)
  }
}

ebird_2018 <- read.csv("GBIF_ebird_table_code_year.csv")
colnames(ebird_2018)[1] <- "Name"

all_years <- rbind.fill(all_ledgers, ebird_2018)

new <- gather(data = all_years, 
              key = Week, 
              value = Record, 
              -c(Name, Year))

write.csv(new, file = "final_long_list.csv" ,row.names=FALSE)

agg <- aggregate(new,
                 by = list(new$Year, new$Week),
                 FUN = mean)

agg <- agg[, c(2,4,6)]

years <- unique(agg$Year)
weeks <- c(paste("Wk_", 1:52, sep=""))

data <- data.frame(matrix(ncol = 52, nrow = 20))
colnames(data) <- weeks
rownames(data) <- years

for(year in years){
  for(week in weeks){
    sample =  agg[ which(agg$Group.2==week & agg$Year == year), ]
    if(is.na(sample$Record[1])){
      data[as.character(year), week] = 0
    }
    else{
      data[as.character(year), week] = 1
    }
  }
}
write.csv(data, file = "data_table.csv")

data <- data[9:36]

data$Sample_Weeks <- rowSums(data)
data_output <- select(data, Sample_Weeks)

write.csv(data_output, file = "data_table_output.csv")

rm(list=ls())

start <- read.csv("final_long_list.csv")

start[start]
start <- start[!duplicated(start), ]

start[start == 0] <- NA
  
start <- na.omit(start)

start[start]
start <- start[!duplicated(start), ]

start$Record <- as.numeric(start$Record)

start <- split(start, start$Year)

estimate_func <- function(sobs, m, q1, q2) {
  estimate <- sobs + ((m-1)/m)*(q1*(q1-1)/(2*(q2+1)))
  return(estimate)
}

var_func <- function(m, q1, q2){
  var <- ((m-1)/m)*(q1*(q1-1)/(2*(q2+1))) + 
    (((m-1)/m)^2)*((q1*(2*q1-1)^2)/(4*(q2+1)^2)) + 
    (((m-1)/m)^2)*(((q1^2)*q2*((q1-1)^2))/(4*(q2+1)^4))
  return(var)
}

confidence_func <- function(sobs, m, q1, q2){
  estimate <- estimate_func(sobs, m, q1, q2)
  var <- var_func(m, q1, q2)
  t <- estimate - sobs
  k <- exp((1.96*(log(1+((var*estimate)/(t^2)))))^(0.5))
  upper <- sobs + t*k
  lower <- sobs + t/k
  return(c(lower, upper))
}

for(item in start){
  sample <- length(unique(item$Week))
  this_year <- item$Year[1]
  #print(this_year)
  item$Year <- NULL
  item <- rbind(data.frame(Name = "Sample", Week = "All", Record = sample), item)
  item$Week <- NULL
  item <- item %>% 
    group_by(Name) %>% 
    summarise(Record = sum(Record))
  item$Year <- this_year
  sobs <- length(unique(item$Name) - 1)
  item$Observed <- sobs
  m <- item$Record[1]
  q1 <- length(which(item$Record ==1))
  q2 <- length(which(item$Record ==2))
  item$Estimate <- estimate_func(sobs, m, q1, q2)
  item$Var <- var_func(m, q1, q2)
  item$Lower<- confidence_func(sobs, m, q1, q2)[1]
  item$Upper<- confidence_func(sobs, m, q1, q2)[2]
  file = paste("output_", this_year, ".csv", sep="")
  item <- item[1,]
  write.csv(item, file = file)
}

#2 sp. richness

rm(list=ls())

#using 1973 as a jumping off point template
all_output <- read.csv("output_1973.csv", strip.white = TRUE)

#append all other years next
output_years <- c(1974:2018)

for (year in output_years){
  filename = paste("output_", year, ".csv", sep="")
  if (file.exists(filename)){
    output <- read.csv(filename, strip.white = TRUE)
    all_output <- rbind.fill(all_output, output)
    #print(filename)
  }
}

all_output$X <- "Historic"
all_output$X[18] <- "Modern"

write.csv(all_output, file = "all_output.csv" ,row.names=FALSE)

library(ggfortify)

linear <- lm(Estimate ~ Year, data = minus)
summary(linear)
confint(linear)
par(mfrow=c(1,1))
plot(Estimate ~ Year, data = all_output)
autoplot(linear)
abline(linear)
x <- residuals(linear)
auto <- acf(residuals(linear))
plot(auto, main = NA) + theme_minimal()
plot(linear)

bacf <- acf(x, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ci2 = qnorm((1 + .95)/2)/sqrt(length(x))

q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept = c(ci2, -ci2), color = "red", linetype = "dashed") +
  theme_minimal()
q + ylab("Autocorrelation Function (ACF)") + xlab("Lag") 

plot <- ggplot(all_output, aes(x = Year, y = Estimate)) + 
  geom_point(size = 2, (aes(colour = factor(X)))) +
  scale_color_manual(values = c("red",  "blue")) +
  geom_smooth(method='lm', se=FALSE, colour="blue") +
  geom_smooth(data=minus, method ='lm', se=FALSE, linetype="dashed", colour="red") +#geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
  labs(color='Data Type', ylab('Chao2-bc Estimate'), xlab('Year')) +
  theme_minimal()

plot + ylab('Species Richness Estimate')
plot(linear[1])

#using 1973 as a jumping off point template
minus <- read.csv("output_1973.csv", strip.white = TRUE)

#append all other years next
output_years <- c(1974:2017)

for (year in output_years){
  filename = paste("output_", year, ".csv", sep="")
  if (file.exists(filename)){
    output <- read.csv(filename, strip.white = TRUE)
    minus <- rbind.fill(minus, output)
    #print(filename)
  }
}

summary(minus)

plot <- ggplot(all_output, aes(x = Year, y = Estimate)) + 
  geom_point(size = 1) +
  geom_smooth(method='lm', se=FALSE, aes(colour="w/ 2018")) +
  geom_smooth(data=minus, method ='lm', se=FALSE) + 
  labs(color='Trend', ylab('Estimate'), xlab('Year')) +
  theme_minimal()

plot <- ggplot(all_output, aes(x = Year, y = Estimate)) + 
  geom_point(size = 2, (aes(colour = factor(X)))) +
  geom_smooth(method='lm', se=FALSE, colour="black") + #geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
  geom_smooth(method='lm') +
  labs(color='Data', ylab('Estimate'), xlab('Year')) +
  theme_minimal()

plot + ggtitle("Linear Regression; Total Species Richness")

library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)


linear2 <- lm(Estimate ~ Year, data = minus)
summary(linear2)
par(mfrow=c(1,1))
plot(Estimate ~ Year, data = minus)
abline(linear2)

par(mfrow=c(2,2))
plot(linear2)

par(mfrow=c(2,2))
plot(linear)

par(mfrow=c(1,1))
plot(residuals(linear))

plot(residuals(linear),type="b")
abline(h=0,lty=3)

acf(residuals(linear))

linear.ac <- gls(Estimate ~ Year, data = all_output, 
                 correlation = corAR1(form=~Year),
                 na.action=na.omit)
summary(linear.ac)

coef(linear)

coef(linear.ac)

plot(fitted(linear.ac),residuals(linear.ac))
abline(h=0,lty=3)

qqnorm(linear.ac)

acf(residuals(linear.ac,type="p"))

model.sel(linear,linear.ac)

#2 diagnostics

#2 autocorr

#3 functional tables

#4 sp. richness

data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)

rm(list=ls())

start <- read.csv("final_long_clusters.csv")

start[start == 0] <- NA
start <- na.omit(start)

start <- split(start, start$Year)

clusters <- c(1,2,3,4)

clusters_per_year <- data.frame()
for(item in start){
  species <- length(unique(item$Name))
  this_year <- item$Year[1]
  for(cluster in clusters){
    year_cluster <- filter(item, clusters %in% cluster)
    cluster_species <- length(unique(year_cluster$Name))
    clusters_per_year <- rbind(clusters_per_year, c(this_year, species, cluster_species))
  }
}

colnames(clusters_per_year)[1] <-  "Year"
colnames(clusters_per_year)[2] <-  "total_Species"
colnames(clusters_per_year)[3] <-  "cluster_Species"

clusters_per_year$cluster <- rep(cbind(1,2,3,4))

plot2 <- ggplot(data = clusters_per_year, aes(x=Year, y=cluster_Species, col=as.factor(cluster))) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE) + 
  labs(color='Functional Group', ylab('Estimate'), xlab('Year')) +
  scale_color_manual(labels = c("Green-Space Obligate", "Urban-Space Obligate", 
                                "Facultative", "None"), values = c("green", "brown", "red", "blue")) +
  theme_minimal()

plot2 + ylab('Species Richness Estimate')

ggplot(clusters_per_year, aes(x=Year, y=cluster_Species, fill=cluster)) +
  geom_bar(stat="identity")+theme_minimal()

multiple <- lm(cluster_Species ~ as.factor(cluster), data = clusters_per_year)
summary(multiple)

par(mfrow=c(2,2))
plot(multiple)

par(mfrow=c(1,2))
plot(linear)

par(mfrow=c(1,1))
plot(residuals(multiple))

plot(residuals(multiple),type="b")
abline(h=0,lty=3)

autom <- acf(residuals(linear))

plot(autom, main =" Regression; Autocorrelation")

par

linear.ac <- gls(Estimate ~ Year, data = all_output, 
                 correlation = corAR1(form=~Year),
                 na.action=na.omit)
summary(linear.ac)

coef(linear)

coef(linear.ac)

plot(fitted(linear.ac),residuals(linear.ac))
abline(h=0,lty=3)

qqnorm(linear.ac)

acf(residuals(linear.ac,type="p"))

model.sel(linear,linear.ac)


######

#RAREFACTION

data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6, xlab = Observation, ylab = Unique Species)

rm(list=ls())

start <- read.csv("final_long_clusters.csv")

start[start == 0] <- NA
start <- na.omit(start)

start <- split(start, start$Year)

clusters <- c(1,2,3,4)

clusters_per_year <- data.frame()
for(item in start){
  species <- length(unique(item$Name))
  this_year <- item$Year[1]
  for(cluster in clusters){
    year_cluster <- filter(item, clusters %in% cluster)
    cluster_species <- length(unique(year_cluster$Name))
    clusters_per_year <- rbind(clusters_per_year, c(this_year, species, cluster_species))
  }
}

colnames(clusters_per_year)[1] <-  "Year"
colnames(clusters_per_year)[2] <-  "total_Species"
colnames(clusters_per_year)[3] <-  "cluster_Species"

clusters_per_year$cluster <- rep(cbind(1,2,3,4))

rownames(clusters_per_year) <- paste(clusters_per_year$Year, "_", clusters_per_year$cluster, sep="")

m <- 72 #length of 
n <- length(paste(unique(start$Name)))
rarefaction <- matrix(0, nrow = m, ncol = n)
colnames(rarefaction) <- paste(unique(start$Name))         
rownames(rarefaction) <- paste(clusters_per_year$Year, "_", clusters_per_year$cluster, sep="")

rownumbers <- c(1:length(start[, 1]))
for(rownumber in rownumbers){
  this_row <- start[rownumber, ]
  this_name <- paste( unlist(this_row[1]), collapse='')
  year_cluster <- paste(this_row$Year, "_", this_row$cluster, sep="")
  this_record <- as.numeric(paste( unlist(this_row[4]), collapse=''))
  rarefaction[year_cluster, this_name] = rarefaction[year_cluster, this_name] + this_record
}

S <- specnumber(rarefaction) # observed number of species
(raremax <- min(rowSums(rarefaction)))
Srare <- rarefy(rarefaction, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(rarefaction, step = 20, sample = raremax, col = c("green", "chocolate", "red", "blue"), cex = NA, xlab = "Observation", ylab = "Unique Species")

oof <- as.data.frame(Srare)
oof$cluster <- c(1:4) 

years <- unique(start$Year)

oof$years <- rep(years, each=4)

plot2 <- ggplot(data = oof, aes(x=years, y=Srare, col=as.factor(cluster))) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE) + 
  labs(color='Functional Group', ylab('Estimate'), xlab('Year')) +
  scale_color_manual(labels = c("Green-Space Obligate", "Urban-Space Obligate", 
                                "Facultative", "None"), values = c("green", "chocolate", "red", "blue")) +
  theme_minimal()

plot2 + ylab('Species Richness Estimate') + xlab('Year')# + ggtitle("Linear Regression: Per Functional Group") + ylim(0,8)

multiple <- lm(Srare ~ years, data = cluster3)
summary(multiple)
confint(multiple)
par(mfrow=c(2,2))
plot(multiple)

cluster1 <- oof[ which(oof$cluster==1), ]
cluster2 <- oof[ which(oof$cluster==2), ]
cluster3 <- oof[ which(oof$cluster==3), ]
cluster4 <- oof[ which(oof$cluster==4), ]


library(lme4)
fits <- lmList(Srare ~ years | factor(cluster), data=oof, pool=FALSE)
summary(fits)
confint(fits)
plot(fits)

check <- lm(Srare ~ years*cluster, data=oof)
summary(check)
