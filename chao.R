  # script: estimating chao species richness for each year
  #           plotting / linear regression
  #
  # input:  "final_long_list.csv"
  #           long format of all data combined (see master_df.R)
  #
  # output: "chao_*.csv"
  #           chao species estimator values
  #           *entries for 1973 - 2018 if exists
  #        "all_chao.csv"
  #           all chao estimate outputs for all years
  #
  # author: Casey Patmore
  #           casey.patmore@ucdconnect.ie
  #
  # date:   last modified 10/11/2018
  
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
  
  rm(list=ls())
  
  start <- read.csv("final_long_list.csv")
  
  start[start == 0] <- NA
  start <- na.omit(start)
  
  start$Record <- as.numeric(start$Record)
  
  start <- split(start, start$Year)
  
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
    value <- nrow(item)
    words <- paste(value, "in", this_year)
    print(words)
  }
  
  years <- c(1973, 1974, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
             1992, 1993, 1998, 2018)
  species <- c(53, 45, 39, 48, 46, 50, 59, 56, 58, 58, 61, 59, 56, 54, 91, 57)
  check <- data.frame(years, species)
  
  library(lme4)
  linear <- glm(species ~ years, data = check)
  summary(linear)
  par(mfrow=c(1,1))
  plot(species ~ years, data = check)
  abline(linear)
  
  plot(linear)
  
  #autocorrelation?
  
  durbinWatsonTest(linear)
  
  plot(residuals(linear),type="b")
  abline(h=0,lty=3)
  
  acf(residuals(linear))
  
  library(nlme)
  mdl.ac <- gls(species ~ years, data = check, 
                correlation = corAR1(form=~years),
                na.action=na.omit)
  summary(mdl.ac)
  
  plot(fitted(mdl.ac),residuals(mdl.ac))
  abline(h=0,lty=3)
  
  qqnorm(mdl.ac)
  
  acf(residuals(mdl.ac,type="p"))
  
  library(MuMIn)
  model.sel(linear,mdl.ac)
  
  ###
  
  for(item in start){
    sample <- length(unique(item$Week))
    this_year <- item$Year[1]
    print(this_year)
    item$Year <- NULL
    item <- rbind(data.frame(Name = "Sample", Week = "All", Record = sample), item)
    item$Week <- NULL
    item <- item %>% 
      group_by(Name) %>% 
      summarise(Record = sum(Record))
    chao <- ChaoSpecies(item$Record,"incidence_freq", k = 10, conf = 0.95)   
    chao <- chao$Species_table[3, ]
    chao$Year <- this_year
    file = paste("chao_", this_year, ".csv", sep="")
    write.csv(chao, file = file)
  }
  
  library(vegan)
  
  data(dune)
  data(dune.env)
  attach(dune.env)
  pool <- specpool(dune, Management)
  pool
  op <- par(mfrow=c(1,2))
  boxplot(specnumber(dune) ~ Management, col="hotpink", border="cyan3",
          notch=TRUE)
  boxplot(specnumber(dune)/specpool2vect(pool) ~ Management, col="hotpink",
          border="cyan3", notch=TRUE)
  par(op)
  data(BCI)
  ## Accumulation model
  pool <- poolaccum(BCI)
  summary(pool, display = "chao")
  plot(pool)
  ## Quantitative model
  estimateR(BCI[1:5,])
  
  rm(list=ls())
  
  #using 1973 as a jumping off point template
  all_chao <- read.csv("chao_1973.csv", strip.white = TRUE)
  
  #append all other years next
  chao_years <- c(1974,1982:1993,1998:2018)
  
  for (year in chao_years){
    filename = paste("chao_", year, ".csv", sep="")
    if (file.exists(filename)){
      chao <- read.csv(filename, strip.white = TRUE)
      all_chao <- rbind.fill(all_chao, chao)
      #print(filename)
    }
  }
  
  write.csv(all_chao, file = "all_chao.csv" ,row.names=FALSE)
  
  colnames(all_chao)[1]<-"Test"
  all_chao$Test <- trim(all_chao$Test)
  #all_chao$Test=="iChao2 (Chiu et al. 2014)"
  chao_test <- subset(all_chao, Test == "Chao2-bc")
  #or any other test
  library(lme4)
  linear <- glm(Estimate ~ Year, data = chao_test)
  summary(linear)
  par(mfrow=c(1,1))
  plot(Estimate ~ Year, data = chao_test)
  abline(linear)
  
  plot(linear)
  
  #autocorrelation?
  
  durbinWatsonTest(linear)
  
  #hooray, they're independent
  
  #try to do clustering estimates?
  
  #rm(list=ls())
  
  #start <- read.csv("final_long_clusters.csv")
  
  #start[start == 0] <- NA
  #start <- na.omit(start)
  
  #start <- split(start, start$Year)
  
  #sample_year <- data.frame()
  #for(item in start){
  #  sample <- length(unique(item$Week))
  #  this_year <- item$Year[1]
  #  sample_year <- rbind(sample_year, c(this_year, sample))
  #}
  
  #colnames(sample_year)[1] <-  "Year"
  #colnames(sample_year)[2] <-  "Record"
  
  #c_start <- read.csv("final_long_clusters.csv")
  
  #c_start[c_start == 0] <- NA
  #c_start <- na.omit(c_start)
  
  #c_start <- split(c_start, c_start$clusters)
  
  #for(item in c_start){
  #  this_cluster <- item$clusters[1]
  #  item$clusters <- NULL
  #  item$Week <- NULL
  #  item <- split(item, item$Year)
  #  for(item in item){
  #    tryCatch({
  #    this_year <- item$Year[1]
  #    this_sample <- filter(sample_year, Year %in% this_year)
  #    item$Week <- NULL
  #    item <- rbind(data.frame(Name = "Sample", Year = this_year, Record = this_sample$Record), item)
  #    item <- item %>% 
  #      group_by(Name) %>% 
  #      summarise(Record = sum(Record))
  #    chao <- ChaoSpecies(item$Record,"incidence_freq", k = 10, conf = 0.95)   
  #    chao <- chao$Species_table
  #    chao$Year <- this_year
  #    chao$cluster <- this_cluster
  #    # }, error=function(e){})
  #    file = paste("chao_cluster", this_cluster, "_", this_year, ".csv", sep="")
  #    write.csv(chao, file = file)
  #    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #  }
  #}
  
  #rm(list=ls())
  
  #doesn't work unfortunately
  
  #guess we're doing proportions
  
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
  clusters_per_year$proportions <- (clusters_per_year$cluster_Species / clusters_per_year$total_Species)
  
  ggplot(data = clusters_per_year, aes(x=Year, y=proportions, col=as.factor(cluster))) +
    geom_point() +
    geom_smooth(method='lm',se=FALSE) + theme_minimal() +
    coord_cartesian(ylim=c(0, 1))
  
  ggplot(clusters_per_year, aes(x=Year, y=proportions, fill=cluster)) +
    geom_bar(stat="identity")+theme_minimal()
  
  multiple <- lm(proportions ~ as.factor(cluster), data = clusters_per_year)
  summary(multiple)
  
  plot(multiple)
  
  
  ###########
  
  years <- unique(clusters_per_year$Year)
  years <- paste(years[1:length(years)])
    
  frame <- data.frame(matrix(nrow=length(years), ncol=4))
  rownames(frame) = years
  colnames(frame) = clusters
  
  for(row in 1:nrow(clusters_per_year)){
    print(row)
    year = clusters_per_year$Year[row]
    cluster = clusters_per_year$cluster[row]
    value = clusters_per_year$cluster_Species[row]
    frame[paste(year), cluster] = value
    print(frame[paste(year), cluster])
  }

shannon <- diversity(frame)
shannon <- as.data.frame(shannon)
shannon$year <- years

ggplot(shannon, aes(x=year, y=shannon)) + 
  geom_boxplot()

ggplot(shannon, aes(x=shannon)) + geom_histogram() +
  stat_function(fun = dnorm, args = list(mean = mean(shannon$shan), sd = sd(shannon$shan)))

linear <- glm(shan ~ year, data = shannon)
summary(linear)
par(mfrow=c(1,1))
plot(shan ~ year, data = shannon)
abline(linear)

shannon$shan <- as.numeric(shannon$shannon)
shannon$year <- as.numeric(shannon$year)


##################


par(mfrow=c(2,2))
plot(linear)

par(mfrow=c(1,1))
plot(residuals(linear))

plot(residuals(linear),type="b")
abline(h=0,lty=3)

acf(residuals(linear))

linear.ac <- gls(Estimate ~ Year, data = chao_test, 
              correlation = corAR1(form=~Year),
              na.action=na.omit)
summary(linear.ac)

coef(linear)

coef(linear.ac)

plot(fitted(linear.ac),residuals(linear.ac))
abline(h=0,lty=3)

qqnorm(linear.ac)

acf(residuals(linear.ac,type="p"))

library(MuMIn)
model.sel(linear,linear.ac)
