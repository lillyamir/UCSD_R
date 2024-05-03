getwd()
setwd("/Users/lillyamirjavadi/Desktop/POLI271")
getwd()

load("fraud.RData")

library(dplyr)

#Question 1:
russia2011$vote_share <- russia2011$votes / russia2011$turnout

top_fractions <- russia2011 %>%
  count(vote_share) %>%
  arrange(desc(n)) %>%
  head(10)


top_vals <- top_fractions$vote_share
print(top_vals)

#Round to the 4th decimal place
russia2011$vote_share <- round(russia2011$votes / russia2011$turnout, 4)



hist(russia2011$vote_share, 
     breaks = length(unique(russia2011$vote_share)), 
     main = "Histogram of Vote Shares in 2011", 
     xlab = "Vote Share", 
     ylab = "Frequency")


length(unique(russia2011$vote_share))

#For each fraction with a low numerator and denominator, such as 1/2 or 2/3,
#the frequency dramatically increases.


#Question 2:

sims <- 1
sim_vshare <- numeric()

for(i in 1:nrow(russia2011)) {
  precinct <- russia2011[i, ]
  for(j in 1:sims) {
   
    sim_votes <- rbinom(1, size = precinct$turnout, prob = precinct$vote_share)
    
    vote_shares <- sim_votes / precinct$turnout
  
    sim_vshare <- c(vote_shares, sim_vshare)}}


#New DF
simulated_data <- data.frame(vote_share = sim_vshare)
simulated_data


top_sim_fractions <- simulated_data %>%
  count(vote_share) %>%
  arrange(desc(n)) %>%
  head(10)
print(top_sim_fractions)

#histogram
hist(simulated_data$vote_share, 
     breaks = length(unique(simulated_data$vote_share)), 
     main = "Simulated Vote Shares in 2011 Election", 
     xlab = "Simulated Vote Share", 
     ylab = "Frequency")


#This histogram shows how there are also spikes in frequency for 
#fractions with low numerators and denominators.

#Question 3

fractions <- c(0.5, 0.3333, 0.6, 0.6667)


russia2011$vote_share_rounded <- round(russia2011$vote_share, 4)
simulated_data$vote_share_rounded <- round(simulated_data$vote_share, 4)

# Filter df
actual_fractions <- russia2011[russia2011$vote_share_rounded %in% fractions, ]
simulated_fractions <- simulated_data[simulated_data$vote_share_rounded %in% fractions, ]


print(actual_fractions)
print(simulated_fractions)

# Create histograms
par(mfrow = c(1, 2))

hist(actual_fractions$vote_share_rounded, breaks = length(fractions),
     main = "Actual Vote Shares", xlab = "Vote Share", ylab = "Frequency")

hist(simulated_fractions$vote_share_rounded, breaks = length(fractions),
     main = "Simulated Vote Shares", xlab = "Vote Share", ylab = "Frequency")



#The simulated vote shares mirrored the histogram of the actual vote shares.
#This shows that there likely was not fraudulent behavior in the 2011 election. 
