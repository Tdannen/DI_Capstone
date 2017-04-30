#Q1:
#Coins with values 1 through N (inclusive) are placed into a bag. All the coins from the 
#bag are iteratively drawn (without replacement) at random. For the first coin, you are paid 
#the value of the coin. For subsequent coins, you are paid the absolute difference between 
#the drawn coin and the previously drawn coin. For example, if you drew 5,3,2,4,1, 
#your payments would be 5,2,1,2,3 for a total payment of 13.

#load library
library(nortest)

### Generation of sample

#Setting parameters
observations = 10000

#Define Sampling function
Payment_sample <- function(obs, draws){
  
  #Create empty array to store calculated payments of each randomly drawed array.
  observations = c()
  
  #Generate payment observation by repated sampling of drawn series and the calculated payment value
  for (i in 1:obs) {
    
    #sample of 10 draws without replacement
    base_array = sample(1:draws, draws, replace=FALSE)
    
    #Calculate Payment Value according to description ("For the first coin, you are paid the value 
    #of the coin. For subsequent coins you are paid the absolute difference between the drawn coin and 
    #the previously drawn coin)
    payment = base_array[1]
    for (i in 2:length(base_array)){
      payment = payment + (abs(base_array[i] - base_array[i-1]))
    }
    
    #Store payment observation in array to answer exercise questions
    observations = c(observations, payment)
    
  } 
  return(observations)
}


#Q1.1. What is the mean of your total payment for N=10?
obs_10 <- Payment_sample(observations,10)
mean(obs_10)

#Q1.2. What is the standard deviation of your total payment for N=10?
#obs_10
sd(obs_10)

#Q1.3. What is the mean of your total payment for N=20?
obs_20 <- Payment_sample(observations,20)
mean(obs_20)

#Q1.4. What is the standard deviation of your total payment for N=20?
#obs_20
sd(obs_20)

#print results
cat(paste("Mean of total payment for N=10 and ", observations, "observations -->", mean(obs_10)),
    paste("Standard deviation of total payment for N=10 and ", observations, "observations -->",round(sd(obs_10),digits = 10)),
    paste("Mean of total payment for N=20 and", observations, "observations -->", mean(obs_20)),
    paste("Standard deviation of total payment for N=20 and", observations, "observations -->", round(sd(obs_20),digits = 10)), sep = "\n")


#Q1.4. What is the probability that your total payment is greater than or equal to 45 for N=10?
#Visual check of distribution
hist(obs_10)
hist(obs_10, breaks="FD")
boxplot(obs_10, horizontal=T)
qqnorm(obs_10)
qqline(obs_10)

#Visual examinations indicates normal distribution
#Check assumption by conducting distribution test (Anderson Darling test)
ad.test(obs_10)
#Recheck if time is enough

#Assuming normal distribution - probability for payment >= 45 with N = 10
prob_10 = pnorm(45, mean(obs_10), sd(obs_10), lower.tail=FALSE)
1-pnorm(45, mean(obs_10), sd(obs_10))
print(paste("The probability of a payment >= 45 is: ", round((prob_10), digits = 10)))

#Q1.5. What is the probability that your total payment is greater than or equal to 160 for N=20?
#Visual check of distribution
hist(obs_20)
hist(obs_20, breaks="FD")
boxplot(obs_20, horizontal=T)
qqnorm(obs_20)
qqline(obs_20)

#Check normality assumption with Anderson-Darling test
ad.test(obs_20)

#Calculate probability of payment >= 160
prob_20 = pnorm(160, mean(obs_20), sd(obs_20), lower.tail=FALSE)
1-pnorm(160, mean(obs_20), sd(obs_20))
print(paste("The probability of a payment >= 160 is: ", round((prob_20), digits = 10)))


