## hypothesis testing
## two sample one sided ttest 
## comparison of means between two groups

boys = c(6,8,9,5,4,7,5,7,8,5,6,7,6,7,8)
length(boys)
girls = c(8,9,8,7,6,5,4,6,7,8,7,7,6,8)
length(girls)

## h0 : mean(Gilrs) <= mean(boys)
## ha : Mean(girls) > mean(boys)

mean(girls)
mean(boys)

#t.test( x , y , alternative = )

t.test(girls, boys, alternative = "greater")

### Two sample two sided test

# h0 = Mean(gilrs) = Mean(boys)
# ha = Mean(girls) != Mean(boys)

t.test(girls, boys, alternative = "two.sided")


### one sample t test 

weight = c( 9.99, 10.01, 10.06, 9.98, 9.97, 10.0, 10.04, 10.0, 10.09, 9.91, 9.96, 9.93, 9.98, 10.01, 10.05, 10.06, 10.0, 10.09,
            10.05, 10.15, 9.85, 9.88, 10.12, 10.2, 9.8)

length(weight)

# h0 : Mean weight =  10 g
# ha : mean weight != 10 g 

t.test( weight, mu = 10, alternative = "two.sided")

### one sample one sided 

# h0 = Mean weight <= 10 g 
# ha = mean weight > 10 g 


t.test(weight, mu=10, alternative = "greater")

