#Hypothesis Testing
#If P <= alpha then reject Ho, else don't reject
#If P <= 0.05 then reject Ho; else don't reject
#Z test

x = c(0.38,0.57,0.28,0.71,0.89,1.16,0.84,0.78,0.93,0.91,0.79,0.76 )
mean = sum(x) / length(x)
mean
#If sample mean is known then

mean = 37
population_sd = 8
mu_0 = 30
significance_level = 1
alpha = (significance_level/100)
n = 16
z = (mean - mu_0) / (population_sd / sqrt(n))
p_left = pnorm(z)	#If left tailed
p_right = 1-pnorm(z)	#If right tailed
p_twotail = 2*pnorm(z)	#If two tailed
z
p_left
p_right
p_twotail
alpha

#T test

x = c(180,187,181,182,185,181 )
#"greater" if right tailed; "less" if left tailed; "two.sided" if two tailed
#t.test(x, alternative = "greater", mu =  180)

#mean = sum(x) / length(x)
#sample_sd = sd(x)
mean = 42.66
sample_sd = 11.951
mu_0 = 41
significance_level = 5
alpha = (significance_level/100)
n = 20
t = (mean - mu_0) / (sample_sd / sqrt(n))
p_left = pt(t,df = n-1)	#If left tailed
p_right = 1-pt(t,df = n-1)	#If right tailed
p_twotailsmall = 2*pt(t,df = n-1)	#If two tailed
p_twotaillarge = 2*(1-pt(t,df = n-1)) #If two tailed and t > 0
t
p_left
p_right
p_twotailsmall
p_twotaillarge
alpha
