############################
#Pooled t-test
############################

smean1<-10.02
ssd1<-4.00
n1<-10
smean2<-18.63
ssd2<-5.74
n2<-10
repn1 = 1/n1
repn2 = 1/n2
confidence_interval = 99
alpha = 1 - (confidence_interval/100)
sp = sqrt(((n1-1)*(s1^2)+(n2-1)*(s2^2))/(n1+n2-2))
t = (mean_x1-mean_x2) / (sp*sqrt(repn1+repn2))
p_left = pt(t,df = n1+n2-2)
p_right = 1 - pt(t, df = n1+n2-2)		#Right tail
p_twotsmall = 2*pt(t, df = n1+n2-2)		#Two tail if t<0
p_twotlarge = 2*(1-pt(t, df = n1+n2-2))	#Two tail if t>0
sp
t
p_left
p_right
p_twotsmall
p_twotlarge

###################
#Pooled t interval#
###################

smean1<-11.22
ssd1<-4.22
n1<-10
smean2<-18.22
ssd2<-5.51
n2<-10
confidence_i<-98
alpha<-1-(confidence_i/100)
t = qt(alpha/2,df=n1+n2-2)
mean_difference = smean1-smean2
spool<-sqrt(((n1-1)*ssd1^2+(n2-1)*ssd2^2)/(n1+n2-2))
error<-qt(alpha/2,df=n1+n2-2)*spool*sqrt((1/n1)+(1/n2))
right <- (smean1-smean2)-error
left <- (smean1-smean2)+error
left
right

##############################
#Pooled t hypothesis testing##
##############################

smean1<-10.02
ssd1<-4.00
n1<-10
smean2<-18.63
ssd2<-5.74
n2<-10
spool<-sqrt(((n1-1)*ssd1^2+(n2-1)*ssd2^2)/(n1+n2-2))
tscore<-(smean1-smean2)/(spool*sqrt((1/n1)+(1/n2)))
pval_left<-pt(tscore,df=n1+n2-2)
pval_right<-1-pt(tscore,df=n1+n2-2)
pval_two_smallerthan_zero<-2*pt(tscore,df=n1+n2-2)
pval_two_largerthan_zero<-2*(1-pt(tscore,df=n1+n2-2))
sp
tscore
pval_left
pval_right
pval_two_largerthan_zero
pval_two_smallerthan_zero

################################
#Nonpooled t- test and interval#
################################

smean1<-29.2
ssd1<-5
n1<-33
smean2<-22.5
ssd2<-8
n2<-21
confidence_i<-90
alpha<-1-(confidence_i/100)
mean_difference = smean1-smean2
tscore<-(smean1-smean2)/sqrt((ssd1^2/n1)+(ssd2^2/n2))
unpooldf=(((ssd1^2/n1)+(ssd2^2/n2))^2)/(((ssd1^2/n1)^2/(n1-1))+((ssd2^2/n2)^2/(n2-1)))
t_alpha_half<-pt(alpha/2,df = unpooldf)
error<-t_alpha_half*sqrt(((ssd1^2)/n1) + ((ssd2^2)/n2))
bound_lower<-mean_difference-error
bound_upper<-mean_difference+error
pval_left<-pt(tscore,df=unpooldf)
pval_right<-1-pt(tscore,df=unpooldf)
pval_two_smallerthan_zero<-2*pt(tscore,df=unpooldf)
pval_two_largerthan_zero<-2*(1-pt(tscore,df=unpooldf))


