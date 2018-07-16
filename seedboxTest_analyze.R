folder = "~/Desktop/JOB_SEARCH/datasciencetest-master"

load( paste(folder,"/sTest.Rdata", sep = ""))
load(paste(folder,"/dTest.Rdata", sep = ""))
load(paste(folder,"/test.Rdata", sep = ""))
load(file = paste(folder,"/test_wide.Rdata", sep = ""))
load(file = paste(folder,"/test_long.Rdata", sep = ""))
load(file = paste(folder,"/test_wide0.Rdata", sep = ""))


#1. what is the distribution between test and control groups? 

t <- table(sTest)

n <-table(sTest[,2])
m <- apply(t,2,mean)
se <- m[1]*m[2]/sqrt(length(sTest$sample_id))

sample <- cbind(n,round(m,3),round(se,3))
colnames(sample) <- c("n", "prop", "sd")
print(sample)

#2. Is a user that must call-in to cancel more likely to generate at least 1 addition REBILL?

rebill1_c <- sum(test_wide0$transaction_type.1 =="REBILL" & test_wide0$test_group =="Ctrl")#/length(sTest[sTest$test_group=="Ctrl",1])
rebill1_t <-sum(test_wide0$transaction_type.1 =="REBILL" & test_wide0$test_group =="Test")#/length(sTest[sTest$test_group=="Test",1])
sc<- length(sTest[sTest$test_group=="Ctrl",1])
st<- length(sTest[sTest$test_group=="Test",1])

prop.test(matrix(c(rebill1_c,rebill1_t,sc-rebill1_c,st-rebill1_t ),2),alternative ="less")

#3. revenue per person

money<- data.frame(test_wide0$sample_id, test_wide0$test_group,apply(test_wide0[,paste("transaction_amount.",1:22,sep="")],1,sum))

colnames(money) <- c("sample_id", "group", "total_amount")

hist(money$total_amount[money$group=="Ctrl" & money$total_amount!= 0])
shapiro.test(money$total_amount[money$group=="Ctrl"& money$total_amount!= 0 ])

hist(money$total_amount[money$group=="Test" & money$total_amount!= 0])
shapiro.test(money$total_amount[money$group=="Test"& money$total_amount!= 0 ])


#well... the distribution of the guys that stayed 
#is not too skewed so the mean should  be normal enough 
#for a t.test
t.test(total_amount~group, data = money, alternative = "less")

t.test(total_amount~group, data = money$total_amount!= 0, alternative = "less")

#what's that non-parametric test for comparing 2 distributions called??? k something??? 


#4. higher changeback rate??


rcc <- sum(test$transaction_type =="CBACK" & test$test_group =="Ctrl")
rct <- sum(test$transaction_type =="CBACK" & test$test_group =="Test")
scc <- length(sTest[sTest$test_group=="Ctrl",1])
sct <- length(sTest[sTest$test_group=="Test",1])
prop.test(matrix(c(rcc,rct,scc-rcc,sct-rct),2), alternative ="less")

rc <- sum(test$transaction_type =="REFUND" & test$test_group =="Ctrl")#/length(sTest[sTest$test_group=="Ctrl",1])
rt <-sum(test$transaction_type =="REFUND" & test$test_group =="Test")#/length(sTest[sTest$test_group=="Test",1])
sc<- length(sTest[sTest$test_group=="Ctrl",1])
st<- length(sTest[sTest$test_group=="Test",1])

prop.test(matrix(c(rc,rt,sc-rc,st-rt),2),alternative ="less")



#just checking out other things
#survival analysis 

library("survival")


t12 <- subset(surv_test, time <= 12)
summary(t12)
layout(matrix(1:1, ncol = 2))

plot(survfit(Surv(time, death) ~ test_group, data = t12),
        main = "Continued Subsciptions", lty = c(2, 1),
        ylab = "Probability", xlab = "Survival Time in Months",
        ylim = c(0,0.15), conf.int = "both")


#better plot
library(scales)
s_model <- summary(survfit(Surv(time, death) ~ test_group, data = surv_test))
#polygon(s_model[1:20]$time)
with(s_model,plot(time,surv,type="n", xlim=c(0,12),ylim=c(0,0.12),
                  ylab = "Probability",
                  xlab = "Time (months)",
                  main = "Probability of continued subscription"))


tc <- c(c(1:20),c(2:21))
tc <- tc[order(tc)]
lowc <-c(1:20, 1:20) 
lowc <- lowc[order(lowc)]
with(s_model,polygon(c(tc,rev(tc)),c(lower[lowc], upper[rev(lowc)]),
                 col = alpha("blue",0.3), border = FALSE))
with(s_model,lines(c(1,time[1:19]),c(1,surv[1:19]),type="s", col = "blue", lwd = 2))


tt <- c(c(1:11),c(2:12))
tt <- tt[order(tt)]
lowt <-c(21:31, 21:31) 
lowt <- lowt[order(lowt)]
with(s_model,polygon(c(tt,rev(tt)),c(lower[lowt], upper[rev(lowt)]),
                     col = alpha("red",0.3), border = FALSE))
with(s_model,lines(c(1,time[21:31]),c(1,surv[21:31]),type="s", col = "red", lwd =2))
legend('topright', legend = c("Control", "Test"), 
       lty = c(1,1), lwd = c(2,2), col = c("blue", "red"))

#are the 2 curves different? 
survdiff(Surv(time, death) ~ test_group, data = surv_test)



#trying to find average time until death but it's a gamma distribution... 
ttime <- mean(surv_test$time[surv_test$test_group == "Test"]) 
ctime <- mean(surv_test$time[surv_test$test_group == "Ctrl"]) 
t.test(surv_test$time[surv_test$test_group == "Test"], 
       surv_test$time[surv_test$test_group == "Ctrl"])

midpointPFS <-survfit(Surv(time/2, death)~ test_group, data = surv_test, conf.type="log-log")

summary(midpointPFS)
plot(midpointPFS) 

#try to fit a gamma distribution to survival time
library(fitdistrplus)
fg_ctrl <- fitdist(surv_test$time[surv_test$test_group== "Ctrl"], "gamma")
fg_test <- fitdist(surv_test$time[surv_test$test_group== "Test"], "gamma")
summary(fg_ctrl)
summary(fg_test)


denscomp(list(fg_ctrl), legendtext = plot.legend)
denscomp(list( fg_test), legendtext = plot.legend)

#k i think i'm done
