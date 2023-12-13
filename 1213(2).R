library(survival)
summary(data.s)
#step 1 to est beta
model <- glm(case~x1+x2+x3+x4+x5, data = data.s, family = binomial)
summary(model)
names(model)
b_1<-model$coefficients

#step 2 
head(x_s)
x_s <- as.matrix(data.s[,7:11]) 
tmp_index1 <- which(data.s$case==0) 
b1
dim(b_1)
xb_b1_tmp1 <- x_s[tmp_index1,]%*%b_1[-1] 
assir_m <- pop_data[,3]/pop_data[,2] 

age <- (30:76)/10
mu_1 <- mu_2 <- NULL #mu.1: all control, mu.2: age-specific control
i=1
for(i in 1:length(age)){ #估每個年齡層的mu
  mu_tmp1 <- uniroot(f.mu2, assir.age=assir_m[i], xb.b1=xb_b1_tmp1, interval=c(-10,10))$root
  mu_1 <- c(mu_1, mu_tmp1)
  
  tmp_index2 <- intersect(which(data.s$case==0), which(data.s$age==age[i]))
  if(length(tmp_index2) >= 10){ #總共多少人(樣本數夠大則符合條件)
    xb_b1_tmp2 <- x_s[tmp_index2,]%*%b_1[-1] #算出xbeta(=beta2*x2+beta3*x3)
    mu_tmp2 <- uniroot(f.mu2, assir.age=assir_m[i], xb.b1=xb_b1_tmp2, interval=c(-10,10))$root
    mu_2 <- c(mu_2, mu_tmp2)
  }else{
    mu_2 <- c(mu_2,NA)
  }
}  
