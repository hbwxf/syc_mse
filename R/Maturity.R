# Figure out maturity parameters
# If range was given, mean of range used

library(tidyverse)
library(mgcv)

matdat <- read_csv("Data/Maturity.csv")
paramdat <- read_csv("Data/SYC_params.csv")

ggplot(matdat,mapping=aes(x=Year,y=MBL)) +
	geom_point() + 
	geom_smooth()

calc_age <- function(Lt,Linf,K,t0) {
	res <- (log(1-(Lt/Linf))/(-K)) + t0
	return(res)
}

vBgrowth <- function(t,Linf,K,t0) {
	res <- Linf * (1-exp(-K*(t-t0)))
	return(res)
}

matmodel <- function(datage,datprop,predpop) {
	res <- glm(datprop~datage,family=binomial)
	coeffs <- coef(res)
	predage <- (log(predpop/(1-predpop))-coeffs[[1]])/coeffs[[2]]
	return(predage)
}

combdat <- inner_join(paramdat,matdat,by=c("tYear"="Year")) %>%
				mutate(MBL=MBL/10,L50=L50/10) %>%
				transmute(Year=tYear,
						  Amin=calc_age(MBL,Linf,k,t0),
						  A50=calc_age(L50,Linf,k,t0),
						  Pa1,
						  Pa2,
						  Pa0=0)
by_age <- combdat %>%
			gather(Pa1,Pa2,Pa0,key="Age",value="Proportion") %>%
			transmute(Year,Age=as.numeric(str_sub(Age,-1)),
						Proportion=Proportion/100) %>%
			drop_na()


by_length <- combdat %>%
				gather(Amin,A50,key="Proportion",value="Age") %>%
				transmute(Year,Age,Proportion=
						case_when(
							Proportion=="Amin"~0.1,
							Proportion=="A50"~50,
							TRUE ~ as.numeric(Proportion)
							)) %>%
				mutate(Proportion=Proportion/100) %>%
				drop_na()

newdat <- bind_rows(by_length,by_age)

ggplot(newdat,mapping=aes(x=Age,y=Proportion,color=Year)) +
	geom_point() +
	geom_smooth()

params <- newdat %>%
			group_by(Year) %>%
			summarise(A50=matmodel(Age,Proportion,0.5),
					  A95=matmodel(Age,Proportion,0.95)) %>%
			drop_na() %>%
			gather(A50,A95,key="Proportion", value="Age") %>%
			mutate(Proportion=case_when(
						Proportion=="A50"~0.5,
						Proportion=="A95"~0.95,
						TRUE ~ NA_real_))

write_csv(params,"Data/NewMaturity.csv")

newyears <- list(Year=1959:2013)

A50.gam <- gam(Age~s(Year,k=3), data=params %>% filter(Proportion==0.5))
A50.preds <- predict(A50.gam,newdata=newyears,se=TRUE)

A95.gam <- gam(Age~s(Year,k=3), data=params %>% filter(Proportion==0.95))
A95.preds <- predict(A95.gam,newdata=newyears,se=TRUE)


plot(x=1959:2013, A50.preds$fit,type="l",lwd=2,
     xaxt='n',yaxt='n',xlab=NA,ylab=NA,ylim=range(c(A50.preds$fit+(2*A50.preds$se.fit),A50.preds$fit-(2*A50.preds$se.fit))))
lines(x=1959:2013, A50.preds$fit+(2*A50.preds$se.fit), lty=2)
lines(x=1959:2013, A50.preds$fit-(2*A50.preds$se.fit), lty=2)
points(y=filter(params,Proportion==0.5)$Age, x=filter(params,Proportion==0.5)$Year, cex=.6, pch=19)

plot(x=1959:2013, A95.preds$fit,type="l",lwd=2,
     xaxt='n',yaxt='n',xlab=NA,ylab=NA,ylim=range(c(A95.preds$fit+(2*A95.preds$se.fit),A95.preds$fit-(2*A95.preds$se.fit))))
lines(x=1959:2013, A95.preds$fit+(2*A95.preds$se.fit), lty=2)
lines(x=1959:2013, A95.preds$fit-(2*A95.preds$se.fit), lty=2)
points(y=filter(params,Proportion==0.95)$Age, x=filter(params,Proportion==0.95)$Year, cex=.6, pch=19)




ggplot(params,mapping=aes(x=Age,y=Proportion,color=Year)) +
	geom_point()


temp <- glm(Proportion~Age,data=by_age[by_age$Year==2013,2:3],family=binomial)
temp1 <- predict(temp,type="response",data.frame(Age=seq(0,3,by=0.1)))
plot(Proportion~Age,data=by_age[by_age$Year==2013,2:3])
lines(x=seq(0,3,by=0.1),y=temp1)



