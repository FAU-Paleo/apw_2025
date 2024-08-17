#Lee Hsiang Liow Tutorial for Erlangen
#Tutorial 4 time series basics and very quick jump to RW, OU in paleoTS and evoTS and then layerranalyzer
#Hopefully this tutorial will show you some of the tools available and some ways to think about temporally organized
#data (very often the case in paleontology)


#https://nicolarighetti.github.io/Time-Series-Analysis-With-R/basic-concepts.html
#a nice set of tutorials for "standard" time series analyses, and I cite the author here:
#"A time series is a serially sequenced set of values representing a variable value at different points in time 
#(VanLear, “Time Series Analysis”). It consists in measures collected through time, at regular time intervals, 
#about an unit of observation, resulting in a set of ordered values. This regularity is the frequency of time series 
#(which can be, for instance, hourly, weekly, monthly, quarterly, yearly etc.)."

#This definition of time series (above) is a bit too restrictive for us. Many paleo and geo time series are not and cannot
#be collected at regular time intervals. In the next short section though, we whiz through some typical
#time series descriptors. Many paleo papers force their time series data to be temporally "regular" by binning data by time chunks.
#This decision (think about assumptions of models and assumptions of methods) sometimes can really affect inferences.

#https://nicolarighetti.github.io/Time-Series-Analysis-With-R/time-series-objects.html#time-series-objects-1
#data from https://drive.google.com/file/d/1vzpdPFb_ihBlqqbHxJS7mEvzQ3tNhmEm/view
#many of us are following election news, so let's play with some of that
#elections_news <- read.csv("/Users/leehl/Desktop/FAU 2024/elections-stories-over-time-20210111144254.csv")
elections_news <- read.csv("elections-stories-over-time-20210111144254.csv")
str(elections_news)
#change the "date" to date format (it's read as character strings or "chr")
elections_news$date <- as.Date(elections_news$date)

#always plot your raw data, don't start fitting models before you know what your data looks like and you have
#thought about your data. It's too easy not to dive deep into data that you can download from databases etc! 
#think about assumptions, how the data are actually collected, their limitations for perhaps the questions you
#are asking etc.

plot(elections_news$date,elections_news$ratio, type="l") # the ratio of news items that are about the elections
#it's about 6 years of daily counts and ratios of news ratios, the kind of length of time series data (i.e.
#number of time points) that we almost never have, but that sometimes paleoenvironmental proxies from the 
#past may have...

#let's see if there is temporal autocorrelation (it is "auto" beacuse it is how data in the time series
#is related to itself)
acf_res=acf(elections_news$ratio, ci=0.95) #the default lag  for this data is is two months 
#if you need the values to be output just use
acf_res
#or 
acf(elections_news$ratio, plot=F)
#Q what are the blue dashed lines)?
?acf # this shows the autocorrelation of the time series

# you can see that when there is no lag, the time series is compare "directly" with itself at the same time points, and there is 
#full correlation, that's expected. As the lag grows, the correlation drops, but this figure shows that there is temporal
#autocorrelation in the data (makes sense if you think about it right? The daily news you read tend to be related to what was on the day before
#Taylor Swift today, Taylor Swift tomorrow too, etc, but it might dwindle over time, just like temporally "bundled" election news )

pacf(elections_news$ratio) # this shows the partial autocorrelation when "measures the correlation between  
#x_t and  x_(t+k) after it has stripped out the effect of the intermediate (in between) x’s; blue line:
#if time series is white noise, should autocorrelation zero, with some random variation. 
#For a white noise series, we expect 95% of the spikes in the ACF to lie within ±2/√T where T is the length of the time series. 
#the ±2/√T are the blue dashed lines  If one or more large spikes are outside these bounds
#or if substantially more than 5% of spikes are outside these bounds, then the series is probably not white noise.

#read https://nicolarighetti.github.io/Time-Series-Analysis-With-R/correlations-and-arima.html#auto-correlation-acf-and-pacf

#A stationary time series is one whose statistical properties, 
#such as mean, variance, and autocorrelation, remain constant over time. 
#Stationary data is easier to model and analyze. 
#from #https://www.geeksforgeeks.org/stationarity-of-time-series-data-using-r/
#checking for stationarity, visual or with a test.

#install.packages("tseries") if you don't already have it
library(tseries)
#Augmented Dickey-Fuller (ADF) Test is a test for stationarity
adf.test(elections_news$ratio)
#p value is less than 0.05, we reject the null (not stationary) the alternative is stationary, so the time series 
#can be considered stationary.
#this also makes sense when you look at the pacf plot before (although it seems a bit border line)

#differencing (Differencing to remove a trend or seasonal effects)
#https://atsa-es.github.io/atsa-labs/sec-tslab-differencing-to-remove-a-trend-or-seasonal-effects.html
# you will likely hear a lot of about "first differencing" in paleo papers.
# first differencing can remove linear trends and might be a bit helpful in reducing the chance
#that two time series that are trending in time for no real reason will not be mistakenly 
#thought to be correlated. 

#white noise
#https://atsa-es.github.io/atsa-labs/sec-ts-definition.html
GWN <- rnorm(n = 100, mean = 5, sd = 0.2)
plot.ts(GWN)
acf(GWN, main = "", lag.max = 20)
acf(GWN, main = "", lag.max = 99) #are any of the correlations sticking out of the blue dotted lines?
#are you "worried" that the data generated by R is not "white noise"?
adf.test(GWN) #the p value is less than 0.05, so the null is rejected, hence the time series is (as simulated)
#stationary
#now that we know what white noise is, let's think about a type of timeseries that is quite common 
# in the paleo literature, the random walk? 

# Is the random walk (RW) white noise? No, it isn't.
# a random walk is defined as X(at time t)= X (at time t-1) + white noise (at t)

#let's simulate a RW
set.seed(123)
## length of time series
TT <- 100
## initialize {x_t} and {w_t}
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1) #so now both xx and ww contain the same 
#100 values that are generated from a random normal distribution, which is white noise (wn)
# we keep xx[1] as it is, but then from xx[2] onwards, we add wn
## compute values 2 thru TT
for (t in 2:TT) {
  xx[t] <- xx[t - 1] + ww[t]
}

par(mfrow = c(1, 2))
## plot line
plot.ts(xx, ylab = expression(italic(x[t])))
## plot ACF
acf(xx)
adf.test(xx) # null is not rejected so this is a non-stationary time series

#there is a lot more to time series analyses, and there are many tutorials available, including the ones I have suggested here
#from which I used some material. 
# you may also read/hear about time-domain (what we have done so far) and frequency-domain (wavelet, spectral analyses, useful for time series with periodic 
#behavior. Some paleo literature talk about the cyclicity of extinctions and may thus use frequency-domain tools but 
#more often than not we do not have enough time points to study cyclcity. 

########################################
########################################
########################################

#But let's now turn to time series tools in R that are available for paleo just so you get introduced to their
#uses 

#PaleoTS is an R package written by Gene Hunt first to study Punc-Eq but then has evolved to doing more. It was one of the earlier 
#uses of model comparisons (e.g. via AIC) to select models. Classically, folks tried to distinguish between random walks, a specific case of rw with a directional trend
# & stasis (wn) mostly looking at univariate morphological data, like body size.
#evolTS, another R package, is a follow up to PaleoTS, expanding on it, written by Kjetil Voje.
#install these if you don't have them using
#install.packages("paleoTS")
#install.packages("evoTS")
#then load them

library(paleoTS)
library(evoTS)

#the data are from a diatom lineage that invaded Yellowstone lake after the recession of ice, see Theriot 1992 and
#Theriot et al. 2006. So we are following morphological changes here from a lake core. Three morphological traits
#were measured, diameter, the number of ribs and the number of spines

#dia<-read.table("/Users/leehl/Desktop/FAU 2024/Voje2020/Theriot et al 2006 dia.txt", header=T)
dia<-read.table("Theriot et al 2006 dia.txt", header=T)
#ribs<-read.table("/Users/leehl/Desktop/FAU 2024/Voje2020/Theriot et al 2006 ribs.txt", header=T)
ribs<-read.table("Theriot et al 2006 ribs.txt", header=T)
#spines<-read.table("/Users/leehl/Desktop/FAU 2024/Voje2020/Theriot et al 2006 spines.txt", header=T)
spines<-read.table("Theriot et al 2006 spines.txt", header=T)


#always plot the data!
par(mfrow=c(2,2))
plot(-dia$age, dia$mm, pch=20, main="diameter", ylab="years ago")
points(-dia$age, dia$mm, type="l")
plot(-ribs$age,ribs$mm, pch=20, main="ribs")
points(-ribs$age, ribs$mm, type="l")
plot(-spines$age,spines$mm, pch=20, main="spines")
points(-spines$age, spines$mm, type="l")
plot(-spines$age,spines$N, main="sample size")#some "difficult" time intervals didn't make it to N = 50

#need to make the standard format
y1<-as.paleoTS(dia$mm, dia$vv, dia$N, dia$age.in.MY, oldest=c("first"), reset.time=TRUE)
y2<-as.paleoTS(ribs$mm, ribs$vv, ribs$N, ribs$age.in.MY, oldest=c("first"), reset.time=TRUE)
y3<-as.paleoTS(spines$mm, spines$vv, spines$N, spines$age.in.MY, oldest=c("first"), reset.time=TRUE)

#Standardize time to unit length for paleoTS analyses
y1$tt<-y1$tt/max(y1$tt)
y2$tt<-y2$tt/max(y2$tt)
y3$tt<-y3$tt/max(y3$tt)

data<-as.data.frame(cbind(y1$mm, y2$mm,y3$mm))
colnames(data)=c("dia","ribs","spines")
cor(data, method = "spearman") # look at the correlations between the three traits, do the numbers "surpise"
#you? Why or why not?

# Estimate correlations across traits after first differencing
detrended_diameter<-diff(y1$mm, 1)
detrended_ribs<-diff(y2$mm, 1)
detrended_spines<-diff(y3$mm, 1)

detrended_data<-as.data.frame(cbind(detrended_diameter, detrended_ribs,detrended_spines))
colnames(detrended_data)=c("dia","ribs","spines")
cor(detrended_data, method = "spearman") # look at the correlations between the three traits after detrending

#ok let's fit some models focussing on diameter, but you can explore the other traits yourself,try to show that even
#though the three traits are correlated, you don't nesssarily find the same best model!

w.grw <- fitSimple(y1, model = "GRW", pool=FALSE) #fits generalized random walk (has directionality)
w.urw <- fitSimple(y1, model = "URW", pool=FALSE) #fits generalized random walk 
w.ou <- fitSimple(y1, model = "OU", pool=FALSE) # Orstein Uhlen Beck model (see class notes)
compareModels(w.grw, w.urw,w.ou) # best model for this trait (diameter) is the OU model.

#what is the OU model? nice explanation for evolution:
#https://revbayes.github.io/tutorials/cont_traits/simple_ou.html#:~:text=Carlo%20(MCMC).-,Ornstein%2DUhlenbeck%20Model,a%20drift%20parameter%2C%20%CF%832.
#"Under the simple Ornstein-Uhlenbeck (OU) model, a continuous character is assumed to evolve toward an 
#optimal value, θ
#The character evolves stochastically according to a drift parameter, σ2
#The character is pulled toward the optimum by the rate of adaptation, α
#larger values of alpha indicate that the character is pulled more strongly toward θ
#As the character moves away from θ the parameter α
#determines how strongly the character is pulled back. For this reason, α
#is sometimes referred to as a ‘‘rubber band’’ parameter. When the rate of adaptation parameter α=0
#the OU model collapses to the BM model."

#check the parameters estimated
w.ou$parameters #anc is the starting or ancestral value; vstep is the variance, theta is θ, alpha is α
#vstep is related to σ2
w.urw$parameters
w.grw$parameters
#how many parameters does each model have and do the "same" estimates look similar in value?

three.models=compareModels(w.grw, w.urw,w.ou) #lets re run it and then save the output
nine.models=fit.all.univariate(y1, pool=FALSE) #then let's run 9 models, some not implemented in paleoTS 

#compare the outputs of three.models and nine.models and discuss!

#it's important to look at parameter estimates
opt.joint.GRW(y1, pool = FALSE)$parameters
opt.joint.URW(y1, pool = FALSE)$parameters
opt.joint.OU(y1, pool = FALSE)$parameters
opt.joint.OUBM(y1, opt.anc = TRUE, pool = FALSE)$parameters
opt.joint.OUBM(y1, opt.anc = FALSE, pool = FALSE)$parameters

#play with the other two traits
u2<- fit.all.univariate(y2, pool=TRUE)
u3<- fit.all.univariate(y3, pool=TRUE)

#evoTS can fit multivariate models, but that's for another day
#model adequancy (adeM) also for another day

#layeranalyzer see https://github.com/trondreitan/layeranalyzer (read the README)
#install.packages("https://github.com/trondreitan/layeranalyzer/raw/master/layeranalyzer_0.2.0.tar.gz")

library(layeranalyzer)
#read bird data from MEE paper
malta=read.table("http://folk.uio.no/trondr/layered/malta.csv",sep=";",header=T)

p=layer.prior(mu=c(log(1),log(1000)),init=c(log(0.1),log(10000)),dt=c(0.5,20),sigma=c(0.01,2),obs=c(0.01,1),lin=c(-
                                                                                                                    0.1,0.1))


# Define time series (time points, values and in this case also
# sample standard deviations and sample size, used for calculating
# standard errors for each measurement). 
X=layer.data.series(time.points=malta$Time.Year,
                    value.points=malta$Mean..log.body.mass.,
                    std.dev=sqrt(malta$Variance.calculated.from.the.data), 
                    num.meas.per.value=malta$Sample.size,name="log.body.size")



# no trend, just a simple OU
X.mod2=layer.series.structure(X, numlayers=1,
                              lin.time=F, prior=p, init.time=1996) 
mod2=layer.analyzer(X.mod2, num.MCMC=1000, burnin=10000,spacing=10,num.temp=1,
                    do.model.likelihood = FALSE, 
                    smoothing.specs=list(do.smoothing=TRUE, smoothing.time.diff=1, 
                                         smoothing.start=1996, smoothing.end=2020.5,
                                         num.smooth.per.mcmc=10))

# Plot data points:
plot(X$time, X$value,type="b",ylim=c(2.2,2.7),xlim=c(1996,2020),
     xlab="year",ylab="log(size)")

# Plot measurement uncertainties:
for(i in 1:length(X$time))
  lines(c(X$time[i],X$time[i]),
        c(X$value[i]-1.96*X$std.dev[i]/sqrt(X$num.meas.per.value[i]),
          X$value[i]+1.96*X$std.dev[i]/sqrt(X$num.meas.per.value[i])))
#so far you have plotted the data, now let's plot the model prediction
lines(mod2$process.time.points,mod2$process.mean, col="red",lwd=3)
lines(mod2$process.time.points,mod2$process.lower95, col="green",lwd=3)
lines(mod2$process.time.points,mod2$process.upper95, col="green",lwd=3)



############################################
# Take a further look at OU+linear trend:
############################################

# Look at Bayesian analysis, in order to get process inference:
X.mod3=layer.series.structure(X, numlayers=1,
                              lin.time=T, prior=p, init.time=1996) 
mod3=layer.analyzer(X.mod3, num.MCMC=1000, burnin=10000,spacing=10,num.temp=1,
                    do.model.likelihood = FALSE, 
                    smoothing.specs=list(do.smoothing=TRUE, smoothing.time.diff=1, 
                                         smoothing.start=1996, smoothing.end=2020.5,
                                         num.smooth.per.mcmc=10))





# Make plot of measurements, process inference estimate and
# process inference uncertainty:


# Plot data points:
plot(X$time, X$value,type="b",ylim=c(2.2,2.7),xlim=c(1996,2020),
     xlab="year",ylab="log(size)")

# Plot measurement uncertainties:
for(i in 1:length(X$time))
  lines(c(X$time[i],X$time[i]),
        c(X$value[i]-1.96*X$std.dev[i]/sqrt(X$num.meas.per.value[i]),
          X$value[i]+1.96*X$std.dev[i]/sqrt(X$num.meas.per.value[i])))

# Plot process expectation value as a function of time:
# PS: The 'mu' parameter is in this case the intercept of the
# line at the mean measurement time.
# PSS: The expected value will lag one characteristic time behind
# the line that it is tracking, thus the subtraction of the characteristic 
# time here.
abline(c(mod3$mu_log.body.size$mean-mod3$lin_t_log.body.size$mean*((max(X$time)+min(X$time))/2 + mod3$dt_log.body.size_1$mean), mod3$lin_t_log.body.size$mean),lwd=3)

lines(mod3$process.time.points,mod3$process.mean, col="red",lwd=3)
lines(mod3$process.time.points,mod3$process.lower95, col="green",lwd=3)
lines(mod3$process.time.points,mod3$process.upper95, col="green",lwd=3)


###this one takes a while, you can changes some of the numbers (e.g. burnin and num.MCMC and nu.temp=4) if you want to make it go faster just to see the out put

all.single <- traverse.standalone.layered(X,
                                    max.layers=1, talkative=T,allow.one.feedback.loop=T, 
                                    just.stationary=F,no.rw=F, time.integrals.possible=F,                                    
                                    allow.deterministic.layers=T,do.maximum.likelihood=T, 
                                    maximum.likelihood.numstart=1000, num.MCMC=1000, spacing=10, 
                                    burnin=2000, num.temp = 4)

compare.layered(all.single)
summary(all.single[[1]])
summary(all.single[[2]])
summary(all.single[[3]])

#####
#Q can you now make one of the morphological data time series (say y1) into layer analyzer format (see X) and then try
#running a "traverse.standalone.layered" analysis?

###pairwise comparisons
#This is part of the code from our paper Lidgard S., Di Martino E., Zárgošek K. & Liow L. H. 2021 
#When fossil clades ‘compete’: local dominance, global diversification dynamics and causation. Proceedings of the Royal Society, B. 88: 20211632
#this will show you how to compare two time series to test for links (correlative or causal links) using layer
#analyzer.

load("/Users/leehl/Desktop/FAU 2024/layerdata.RData")
#or go to https://datadryad.org/stash/dataset/doi:10.5061/dryad.zpc866t6s
#Data Files and then find layerdata.Rdata
time$dur=time$Start-time$End

#library(layeranalyzer)

#we always have to think some about priors when we do Bayesian analyses, but let's say we have
#done that and use these priors for origination rates for Cheilostomes and Cyclosomes.
prior=layer.prior(mu=c(log(0.05),log(10)),init=c(log(0.01),log(100)),dt=c(1,100),
                  sigma=c(0.001,20),obs=c(0.005,10),lin=c(-2,2))

#Chei.o is the cheilostome origination time series where I have separately, running "standalone"
#analyses finding that a single layered OU is best model, so I specific that numlayers=1)
Chei.o=layer.series.structure(chei.ori, numlayers=1, prior=prior) 

#Cy.o is the cyclostome origination time series where I have separately, running "standalone"
#analyses finding that a single layered OU is best model, so I specific that numlayers=1)
Cy.o=layer.series.structure(Cy.or, numlayers=1, prior=prior) 

#I pitch them against each other
ptm <- proc.time() #it takes a little while to run, even when the number of MCMC is quite small, e.g. here:
#so we can time it a bit to check how long our coffee break needs to be, before "upping" the numbers (scroll down)
resp1=traverse.connections.layered(Chei.o,Cy.o,
                                   num.MCMC=1000, burnin=100,spacing=10,
                                   num.temp=1,T.ground=1.1)
proc.time() - ptm

compare.layered(resp1,first.is.nullhypothesis=TRUE)
#log(lik) Post. Prob.(%)
#Model   1 -90.14196       41.50536
#Model   2 -90.10329       10.78546
#Model   3 -89.46635       20.39200
#Model   4 -89.58588       18.09448
#Model   5 -90.25982        9.22270

#look at the model estimates
summary(resp1[[1]])
summary(resp1[[2]])
summary(resp1[[3]])
summary(resp1[[4]])
summary(resp1[[5]])


### below are the results we reported  with
#a very long run: traverse.connections.layered(Chei.o,Cy.o,
#num.MCMC=1000, burnin=10000,spacing=10,
#num.temp=10,T.ground=1.1)

#compare.layered(resp1) #Chei.o,Cy.o
#compare.layered(resp1,first.is.nullhypothesis=TRUE)
#log(lik) Post. Prob.(%)
#Model   1 -90.12515       41.78931
#Model   2 -90.12054       10.49560
#Model   3 -89.45622       20.39463
#Model   4 -89.58460       17.93759
#Model   5 -90.23261        9.38287
#user  system elapsed 
#715.474   1.062 726.960
# proc.time() - ptm (revised chei.p)
#user   system  elapsed 
#6619.008   26.645 6741.832 

