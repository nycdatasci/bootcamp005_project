#DATA Cleaning and Transformation
df<-read.table("MLB.txt", header = FALSE, sep = "\t", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
head(df)
#Drop the column we do not need
df<-subset(df, select = -V2)

head(df)
#Add header for the df
colnames(df) = c('Player', 'W','L','IP','ERA','R','ER','HR',
                 'WHIP','WPCT','GO_AO','OBP','SLG','OPS','K_9','BB_9','H_9','K_BB','P_IP')
df=df[df[, "IP"]>100, ]
df$WR <- (df$W-df$L)/(df$L+df$W)
#df$KBB<- df$SO/df$BB
df$ERA <- as.numeric(as.character(df$ERA))
df$R <- as.numeric(as.character(df$R/df$IP))
df$ER <- as.numeric(as.character(df$ER/df$IP))
df$HR<-as.numeric(as.character(df$HR/df$IP))
df$WHIP <- as.numeric(as.character(df$WHIP))
df$WPCT <- as.numeric(as.character(df$WPCT))
df$GO_AO <- as.numeric(as.character(df$GO_AO))
df$OBP<-as.numeric(as.character(df$OBP))
df$SLG<-as.numeric(as.character(df$SLG))
df$K_9<-as.numeric(as.character(df$K_9))
df$BB_9<-as.numeric(as.character(df$BB_9))
df$H_9<-as.numeric(as.character(df$H_9))
df$K_BB<-as.numeric(as.character(df$K_BB))
df$P_IP<-as.numeric(as.character(df$P_IP))



#df$AVG<-as.numeric(as.character(df$AVG))

tail(df)
df<-subset(df, select = c(-W))
df<-subset(df, select = c(-L))
df<-subset(df,select=c(-Player))
df<-subset(df,select=c(-WPCT))
df<-subset(df,select=c(-IP))
#df<-subset(df,select=c(-ER))
#df<-subset(df,select=c(-HR))

#df<-subset(df,select=c(-OBP))
#df<-subset(df,select=c(-OPS))
head(df)
#EDA




tail(df)




#df$BB<-df$BB/df$IP
#df$SO<-df$SO/df$IP
#df$HR<-df$HR/df$IP
#summary(df)
#write.table(df, file = "my.df.txt", sep = " ", col.names = colnames(df))
#df<-subset(df, select = c(-Player,-G,-GS,-H,-R,-ER,-W,-IP))

#require(car)
#scatterplotMatrix(~IP+ERA+WHIP+WR+GO_AO+SLG+OBP+OPS+K_9+BB_9+H_9+K_BB+P_IP,data=df,
#                  main="scatterplot matrix")

#plot(df)
#ERA,AVG,WHIP,SO seems to be correlated to WR
#Use intutive multilinear regression to train multiregression model
#Use Ridge approach to train the model
#Compare the prediction power with the same data set

#model.saturated = lm(WR ~ ERA + HR + KBB+AVG+WHIP, data = df)
#summary(model.saturated)

#model.reduced1 = lm(WR ~ ERA + KBB+AVG, data = df)
#summary(model.reduced1)


#model.reduced2 = lm(WR ~ HR + KBB+AVG, data = df)
#summary(model.reduced2)

#model.reduced3 = lm(WR ~ ERA  +AVG, data = df)
#summary(model.reduced3)

#model.reduced4 = lm(WR ~ ERA, data = df)
#summary(model.reduced4)
#plot(model.reduced4)


model.empty = lm(WR ~ 1, data=df) 
model.saturated = lm(WR ~ ., data = df) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.saturated))

library(MASS) #The Modern Applied Statistics library.

#Stepwise regression using AIC as the criteria (the penalty k = 2).

#forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
#summary(forwardAIC)
#backwardAIC = step(model.saturated, scope, direction = "backward", k = 2)
#summary(backwardAIC)
#bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
#summary(bothAIC.empty)
#bothAIC.saturated = step(model.saturated, scope, direction = "both", k = 2)
#summary(bothAIC.saturated)



#model.final = lm(WR ~ ERA+WHIP, data = df)
#summary(model.final)
#plot(model.final)

#forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
#summary(forwardAIC)
#backwardAIC = step(model.saturated, scope, direction = "backward", k = 2)
#summary(backwardAIC)
#bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
#summary(bothAIC.empty)
#bothAIC.saturated = step(model.saturated, scope, direction = "both", k = 2)
#summary(bothAIC.saturated)


model.empty = lm(WR ~ 1, data=df) 
model.saturated = lm(WR ~ ., data = df) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.saturated))
dim(df)
#We care about prediction
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
summary(forwardAIC)
#plot(forwardBIC)

backwardAIC = step(model.saturated, scope, direction = "backward", k = 2)
summary(backwardAIC)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
summary(bothAIC.empty)
bothAIC.saturated = step(model.saturated, scope, direction = "both", k = 2)
summary(bothAIC.saturated)

#summary(forwardAIC)
#plot(forwardAIC)
#influencePlot(forwardAIC)
#vif(forwardAIC)
#avPlots(forwardAIC)
#confint(forwardAIC)

#backwardBIC = step(model.saturated, scope, direction = "backward", k = 2)
#summary(backwardBIC)
#bothBIC.empty = step(model.empty, scope, direction = "both", k = 2)
#summary(bothBIC.empty)
#bothBIC.saturated = step(model.saturated, scope, direction = "both", k = 2)
#summary(bothBIC.saturated)


#model.empty = lm(WR ~ 1, data=df) 
#model.saturated = lm(WR ~ ., data = df) #The model with ALL variables.
#scope = list(lower = formula(model.empty), upper = formula(model.saturated))
#dim(df)
#We care about prediction
#forwardBIC = step(model.empty, scope, direction = "forward", k = log(1144))
#summary(forwardBIC)
#plot(forwardBIC)

#backwardBIC = step(model.saturated, scope, direction = "backward", k = log(1144))
#summary(backwardBIC)
#bothBIC.empty = step(model.empty, scope, direction = "both", k = log(1144))
#summary(bothBIC.empty)
#bothBIC.saturated = step(model.saturated, scope, direction = "both", k = log(1144))
#summary(bothBIC.saturated)

df_pre<-read.table("MLB2016_pred.txt", header = FALSE, sep = "\t", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

df_pre<-subset(df_pre, select = -V2)

head(df_pre)
#Add header for the df
colnames(df_pre) = c('Player', 'W','L','IP','ERA','R','ER','HR',
                 'WHIP','WPCT','GO_AO','OBP','SLG','OPS','K_9','BB_9','H_9','K_BB','P_IP')
df_pre=df_pre[df_pre[, "IP"]>30, ]
df_pre$WR <- 0.0

#df$KBB<- df$SO/df$BB
df_pre$ERA <- as.numeric(as.character(df_pre$ERA))
df_pre$R <- as.numeric(as.character(df_pre$R/df_pre$IP))
df_pre$ER <- as.numeric(as.character(df_pre$ER/df_pre$IP))
df_pre$HR<-as.numeric(as.character(df_pre$HR/df_pre$IP))
df_pre$WHIP <- as.numeric(as.character(df_pre$WHIP))
df_pre$WPCT <- as.numeric(as.character(df_pre$WPCT))
df_pre$GO_AO <- as.numeric(as.character(df_pre$GO_AO))
df_pre$OBP<-as.numeric(as.character(df_pre$OBP))
df_pre$SLG<-as.numeric(as.character(df_pre$SLG))
df_pre$K_9<-as.numeric(as.character(df_pre$K_9))
df_pre$BB_9<-as.numeric(as.character(df_pre$BB_9))
df_pre$H_9<-as.numeric(as.character(df_pre$H_9))
df_pre$K_BB<-as.numeric(as.character(df_pre$K_BB))
df_pre$P_IP<-as.numeric(as.character(df_pre$P_IP))

#Vvalidation with older data out of our training set to see if the prediction is good

#Predict resluts
result<-predict(forwardAIC, df_pre, interval = "confidence")
df_re<-data.frame(result)
df_re$fit=0.5*(df_re$fit+1)
df_re$fit
df_pre$WR=df_re$fit













