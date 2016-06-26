#Step 1: Clean the data and evaluate missingness
winton = read.csv('/Users/zacharyescalante/Downloads/train.csv')
class(winton) #Dataframe
str(winton)   #all numeric data types
summary(winton) 
#Feature 1: Integer (1 - 10), NA = 33313~  83% of it's data/drop
#Feature 2: Continuous, NA = 9146   (min = 1.00, max = 10.00, mean = 3.59, median = 3.00) -> Impute 3.00 for missing
#Feature 3: Continuous, NA = 1237   (min = -4.6435, max = 4.5304, mean = 0.5584)
#Feature 4: Continuous, NA = 77721  (min = -5.441, max = 2.953, mean = 0.406)
#Feature 5: Integer, NA = 0
#Feature 6: Continuous, NA = 1933  (min = 0.9366, max = 12.6099, mean = 0.4310)
#Feature 7: Integer, NA = 0        (min = 338, max = 99861, mean = 49245)
#Feature 8: Continuous, NA = 469  (min = 0.0098, max = 0.3650, mean = 0.1970)
#Feature 9: Integer, NA = 1875 (min = 0.00, max = 36.00, mean = 10.68, median = 11.00) -> Impute 11.00 for missing
#Feature 10: Integer, NA = 19471 (min = 1.00, max = 6.00, mean = 4.745, median = 5.00) -> Impute 5.00 for missing values
#Feature 11: Continuous, NA = 987   (min = -7.3591, max = 1.7869, mean = -0.5722)
#Feature 12: Continuous, NA = 1096  (min = 0.00, max = 1.00, mean = 0.4985)
#Feature 13: Continuous, NA = 594   (min = 0.00, max = 9.00, mean = 4.238, median = 4.00) -> Impute 4.00 for missing values
#Feature 14: Continuous, NA = 728   (min = -0.1493, max = 3.1618, mean = 1.5885)
#Feature 15: Continuous, NA = 2141  (min = 0.0217, max = 28.0181, mean = 3.8914)
#Feature 16: Continuous, NA = 610   (min = 1.00, max = 2.00, mean = 1.007, median = 1.00) -> Impute 1.00 for missing values
#Feature 17: Continuous, NA = 646   (min = -2.614, max = 1.7869, mean = -0.5722)
#Feature 18: Continuous, NA = 568   (min = -5.758, max = 6.3524, mean = 0.8031)
#Feature 19: Continuous, NA = 1190  (min = -3.2929, max = 0.8982, mean = -1.2054)
#Feature 20: Intger, NA = 7826    (min = 2, max = 10, mean = 5.267, median = 5.00) -> Impute 5.00 for missing values
#Feature 21: Continuous, NA = 1018  (min = -1.515, max = 7.737, mean = 0.6056)
#Feature 22: Continuous, NA = 1345  (min = -5.8199, max = 2.2850, mean = -0.7731)
#Feature 23: Continuous, NA = 1711  (min = -7.2214, max = 3.2289, mean = 0.7998)
#Feature 24: Continuous, NA = 726   (min = -11.4422, max = 2.5267, mean = -1.2093)
#Feature 25: Continuous, NA = 655   (min = -1.903, max = 4.0203, mean = -0.3297)

#Drop 'Feature_1' due to over 80% missignness (was column 2)
data = winton[, -2]

#Impute the median to these variables
categorical <- c(5, 9, 10, 13, 16, 20)
#Convert potentially categorical variables to factors
for(i in categorical){
  data[,i] = as.factor(data[,i])
}


cont.median <- c(2, 12, 15)
#Impute the mean to these variables
cont.mean <- c(3, 4, 6, 7, 8, 11, 14, 17, 18, 19, 21, 22, 23, 24, 25)

#Impute values to every missing data point
for (i in 1:ncol(data)){
  if(i %in% categorical){
    data[,i][is.na(data[,i])] = names(which(max(table(data[,i])) == table(data[,i])))
  }
  else if(i %in% cont.median){
    data[,i][is.na(data[,i])] = median(data[,i], na.rm=TRUE)
  }
  else{
    data[,i][is.na(data[,i])] = mean(data[,i], na.rm=TRUE)
  }
}

data_ESN = as.matrix(data[,-c(1:27, 207, 208, 210)])
dailyWeights = data_ESN[,180]
data_ESN = data_ESN[, -180]
data_ESN = t(data_ESN)

####################################
##########ECHO STATE NETWORK########
####################################
# A minimalistic Echo State Networks demo with Mackey-Glass (delay 17) data 
# in "plain" R.
# by Mantas Lukosevicius 2012
# http://minds.jacobs-university.de/mantas

# load the data
trainLen = 120
testLen = 60
initLen = 20

# plot some of it
while( dev.cur() != 1 ) dev.off() # close all previous plots
dev.new()
plot(data_ESN[1, ],type='l')
title(main='A sample of data')



#Declare two empty vectors to calculate WMAE
predicted_vector = c(rep(0, 100))
zeros_vector = c(rep(0, 100))

file_name1 = paste0('~/Desktop/ESN_Parameters/resSize_pred1.csv')

#Variable iteration search
#resSize = seq(80, 130, 5) -> resSize = 125 gave us the lowest WMAE
resSize = 150
rho = 1.825
a = 0.5

#Declare an empty vector to store WMAE
store_WMAE = c(rep(0, length(resSize)))
store_Zeros = c(rep(0, length(resSize)))

# generate the ESN reservoir
inSize = outSize = 1
#resSize = 160

for (j in 1:length(resSize)){
  numrounds = 100
  for (i in 1:numrounds){
    i = 1
    set.seed(42)
    Win = matrix(runif(resSize*(1+inSize),-0.5,0.5),resSize)
    W = matrix(runif(resSize*resSize,-0.5,0.5),resSize)
    # Option 1 - direct scaling (quick&dirty, reservoir-specific):
    #W = W * 0.135 
    # Option 2 - normalizing and setting spectral radius (correct, slow):
    #cat('Computing spectral radius...')
    rhoW = abs(eigen(W,only.values=TRUE)$values[1])
    #print('done.')
    W = W * rho/ rhoW
    
    # allocated memory for the design (collected states) matrix
    X = matrix(0,1+inSize+resSize[j],trainLen-initLen)
    # set the corresponding target matrix directly
    Yt = matrix(data_ESN[(initLen+2):(trainLen+1)],1)
    
    # run the reservoir with the data and collect X
    x = rep(0,resSize[j])
    for (t in 1:trainLen){
      u = data_ESN[t, i]
      x = (1-a)*x + a*tanh( Win %*% rbind(1,u) + W %*% x )
      if (t > initLen)
        X[,t-initLen] = rbind(1,u,x)
    }
    
    # train the output
    reg = 1e-6  # regularization coefficient
    X_T = t(X)
    Wout = Yt %*% X_T %*% solve( X %*% X_T + reg*diag(1+inSize+resSize[j]) )
    
    # run the trained ESN in a generative mode. no need to initialize here, 
    # because x is initialized with training data and we continue from there.
    Y = matrix(0,outSize,testLen)
    u = data_ESN[trainLen+1, i]
    for (t in 1:1){
      x = (1-a)*x + a*tanh( Win %*% rbind(1,u) + W %*% x )
      y = Wout %*% rbind(1,u,x)
      Y[,t] = y
      # generative mode:
      u = y
      ## this would be a predictive mode:
      #u = data[trainLen+t+1] 
    }
    
    # compute WMAE for the first errorLen time steps with the predicted values
    errorLen = 60
    WMAE = ( sum( (abs(data_ESN[(trainLen):(trainLen+errorLen-1), i] - Y[1,1:errorLen]))*dailyWeights[i] )
             / errorLen )
    #print( paste( 'WMAE = ', WMAE ) )
    
    # compute WMAE for the first errorLen time steps with 0
    errorLen = 60
    WMAE_zero = ( sum( (abs(data_ESN[(trainLen):(trainLen+errorLen-1), i] - 0))*dailyWeights[i] )
                  / errorLen )
    #print( paste( 'WMAE_zero = ', WMAE_zero ) )
    
    predicted_vector[i] = WMAE
    zeros_vector[i] = WMAE_zero
    
  }

  
  Average_WMAE = sum(predicted_vector)/numrounds
  Zero_WMAE = sum(zeros_vector)/numrounds
  
  print( paste( 'Predicted_Average = ', Average_WMAE ) )
  print( paste( 'Zero Average = ', Zero_WMAE ) )
  
  store_WMAE[j] = Average_WMAE
  store_Zeros[j] = Zero_WMAE
  
  store.csv = data.frame(WMAE = store_WMAE, Zeros = store_Zeros, Parameters = resSize)

  
  write.csv(store.csv, file_name1, row.names = FALSE)
  
}



# plot some signals
plot.new()
plot( data_ESN[(trainLen+1):(trainLen+testLen-1), i], type='l', col='green' )
lines( c(Y), col='blue' )
title(main=expression(paste('Target and generated signals ', bold(y)(italic(n)), 
                            ' starting at ', italic(n)==0 )))
legend('bottomleft',legend=c('Target signal', 'Free-running predicted signal'), 
       col=c('green','blue'), lty=1, bty='n' )



# dev.new()
# matplot( t(X[(1:20),(1:200)]), type='l' )
# title(main=expression(paste('Some reservoir activations ', bold(x)(italic(n)))))
# 
# dev.new()
# barplot( Wout )
# title(main=expression(paste('Output weights ', bold(W)^{out})))