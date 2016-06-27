###############################
#### Multi Factor Analysis ####
###############################

library(FactoMineR)
library(ggplot2)

#------------- Prep data for FactoMineR ----------------------------#
#Index of character columns
char.col = c(2,3,4,42)

#Index of binary columns
bin.col = data.frame(matrix(ncol = 4, nrow = dim(KDD.train)[2]))
colnames(bin.col) = c('col','unique','max','min')
for (i in 1:(dim(KDD.train)[2])) {
  bin.col[i,1]=i
  bin.col[i,2]=length(unique(KDD.train[,i]))
  bin.col[i,3]=max(KDD.train[,i])
  bin.col[i,4]=min(KDD.train[,i])
}
bin.col = bin.col[bin.col$unique==2,1]

#Define function to prep Train and Test datasets
prep.pca = function(df) {
  colnames(df)[42]='outcome'
  colnames(df)[43]='outcome.response'
  df[43] <- ifelse(df$outcome == 'normal',0,1)

  df$protocol_type = as.factor(df$protocol_type)
  df$service = as.factor(df$service)
  df$flag = as.factor(df$flag)
  df$outcome = as.factor(df$outcome)
  return(df)
}

#Run function on Train and Test datasets
KDD.train2 = prep.pca(KDD.train)
KDD.test2 = prep.pca(KDD.test)


#------------- FactoMineR PCA for Train ----------------------------#
#PCA
pca.train = PCA(KDD.train2, scale.unit = TRUE, ncp = 5, ind.sup = NULL, 
                quanti.sup = NULL, 
                quali.sup = c(char.col, bin.col, 43), #Binary are considered supplementary
                #quali.sup = c(char.col), #Binary are considered active
                graph = TRUE, axes = c(1,2))
summary(pca.train)
dimdesc(pca.train, axes=c(1,2))
pca.train$quali.sup
pca.train$quanti.sup
#write.csv(pca.train$var$cor, file='pca_train_cont_cor.csv')

#Plot scree
barplot(pca.train$eig[,1], 
        main = "Scree Plot - Train", xlab = 'Component number', ylab = 'Eigenvalues of principal components',
        names.arg = paste("Dim", 1:nrow(pca.train$eig), sep = ""))
abline(h=1)

#Plot variables along other dimensions
plot(pca.train, choix = "var", axes = c(3,4), lim.cos2.var = 0)

#TBD - Plot attacks projected on Dim 1 and Dim 2 -> NEED TO UNDERSTAND MATH
draw.train = as.data.frame(pca.train$quali.sup$coord[,1:2])
draw.train$attack = row.names(draw.train)
ggplot(draw.train,aes(Dim.1, Dim.2)) + 
  geom_text(label=draw.train$attack)


#------------- FactoMineR PCA for Test ----------------------------#
#PCA
pca.test = PCA(KDD.test2, scale.unit = TRUE, ncp = 5, ind.sup = NULL, 
                quanti.sup = c(43), 
                quali.sup = c(char.col, bin.col), #Binary are considered supplementary
                #quali.sup = c(char.col), #Binary are considered active
                graph = TRUE, axes = c(1,2))
# summary(pca.test)
# dimdesc(pca.test, axes=c(1,2))
# pca.test$quali.sup
# pca.test$quanti.sup
# write.csv(pca.test$var$cor, file='pca_test_cont_cor.csv')

#Plot scree
barplot(pca.test$eig[,1], 
        main = "Scree Plot - Test", xlab = 'Component number', ylab = 'Eigenvalues of principal components',
        names.arg = paste("Dim", 1:nrow(pca.test$eig), sep = ""))
abline(h=1)

#Plot variables along other dimensions
plot(pca.test, choix = "var", axes = c(3,4), lim.cos2.var = 0)

#TBD - Plot attacks projected on Dim 1 and Dim 2 -> NEED TO UNDERSTAND MATH
draw.test = as.data.frame(pca.test$quali.sup$coord[,1:2])
draw.test$attack = row.names(draw.test)
ggplot(draw.test,aes(Dim.1, Dim.2)) + 
  geom_text(label=draw.test$attack)



# #------------- MFA function in FactoMineR ----------------------------#
# res = MFA(df, group = c(4,5,34), 
#           type = c('n','c','s'),
#           name.group = c('char','bin','continuous'))
# # res.adfm = DMFA(df, num.fact = 1, scale.unit = FALSE)
# # res.famd = FAMD(df)