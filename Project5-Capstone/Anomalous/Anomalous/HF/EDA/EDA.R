library(ggplot2)

names(KDD.train)[42] <- "outcome"
KDD.train$outcome <- as.factor(KDD.train$outcome)
KDD.train$outcome.response <- as.factor(ifelse(KDD.train$outcome == 'normal',0,1))
names(KDD.train)[43] <- "score21"

#Plots for protocol_type
ptc_table <- table(KDD.train$protocol_type)
ptc_levels <- names(ptc_table)[rev(order(ptc_table))]
KDD.train$protocol_type2 <- factor(KDD.train$protocol_type, levels = ptc_levels)

ggplot(KDD.train, aes(protocol_type2)) + 
  geom_bar(position="dodge", aes(fill=outcome.response)) + 
  labs(title = 'Connections by protocol',
       x="Protocol type",
       y="Count of connections") +
  scale_fill_discrete(labels = c("Normal", "Malicious"),
                      guide = guide_legend(title = "Connection type")) +
  theme_bw()



#Plots for service
srv_table <- table(KDD.train$service)
srv_levels <- names(srv_table)[order(srv_table)]
KDD.train$service2 <- factor(KDD.train$service, levels = srv_levels)

ggplot(KDD.train, aes(service2)) + 
  geom_bar(aes(fill = outcome.response), position = "fill") +
  coord_flip() +
  labs(title = 'Connections by service',
       x="Service",
       y="Count of connections") +
  scale_fill_discrete(labels = c("Normal", "Malicious"),
                      guide = guide_legend(title = "Connection type")) +
  theme_bw() +
  theme(legend.position="bottom")


#Plots for flag
flg_table <- table(KDD.train$flag)
flg_levels <- names(flg_table)[rev(order(flg_table))]
KDD.train$flag2 <- factor(KDD.train$flag, levels = flg_levels)

ggplot(KDD.train, aes(flag2)) + 
  geom_bar(position="dodge", aes(fill=outcome.response)) + 
  labs(title = 'Connections by flag',
       x="Flag",
       y="Count of connections") +
  scale_fill_discrete(labels = c("Normal", "Malicious"),
                      guide = guide_legend(title = "Connection type")) +
  theme_bw()




#Other
ggplot(KDD.train, aes(wrong_fragment)) + 
  geom_density(aes(colour=outcome.response))

ggplot(KDD.train, aes(urgent)) + 
  geom_density(aes(colour=outcome.response))

ggplot(KDD.train, aes(hot)) + 
  geom_density(aes(colour=outcome.response))

ggplot(KDD.train, aes(hot)) + 
  geom_bar(aes(fill=outcome.response), position='stack')

ggplot(KDD.train, aes(num_failed_logins)) + 
  geom_density(aes(colour=outcome.response))

ggplot(KDD.train, aes(num_failed_logins)) + 
  geom_bar(aes(fill=outcome.response), position='stack')