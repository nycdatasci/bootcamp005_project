#modifying test dataset
dfTest <- read.csv('./test.csv', header=T)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)
test <- dfTest[,-1]
test[test==-999.0] <- NA


rotate_phi <- function(phi,ref_phi) {
  rot_phi = phi                                #create result object
  for (i in 1:length(phi)) {                   #loop through each row in phi
    if (is.na(ref_phi[i]) | is.na(phi[i]) )    #return original phi when angle is not meaningful
      rot_phi[i]=phi[i]
    else
      rot_phi[i]=phi[i]-ref_phi[i]             #calculate rotated phi
  }
  a = as.integer(rot_phi/pi)                   #find rot_phi outside of range +pi and -pi
  rot_phi = rot_phi - a*pi                     #adjust rotated phi to have values between +pi and -pi
  return(rot_phi)
} 

PRI_lep_phi_rot <- rotate_phi(test$PRI_lep_phi, test$PRI_tau_phi)
PRI_met_phi_rot <- rotate_phi(test$PRI_met_phi, test$PRI_tau_phi)
PRI_jet_leading_phi_rot <- rotate_phi(test$PRI_jet_leading_phi, test$PRI_tau_phi)
PRI_jet_subleadint_phi_rot <- rotate_phi(test$PRI_jet_subleading_phi, test$PRI_tau_phi)

test2 <- data.frame(test, PRI_lep_phi_rot, PRI_met_phi_rot, PRI_jet_leading_phi_rot, PRI_jet_subleadint_phi_rot)
test2 <- subset(test2, select = -c(PRI_tau_phi,PRI_lep_phi,PRI_met_phi,PRI_jet_leading_phi,PRI_jet_subleading_phi))

Mass_miss <- is.na(test2$DER_mass_MMC)
test2 <- data.frame(test2, Mass_miss)
test2$DER_mass_MMC[is.na(test2$DER_mass_MMC)] = mean(test2$DER_mass_MMC, na.rm=TRUE)

test2$Miss1 <- ifelse(test2$PRI_jet_num == 0 & test2$Mass_miss == 0, 1, 0)
test2$Miss2 <- ifelse(test2$PRI_jet_num == 0 & test2$Mass_miss == 1, 1, 0)
test2$Miss3 <- ifelse(test2$PRI_jet_num == 1 & test2$Mass_miss == 0, 1, 0)
test2$Miss4 <- ifelse(test2$PRI_jet_num == 1 & test2$Mass_miss == 1, 1, 0)
test2$Miss5 <- ifelse(test2$PRI_jet_num == 2 & test2$Mass_miss == 0, 1, 0)
test2$Miss6 <- ifelse(test2$PRI_jet_num == 2 & test2$Mass_miss == 1, 1, 0)
test2$Miss7 <- ifelse(test2$PRI_jet_num == 3 & test2$Mass_miss == 0, 1, 0)
test2$Miss8 <- ifelse(test2$PRI_jet_num == 3 & test2$Mass_miss == 1, 1, 0)

colnames(test2)[apply(is.na(test2), 2, any)]  

test2$DER_deltaeta_jet_jet [is.na(test2$DER_deltaeta_jet_jet)] = mean(test2$DER_deltaeta_jet_jet, na.rm=TRUE)
test2$DER_mass_jet_jet[is.na(test2$DER_mass_jet_jet)] = mean(test2$DER_mass_jet_jet, na.rm=TRUE)
test2$DER_prodeta_jet_jet[is.na(test2$DER_prodeta_jet_jet)] = mean(test2$DER_prodeta_jet_jet, na.rm=TRUE)
test2$DER_lep_eta_centrality[is.na(test2$DER_lep_eta_centrality)] = mean(test2$DER_lep_eta_centrality, na.rm=TRUE)
test2$PRI_jet_leading_pt[is.na(test2$PRI_jet_leading_pt)] = mean(test2$PRI_jet_leading_pt, na.rm=TRUE)
test2$PRI_jet_leading_eta[is.na(test2$PRI_jet_leading_eta)] = mean(test2$PRI_jet_leading_eta, na.rm=TRUE)
test2$PRI_jet_subleading_pt[is.na(test2$PRI_jet_subleading_pt)] = mean(test2$PRI_jet_subleading_pt, na.rm=TRUE)
test2$PRI_jet_subleading_eta[is.na(test2$PRI_jet_subleading_eta)] = mean(test2$PRI_jet_subleading_eta, na.rm=TRUE)
test2$PRI_jet_leading_phi_rot[is.na(test2$PRI_jet_leading_phi_rot)] = mean(test2$PRI_jet_leading_phi_rot, na.rm=TRUE)
test2$PRI_jet_subleadint_phi_rot[is.na(test2$PRI_jet_subleadint_phi_rot)] = mean(test2$PRI_jet_subleadint_phi_rot, na.rm=TRUE)



View(test2)



