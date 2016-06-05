
    src.Train       <- read.csv('Data/Training.csv')
    src.Train[src.Train==-999] <- NA
    
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
    
    PRI_lep_phi_rot         <- rotate_phi(src.Train$PRI_lep_phi, src.Train$PRI_tau_phi)
    PRI_met_phi_rot         <- rotate_phi(src.Train$PRI_met_phi, src.Train$PRI_tau_phi)
    PRI_jet_leading_phi_rot <- rotate_phi(src.Train$PRI_jet_leading_phi, src.Train$PRI_tau_phi)
    PRI_jet_subleading_phi_rot <- rotate_phi(src.Train$PRI_jet_subleading_phi, src.Train$PRI_tau_phi)
    
    mod.train <- data.frame(src.Train, 
                            PRI_lep_phi_rot,
                            PRI_met_phi_rot,
                            PRI_jet_leading_phi_rot,
                            PRI_jet_subleading_phi_rot)
    mod.train <- subset(mod.train, select = - c(PRI_tau_phi, PRI_lep_phi,
                                                PRI_jet_leading_phi,PRI_jet_subleading_phi))
    Mass_miss <- ifelse(is.na(mod.train$DER_mass_MMC),1,0)
    mod.train <- data.frame(mod.train, Mass_miss)
    mod.train$DER_mass_MMC[is.na(mod.train$DER_mass_MMC)] = mean(mod.train$DER_mass_MMC, na.rm=TRUE)
    
    #add missing variation from columns that will be mean imputed
    mod.train$Miss1 <- ifelse(mod.train$PRI_jet_num == 0 & mod.train$Mass_miss == 0, 1, 0)
    mod.train$Miss2 <- ifelse(mod.train$PRI_jet_num == 0 & mod.train$Mass_miss == 1, 1, 0)
    mod.train$Miss3 <- ifelse(mod.train$PRI_jet_num == 1 & mod.train$Mass_miss == 0, 1, 0)
    mod.train$Miss4 <- ifelse(mod.train$PRI_jet_num == 1 & mod.train$Mass_miss == 1, 1, 0)
    mod.train$Miss5 <- ifelse(mod.train$PRI_jet_num == 2 & mod.train$Mass_miss == 0, 1, 0)
    mod.train$Miss6 <- ifelse(mod.train$PRI_jet_num == 2 & mod.train$Mass_miss == 1, 1, 0)
    mod.train$Miss7 <- ifelse(mod.train$PRI_jet_num == 3 & mod.train$Mass_miss == 0, 1, 0)
    mod.train$Miss8 <- ifelse(mod.train$PRI_jet_num == 3 & mod.train$Mass_miss == 1, 1, 0)
    
    #mean impute within columns across the dataset
    colnames(mod.train)[apply(is.na(mod.train), 2, any)]      
    
    mod.train$DER_deltaeta_jet_jet [is.na(mod.train$DER_deltaeta_jet_jet)] = mean(mod.train$DER_deltaeta_jet_jet, na.rm=TRUE)
    mod.train$DER_mass_jet_jet[is.na(mod.train$DER_mass_jet_jet)] = mean(mod.train$DER_mass_jet_jet, na.rm=TRUE)
    mod.train$DER_prodeta_jet_jet[is.na(mod.train$DER_prodeta_jet_jet)] = mean(mod.train$DER_prodeta_jet_jet, na.rm=TRUE)
    mod.train$DER_lep_eta_centrality[is.na(mod.train$DER_lep_eta_centrality)] = mean(mod.train$DER_lep_eta_centrality, na.rm=TRUE)
    mod.train$PRI_jet_leading_pt[is.na(mod.train$PRI_jet_leading_pt)] = mean(mod.train$PRI_jet_leading_pt, na.rm=TRUE)
    mod.train$PRI_jet_leading_eta[is.na(mod.train$PRI_jet_leading_eta)] = mean(mod.train$PRI_jet_leading_eta, na.rm=TRUE)
    mod.train$PRI_jet_subleading_pt[is.na(mod.train$PRI_jet_subleading_pt)] = mean(mod.train$PRI_jet_subleading_pt, na.rm=TRUE)
    mod.train$PRI_jet_subleading_eta[is.na(mod.train$PRI_jet_subleading_eta)] = mean(mod.train$PRI_jet_subleading_eta, na.rm=TRUE)
    mod.train$PRI_jet_leading_phi_rot[is.na(mod.train$PRI_jet_leading_phi_rot)] = mean(mod.train$PRI_jet_leading_phi_rot, na.rm=TRUE)
    mod.train$PRI_jet_subleading_phi_rot[is.na(mod.train$PRI_jet_subleading_phi_rot)] = mean(mod.train$PRI_jet_subleading_phi_rot, na.rm=TRUE)
    
    saveRDS(mod.train,'MichaelsData.rds')
    
    
     