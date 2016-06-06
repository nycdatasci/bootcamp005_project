library(dplyr)
# Momentum
px = function(pt, eta, phi) {
  return(ifelse(pt<0, 0, pt * cos(phi)))
}
py = function(pt, eta, phi) {
  return(ifelse(pt<0, 0, pt * sin(phi)))
}
pz = function(pt, eta, phi) {
  return(ifelse(pt<0, 0, pt * sinh(eta)))
}
p_tot = function(pt, eta, phi) {
  x = px(pt, eta, phi)
  y = py(pt, eta, phi)
  z = pz(pt, eta, phi)
  return(ifelse(pt<0, 0, sqrt(x*x + y*y + z*z)))
}

calculate_momenta = function(df, prefix) {
  pt = df[prefix+'pt']
  eta = df[prefix+'eta']
  phi = df[prefix+'phi']
  out = as.data.frame(cbind(px(pt, eta, phi),
                            py(pt, eta, phi),
                            pz(pt, eta, phi),
                            p_tot(pt, eta, phi)))
  names(out) = c(prefix+'px',
                 prefix+'py',
                 prefix+'pz',
                 prefix+'p_tot')
  return(out)
}

#abs values of these px py pz can have separation
abs_p = function(df,prefix) {
  out = abs(df[prefix])
  names(out) = prefix+'_abs'
  return(out)
}

get_abs_momenta = function(df,prefix) {
  out = as.data.frame(cbind(abs_p(df,prefix+'px'),
                            abs_p(df,prefix+'py'),     
                            abs_(df,prefix+'pz')))
  names(out) = c(prefix+'px_abs',
                 prefix+'py_abs',
                 prefix+'pz_abs')
}

get_momentum_features = function(df) {
  lep = calculate_momenta(df, 'PRI_lep_')
  jet_leading = calculate_momenta(df, 'PRI_jet_leading_')
  jet_subleading = calculate_momenta(df, 'PRI_jet_subleading_')
  tau = calculate_momenta(df, 'PRI_tau_')
  out = lep %>% 
    left_join(tau) %>%
    left_join(jet_leading) %>%
    left_join(jet_subleading)
  return(out)
}

get_abs_momentum_features = function(df){
  lep = get_abs_momenta(df, 'PRI_lep_')
  jet_leading = get_abs_momenta(df, 'PRI_jet_leading_')
  jet_subleading = get_abs_momenta(df, 'PRI_jet_subleading_')
  tau = get_abs_momenta(df, 'PRI_tau_')
  out = lep %>% 
    left_join(tau) %>%
    left_join(jet_leading) %>%
    left_join(jet_subleading)
  return(out)
}
# 
# #=> TBD
# with_momentum_features = function(df) {
#   out = df %>% 
#     left_join(get_momentum_features(df))
#     # replace([np.inf, -np.inf], np.nan).fillna(-999.)
#   return(out)
# }
# 
# with_abs_momentum_features = function(df) {
#   out = df %>%
#     left_join(get_abs_momentum_features(df))
#   # replace([np.inf, -np.inf], np.nan).fillna(-999.)
#   return(out)
# }

# sum PT feature of vector summations
pt_sqrt = function(x,y) {
  return(sqrt(x*x+y*y))
}

tau_lep_vec_sum_pt = function(row) {
  x = row['PRI_tau_px']+row['PRI_lep_px']
  y = row['PRI_tau_py']+row['PRI_lep_py']
  out = pt_sqrt(x,y)
  names(out) = 'New_tau_lep_pt_vec_sum'
  return(out)
}

tau_jet_vec_sum_pt = function(row){
  x = row['PRI_tau_px']+row['PRI_jet_leading_px']
  y = row['PRI_tau_py']+row['PRI_jet_leading_py']
  out = pt_sqrt(x,y)
  names(out) = 'New_tau_jet_pt_vec_sum'
  return(ifelse(row['PRI_jet_num']==0, 0, out))
}

lep_jet_vec_sum_pt = function(row) {
  x = row['PRI_lep_px']+row['PRI_jet_leading_px']
  y = row['PRI_lep_py']+row['PRI_jet_leading_py']
  out = pt_sqrt(x,y)
  names(out) = 'New_lep_jet_pt_vec_sum'
  return(ifelse(row['PRI_jet_num']==0, 0, out))
}

vec_pt_features = cbind(tau_lep_vec_sum_pt,tau_jet_vec_sum_pt,lep_jet_vec_sum_pt)

# Eta Features

# The distance from the x=y line in jet eta0, eta1 space
eta_plus = function(x, y) {
  return(sqrt(x*x/2 + 2*y*y - 2*x*y))
}

jet_eta_plus = function(row) {
  x = row['PRI_jet_leading_eta']
  y = row['PRI_jet_subleading_eta']
  out = eta_plus(x, y)
  names(out) = 'New_jet_eta_plus'
  return(ifelse(row['PRI_jet_num']<2, 0, out))
}

# Do the same with the lepton and tau
lep_tau_eta_plus = function(row) {
  x = row['PRI_lep_eta']
  y = row['PRI_tau_eta']
  out = eta_plus(x, y)
  names(out) = 'New_lep_tau_eta_plus'
  return(out)
}

# Do the same with the lepton and jet
lep_jet_eta_plus = function(row) {
  x = row['PRI_lep_eta']
  y = row['PRI_jet_leading_eta']
  out = eta_plus(x, y)
  names(out) = 'New_lep_jet_eta_plus'
  return(out)
}

# Do the same with the tau and jet
tau_jet_eta_plus = function(row) {
  x = row['PRI_tau_eta']
  y = row['PRI_jet_leading_eta']
  out = eta_plus(x, y)
  names(out) = 'New_tau_jet_eta_plus'
  return(out)
}

jet_leading_abs_eta = function(row) {
  out = abs(row['PRI_jet_leading_eta'])
  names(out) = 'PRI_jet_leading_eta_abs'
  return(ifelse(row['PRI_jet_num']==0, 0, out))
}

jet_subleading_abs_eta = function(row) {
  out = abs(row['PRI_jet_subleading_eta'])
  names(out) = 'PRI_jet_subleading_eta_abs'
  return(ifelse(row['PRI_jet_num']<2, 0, out))
}

rapidity_features = cbind(jet_eta_plus, lep_jet_eta_plus, tau_jet_eta_plus, 
                          lep_tau_eta_plus, jet_leading_abs_eta,jet_subleading_abs_eta)


# Z Momentum Features: only when jet is valid

lep_z_diff_momemtum = function(row) {
  out = abs(row['PRI_lep_pz'] - row['PRI_tau_pz']) #make it non symmetric
  names(out) = 'New_lep_z_diff_momentum'
  return(out)
}

lep_z_momentum = function(row) {
  out = abs(row['PRI_lep_pz'] + row['PRI_tau_pz'])
  names(out) = 'New_lep_z_momentum'
  return(out)
}

jet_z_momentum = function(row) {
  out = row['PRI_jet_leading_pz'] + row['PRI_jet_subleading_pz']
  names(out) = 'New_jet_z_momentum'
  return(ifelse(row['PRI_jet_num']<2, 0, out))
}

jet_lep_sum_z_momentum = function(row) {
  out = lep_z_momentum(row) + jet_z_momentum(row)
  names(out) = 'New_jet_lep_sum_z_momentum'
  return(ifelse(row['PRI_jet_num']<2, 0, out))
}

jet_lep_diff_z_momentum = function(row) {
  out = lep_z_momentum(row) - jet_z_momentum(row)
  names(out) = 'New_jet_lep_diff_z_momentum'
  return(ifelse(row['PRI_jet_num']<2, 0, out))
}

z_momentum_features = cbing(lep_z_momentum, lep_z_diff_momemtum,
                       jet_z_momentum, jet_lep_sum_z_momentum, jet_lep_diff_z_momentum)


# Transverse Momenta Features
#=> Check
max_jet_pt = function(row) {
  out = max(row[c('PRI_jet_leading_pt','PRI_jet_subleading_pt')])
  names(out) = 'New_max_jet_pt'
  return(out)
}
#=> Check
min_jet_pt = function(row) {
  out = min(row[c('PRI_jet_leading_pt','PRI_jet_subleading_pt')])
  names(out) = 'New_min_jet_pt'
  return(out)
}
#=> Check
max_lep_pt = function(row) {
  out = max(row[c('PRI_tau_pt','PRI_lep_pt')])
  names(out) = 'New_max_lep_pt'
  return(out)
}
#=> Check
min_lep_pt = function(row) {
  out = min(row[c('PRI_tau_pt','PRI_lep_pt')]) 
  names(out) = 'New_min_lep_pt'
  return(out)
}

max_pt = function(row) {
  max_jet_pt_frame = as.data.frame(max_jet_pt = max_jet_pt(row))
  max_lep_pt_frame = as.data.frame(max_lep_pt = max_lep_pt(row))
  out = max_jet_pt_frame %>%
    left_join(max_lep_pt_frame)
  #=> TBD
  names(out) = 'New_max_pt'
  return(out)
}

min_pt = function(row) {
  min_jet_pt_frame = as.data.frame(min_jet_pt = min_jet_pt(row))
  min_lep_pt_frame = as.data.frame(min_lep_pt = min_lep_pt(row))
  out = min_jet_pt_frame %>%
    left_join(min_lep_pt_frame)
  #=> TBD
  out = 'New_min_pt'
  return(out)
}

sum_jet_pt = function(row) {#??
  out = row['PRI_jet_leading_pt'] + row['PRI_jet_subleading_pt']
  names(out) = 'New_sum_jet_pt'
  return(ifelse(row['PRI_jet_subleading_phi']==0, 0, out))
}

sum_jet_vec_pt = function(row) {#??
  x = row['PRI_jet_leading_px'] + row['PRI_jet_subleading_px']
  y = row['PRI_jet_leading_py'] + row['PRI_jet_subleading_py']
  out = pt_sqrt(x,y)
  names(out) = 'New_sum_jet_vec_pt'
  return(ifelse(row['PRI_jet_num']<2, 0, out))
}

sum_lep_pt = function(row) {#should be replaced by the tau-lep vect sum pt
  out = row['PRI_tau_pt'] + row['PRI_lep_pt']
  names(out) = 'New_sum_lep_pt'
  return(out)
}

#pt(ditau) ~ pt_tau+pt_p_j+met for one jet > 140GeV
pt_tautau_single_jet = function(row) {
  out = row['PRI_tau_pt']+row['PRI_jet_leading_pt']+row['PRI_met']
  names(out) = 'New_pt_tautau_single_jet'
  return(ifelse(row['PRI_jet_num']==0, 0, out))
}

#pt(ditau) ~ pt_tau+pt_p_j+met for two jet >110 GeV with delta eta jj > 2.5
pt_tautau_multi_jet = function(row) {
  out = row['PRI_tau_pt']+row['PRI_jet_leading_pt']+row['PRI_jet_subleading_pt']+row['PRI_met']
  names(out) = 'New_pt_tautau_multi_jet'
  return(ifelse(row['PRI_jet_num']<2, 0, out))
}

transverse_momentum_features = cbind(max_jet_pt, min_jet_pt, max_lep_pt, min_lep_pt,
                                     sum_jet_pt, sum_lep_pt)

# Momentum Ratio Features

frac_tau_pt = function(row) {
  tau_pt = row['PRI_tau_pt']
  lep_pt = row['PRI_lep_pt']
  out = tau_pt / (tau_pt + lep_pt)
  names(out) = 'New_frac_tau_pt'
  return(out)
}

frac_lep_pt = function(row) {
  tau_pt = row['PRI_tau_pt']
  lep_pt = row['PRI_lep_pt']
  out = lep_pt / (tau_pt + lep_pt)
  names(out) = 'New_frac_lep_pt'
  return(out)
}

frac_tau_p = function(row) {
  tau_p = row['PRI_tau_p_tot']
  lep_p = row['PRI_lep_p_tot']
  out = tau_p / (tau_p + lep_p)
  names(out) = 'New_frac_tau_p'
  return(out)
}

frac_lep_p = function(row) {
  tau_p = row['PRI_tau_p_tot']
  lep_p = row['PRI_lep_p_tot']
  out = lep_p / (tau_p + lep_p)
  names(out) = 'New_frac_lep_p'
  return(out)
}

momentum_ratio_features = cbind(frac_tau_pt, frac_lep_pt, frac_tau_p, frac_lep_p)



#Down to line 373 of https://github.com/phunterlau/kaggle_higgs/blob/master/add_features.py