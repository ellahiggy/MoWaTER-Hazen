load("HSMaster.rda")

#plot train 1 variables over time 
#each one that I felt was helpful has one with points and one with line



#All together, it seems there are 4 - 6 CIPs in train 1 - DP only shows 1 though
#---------------------------------

#These felt more clear in helping identify CIP 
#Salt Passage (both), Differential Pressure (both, stage1, and stage2), Specific Flux
#Net driving Pressure (both), Permeate Conductivity (both), Feedwater Pressure
#Online Status could maybe help

#Percent Salt Passage
plot(HSMaster$Date_Time, HSMaster$SP_a_TR1)
plot(HSMaster$Date_Time, HSMaster$SP_a_TR1, type = 'l')
#Normalized Salt Passage  (Helpful?)
plot(HSMaster$Date_Time, HSMaster$SP_n_TR1)
plot(HSMaster$Date_Time, HSMaster$SP_n_TR1, type = 'l')
#one around February, one between Sep-Nov, maybe two in Nov-Jan, one around May
#one between May-July (in the middle)



#Differential Pressure Normalized (Helpful?)
plot(HSMaster$Date_Time, HSMaster$DP_n_TR1)
plot(HSMaster$Date_Time, HSMaster$DP_n_TR1, type = 'l')
#only really shows one CIP between Jan-Mar
#what is the one value at May?

#DP Stage 1 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s1_TR1)
#DP Stage 2 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s2_TR1)
#Differential Pressure Stage 1
plot(HSMaster$Date_Time, HSMaster$DP_s1_TR1)
#Total Differential Pressure
plot(HSMaster$Date_Time, HSMaster$DP_t_TR1)




#Net driving pressures at actual conditions
plot(HSMaster$Date_Time, HSMaster$NetDp_a_TR1)
plot(HSMaster$Date_Time, HSMaster$NetDp_a_TR1, type = 'l')
#Net Driving pressure (Helpful)
plot(HSMaster$Date_Time, HSMaster$Nt_DP_TR1)
plot(HSMaster$Date_Time, HSMaster$Nt_DP_TR1, type = 'l')
#One between Sep-Nov, maybe one right after Nov, one between Jan-Mar, one in May



#Specific Flux (Helful)
plot(HSMaster$Date_Time, HSMaster$Sp_Fl_TR1)
plot(HSMaster$Date_Time, HSMaster$Sp_Fl_TR1, type = 'l')
#Specific Flux Calc (helpful)
plot(HSMaster$Date_Time, HSMaster$SF_Cal_TR1)
plot(HSMaster$Date_Time, HSMaster$SF_Cal_TR1, type = 'l')
#One between Sep-Nov, one between nov-Jan?, One between Jan-Mar, One at May
#maybe one in between May-Jul, and one at Jul?



#Normalized Permeate Conductivity (helpful)
plot(HSMaster$Date_Time, HSMaster$NCp_TR1)
plot(HSMaster$Date_Time, HSMaster$NCp_TR1, type = 'l')
#Permeate Conductivity (Helpful?)
plot(HSMaster$Date_Time, HSMaster$C_p_TR1)
plot(HSMaster$Date_Time, HSMaster$C_p_TR1, type = 'l')
#One btw Sep-Nov, one right after Nov (maybe another one?), One btw Jan-mar,
#One at May, One between May-Jul

#Feedwater pressure (Helpful?)
plot(HSMaster$Date_Time, HSMaster$P_f_TR1)
plot(HSMaster$Date_Time, HSMaster$P_f_TR1, type = 'l')
#One between Sep-Nov, one after nov, one between Jan-Mar, one in May, 
#maybe one between May-Jul?

#ONline Status 
plot(HSMaster$Date_Time, HSMaster$ON_TR1)
#After September, system is off for a period of time between Sep-Nov
#small points between Nov-Jan, a large time in Jan-Mar, a couple times btw Mar-May
#Some at May, and and some point between May-July



#--------------------------------------
#these were not as helpful in my opinion

#Recovery at actual conditions
plot(HSMaster$Date_Time, HSMaster$R_a_TR1)
#Percentage Recovery
plot(HSMaster$Date_Time, HSMaster$Per_R_a_TR1)

#Feed/brine Conductivity  (helpful?)
plot(HSMaster$Date_Time, HSMaster$C_fb_a_TR1)

#Feed/brine Osmotic Pressure 
plot(HSMaster$Date_Time, HSMaster$OP_fb_a_TR1)

#Permeate Osmotic Pressure
plot(HSMaster$Date_Time, HSMaster$OP_p_a_TR1)

#Average Osmotic Pressure Gradient
plot(HSMaster$Date_Time, HSMaster$OP_TR1)

#Normalized Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_n_TR1)
#Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_TR1)

#Flux 
plot(HSMaster$Date_Time, HSMaster$Flux_TR1)

#Concentrate Conductivity
plot(HSMaster$Date_Time, HSMaster$C_c_TR1)

#Conc Valve Position ?
plot(HSMaster$Date_Time, HSMaster$Con_F_TR1)

#Concentrate Flow 
plot(HSMaster$Date_Time, HSMaster$F_c_TR1)

#Feed Pump Feedback
plot(HSMaster$Date_Time, HSMaster$P_M110_1)

#Permeate Pressure 
plot(HSMaster$Date_Time, HSMaster$P_p_TR1)

#Starting Status
plot(HSMaster$Date_Time, HSMaster$RO_T1_STARTING)

#Stopping Status
plot(HSMaster$Date_Time, HSMaster$RO_T1_STOPPING)
