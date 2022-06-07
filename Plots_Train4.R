load("HSMaster.rda")

#Train 4 variables over time, wanted to see if booster pump affects anything 


#train 4 also has drop in graph between Jan-Mar on some graphs 
#    Differential Pressure stage 1, Permeate Conductivity, Permeate Flow, 
#    Flow, Pump Speed Feedback


#These variables seem to demonstrate some evidence of CIP
#   Normalized Salt passage, 
#   normalized permeate conductivity and permeate conductivity
#Maybe Permeate Flow stage 2?


#-------------------------------------
#These variables seem helpful

#Percent Salt Passage
plot(HSMaster$Date_Time, HSMaster$SP_a_TR4)
#Normalized Salt Passage
plot(HSMaster$Date_Time, HSMaster$SP_n_TR4)
#Normalized - shows CIP at between Jan-Mar, Mar, and May, maybe one after Nov?


#Normalized Permeate Conductivity 
plot(HSMaster$Date_Time, HSMaster$NCp_TR4)
#One at nov?< one at Mar, one at May
#Permeate Conductivity 
plot(HSMaster$Date_Time, HSMaster$C_p_TR4, ylim = c(40, 70))
#one maybe in Sep-nov?, at mar and may?


#ONline Status 
plot(HSMaster$Date_Time, HSMaster$ON_TR4)
#significant time off in May


#------
#I don't know if these are helpful

#Specific Flux
plot(HSMaster$Date_Time, HSMaster$Sp_Fl_TR4)
#It's only decreasing- not helpful like Train1 and 2. Maybe on at may?

#permeate Flow Stage 2
plot(HSMaster$Date_Time, HSMaster$F_p_s2_TR4)
#only decreasing

#Feed/brine Conductivity
plot(HSMaster$Date_Time, HSMaster$C_fb_a_TR4)

#Feed/brine Osmotic Pressure 
plot(HSMaster$Date_Time, HSMaster$OP_fb_a_TR4)

#Permeate Osmotic Pressure
plot(HSMaster$Date_Time, HSMaster$OP_p_a_TR4)

#Concentrate Conductivity
plot(HSMaster$Date_Time, HSMaster$C_c_TR4)



#--------------------------------------------
#not has helpful graphs (in my opinon)


#Net Driving pressure
plot(HSMaster$Date_Time, HSMaster$Nt_DP_TR4)
#there's gaps in the graph
#only going up?

#Differential Pressure Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_TR4)
#DP Stage 1 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s1_TR4)
#DP Stage 2 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s2_TR4)
#Differential Pressure Stage 1
plot(HSMaster$Date_Time, HSMaster$DP_s1_TR4)
#this one is significantly different from the others (not normalized)

#Total Differential Pressure
plot(HSMaster$Date_Time, HSMaster$DP_t_TR4)
#this is a straight line? why

#Net driving pressures at actual conditions 
plot(HSMaster$Date_Time, HSMaster$NetDp_a_TR4)
#negative values?

#Feedwater pressure
plot(HSMaster$Date_Time, HSMaster$P_f_TR4)
#it's going up?

#Recovery at actual conditions
plot(HSMaster$Date_Time, HSMaster$R_a_TR4)
#Percentage Recovery
plot(HSMaster$Date_Time, HSMaster$Per_R_a_TR4)

#Average Osmotic Pressure Gradient
plot(HSMaster$Date_Time, HSMaster$OP_TR4, ylim = c(8, 14))

#Normalized Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_n_TR4)
#Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_TR4)

#Flux 
plot(HSMaster$Date_Time, HSMaster$Flux_TR4)

#Conc Valve Position ?
plot(HSMaster$Date_Time, HSMaster$Con_F_TR4)

#Concentrate Flow 
plot(HSMaster$Date_Time, HSMaster$F_c_TR4)

#Feed Pump Feedback
plot(HSMaster$Date_Time, HSMaster$P_M110_4)

#Permeate Pressure 
plot(HSMaster$Date_Time, HSMaster$P_p_TR4)

#Starting Status
plot(HSMaster$Date_Time, HSMaster$RO_T4_STARTING)

#Stopping Status
plot(HSMaster$Date_Time, HSMaster$RO_T4_STOPPING)



#-----------------------------
#New variables in Train 4 and 5 (don't seem helpful in determining CIP)

#IPB Speed Feedback
plot(HSMaster$Date_Time, HSMaster$P_M115_4_PV)

#Pump Speed Feedback
plot(HSMaster$Date_Time, HSMaster$P_M110_4_PV)

#interstage suction Pressure
plot(HSMaster$Date_Time, HSMaster$P_is_TR4)

#interstage pressure
plot(HSMaster$Date_Time, HSMaster$P_id_TR4)

#stage 2 permeate conductivity
plot(HSMaster$Date_Time, HSMaster$C_p_i_TR4)

