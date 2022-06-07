load("HSMaster.rda")

#Train 5 variables over time, also has booster pump like Train 4



#These variables seem to demonstrate some evidence of CIP
#   Normalized Salt passage, 
#   normalized permeate conductivity/ permeate conductivity
#Maybe Permeate Flow stage 2?



# 2 CIPs?

#-----------------------
#these variables seemed helpful

#Percent Salt Passage
plot(HSMaster$Date_Time, HSMaster$SP_a_TR5)
#Normalized Salt Passage  (Helpful?)
plot(HSMaster$Date_Time, HSMaster$SP_n_TR5)
#Normalized - shows CIP at between at Mar and may?


#Normalized Permeate Conductivity 
plot(HSMaster$Date_Time, HSMaster$NCp_TR5)
#one at Mar, one at May
#Permeate Conductivity 
plot(HSMaster$Date_Time, HSMaster$C_p_TR5, ylim = c(25, 55))
#one maybe between Jan-Mar? I don't know what the gap is


#ONline Status 
plot(HSMaster$Date_Time, HSMaster$ON_TR5)
#Clear time off in May


#-----------
#I don't know if these are helpful

#Specific Flux
plot(HSMaster$Date_Time, HSMaster$Sp_Fl_TR5)
#It's only decreasing like train4- not helpful like Train1 and 2. Maybe on at may?

#permeate Flow Stage 2
plot(HSMaster$Date_Time, HSMaster$F_p_s2_TR5)
#only decreasing

#Feed/brine Conductivity
plot(HSMaster$Date_Time, HSMaster$C_fb_a_TR5)

#Feed/brine Osmotic Pressure 
plot(HSMaster$Date_Time, HSMaster$OP_fb_a_TR5)

#Permeate Osmotic Pressure
plot(HSMaster$Date_Time, HSMaster$OP_p_a_TR5)

#Concentrate Conductivity
plot(HSMaster$Date_Time, HSMaster$C_c_TR5)



#--------------------------------------------
#not has helpful graphs (in my opinon)

#Net Driving pressure 
plot(HSMaster$Date_Time, HSMaster$Nt_DP_TR5)
#another drop between Jan-Mar
#Just a straight line

#Differential Pressure Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_TR5)
#DP Stage 1 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s1_TR5)
#DP Stage 2 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s2_TR5)
#Differential Pressure Stage 1
plot(HSMaster$Date_Time, HSMaster$DP_s1_TR5)
#this one is significantly different from the others (not normalized)

#Total Differential Pressure
plot(HSMaster$Date_Time, HSMaster$DP_t_TR5)
#this is a straight line? why

#Net driving pressures at actual conditions 
plot(HSMaster$Date_Time, HSMaster$NetDp_a_TR5)
#negative values?
#Net Driving Pressure 
plot(HSMaster$Date_Time, HSMaster$Nt_DP_TR5)

#Feedwater pressure
plot(HSMaster$Date_Time, HSMaster$P_f_TR5)
#it's going up?

#Recovery at actual conditions
plot(HSMaster$Date_Time, HSMaster$R_a_TR5)
#Percentage Recovery
plot(HSMaster$Date_Time, HSMaster$Per_R_a_TR5)

#Average Osmotic Pressure Gradient
plot(HSMaster$Date_Time, HSMaster$OP_TR5)

#Normalized Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_n_TR5)
#Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_TR5)

#Flux 
plot(HSMaster$Date_Time, HSMaster$Flux_TR5)

#Conc Valve Position ?
plot(HSMaster$Date_Time, HSMaster$Con_F_TR5)

#Concentrate Flow 
plot(HSMaster$Date_Time, HSMaster$F_c_TR4)

#Feed Pump Feedback
plot(HSMaster$Date_Time, HSMaster$P_M110_5)

#Permeate Pressure 
plot(HSMaster$Date_Time, HSMaster$P_p_TR4)

#Starting Status
plot(HSMaster$Date_Time, HSMaster$RO_T5_STARTING)

#Stopping Status
plot(HSMaster$Date_Time, HSMaster$RO_T5_STOPPING)

#----------------------------
#New variables in Train 4 and 5 (don't seem helpful in determining CIP)

#IPB Speed Feedback
plot(HSMaster$Date_Time, HSMaster$P_M110_5)

#Pump Speed Feedback
plot(HSMaster$Date_Time, HSMaster$P_M115_5)

#interstage suction Pressure
plot(HSMaster$Date_Time, HSMaster$P_is_TR5)

#interstage pressure
plot(HSMaster$Date_Time, HSMaster$P_id_TR5)

#stage 2 permeate conductivity
plot(HSMaster$Date_Time, HSMaster$C_p_i_TR5)


