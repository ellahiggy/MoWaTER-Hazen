load("HSMaster.rda")
load("HSCompleteEntries.rda")

#train 3 variables over time 

#train 3 isn't very clear
#Some of these graphs have a drop in the values around Jan-Mar.
#    Permeate Cond, Permeate OP, 


#Specfic Flux (both), Net Driving Pressure, 
#maybe permeate conductivity (both), feed water pressure??


#maybe 2 or 3 CIPs
#---------------------------
#Helpful graphs?

#Net Driving pressure 
plot(HSMaster$Date_Time, HSMaster$Nt_DP_TR3)
#CIP right after Nov?, maybe between Jan-Mar, one at May
#really small drops

#Specific Flux 
plot(HSMaster$Date_Time, HSMaster$Sp_Fl_TR3)
#maybe right after Nov?, and one at May
#what is going on between Jan-Mar

#Specific Flux Calc
plot(HSMaster$Date_Time, HSMaster$SF_Cal_TR3)



#ONline Status 
plot(HSMaster$Date_Time, HSMaster$ON_TR3)
#maybe helpful
#machine doesn't appear to be off for a long period of time like train 1.
#It doesn't appear when the Jan-Mar drops occurs 



#------------------------
#I don't know if these are helpful

#Feedwater pressure
plot(HSMaster$Date_Time, HSMaster$P_f_TR3)
#somewhat increasing
#one right after nov?

#Normalized Permeate Conductivity
plot(HSMaster$Date_Time, HSMaster$NCp_TR3)
#Maybe between Jan-Mar
#Permeate Conductivity
plot(HSMaster$Date_Time, HSMaster$C_p_TR3, ylim = c(20, 50))
#maybe one between Jan-Mar, one at Mar?


#------------------------------
#These were not as helpful in my opinion


#Percent Salt Passage
plot(HSMaster$Date_Time, HSMaster$SP_a_TR3)
#Normalized Salt Passage
plot(HSMaster$Date_Time, HSMaster$SP_n_TR3)
#not very helpful

#Net driving pressures at actual conditions 
plot(HSMaster$Date_Time, HSMaster$NetDp_a_TR3)
#not really helpful - has negative values

#Differential Pressure Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_TR3)
#DP Stage 1 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s1_TR3)
#DP Stage 2 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s2_TR3)
#Differential Pressure Stage 1
plot(HSMaster$Date_Time, HSMaster$DP_s1_TR3)
#Total Differential Pressure
plot(HSMaster$Date_Time, HSMaster$DP_t_TR3)


#Feed/brine Conductivity
plot(HSMaster$Date_Time, HSMaster$C_fb_a_TR3)

#Feed/brine Osmotic Pressure 
plot(HSMaster$Date_Time, HSMaster$OP_fb_a_TR3)

#Permeate Osmotic Pressure
plot(HSMaster$Date_Time, HSMaster$OP_p_a_TR3)

#Concentrate Conductivity
plot(HSMaster$Date_Time, HSMaster$C_c_TR3)

#Recovery at actual conditions
plot(HSMaster$Date_Time, HSMaster$R_a_TR3)
#Percentage Recovery
plot(HSMaster$Date_Time, HSMaster$Per_R_a_TR3)

#Average Osmotic Pressure Gradient
plot(HSMaster$Date_Time, HSMaster$OP_TR3)
#looks the same as other osmotic pressure graphs

#Normalized Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_n_TR3)
#this graph is weird 

#Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_TR3)

#Flux 
plot(HSMaster$Date_Time, HSMaster$Flux_TR3)

#Conc Valve Position ?
plot(HSMaster$Date_Time, HSMaster$Con_F_TR3)

#Concentrate Flow 
plot(HSMaster$Date_Time, HSMaster$F_c_TR3)

#Feed Pump Feedback
plot(HSMaster$Date_Time, HSMaster$P_M110_3)

#Permeate Pressure 
plot(HSMaster$Date_Time, HSMaster$P_p_TR3)

#Starting Status
plot(HSMaster$Date_Time, HSMaster$RO_T3_STARTING)

#Stopping Status
plot(HSMaster$Date_Time, HSMaster$RO_T3_STOPPING)



