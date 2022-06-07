load("HSMaster.rda")
load("HSCompleteEntries.rda")

#plot train 2 variables over time 
#specifically the ones found to be helpful in train 1 

#Train 2 is less clear than train 1
#A lot of these graphs have a drop in the values around Jan-Mar. What is going on?


#Specfic Flux (both), Net Driving Pressure, maybe more?

#I think there is a clear CIP at May, I don't know what the random drop is?
#Maybe 3 CIPs (one in may, between Jan-Mar, right after Nov?), possibly more?

#Percent Salt Passage
plot(HSMaster$Date_Time, HSMaster$SP_a_TR2)
#Normalized Salt Passage
plot(HSMaster$Date_Time, HSMaster$SP_n_TR2)
#not very helpful

#Differential Pressure Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_TR2)
#DP Stage 1 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s1_TR2)
#DP Stage 2 Normalized
plot(HSMaster$Date_Time, HSMaster$DP_n_s2_TR2)
#Differential Pressure Stage 1
plot(HSMaster$Date_Time, HSMaster$DP_s1_TR2)
#interesting drop between Jan-Mar- not sure what happened

#Total Differential Pressure
plot(HSMaster$Date_Time, HSMaster$DP_t_TR2)
#why is it straight line?
#not helpful really


#Net driving pressures at actual conditions 
plot(HSMaster$Date_Time, HSMaster$NetDp_a_TR2)
#not really helpful - has negative values
#what is the drop in the middle of the graph

#Net Driving pressure (Helpful)
plot(HSMaster$Date_Time, HSMaster$Nt_DP_TR2)
#still has dramatic drop between Jan-Mar
#CIP right after Nov?, maybe between Jan-Mar, one at May

#Specific Flux (Helpful?)
plot(HSMaster$Date_Time, HSMaster$Sp_Fl_TR2)
#Specific Flux Calc (helpful?)
plot(HSMaster$Date_Time, HSMaster$SF_Cal_TR2)
#CIP between Jan-Mar, at May, maybe some between Sep-nov, and after Nov

#Normalized Permeate Conductivity
plot(HSMaster$Date_Time, HSMaster$NCp_TR2)
#not really helpful?
#again what is the drop in the middle

#Permeate Conductivity
plot(HSMaster$Date_Time, HSMaster$C_p_TR2, ylim = c(20, 50))
#If you zoom in its more helpful, but I don't really understand it.


#Feedwater pressure (Helpful?)
plot(HSMaster$Date_Time, HSMaster$P_f_TR2)
#has drop again


#ONline Status 
plot(HSMaster$Date_Time, HSMaster$ON_TR2)
#maybe helpful
#machine doesn't appear to be off for a long period of time like train 1.
#It doesn't appear when the Jan-Mar drops occurs 


#Values that are maybe helpful that were not in Train 1 
#(the y axis is zoomed in to see the trends, but it excludes some outliers)

#Feed/brine Conductivity
plot(HSMaster$Date_Time, HSMaster$C_fb_a_TR2, ylim = c(1800, 2500))

#Feed/brine Osmotic Pressure 
plot(HSMaster$Date_Time, HSMaster$OP_fb_a_TR2, ylim = c(100, 200))

#Permeate Osmotic Pressure
plot(HSMaster$Date_Time, HSMaster$OP_p_a_TR2, ylim = c(5, 10))

#Concentrate Conductivity
plot(HSMaster$Date_Time, HSMaster$C_c_TR2)


#-------------------------

#Recovery at actual conditions
plot(HSMaster$Date_Time, HSMaster$R_a_TR2)
#Percentage Recovery
plot(HSMaster$Date_Time, HSMaster$Per_R_a_TR2)

#Average Osmotic Pressure Gradient
plot(HSMaster$Date_Time, HSMaster$OP_TR2, ylim = c(10,15))
#looks the same as other osmotic pressure graphs

#Normalized Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_n_TR2)
#Permeate Flow
plot(HSMaster$Date_Time, HSMaster$F_p_TR2)

#Flux 
plot(HSMaster$Date_Time, HSMaster$Flux_TR2)

#Conc Valve Position ?
plot(HSMaster$Date_Time, HSMaster$Con_F_TR2)

#Concentrate Flow 
plot(HSMaster$Date_Time, HSMaster$F_c_TR2)

#Feed Pump Feedback
plot(HSMaster$Date_Time, HSMaster$P_M110_2)

#Permeate Pressure 
plot(HSMaster$Date_Time, HSMaster$P_p_TR2)

#Starting Status
plot(HSMaster$Date_Time, HSMaster$RO_T2_STARTING)

#Stopping Status
plot(HSMaster$Date_Time, HSMaster$RO_T2_STOPPING)



plot(HSCompleteEntries$Date_Time, HSCompleteEntries$C_fb_a_TR2, ylim = c(1800, 2500))
#tried Complete Entries but it doesn't really help in terms of determining CIPs
