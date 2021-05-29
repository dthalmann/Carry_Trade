##############################################################################
#Carries & Plotting 
##############################################################################

#------------------------------------------------------------------------------
#1Y / 1Y Fixed Carry
#------------------------------------------------------------------------------ 

Carry_1 <- fixed_carry(1)

plot_rates("caption","title", Carry_1)
plot_diff("caption","title", Carry_1)

#------------------------------------------------------------------------------
#90d / 90d Fixed Carry
#------------------------------------------------------------------------------ 

Carry_2 <- fixed_carry(90, days = T)

plot_rates("caption","title", Carry_2)
plot_diff("caption","title", Carry_2)

#------------------------------------------------------------------------------
#5Y / 5Y Fixed Carry
#------------------------------------------------------------------------------ 

Carry_3 <- fixed_carry(5)

plot_rates("caption","title", Carry_3)
plot_diff("caption","title", Carry_3)

#------------------------------------------------------------------------------
#10Y / 10Y Fixed Carry
#------------------------------------------------------------------------------ 

Carry_4 <- fixed_carry(10)

plot_rates("caption","title", Carry_4)
plot_diff("caption","title", Carry_4)


#------------------------------------------------------------------------------
#Other Plots
#------------------------------------------------------------------------------ 

#Interest Rates / Exchange Rate
ggplot(data = Rates, aes(x = Date))+
  geom_line(aes(y = NZD_1Y, color = "NZD_1Y"))+
  geom_line(aes(y = JPY_1Y, color = "JPY_1Y"))+
  geom_line(aes(y = Spot/15, color = "JPY/NZD"))+
  scale_y_continuous(name = "90d Rates",
                     sec.axis = sec_axis(~.*15, name = "JPY/NZD")
  )+
  labs(color = "", title = "", caption = "", y = "")+
  theme_classic()
