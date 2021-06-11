##############################################################################
#Carries & Plotting 
##############################################################################

#------------------------------------------------------------------------------
#1Y / 1Y Carry
#------------------------------------------------------------------------------ 

Carry_1 <- carry(1)
plot_rates(caption ="Date shows spot price at date of forward delivery
           Forward rate shows implied forward rate 1 year earlier based on yield differentials", 
           title="1-Year Carry JPY/NZD", 
           Carry_1)
plot_diff(caption ="> implies that NZD was stronger / JPY was weaker than predicted
          < implies that NZD was weaker / JPY was stronger than predicted", 
          title="JPY/NZD Spot at Delivery - Implied Forward (1 Year)",
          Carry_1)

#------------------------------------------------------------------------------
#90d / 90d Carry
#------------------------------------------------------------------------------ 

Carry_90 <- carry(90, days = T)
plot_rates(caption ="Date shows spot price at date of forward delivery
           Forward rate shows implied forward rate 90 days earlier based on yield differentials", 
           title="90-Day Carry JPY/NZD",
           Carry_90)
plot_diff(caption ="> implies that NZD was stronger / JPY was weaker than predicted
          < implies that NZD was weaker / JPY was stronger than predicted", 
          title="JPY/NZD Spot at Delivery - Implied Forward (90 Days)",
          Carry_90)

#------------------------------------------------------------------------------
#5Y / 5Y Carry
#------------------------------------------------------------------------------ 

Carry_5 <- carry(5)
plot_rates(caption ="Date shows spot price at date of forward delivery
           Forward rate shows implied forward rate 5 years earlier based on yield differentials", 
           title="5-Year Carry JPY/NZD", 
           Carry_5)
plot_diff(caption ="> implies that NZD was stronger / JPY was weaker than predicted
          < implies that NZD was weaker / JPY was stronger than predicted", 
          title="JPY/NZD Spot at Delivery - Implied Forward (5 Years)",
          Carry_5)

#------------------------------------------------------------------------------
#10Y / 10Y Carry
#------------------------------------------------------------------------------ 

Carry_10 <- carry(10)
plot_rates(caption ="Date shows spot price at date of forward delivery
           Forward rate shows implied forward rate 10 years earlier based on yield differentials", 
           title="10-Year Carry JPY/NZD", 
           Carry_10)
plot_diff(caption ="> implies that NZD was stronger / JPY was weaker than predicted
          < implies that NZD was weaker / JPY was stronger than predicted", 
          title="JPY/NZD Spot at Delivery - Implied Forward (10 Years)",
          Carry_10)

#------------------------------------------------------------------------------
#Plot: Interest Rates / Exchange Rate
#------------------------------------------------------------------------------ 

#Interest Rates / Exchange Rate (1Year)
ggplot(data = Rates, aes(x = Date))+
  geom_line(aes(y = NZD_1Y, color = "NZD_1Y"))+
  geom_line(aes(y = JPY_1Y, color = "JPY_1Y"))+
  geom_line(aes(y = Spot/15, color = "JPY/NZD"))+
  scale_y_continuous(name = "1 Year Rates",                      #adapt axis according to the used rate
                     sec.axis = sec_axis(~.*15, name = "JPY/NZD")
  )+
  labs(color = "", title = "JPY/NZD Exchange Rate and 1-Year Interest Rates", caption = "", y = "")+
  scale_x_date(breaks = function(x) seq.Date(from = ymd("1990-01-01"), to = ymd("2020-01-01"), 
                                             by = "5 years"), date_labels = "%Y")+
  theme_classic()+ theme(axis.text.x = element_text(size=10),
                         axis.text.y = element_text(size=10),
                         plot.title = element_text(size=12),
                         legend.position = c(0.9,0.9))
