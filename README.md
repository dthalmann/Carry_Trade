# JPY / NZD Carry Trade Strategies 

- This exercise is based on an assignment in International Economics at the University of St. Gallen. The purpose is to examine whether carry trading involving JPY and NZD has been profitable over the past 20 years. In order to examine this, implied forward rates are calculated based on current exchange rates and interest differentials, and plotted against the spot rate at delivery.

## Data

- **JPY 1 Day Rates**

Federal Reserve Bank of St.Louis (2021). *Immediate Rates: Less than 24 Hours: Call Money/Interbank Rate for Japan(IRSTCI01JPM156N)*, Retrieved, from: https://fred.stlouisfed.org/series/IRSTCI01JPM156N

- **JPY 90 Days Libor**

Federal Reserve Bank of St.Louis, FRED Economic Data (2021). *3-Month London Interbank Offered Rate (LIBOR), based on Japanese Yen ((JPY3MTD156N)*, Retreived from: https://fred.stlouisfed.org/series/JPY3MTD156N

- **JPY/NZD Exchage Rate**

Reserve Bank of New Zealand, Statistics (2021). *Exchange Rates and TWI-B1 Daily*, Retreived from: https://www.rbnz.govt.nz/statistics/b1
  
 - **NZD Interest Rates**
 
Reserve Bank of New Zealand, Statistics (2021). *Wholesale Interest Rates - B2*, Retreived from: https://www.rbnz.govt.nz/statistics/b2

- **JPY Interst Rates**

Ministry of Finance, Japan, Japanese Government Bonds (2021). *Historical Data*, Retreived from: https://www.mof.go.jp/english/jgbs/reference/interest_rate/index.htm


## How to use Code 

1. Insert path to the CSV Files provided in the "Data" Folder
2. Run Code for Data Cleaning and definition of Functions

3. Calculate Implied Forward Rates for a Fixed Period / Financing with an equivalent Term Bond 

```
#Avaliable Values: Duration: 1, 2,5,10 (Years), 1, 90 (Days) [Requires to set days = T]
data <- carry(duration, days = F)
```
Output can be generated by running the following functions 
1. Plot Implied Forward Rate and Spot Date at Delivery (at the time the Forward Conctract was entered into or at the Delivery Date)
2. Plot difference between Implied Forward Rate and Spot Date at Delivery or at Trade Entry

``` 
# caption = Plot caption 
# title = Plot Title 
# data = Data Set generated from function carry()
# date = "Delivery" or "Entry" 

plot_rates(caption, title, data, date)
plot_diff(caption, title, data, date)
```
