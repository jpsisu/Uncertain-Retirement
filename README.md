# Uncertain-Retirement

Decision support tool for military members switching from the traditional retirement system to the blended retirement system.
The files create a shinyapp in R, which runs a stochastic simulation and graphical outputs making a reccomendation to remain in the current system or switch.

Summary of the files:

pay_chart is a csv of the 2016 military pay chart.

helpers.R contains some basic functions to look up pay.

createFinances.R has functions to simulate a 'typical' career of a service member.

app.R is the shinyapp which sets up the reactivity.
