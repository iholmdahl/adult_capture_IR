library(stats)

# unpublished data used with permission from: 
# "Delayed effects from LLINs on resistant mosquitoes"
# Mafalda Viana, Angela Hughes, Jason Matthiopoulos, Hilary Ranson, Heather M. Ferguson
# Proceedings of the National Academy of Sciences Aug 2016, 113 (32) 8975-8980; DOI: 10.1073/pnas.1603431113

####################    Gompertz-Makeham function: four days, Unexposed

TIA_AU_4 = nls(TIA_AU_surviving~(exp(-L*Day-A/B*(exp(B*Day)-1))),data=four_days, start=list(A=0.005, B=0.3, L=0.02))

coef(TIA_AU_4)

x = 1:30
A = 0.04134322
B = 0.15696705
L = -0.06214465

TIA_AU_4_cdf <- exp(-L*x-A/B*(exp(B*x)-1))
matplot(TIA_AU_4_cdf, type = 'l', main = "Gompertz–Makeham function: TIA A Unexposed")
points(TIA_AU_surviving~Day, data=four_days)




####################    Gompertz-Makeham function: four days, Exposed

TIA_AP_4 = nls(TIA_AP_surviving~(exp(-L*Day-A/B*(exp(B*Day)-1))),data=four_days, start=list(A=0.005, B=0.3, L=0.02))

coef(TIA_AP_4)

x = 1:30
A = 0.05375555
B = 0.17035282
L = -0.07775021

TIA_AP_4_cdf <- exp(-L*x-A/B*(exp(B*x)-1))
matplot(TIA_AP_4_cdf, type = 'l', col = '2', main = "Gompertz–Makeham function: TIA replicate A, 4 days")
points(TIA_AP_surviving~Day, data=four_days, col = '2')

#####################      plot exposed & unexposed together

lines(TIA_AU_4_cdf, type = 'l', col='4')
points(TIA_AU_surviving~Day, data=four_days, col = '4')
legend ("topright", inset = .05, par(xpd = TRUE), legend = c("unexposed", "exposed"), col = c(4,2), lty = 1, pt.lwd = 1, bty = "n")



##################################################################
####
####   Gompertz-Makeham function: four days, replicate B exposed

TIA_BP_4 = nls(TIA_BP_surviving~(exp(-L*Day-A/B*(exp(B*Day)-1))),data=four_days, start=list(A=0.005, B=0.3, L=0.02))
coef(TIA_BP_4)

A = 0.04160651
B = 0.18646977
L = -0.06292353

TIA_BP_4_cdf <- exp(-L*x-A/B*(exp(B*x)-1))
lines(TIA_BP_4_cdf, type = 'l', col = '2', main = "Gompertz–Makeham function: TIA replicate B, 4 days")
points(TIA_BP_surviving~Day, data=four_days, col = '2')


##################################################################
####
####   Gompertz-Makeham function: four days, replicate B unexposed

TIA_BU_4 = nls(TIA_BU_surviving~(exp(-L*Day-A/B*(exp(B*Day)-1))),data=four_days, start=list(A=0.005, B=0.3, L=0.02))
coef(TIA_BU_4)

A = 0.12800172
B = 0.08378319
L = -0.15303731

TIA_BU_4_cdf <- exp(-L*x-A/B*(exp(B*x)-1))
matplot(TIA_BU_4_cdf, type = 'l', col = '4', main = "Gompertz–Makeham: TIA replicate B, 4 days")
points(TIA_BU_surviving~Day, data=four_days, col = '4')

lines(TIA_BP_4_cdf, type = 'l', col='2')
points(TIA_BP_surviving~Day, data=four_days, col = '2')


##################################################################
####
####   Gompertz-Makeham function: four days, Unexposed

TOR_AU_4 = nls(TOR_AU_surviving~(exp(-L*Day-A/B*(exp(B*Day)-1))),data=four_days, start=list(A=0.005, B=0.3, L=0.02))
coef(TOR_AU_4)

x = 1:30
A = 0.10948419
B = 0.07209189
L = -0.12536147

TOR_AU_4_cdf <- exp(-L*x-A/B*(exp(B*x)-1))
matplot(TOR_AU_4_cdf, type = 'l', col = "4",main = "Gompertz–Makeham: TOR A Unexposed")
points(TOR_AU_surviving~Day, data=four_days, col = "4")


## DOESNT WORK ##################    Gompertz-Makeham function: four days, Exposed

#TOR_AU_4 = nls(TOR_AU_surviving~(exp(-L*Day-A/B*(exp(B*Day)-1))),data=four_days, start=list(A=0.3, B=0.15, L=-.35))
#coef(TOR_AP_4)

#x = 1:30
#A = 0.3
#B = 0.15
#L = -.35

#TOR_AP_4_cdf <- exp(-L*x-A/B*(exp(B*x)-1))
#matplot(TOR_AP_4_cdf, type = 'l', col = '2', main = "Gompertz–Makeham function: TOR replicate A, 4 days", ylim = 0:1)
#points(TOR_AP_surviving~Day, data=four_days, col = '2')

#####################      plot exposed & unexposed together

lines(TOR_AU_4_cdf, type = 'l', col='4')
points(TOR_AU_surviving~Day, data=four_days, col = '4')
legend ("topright", inset = .05, par(xpd = TRUE), legend = c("unexposed", "exposed"), col = c(4,2), lty = 1, pt.lwd = 1, bty = "n")



