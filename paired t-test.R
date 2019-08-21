a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)

t.test(a,b,paired=T)


qt(0.975, 9) # t-dist of one tail of 95% confidence (95 + 2.5 + 2.5), 9 dof


t.test(a,b,paired=T,alt="greater") #data in a is NOT greater than data in b



-------------------
  
  
  
