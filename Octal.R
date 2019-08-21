
##convert to octal number

num <- as.integer(readline(prompt='Enter value : '))
tmp.dig<-NULL
tmp.num<-num
nextdigit<-NULL
while (tmp.num != 0) {nextdigit<-tmp.num %% 8;
                      tmp.dig<-c(nextdigit,tmp.dig);
                      tmp.num<-floor(tmp.num/8);
                      }

octal<-paste(num,'in Octal numbers is: ',paste0(tmp.dig,collapse=" "))
octal

