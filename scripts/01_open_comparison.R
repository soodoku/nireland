##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++
##   Northern Ireland
##   Open-Ended
##   Last Edited: 1.18.13   
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~--++

# Set Working dir.
setwd(githubdir)
setwd("ireland")

## Load Data 
load("data/nireland.rdata")

##Text - Reasons response (compared t2 and t3 control group)
#Two comparisons can be done -
#T3 between participants and control group; T2 between participants, and t3 control group

ireland_4 <- subset(nireland, !is.na(attend) & nireland$attend ==1 | control==1) #Participants and Control group
ireland_4$t2q18a1

########Text of open-ended questions being dumped#######
#################################################
#Open-Ended
#########################

tab0 <- with(ireland_4, data.frame(cserial, t2q18a1, t2q18a2, t2q18a3, t2q18a4, t2q18a5, t2q18anu, 
								   t3q18a1, t3q18a2, t3q18a3, t3q18a4, t3q18a5, t3q18anu, t2q18b1, t2q18b2, 
								   t2q18b3, t2q18b4, t2q18b5, t2q18bnu, t3q18b1, t3q18b2, t3q18b3, t3q18b4, 
								   t3q18b5, t3q18bnu, t2q19a1, t2q19a2, t2q19a3, t2q19a4, t2q19a5, t2q19anu, 
								   t3q19a1, t3q19a2, t3q19a3, t3q19a4, t3q19a5, t3q19anu, t2q19b1, t2q19b2, 
								   t2q19b3, t2q19b4, t2q19b5, t2q19bnu, t3q19b1, t3q19b2, t3q19b3, t3q19b4, 
								   t3q19b5, t3q19bnu, t2q20a1, t2q20a2, t2q20a3, t2q20a4, t2q20a5, t2q20anu, 
								   t3q20a1, t3q20a2, t3q20a3, t3q20a4, t3q20a5, t3q20anu, t2q20b1, t2q20b2, 
								   t2q20b3, t2q20b4, t2q20b5, t2q20bnu,t3q20b1, t3q20b2, t3q20b3, t3q20b4, 
								   t3q20b5, t3q20bnu, t2q21a1, t2q21a2, t2q21a3, t2q21a4, t2q21a5, t2q21anu, 
								   t3q21a1, t3q21a2, t3q21a3, t3q21a4, t3q21a5, t3q21anu, t2q21b1, t2q21b2, 
								   t2q21b3, t2q21b4, t2q21b5, t2q21bnu, t3q21b1, t3q21b2, t3q21b3, t3q21b4, 
								   t3q21b5, t3q21bnu))
write.table(tab0, file="data/open_ended/coding.open.ended.csv", eol = "\n", na = "NA", row.names = FALSE, 
				  sep = ",", col.names = TRUE)


