version 10
capture log close

/* Replication commands for "Rage Against the Machines: Explaining Outcomes in Counterinsurgency Wars (IO 2009)"*/
/* Jason Lyall */
/* January 2009*/

clear
use "C:\Users\jlyall\Documents\Published\Rage Against the Machines\Rage_RepData.dta"
  
ologit wdl modern, cluster(ccode)
ologit wdl modern  support power lelev ldis regime lenerpc coldwar occ, cluster(ccode)
ologit wdl railway support power lelev ldis regime lenerpc occ if modern==0, cluster(ccode)
ologit wdl support power lelev ldis regime lenerpc coldwar occ if modern==1, cluster(ccode)
ologit wdl mech, cluster(ccode)
ologit wdl mech regime occ lenerpc support lelev ldis power, cluster(ccode)
ologit wdl mech regime occ lenerpc support lelev ldis power  ltradegdp numlang, cluster(ccode)
ologit wdl  heli regime occ lenerpc support lelev ldis power ltradegdp numlang, cluster(ccode)
ologit wdl  modern regime occ lenerpc support lelev ldis power coldwar above below, ro
ologit wdl mech regime occ lenerpc support lelev ldis power coldwar above below, ro
ologit wdl  modern regime occ lenerpc support lelev ldis power coldwar  eeurop lamerica   ssafrica asia nafrme namerica, ro
ologit wdl mech regime occ lenerpc support lelev ldis power coldwar eeurop lamerica  ssafrica asia nafrme namerica, ro
ologit wdl mech support power lelev ldis regime lenerpc coldwar occ nwstate, cluster(ccode)
ologit wdl heli support power lelev ldis regime lenerpc coldwar occ nwstate, cluster(ccode)

log close
exit