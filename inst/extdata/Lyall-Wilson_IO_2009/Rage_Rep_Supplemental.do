version 10
capture log close

/* Replication commands for "Rage Against the Machines" (IO 2009): Supplemental Analyses*/
/* Jason Lyall */
/* January 2009*/

clear
use  "C:\Users\jlyall\Documents\Published\Rage Against the Machines\Rage_RepData.dta"  

ologit wdl mech decade2 decade3 decade4 decade5 decade6 decade7 decade8, cluster(ccode)
ologit wdl mech regime occ lenerpc support ldis lelev power decade2 decade3 decade4 decade5 decade6 decade7 decade8, cluster(ccode)
ologit wdl regime occ lenerpc support ldis lelev power decade2 decade3 decade4 decade5 decade6 decade7 decade8, cluster(ccode)
ologit wdl heli decade5 decade6 decade7 decade8, cluster(ccode)
ologit wdl heli regime occ lenerpc support ldis lelev power decade5 decade6 decade7 decade8, cluster(ccode)
ologit wdl regime occ lenerpc support ldis lelev power decade5 decade6 decade7 decade8, cluster(ccode)
ologit wdl mech regime occ lenerpc support ldis lelev power post1945, cluster(ccode)
ologit wdl  heli regime occ lenerpc support ldis power lelev post1945 , cluster(ccode)
ologit wdl mech regime occ lenerpc support ldis lelev power post1989, cluster(ccode)
ologit wdl  heli regime occ lenerpc support ldis power lelev post1989 , cluster(ccode)
ologit wdl modern support power lelev ldis regime lenerpc coldwar occ if ww2!=1, cluster(ccode)
ologit wdl mech support power lelev ldis regime lenerpc coldwar occ if ww2!=1, cluster(ccode)
ologit wdl modern support power lelev ldis regime lenerpc coldwar occ uk, cluster(ccode)
ologit wdl mech support power lelev ldis regime lenerpc coldwar occ uk, cluster(ccode)
logit win modern regime occ ldis lelev support power lenerpc coldwar, cluster(ccode)
logit win railway regime occ ldis lelev support power lenerpc coldwar, cluster(ccode)
logit win mech regime occ ldis lelev support power lenerpc coldwar, cluster(ccode)
logit win heli regime occ ldis lelev support power lenerpc coldwar numlang ltradegdp, cluster(ccode)
logit defeat modern regime occ ldis lelev support power lenerpc coldwar, cluster(ccode)
logit defeat railway regime occ ldis lelev support power lenerpc coldwar, cluster(ccode)
logit defeat mech regime occ ldis lelev support power lenerpc coldwar, cluster(ccode)
logit defeat heli regime occ ldis lelev support power lenerpc coldwar numlang ltradegdp, cluster(ccode)

log close
exit