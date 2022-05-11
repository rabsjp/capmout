clear
cd "~/Desktop/jotarepos/capmout/clean"
insheet using "detf.csv", clear

destring precio bo bb, replace force

g t2 = 0 
replace t2=1 if tre==2

g t3 = 0 
replace t3=1 if tre==3

g a_1 = 0 
replace a_1=1 if activo==1

g a_3 = 0 
replace a_3=1 if activo==3

g a_4 = 0 
replace a_4=1 if activo==4

g a_1_t2 = a_1*t2
g a_3_t2 = a_3*t2
g a_4_t2 = a_4*t2

g a_1_t3 = a_1*t3
g a_3_t3 = a_3*t3
g a_4_t3 = a_4*t3


bootstrap, cluster(sess) idcluster(nuevo) seed(200) rep(100): reg precio  t3 a_1 a_3 a_4  a_1_t3 a_3_t3 a_4_t3



xtset r
bootstrap, cluster(sess) idcluster(nuevo) seed(200) rep(100): xtreg precio  t3 a_1 a_3 a_4  a_1_t3 a_3_t3 a_4_t3, fe



bootstrap, cluster(sess) idcluster(nuevo) seed(200) rep(100): reg precio t2 t3 a_1 a_3 a_4 a_1_t2 a_3_t2 a_4_t2 a_1_t3 a_3_t3 a_4_t3


xtset r
bootstrap, cluster(sess) idcluster(nuevo) seed(200) rep(200): xtreg precio t2 t3 a_1 a_3 a_4 a_1_t2 a_3_t2 a_4_t2 a_1_t3 a_3_t3 a_4_t3, fe

test t2+a_3_t2=0

test a_3_t2=a_3_t3
test a_1+a_1_t3-a_3-a_3_t3=0


xtset r
bootstrap, cluster(sess) idcluster(nuevo) seed(200) rep(200): xtreg z t2 t3 a_1 a_3 a_4 a_1_t2 a_3_t2 a_4_t2 a_1_t3 a_3_t3 a_4_t3, fe
test a_3_t2=a_3_t3
test a_1+a_1_t3-a_3-a_3_t3=0





clear
cd "~/Desktop/jotarepos/capmout/clean"
insheet using "dtrades.csv", clear
g human = 0
replace human =1 if isbot=="FALSE"
g t2 = 0 
replace t2=1 if tre==2

g t3 = 0 
replace t3=1 if tre==3

g a_1 = 0 
replace a_1=1 if asset==1

g a_3 = 0 
replace a_3=1 if asset==3

g a_4 = 0 
replace a_4=1 if asset==4

g a_1_t2 = a_1*t2
g a_3_t2 = a_3*t2
g a_4_t2 = a_4*t2

g a_1_t3 = a_1*t3
g a_3_t3 = a_3*t3
g a_4_t3 = a_4*t3

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(100): reg ex_price t3 a_1 a_3 a_4  a_1_t3 a_3_t3 a_4_t3
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(100): reg ex_price t3 a_1 a_3 a_4  a_1_t3 a_3_t3 a_4_t3 if human==1

xtset r
bootstrap, cluster(sess) idcluster(nuevo) seed(200) rep(100): xtreg ex_price  t3 a_1 a_3 a_4  a_1_t3 a_3_t3 a_4_t3, fe 
preserve
keep if human==1
bootstrap, cluster(sess) idcluster(nuevo) seed(200) rep(100): xtreg ex_price  t3 a_1 a_3 a_4  a_1_t3 a_3_t3 a_4_t3, fe
restore


