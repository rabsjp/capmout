clear
cd "~/Desktop/jotarepos/capmout/"
insheet using "detf.csv", clear

destring precio bo bb, replace force

encode sess, gen(sn)
g s = sn*1000+i*10

g sr = s + r

g t2 = 0 
replace t2=1 if tre==2

g t3 = 0 
replace t3=1 if tre==3

g a_1 = 0 
replace a_1=1 if activo==1

g a_2 = 0 
replace a_2=1 if activo==2

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


g bbc = a_3*bb 

g bob = a_2*bo

bysort sr: egen bc=sum(bbc)

g bod = bc-bob

preserve

keep if i==60
bootstrap, cluster(sess) idcluster(nuevo) seed(100) rep(200): reg bb  t2 t3 a_1 a_3 a_4  a_1_t2 a_3_t2 a_4_t2 a_1_t3 a_3_t3 a_4_t3

test a_3+a_3_t2=0
test a_3+a_3_t3=0

bootstrap, cluster(sess) idcluster(nuevo) seed(100) rep(200): reg bo  t2 t3 a_1 a_3 a_4  a_1_t2 a_3_t2 a_4_t2 a_1_t3 a_3_t3 a_4_t3

test a_3+a_3_t2=0
test a_3+a_3_t3=0


keep if activo==2
bootstrap, cluster(sess) idcluster(nuevo) seed(200) rep(100): reg  bod  t2 t3
test _cons+t2=0
test _cons+t3=0
restore
