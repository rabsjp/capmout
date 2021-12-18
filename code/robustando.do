*****alternative regression
clear
cd "~/Desktop/jotarepos/split/data/clean_data"
insheet using "splitmarket.csv", clear
drop v1 

destring spre as_a as_b p_a p_b spre_b spre_pa spre_pb sprel_pa sprel_pb, replace force

rename spre spre_a
rename spre_pa sprep_a 
rename spre_pb sprep_b
rename sprel sprel_a
rename sprel_pa sprelp_a
rename sprel_pb sprelp_b

reshape long p q maxbi bi minas as nbi nas spre sprep sprel sprelp fv, i(session subsessionround_number) j(asset) string
encode asset, generate(asset2)

destring asset, replace force 

gen rd=(p/fv)-1

gen rpd = (rd+1)/(rd[_n-1]+1)-1 if asset2==2
bysort session subsessionround_number: egen rpdn = max(rpd)

g ronda = subsessionround_number-15

g destodo = 0
replace destodo = 1 if ronda>5
replace destodo = 1 if subsessionround_number>5 & subsessionround_number<16

g sasset = 0 
replace sasset =1 if asset2==1

g sasset_destodo = sasset*destodo

g qadj = q 
replace qadj = q/2 if ronda>5 & asset2==1
replace qadj = q/2 if subsessionround_number>5 & subsessionround_number<16 & asset2==1

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd destodo sasset sasset_destodo
est store A
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg spre destodo sasset sasset_destodo
est store B
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg sprep destodo sasset sasset_destodo
est store C

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg qadj destodo sasset sasset_destodo
est store D 

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rpdn destodo
est store Z

esttab A Z B C D using tablesplits_2markets.tex, se replace f ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N r2)


g medium = 0 
g late = 0 
replace medium = 1 if ronda>5 & ronda<10
replace medium = 1 if subsessionround_number>5 & subsessionround_number<10

replace late =1 if ronda>9
replace late =1 if subsessionround_number>9 & subsessionround_number<16

g sasset_medium = sasset*medium
g sasset_late = sasset*late

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd medium late sasset sasset_medium sasset_late
est store A2
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg spre medium late sasset sasset_medium sasset_late
est store B2
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg sprep medium late sasset sasset_medium sasset_late
est store C2

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg qadj medium late sasset sasset_medium sasset_late
est store D2

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rpdn medium late 
est store Z2

esttab A2 Z2 B2 C2 D2 using tablesplits_all.tex, se replace f ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N r2)
	
drop if subsessionround_number<16

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd medium late sasset sasset_medium sasset_late
test _cons+medium+sasset+sasset_medium  =0


drop if subsessionround_number<16
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd destodo sasset sasset_destodo
est store A
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg spre destodo sasset sasset_destodo
est store B
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg sprep destodo sasset sasset_destodo
est store C

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg qadj destodo sasset sasset_destodo
est store D 

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rpdn destodo
est store Z

esttab A Z B C D using tablesplits_post.tex, se replace f ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N r2)

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd destodo sasset sasset_destodo
test _cons+destodo+sasset+sasset_destodo  =0
	

	
	
	
*****alternative regression REVERSE

*****alternative regression
clear
cd "~/Desktop/jotarepos/split/data/clean_data"
insheet using "reversemarket.csv", clear
drop v1 

destring spre as_a as_b p_a p_b minas_a spre_b spre_pa sprel spre_pb sprel_pa sprel_pb, replace force

rename spre spre_a
rename spre_pa sprep_a 
rename spre_pb sprep_b
rename sprel sprel_a
rename sprel_pa sprelp_a
rename sprel_pb sprelp_b

reshape long p q maxbi bi minas as nbi nas spre sprep sprel sprelp fv, i(session subsessionround_number) j(asset) string
encode asset, generate(asset2)

destring asset, replace force 


gen rd=(p/fv)-1

gen rpd = (rd+1)/(rd[_n-1]+1)-1 if asset2==2
bysort session subsessionround_number: egen rpdn = max(rpd)

g ronda = subsessionround_number-15

g destodo = 0
replace destodo = 1 if ronda>5
replace destodo = 1 if subsessionround_number>5 & subsessionround_number<16

g sasset = 0 
replace sasset =1 if asset2==1

g sasset_destodo = sasset*destodo

g qadj = q 
replace qadj = q/2 if ronda>5 & asset2==1
replace qadj = q/2 if subsessionround_number>5 & subsessionround_number<16 & asset2==1

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd destodo sasset sasset_destodo
est store A
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg spre destodo sasset sasset_destodo
est store B
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg sprep destodo sasset sasset_destodo
est store C

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg qadj destodo sasset sasset_destodo
est store D 

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rpdn destodo
est store Z

esttab A Z B C D using tablesreverse_2markets.tex, se replace f ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N r2)


g medium = 0 
g late = 0 
replace medium = 1 if ronda>5 & ronda<10
replace medium = 1 if subsessionround_number>5 & subsessionround_number<10

replace late =1 if ronda>9
replace late =1 if subsessionround_number>9 & subsessionround_number<16

g sasset_medium = sasset*medium
g sasset_late = sasset*late

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd medium late sasset sasset_medium sasset_late
est store A2
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg spre medium late sasset sasset_medium sasset_late
est store B2
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg sprep medium late sasset sasset_medium sasset_late
est store C2

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg qadj medium late sasset sasset_medium sasset_late
est store D2

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rpdn medium late 
est store Z2

esttab A2 Z2 B2 C2 D2 using tablesreverse_all.tex, se replace f ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N r2)


	
	

drop if subsessionround_number<16
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd destodo sasset sasset_destodo
est store A
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg spre destodo sasset sasset_destodo
est store B
bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg sprep destodo sasset sasset_destodo
est store C

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg qadj destodo sasset sasset_destodo
est store D 

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rpdn destodo
est store Z

esttab A Z B C D using tablesreverse_post.tex, se replace f ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N r2)

bootstrap, cluster(session) idcluster(nuevo) seed(200) rep(200): reg rd destodo sasset sasset_destodo
test _cons+destodo+sasset+sasset_destodo  =0
	
	
	
	
	
	