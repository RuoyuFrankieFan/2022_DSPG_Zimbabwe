clear all
set more off

import delimited "ZimDataReg.csv"
sort district_id year


**variable labels**
lab var yeard "Year(=1)"
lab var m0_k3 "M0"
lab var m1_k3 "M1"
lab var m2_k3 "M2"
lab var maxevi "Max EVI"
lab var rainnovmay "Total Rainfall Nov-May (100mm)"
lab var rainnovdec "Total Rainfall Nov-Dec (100 mm)"
lab var rainnovjan "Total Rainfall Nov-Jan (100 mm)"
lab var rainnovfeb "Total Rainfall Nov-Feb (100 mm)"
lab var rainnovmar "Total Rainfall Nov-Mar (100 mm)"
lab var rainnovapr "Total Rainfall Nov-Apr (100 mm)"
lab var sum10_20 "Dry Spell: 10-20 days Nov-May"
lab var sum20p "Dry Spell: > 20 days Nov-May"
lab var dryspells "Dry Spells: >= 10 days Nov-May"
lab var surfacemoisture "Avg. Surface Soil Moisture (mm) Nov-May"
lab var sum10_20_novdec "Dry Spell: 10-20 days, Nov-Dec"
lab var sum10_20_novjan "Dry Spell: 10-20 days, Nov-Jan"
lab var sum10_20_novfeb "Dry Spell: 10-20 days, Nov-Feb"
lab var sum20p_novdec "Dry Spell: > 20 days, Nov-Dec"
lab var sum20p_novjan "Dry Spell: > 20 days, Nov-Jan"
lab var sum20p_novfeb "Dry Spell: > 20 days, Nov-Feb"


//EVI//

reg m0_k3 yeard maxevi
reg m1_k3 yeard maxevi 
reg m2_k3 yeard maxevi

reg g0_edu_max_k3 yeard maxevi
reg g0_edu_dropout yeard maxevi
reg g0_hea_chronic yeard maxevi 
reg g0_hea_visit_k3 yeard maxevi 
reg g0_assets_k3 yeard maxevi 
reg g0_services_k3 yeard maxevi


//Rainfall and Dry Spells//

*total seasonal rainfall*
reg m0_k3 yeard rainnovmay
reg m1_k3 yeard rainnovmay 
reg m2_k3 yeard rainnovmay 

reg m0_k3 yeard sum10_20 sum20p 
reg m1_k3 yeard sum10_20 sum20p
reg m2_k3 yeard sum10_20 sum20p

reg g0_edu_max_k3 yeard rainnovmay
reg g0_edu_dropout yeard rainnovmay 
reg g0_hea_chronic yeard rainnovmay
reg g0_hea_visit_k3 yeard rainnovmay
reg g0_assets_k3 yeard rainnovmay 
reg g0_services_k3 yeard rainnovmay

reg g0_edu_max_k3 yeard sum10_20 sum20p 
reg g0_edu_dropout yeard sum10_20 sum20p 
reg g0_hea_chronic yeard sum10_20 sum20p 
reg g0_hea_visit_k3 yeard sum10_20 sum20p 
reg g0_assets_k3 yeard sum10_20 sum20p 
reg g0_services_k3 yeard sum10_20 sum20p


reg m0_k3 yeard rainnovdec
reg m1_k3 yeard rainnovdec
reg m2_k3 yeard rainnovdec

reg m0_k3 yeard rainnovjan
reg m1_k3 yeard rainnovjan
reg m2_k3 yeard rainnovjan

reg m0_k3 yeard rainnovfeb
reg m1_k3 yeard rainnovfeb
reg m2_k3 yeard rainnovfeb

reg g0_edu_max_k3 yeard rainnovdec 
reg g0_edu_dropout yeard rainnovdec
reg g0_hea_chronic yeard rainnovdec
reg g0_hea_visit_k3 yeard rainnovdec
reg g0_assets_k3 yeard rainnovdec
reg g0_services_k3 yeard rainnovdec

reg g0_edu_max_k3 yeard rainnovjan
reg g0_edu_dropout yeard rainnovjan
reg g0_hea_chronic yeard rainnovjan
reg g0_hea_visit_k3 yeard rainnovjan
reg g0_assets_k3 yeard rainnovjan
reg g0_services_k3 yeard rainnovjan

reg g0_edu_max_k3 yeard rainnovfeb
reg g0_edu_dropout yeard rainnovfeb
reg g0_hea_chronic yeard rainnovfeb
reg g0_hea_visit_k3 yeard rainnovfeb
reg g0_assets_k3 yeard rainnovfeb
reg g0_services_k3 yeard rainnovfeb

reg m0_k3 yeard sum10_20_novdec 
reg m1_k3 yeard sum10_20_novdec 
reg m2_k3 yeard sum10_20_novdec 

reg m0_k3 yeard sum10_20_novjan
reg m1_k3 yeard sum10_20_novjan 
reg m2_k3 yeard sum10_20_novjan  

reg m0_k3 yeard sum10_20_novfeb
reg m1_k3 yeard sum10_20_novfeb 
reg m2_k3 yeard sum10_20_novfeb  

reg g0_edu_max_k3 yeard sum10_20_novdec 
reg g0_edu_dropout yeard sum10_20_novdec 
reg g0_hea_chronic yeard sum10_20_novdec 
reg g0_hea_visit_k3 yeard sum10_20_novdec 
reg g0_assets_k3 yeard sum10_20_novdec 
reg g0_services_k3 yeard sum10_20_novdec 

reg g0_edu_max_k3 yeard sum10_20_novjan 
reg g0_edu_dropout yeard sum10_20_novjan 
reg g0_hea_chronic yeard sum10_20_novjan 
reg g0_hea_visit_k3 yeard sum10_20_novjan 
reg g0_assets_k3 yeard sum10_20_novjan
reg g0_services_k3 yeard sum10_20_novjan 

reg g0_edu_max_k3 yeard sum10_20_novfeb
reg g0_edu_dropout yeard sum10_20_novfeb 
reg g0_hea_chronic yeard sum10_20_novfeb 
reg g0_hea_visit_k3 yeard sum10_20_novfeb 
reg g0_assets_k3 yeard sum10_20_novfeb 
reg g0_services_k3 yeard sum10_20_novfeb 

//Soil Moisture//

reg m0_k3 surfacemoisture 
reg m1_k3 surfacemoisture
reg m2_k3 surfacemoisture

reg g0_edu_max_k3 surfacemoisture
reg g0_edu_dropout surfacemoisture 
reg g0_hea_chronic surfacemoisture 
reg g0_hea_visit_k3 surfacemoisture
reg g0_assets_k3 surfacemoisture
reg g0_services_k3 surfacemoisture

