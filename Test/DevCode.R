usethis::use_r("TestUI")

usethis::use_r("FunOfTrendAge")
## Tab 1 
dt <- dtRW <- read.csv("Test\\Tdt\\FM.csv")
Nage = 'age'; Nrw = 'MRW' ; Nla = "MAXLA" 
rwA = 1; rwB =-0.01; rwC = 1; rwK = 5 
laA = 1; laB = -0.001; laC = 1; laK = 5

ResReg <- RegData(dt , Nage , Nrw, Nla, 
                 rwA, rwB, rwC, rwK, laA, laB, laC, laK )

