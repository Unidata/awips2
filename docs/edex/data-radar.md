
### Level 3 Radar (All)

    NEXRAD3 ^(SDUS[23578].) .... (......) /p(...)(...)
            FILE    -overwrite -close -edex /awips2/data_store/radar/\4/\3/\1_\4_\3_\2_(seq).rad
            
### Level 3 Radar (Subset) 

    NEXRAD3 ^(SDUS[23578].) .... (......) /p(DHR|DPR|DSP|DTA|DAA|DU3|DU6|DVL|EET|HHC|N3P|N0C|N0K|N0Q|N0S|N0U|N0X|N0Z|NCR|NMD|OHA)(...)
            FILE    -overwrite -close -edex /awips2/data_store/radar/\4/\3/\1_\4_\3_\2_(seq).rad

### FNEXRAD Composites

    FNEXRAD ^rad/NEXRCOMP/(...)/(...)_(........)_(....)
            FILE    -close -edex
            /awips2/data_store/sat/nexrcomp_\3\4_\2.gini.png
    
## WSR-88D Localizations


WFO 
