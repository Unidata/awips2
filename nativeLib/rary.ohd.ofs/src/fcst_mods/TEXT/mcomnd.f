C MODULE MCOMND
C-----------------------------------------------------------------------
C
      SUBROUTINE MCOMND (ICMND,CMDNAM,ISFG,NDTS)
C
C  THIS SUBROUTINE RELATES A MOD COMMAND NAME AND INTERNALLY USED
C  MOD NUMBER.  IT ALSO FILLS THE ISFG AND NDTS VARIABLES WHICH
C  TELL IF THE MOD CAN BE SPECIFIED BY FORECAST GROUP AND THE
C  NUMBER OF DATES REQUIRED, RESPECTIVELY.
C
C  IF ICMND LE 0, CMDNAM IS INPUT  AND ICMND IS OUTPUT
C  IF ICMND GT 0, CMDNAM IS OUTPUT AND ICMND IS INPUT
C
      CHARACTER*8 CMDNAM
C
C length of COMAND, ISITFG, and NDATES
      PARAMETER (NUMCMD=46)
C
C  COMMAND NAMES
      CHARACTER*8 COMAND(NUMCMD)
     1 /'AEICQN  ','AESCCHNG','AIADJ   ','APICQN  ','BASEF   ',
     2  'BFRATE  ','BFRCHNG ','CBASEF  ','CBFRATE ','IGNORETS',
     3  'MFC     ','RAINSNOW','ROCHNG  ','ROMULT  ','RRICHNG ',
     4  'RRIMULT ','SACBASEF','SACCO   ','SETMSNG ','SETQMEAN',
     5  'TSADD   ','TSCHNG  ','TSMULT  ','TSREPL  ','UCBASEF ',
     6  'UCBFRATE','UHGADJ  ','UHGCHNG ','WECHNG  ','XINCO   ',
     7  'APICBASF','APICCO  ','WEADD   ','ZERODIFF','CHGBLEND',
     8  'MATCHNG ','QCSHIFT ','QPSHIFT ','BUBLSHFT','SWITCHTS',
cav added 3/17 for new mod
cav      9  'SSARREG ','WEUPDATE','UADJ    '/
     9  'SSARREG ','WEUPDATE','UADJ    ','UHGCDATE','DPRECIP ',
     A  'DSACST  '/
C
      DIMENSION ISITFG(NUMCMD)
      DIMENSION NDATES(NUMCMD)
C
C  INDICATOR IF CAN BE SPECIFIED BY FORECAST GROUP
      DATA ISITFG
     1 /1,1,1,1,0,
     2  1,1,0,1,1,
     3  1,1,1,1,1,
     4  1,1,1,0,0,
     5  0,0,0,0,0,
     6  1,1,0,1,1,
     7  1,1,1,1,1,
     8  1,0,0,0,1,
     9  0,1,1,0,0,
     A  0/
C
C  NUMBER OF DATES REQUIRED
      DATA NDATES
     1 /1,1,1,1,2,
     2  2,1,1,1,2,
     3  2,0,2,3,2,
     4  3,1,1,0,2,
     5  3,2,3,3,1,
     6  1,1,1,1,1,
     7  1,1,1,1,1,
     8  2,2,2,2,3,
     9  1,1,2,3,3,
     A  1/
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mcomnd.f,v $
     . $',                                                             '
     .$Id: mcomnd.f,v 1.7 2004/08/05 18:05:48 wkwock Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMND.LT.1) GO TO 50
C
C  CHECK ICMND
      IF (ICMND.GT.NUMCMD) THEN
         CMDNAM=' '
         ISFG=0
         NDTS=0
         GO TO 90
	 ENDIF
      CMDNAM=COMAND(ICMND)
      ISFG=ISITFG(ICMND)
      NDTS=NDATES(ICMND)
      GO TO 90
C
C  CHECK CMDNAM
 50   DO 60 I=1,NUMCMD
         IF (CMDNAM.EQ.COMAND(I)) THEN
            ICMND=I
            ISFG=ISITFG(I)
            NDTS=NDATES(I)
            GO TO 90
	    ENDIF
 60      CONTINUE
      ICMND=0
      ISFG=0
      NDTS=0
C
 90   RETURN
C
      END
