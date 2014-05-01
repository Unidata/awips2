C MEMBER PUC25
C  (from old member FCPUC25)
C
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 10/03/95.15:37:54 BY $WC30KH
C
      SUBROUTINE PUC25 (PO)
C
C  SUBROUTINE ... PUC25 ... APRIL, 1982
C
C  PROGRAMMER ... RANDALL P. TETZLOFF, TULSA RFC, FTS 8-736-7121
C
C  PURPOSE ... PRINT THE INFORMATION ABOUT THE OPERATION
C     THAT IS STORED IN THE PO() ARRAY IN AN EASILY
C     READABLE FORMAT AND PUNCH THE INPUT CARDS TO THE OPERATION
C
C  PROCEDURE ... PRINT TABLE ... CONTENTS OF PO() ARRAY
C     PRINT ACTUAL CONTENTS OF PO() ARRAY
C     PUNCH THE INPUT CARDS FOR THE PIN25 OPERATION
C
C  DEFINITION OF FILES ...
C     IN     ... CARD READER  ... //FT05F001  DD  *
C     IPR    ... LINE PRINTER ... //FT06F001  DD  SYSOUT=A
C     IPU    ... CARD PUNCH   ... //FT07F001  DD  SYSOUT=B
C     IODBUG ... LINE PRINTER ... //FT08F001  DD  SYSOUT=A
C
C  DEFINITION OF CALLED SUBROUTINES ...
C     FPRBUG ... CHECKS TRACE LEVEL AND SETS THE
C                DEBUG OUTPUT SWITCH
C
C  DEFINITION OF ARRAYS ...
C     IDEBUG(20) ... LIST OF OPERATION NUMBERS TO PRODUCE
C                    DEBUG PRINTOUT
C     PO(1)     ... USED TO STORE ALL THE PARAMETERS, TIME
C                   SERIES IDENTIFICATION, OPTIONS, TITLES, ETC.
C        CONTENTS OF PO() ARRAY ... PIN 25 ... PLOT OPERATION
C
C      POSITION         CONTENTS                      FORM
C     **********       **********                    ******
C            1          VERSION NUMBER                INTEGER
C            2          IS PLOT A HYDROGRAPH-0/1/9    INTEGER
C            3          TOP OF PAGE OPTION-0/1        INTEGER
C            4          PLOT SIZE OPTION-51/101       INTEGER
C            5          PUNCH STREAM OF PLOT-0/1/2    INTEGER
C            6          MINIMUM SCALE (INCREMENT)     INTEGER
C            7          TIME INTERVAL OF TIME SERIES  INTEGER
C            8          PREFERRED TIME INCREMENT      INTEGER
C            9          TOTAL NO. OF TIME SERIES      INTEGER
C           10          NO. OF TIME SERIES TO LIST    INTEGER
C           11          ORDINATE PLOTTING SYMBOL      1 CHAR
C           12          CURRENT TIME PLOTTING SYMBOL  1 CHAR
C           13          PLOT CRITERIA IF NO
C                             RATING CURVE            INTEGER
C           14          PLOT BASE VALUE               INTEGER
C           15          DUMMY                         INTEGER
C
C        16-25          PLOT NAME LABEL               40 CHAR
C                       ... OR ...
C           16          PLOT STAGE                    INTEGER
C           17          PER CENT OF FLOOD FLOW        INTEGER
C           18          FLOOD FLOW PLOTTING SYMBOL    1 CHAR
C           19          RATING UP LIMIT PLOT SYMBOL   1 CHAR
C           20          MAX OF RECORD PLOTTING SYMBOL 1 CHAR
C        21-22          RATING CURVE IDENTIFIER       8 CHAR
C           23          DUMMY                         INTEGER
C           24          DUMMY                         INTEGER
C           25          DUMMY                         INTEGER
C
C        26-30          LEFT SIDE COLUMN HEADING      20 CHAR
C                       ... OR ...
C        26-30          LEFT SIDE COLUMN HEADING      20 CHAR
C        31-43          RIGHT SIDE COLUMN HEADING     52 CHAR
C
C     FOR EACH TIME SERIES ...
C        +1,+2          TIME SERIES IDENTIFIER        8 CHAR
C           +3          TYPE CODE                     4 CHAR
C           +4          STANDARD METRIC UNITS         4 CHAR
C           +5          DIMENSION OF UNITS            4 CHAR
C           +6          LIST/PLOT/BOTH OPTION         4 CHAR
C           +7          PLOTTING SYMBOL               1 CHAR
C        +8,+9          LISTING FORMAT-REAL           8 CHAR
C          +10          CONVERSION FACTOR             REAL
C          +11          ADDITION CONSTANT             REAL
C          +12          STANDARD ENGLISH UNITS        4 CHAR
C
C     RC(2)     ... 8 CHARACTER IDENTIFIER OF RATING CURVE
C     SUBNAM(2) ... 8 CHARACTER NAME OF SUBROUTINE
C
C  DEFINITION OF VARIABLES ...
C     CURTM  ... ALPHA 1 CHARACTER CURRENT TIME PLOTTING SYMBOL
C     FLDSYM ... ALPHA 1 CHARACTER FOR FLOOD FLOW
C     IBGN   ... BEGINNING ARRAY ELEMENT
C     IBGNP1 ... ARRAY ELEMENT POINTER
C     IBGNP2 ... ARRAY ELEMENT POINTER
C     IBGNP5 ... ARRAY ELEMENT POINTER
C     IBGNP6 ... ARRAY ELEMENT POINTER
C     IBGNP7 ... ARRAY ELEMENT POINTER
C     IBGNP8 ... ARRAY ELEMENT POINTER
C     IBUG   ... DEBUG OUTPUT SWITCH
C     IDBALL ... DEBUG ALL OPERATIONS SWITCH
C     IDUMMY ... DUMMY VARIABLE
C     IHYD   ... HYDROGRAPH PLOT INDICATOR
C     IN     ... UNIT NUMBER OF CARD READER
C     INTRVL ... TIME INTERVAL IN HOURS FOR THE TIME SERIES
C     IODBUG ... UNIT NUMBER OF DEBUG FILE
C     IPR    ... UNIT NUMBER OF LINE PRINTER
C     IPU    ... UNIT NUMBER OF CARD PUNCH
C     ITRACE ... TRACE LEVEL CARRIED IN COMMON
C     JTRACE ... TRACE LEVEL FOR THIS SUBROUTINE
C     MINSCL ... MINIMUM SCALE (INCREMENT)
C     NBASE  ... PLOT BASE VALUE
C     NCALC  ... ARRAY ELEMENT COUNTER FOR PO AND W1 ARRAYS
C     NCRMNT ... PREFERRED TIME INCREMENT TO PLOT
C     NDEBUG ... NUMBER OF OPERATIONS WITH DEBUG OUTPUT
C     NEWPG  ... TOP OF PAGE OPTION
C     NMLIST ... NUMBER OF TIME SERIES TO BE LISTED
C     NOOPER ... OPERATION NUMBER
C     NPLCRT ... PLOT CRITERIA IF NO RATING CURVE DEFINED
C     NPLSTG ... PLOT STAGE
C     NPLTSZ ... PLOT SIZE OPTION
C     NPRCNT ... PER CENT OF FLOOD FLOW
C     NPUNCH ... PUNCH STREAM OF PLOT OPTION
C     NUMTS  ... NUMBER OF TIME SERIES TO BE READ IN
C     ORDSYM ... ALPHA 1 CHARACTER ORDINATE PLOTTING SYMBOL
C     RATLIM ... ALPHA 1 CHARACTER RATING UPPER LIMIT SYMBOL
C     RCDMAX ... ALPHA 1 CHARACTER MAX OF RECORD PLOTTING SYMBOL
C     STORGE ... ALPHA 1 CHARACTER STORAGE LISTING OPTION
C
C
C  *********************************************************************
C
C  SUMMARY OF CARD INPUT
C
C  CARD 1 ...
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      IHYD         2   IS PLOT A HYDROGRAPH-0/1/9    INTEGER    I5
C      NEWPG        3   TOP OF PAGE OPTION-0/1        INTEGER    I5
C      NPLTSZ       4   PLOT SIZE OPTION-51/101       INTEGER    I5
C      NPUNCH       5   PUNCH STREAM OF PLOT-0/1/2    INTEGER    I5
C      MINSCL       6   MINIMUM SCALE (INCREMENT)     INTEGER    I5
C      INTRVL       7   TIME INTERVAL OF TIME SERIES  INTEGER    I5
C      NCRMNT       8   PREFERRED TIME INCREMENT      INTEGER    I5
C      NUMTS        9   TOTAL NO. OF TIME SERIES      INTEGER    I5
C      NMLIST      10   NO. OF TIME SERIES TO LIST    INTEGER    I5
C      ORDSYM      11   ORDINATE PLOTTING SYMBOL      1 CHAR     4X,A1
C      CURTM       12   CURRENT TIME PLOTTING SYMBOL  1 CHAR     4X,A1
C      NPLCRT      13   PLOT CRITERIA-NO RATING CURVE INTEGER    I5
C      NBASE       14   PLOT BASE VALUE               INTEGER    I5
C      IDUMMY      15   DUMMY                         INTEGER    I5
C
C  CARD 2 ... (IF IHYD.EQ.0 .OR. IHYD.EQ.9)
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      PO(16-25) 16-25  PLOT NAME LABEL               40 CHAR    10A4
C
C  CARD 2 ... (IF IHYD.EQ.1)
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      NPLSTG      16   PLOT STAGE                    INTEGER    I5
C      NPRCNT      17   PER CENT OF FLOOD FLOW        INTEGER    I5
C      FLDSYM      18   FLOOD FLOW PLOTTING SYMBOL    1 CHAR     4X,A1
C      RATLIM      19   RATING UP LIMIT PLOT SYMBOL   1 CHAR     4X,A1
C      RCDMAX      20   MAX OF RECORD PLOTTING SYMBOL 1 CHAR     4X,A1
C      RC(1-2)   21-22  RATING CURVE IDENTIFIER       8 CHAR     2X,2A4
C      IDUMMY      23   DUMMY                         INTEGER    I5
C      IDUMMY      24   DUMMY                         INTEGER    I5
C      IDUMMY      25   DUMMY                         INTEGER    I5
C
C  CARD 3 ... (IF NPLTSZ.EQ.101 .OR. IF NPLTSZ.EQ.51 WITH NMLIST.LT.4)
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      PO(26-30) 26-30  LEFT SIDE COLUMN HEADING      20 CHAR    5A4
C
C  CARD 3 ... (IF NPLTSZ.EQ.51 .AND. NMLIST.GT.3)
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      PO(26-30) 26-30  LEFT SIDE COLUMN HEADING      20 CHAR    5A4
C      PO(31-43) 31-43  RIGHT SIDE COLUMN HEADING     52 CHAR    13A4
C
C  CARD 4 TO CARD (NUMTS+3) ... 1 CARD FOR EACH TIME SERIES ...
C     FOR EACH TIME SERIES ...
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      TSID(1-2) +1,+2  TIME SERIES IDENTIFIER        8 CHAR     2A4,2X
C      TYPE        +3   TYPE CODE                     4 CHAR     A4,1X
C      PLTOPT      +6   LIST/PLOT/BOTH OPTION         4 CHAR     A4,1X
C      SYMBOL      +7   PLOTTING SYMBOL               1 CHAR     A1,4X
C      FRMT(1-2) +8,+9  LISTING FORMAT-REAL           8 CHAR     2A4,2X
C
C  CARD FINAL ...
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      TSID(1)          9999                          4 CHAR     A4
C
C  *********************************************************************
C
C
      DIMENSION PO(*)
      DIMENSION SUBNAM(2),RC(2)
      character*4 descrp(5)
C
      COMMON /IONUM/ IN,IPR,IPU
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc25.f,v $
     . $',                                                             '
     .$Id: puc25.f,v 1.4 1997/04/06 12:49:42 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM,NOOPER,IBUG/4HPUC2,4H5   ,25,0/
      DATA CAPS,BLANK1/1HS,1H /
      DATA TCUM/4HTCUM/
C
C
C  CALL SUBROUTINE, FPRBUG, TO CHECK TRACE LEVEL AND DEBUG OPTION
C     JTRACE = TRACE LEVEL FOR SUBROUTINE = 1
C     IBUG   = DEBUG SWITCH = 0
C
      JTRACE=1
      CALL FPRBUG (SUBNAM,JTRACE,NOOPER,IBUG)
C
C  PUNCH THE INPUT CARDS FOR THE PIN25 OPERATION
C
C  *********************************************************************
C
C  CARD 1 ...
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      IHYD         2   IS PLOT A HYDROGRAPH-0/1/9    INTEGER    I5
C      NEWPG        3   TOP OF PAGE OPTION-0/1        INTEGER    I5
C                       IF 0, WILL NOT ADVANCE TO
C                       TOP OF PAGE
C      NPLTSZ       4   PLOT SIZE OPTION-51/101       INTEGER    I5
C      NPUNCH       5   PUNCH STREAM OF PLOT-0/1/2    INTEGER    I5
C                       IF 0 ... NO PUNCH STREAM
C                       IF 1 ... PUNCH STREAM ONLY
C                       IF 2 ... PUNCH AND PRINT STREAM
C      MINSCL       6   MINIMUM SCALE (INCREMENT)     INTEGER    I5
C      INTRVL       7   TIME INTERVAL OF TIME SERIES  INTEGER    I5
C                          1,2,3,4,6,8,12,24
C      NCRMNT       8   PREFERRED TIME INCREMENT      INTEGER    I5
C      NUMTS        9   TOTAL NO. OF TIME SERIES      INTEGER    I5
C      NMLIST      10   NO. OF TIME SERIES TO LIST    INTEGER    I5
C      ORDSYM      11   ORDINATE PLOTTING SYMBOL      1 CHAR     4X,A1
C                       IF BLANK, WILL USE NONE
C      CURTM       12   CURRENT TIME PLOTTING SYMBOL  1 CHAR     4X,A1
C                       IF BLANK, WILL USE NONE
C      NPLCRT      13   PLOT CRITERIA-NO RATING CURVE INTEGER    I5
C      NBASE       14   PLOT BASE VALUE               INTEGER    I5
C      IDUMMY      15   DUMMY                         INTEGER    I5
C
      IHYD   = PO(2)
      NEWPG  = PO(3)
      NPLTSZ = PO(4)
      NPUNCH = PO(5)
      MINSCL = PO(6)
      IF(MINSCL.GT.99998) MINSCL=99999
      INTRVL = PO(7)
      NCRMNT = PO(8)
      NUMTS  = PO(9)
      NMLIST = PO(10)
      ORDSYM = PO(11)
      CURTM  = PO(12)
      NPLCRT = PO(13)
      IF(NPLCRT.GT.99998) NPLCRT=99999
      NBASE = PO(14)
      IF(NBASE.GT.99998) NBASE=99999
      IDUMMY=PO(15)
C
      WRITE(IPU,10) IHYD,NEWPG,NPLTSZ,NPUNCH,MINSCL,INTRVL,
     1   NCRMNT,NUMTS,NMLIST,ORDSYM,CURTM,NPLCRT,NBASE,IDUMMY
 10   FORMAT(9I5,2(4X,A1),3I5)
C
C
C  *********************************************************************
C
C  CARD 2 ... (IF IHYD.EQ.0 .OR. IHYD.EQ.9)
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      PO(16-25) 16-25  PLOT NAME LABEL               40 CHAR    10A4
C
      IF(IHYD.EQ.1) GO TO 30
      WRITE(IPU,20) (PO(I),I=16,25)
 20   FORMAT(10A4)
      GO TO 50
 30   CONTINUE
C
C  CARD 2 ... (IF IHYD.EQ.1)
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      NPLSTG      16   PLOT STAGE                    INTEGER    I5
C      NPRCNT      17   PER CENT OF FLOOD FLOW        INTEGER    I5
C      FLDSYM      18   FLOOD FLOW PLOTTING SYMBOL    1 CHAR     4X,A1
C                       IF BLANK, WILL USE NONE
C      RATLIM      19   RATING UP LIMIT PLOT SYMBOL   1 CHAR     4X,A1
C                       IF BLANK, WILL USE NONE
C      RCDMAX      20   MAX OF RECORD PLOTTING SYMBOL 1 CHAR     4X,A1
C                       IF BLANK, WILL USE NONE
C      RC(1-2)   21-22  RATING CURVE IDENTIFIER       8 CHAR     2X,2A4
C      IDUMMY      23   DUMMY                         INTEGER    I5
C      IDUMMY      24   DUMMY                         INTEGER    I5
C      IDUMMY      25   DUMMY                         INTEGER    I5
C
      IF(IHYD.NE.1) GO TO 50
C
      NPLSTG = PO(16)
      NPRCNT = PO(17)
      FLDSYM = PO(18)
      RATLIM = PO(19)
      RCDMAX = PO(20)
      RC(1)  = PO(21)
      RC(2)  = PO(22)
      IDUMMY = PO(23)
C
      WRITE(IPU,40) NPLSTG,NPRCNT,FLDSYM,RATLIM,RCDMAX,
     1   RC(1),RC(2),IDUMMY,IDUMMY,IDUMMY
 40   FORMAT(2I5,3(4X,A1),2X,2A4,3I5)
 50   CONTINUE
C
C
C  *********************************************************************
C
C  CARD 3 ... (IF NPLTSZ.EQ.101 .OR. IF NPLTSZ.EQ.51 WITH NMLIST.LT.4)
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      PO(26-30) 26-30  LEFT SIDE COLUMN HEADING      20 CHAR    5A4
C
C  CARD 3 ... (IF NPLTSZ.EQ.51 .AND. NMLIST.GT.3)
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      PO(26-30) 26-30  LEFT SIDE COLUMN HEADING      20 CHAR    5A4
C      PO(31-43) 31-43  RIGHT SIDE COLUMN HEADING     52 CHAR    13A4
C
C
      IF(NPLTSZ.EQ.101) WRITE(IPU,60) (PO(I),I=26,30)
 60   FORMAT(18A4)
      IF(NPLTSZ.EQ.51 .AND. NMLIST.le.3)
     1   WRITE(IPU,60) (PO(I),I=26,30)
      IF(NPLTSZ.EQ.51 .AND. NMLIST.GT.3)
     1   WRITE(IPU,60) (PO(I),I=26,43)
C
C
C  *********************************************************************
C
C  CARD 4 TO CARD (NUMTS+3) ... 1 CARD FOR EACH TIME SERIES ...
C     FOR EACH TIME SERIES ...
C
C      VARIABLE   PO()  CONTENTS                      FORM       FORMAT
C     ********** ***** **********                    ******     ********
C      TSID(1-2) +1,+2  TIME SERIES IDENTIFIER        8 CHAR     2A4,2X
C      TYPE        +3   TYPE CODE                     4 CHAR     A4,1X
C      PLTOPT      +6   LIST/PLOT/BOTH OPTION         4 CHAR     A4,1X
C      SYMBOL      +7   PLOTTING SYMBOL               1 CHAR     A1,4X
C      FRMT(1-2) +8,+9  LISTING FORMAT-REAL           8 CHAR     2A4,2X
c      tstime    +13    time series time interval     integer    i2,1x
c      descrp  +14,+18  time series description       20 char    5a4
C
      IF(NPLTSZ.EQ.101) NCALC=31
      IF(NPLTSZ.EQ.51 .AND. NMLIST.LT.4) NCALC=31
      IF(NPLTSZ.EQ.51 .AND. NMLIST.GT.3) NCALC=44
C
      novrsn=po(1)
      DO 80 II=1,NUMTS
      I=II-1
      IBGN=NCALC+(I*12)
      if(novrsn.eq.2.and.i.gt.0) ibgn=ncalc+(i*18)
      IBGNP1=IBGN+1
      IBGNP2=IBGN+2
      IBGNP3=IBGN+3
      STORGE=BLANK1
      IF(PO(IBGNP3).EQ.TCUM) STORGE=CAPS
      IBGNP5=IBGN+5
      IBGNP6=IBGN+6
      IBGNP7=IBGN+7
      IBGNP8=IBGN+8
      if(novrsn.eq.2) goto 65
      WRITE(IPU,70) PO(IBGN),PO(IBGNP1),PO(IBGNP2),PO(IBGNP5),
     1              PO(IBGNP6),PO(IBGNP7),PO(IBGNP8),STORGE
      goto 80
 65   continue
      itime=po(ibgn+12)
      do 66 iii=1,5
         descrp(iii)=' '
 66   continue
      call umemov(po(ibgn+13),descrp(1),5)
      WRITE(IPU,70) PO(IBGN),PO(IBGNP1),PO(IBGNP2),PO(IBGNP5),
     1              PO(IBGNP6),PO(IBGNP7),PO(IBGNP8),STORGE,
     2              itime,descrp
 70   FORMAT(2A4,2X,2(A4,1X),A1,4X,2A4,1X,A1,1x,i2,1x,5a4)
C
 80   CONTINUE
C
C  *********************************************************************
C
C  LAST CARD ...
C
      WRITE(IPU,90)
 90   FORMAT(4HEND )
C
      RETURN
      END
