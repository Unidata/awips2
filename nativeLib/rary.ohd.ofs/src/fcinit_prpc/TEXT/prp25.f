C MEMBER PRP25
C  (from old member FCPRP25)
C
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 10/03/95.15:37:12 BY $WC30KH
C
      SUBROUTINE PRP25(PO)
C
C  SUBROUTINE ... PRP25 ... APRIL, 1982
C
C  PROGRAMMER ... RANDALL P. TETZLOFF, TULSA RFC, FTS 8-736-7121
C
C  PURPOSE ... PRINT THE INFORMATION ABOUT THE OPERATION
C     THAT IS STORED IN THE PO() ARRAY IN AN EASILY
C     READABLE FORMAT
C
C  PROCEDURE ... PRINT TABLE ... CONTENTS OF PO() ARRAY
C     PRINT ACTUAL CONTENTS OF PO() ARRAY
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
C           13          PLOT CRITERIA                 INTEGER
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
c          +13          time series time interval     integer
c       +14,+18         time series description       20 char
C
C     SUBNAM(2) ... 8 CHARACTER NAME OF SUBROUTINE
C
C  DEFINITION OF VARIABLES ...
C     IBGN   ... BEGINNING ARRAY ELEMENT
C     IBGNP2 ... IBGN+2
C     IBGNP5 ... IBGN+5
C     IBUG   ... DEBUG OUTPUT SWITCH
C     IDBALL ... DEBUG ALL OPERATIONS SWITCH
C     IEND   ... ENDING ARRAY ELEMENT
C     IHYD   ... HYDROGRAPH PLOT INDICATOR
C     IN     ... UNIT NUMBER OF CARD READER
C     IODBUG ... UNIT NUMBER OF DEBUG FILE
C     IPR    ... UNIT NUMBER OF LINE PRINTER
C     IPU    ... UNIT NUMBER OF CARD PUNCH
C     ITRACE ... TRACE LEVEL CARRIED IN COMMON
C     JTRACE ... TRACE LEVEL FOR THIS SUBROUTINE
C     NCALC  ... ARRAY ELEMENT COUNTER FOR PO AND W1 ARRAYS
C     NDEBUG ... NUMBER OF OPERATIONS WITH DEBUG OUTPUT
C     NMLIST ... NUMBER OF TIME SERIES TO BE LISTED
C     NOOPER ... OPERATION NUMBER
C     NPLTSZ ... PLOT SIZE OPTION
C     NUMTS  ... NUMBER OF TIME SERIES TO BE READ IN
C     STORGE ... STORAGE LISTING OPTION
C
C
      DIMENSION PO(*)
      DIMENSION SUBNAM(2)
      integer tstime
      character*4 descrp(5),type
      character*8 tsid,font
      character*4 pltopt
C
      COMMON /IONUM/ IN,IPR,IPU
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp25.f,v $
     . $',                                                             '
     .$Id: prp25.f,v 1.3 1996/12/10 20:25:55 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM,NOOPER,IBUG/4HPRP2,4H5   ,25,0/
      DATA BLANK1,CAPS/1H ,1HS/
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
C  PRINT TABLE ... CONTENTS OF PO() ARRAY
C
C
      novrsn=po(1) 
      WRITE(IPR,10) novrsn 
 10   FORMAT(10X,'GENERAL PLOT VARIABLES ... version ',i1,      
     1           ' of PLOT-TUL')     
C
      WRITE(IPR,20) (PO(I),I=1,15)
 20   FORMAT(10X,6HNOVRSN,3X,4HIHYD,5X,5HNEWPG,4X,
     1   6HNPLTSZ,3X,6HNPUNCH,3X,6HMINSCL,
     2   3X,6HINTRVL,3X,6HNCRMNT,3X,5HNUMTS,4X,
     3   6HNMLIST,3X,6HORDSYM,3X,5HCURTM,/,
     4   10X,6(F5.0,4X),
     5   4(F5.0,4X),2(A1,8X),//,
     6   10X,6HNPLCRT,3X,6HNBASE ,3X,6HIDUMMY,/,
     7   6X,3F9.0,/)
C
      IHYD=PO(2)+0.1
      IF(IHYD.EQ.1) GO TO 60
      WRITE(IPR,50) (PO(I),I=16,25)
 50   FORMAT(10X,19HPLOT NAME LABEL ...,/,
     2   10X,10A4,/)
      GO TO 80
C
 60   WRITE(IPR,70) (PO(I),I=16,25)
 70   FORMAT(10X,24HHYDROGRAPH VARIABLES ...,/,
     1   10X,6HNPLSTG,3X,6HNPRCNT,3X,6HFLDSYM,3X,
     2   6HRATLIM,3X,6HRCDMAX,3X,7HRC(1-2),/,
     3   10X,2(F5.0,4X),3(A1,8X),2A4,//,
     4   10X,6HIDUMMY,3X,6HIDUMMY,3X,6HIDUMMY,/,
     5   10X,3(F5.0,4X),/)
C
 80   WRITE(IPR,90) (PO(I),I=26,30)
 90   FORMAT(10X,29HLEFT SIDE COLUMN HEADINGS ...,/,
     1   10X,5A4,/)
C
      NPLTSZ=PO(4)+0.1
      NMLIST=PO(10)+0.1
      IF(NPLTSZ.EQ.51 .AND. NMLIST.GT.3)
     1   WRITE(IPR,100) (PO(I),I=31,43)
 100  FORMAT(10X,30HRIGHT SIDE COLUMN HEADINGS ...,/,
     1   10X,13A4,/)
C
      IF(NPLTSZ.EQ.101) NCALC=31
      IF(NPLTSZ.EQ.51 .AND. NMLIST.LT.4) NCALC=31
      IF(NPLTSZ.EQ.51 .AND. NMLIST.GT.3) NCALC=44
      NUMTS=PO(9)+0.1
      WRITE(IPR,110)
 110  FORMAT(10X,27HTIME SERIES INFORMATION ...,/,
     1   10X,14HTSID(1-2) TYPE,6X,
     2   6HPLTOPT,4X,6HSYMBOL,4X,9HFRMT(1-2),4x,'TSTIME',
     3   4x,'DESCRIPTION',/,
     4   10X,14H========= ====,6X,
     5   6H======,4X,6H======,4X,9H=========,4x,'======',
     6   4x,'==============')
C
      DO 130 II=1,NUMTS
      I=II-1
      IBGN=NCALC+(I*12)
      if (novrsn.eq.2.and.i.gt.0) ibgn=ncalc+(i*18)
c     IBGNP2=IBGN+2
c     IBGNP3=IBGN+3
      STORGE=BLANK1
      tsid=' '
      type=' '
      pltopt=' '
      call umemov(po(ibgn),tsid,2)
      call umemov(po(ibgn+2),type,1)
      call umemov(po(ibgn+5),pltopt,1)  
      call umemov(po(ibgn+7),font,2)
c     IF(PO(IBGNP3).EQ.TCUM) STORGE=CAPS
      if(po(ibgn+3).eq.tcum) storge=caps
c     IBGNP5=IBGN+5
c     IEND=IBGN+8
      do 114 jjj=1,5
         descrp(jjj)=' '
  114 continue
      if (novrsn.ne.2) goto 117
         tstime=po(ibgn+12)
         ix=0
         do 115 iii=ibgn+13,ibgn+17
            ix=ix+1
            call umemov(po(iii),descrp(ix),1)
  115    continue
c
  117 continue
c
      if (novrsn.eq.2) goto 125 
c
c     WRITE(IPR,120) (PO(I),I=IBGN,IBGNP2),(PO(I),I=IBGNP5,IEND),STORGE
      write(ipr,120) tsid,type,pltopt,po(ibgn+6),font,storge
 120  FORMAT(10X,A,2X,2(A4,6X),2x,A1,9X,A,1X,A1)
c
      goto 130
c
  125 write(ipr,126) tsid,type,pltopt,po(ibgn+6),font,storge,
     1               tstime,descrp 
  126 FORMAT(10X,A,2X,2(A4,6X),2x,A1,9X,A,1X,A1,2X,I2,7X,5A4)
C
 130  CONTINUE
C
      RETURN
      END
