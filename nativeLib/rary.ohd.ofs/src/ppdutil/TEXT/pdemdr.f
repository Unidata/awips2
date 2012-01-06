C MODULE PDEMDR
C***********************************************************************
C                                                                      *
C         MODULE PDEDTMDR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDEMDR(JULDA,INTHR,JULHR,NOHR)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDEMDR                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  9-1-84                                         *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS ROUTINE WILL EDIT MDR VALUES ON THE PPDB FOR A SPECIFIED     *
C    DATE, TIME AND PERIOD.  COORDINATES ARE INPUT WTH THE VALUE SO    *
C    THAT THE MDR BOX NUMBER MAY BE CALCULATED IN ORDER TO EDIT A      *
C    SPECIFIC VALUE.  ONLY ONE PERIOD AT A TIME AND ONLY ONE SINGLE    *
C    VALUE AT A TIME MAY BE EDITED.                                    *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       JULDA      I     I    1      JULIAN DAY OF EDIT                *
C       INTHR      I     I    1      INTERNAL HOUR FROM DATE CONVERSION
C       JULHR      I     I    1      HOUR OF EDIT (Z-TIME)             *
C       NOHR       I     I    1      HOUR FLAG
C                                     0 = NO HOUR ON DATE CARD
C                                     1 = HOUR ON DATE CARD CONVERTED
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pddtdr'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      DIMENSION NCARD(2),LEND(2),IDSTA(2)
      INTEGER*2 IPNTRS(1),DATA(3000),MDATA,MSNG
      REAL ARRAY(1100)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdemdr.f,v $
     . $',                                                             '
     .$Id: pdemdr.f,v 1.2 2000/12/18 22:18:37 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA LEND/4hENDM,4hDR  /,MDR/4hMDR6/,ISTBN/4hSTBN/
C                                                                      *
C***********************************************************************
C
C
C
C  DEBUG
C
      IF(IPDTR.GT.0) WRITE(IOGDB,100)
  100 FORMAT(' ENTER PDEMDR')
C
      JERR = 0
      LPNTRS = 1
      LDATA = 3000
      IPER = 0
      IWRIT = 0
      IPTR = 0
      LARRAY = 1100
C
C  READ IN PPPDB CONTROLS
C
      CALL RPPPCO(ISTAT)
      IF(ISTAT.NE.0) GO TO 950
      CALL UMEMST(IBLNK,IDSTA,2)
C
C  CHECK DATA TYPE
C
      IX = IPDCKD(MDR)
      IF(IX.NE.0) GO TO 125
      WRITE(LPE,110)
  110 FORMAT(' **ERROR** DATA TYPE NOT FOUND ON PPDB')
      GO TO 999
  125 CONTINUE
C
C  CONVERT DATE FOR MESSAGES
C
      CALL MDYH2(JULDA,INTHR,IMO,IDAY,IYR,IHR,ITZ,IDSAV,TIME(3))
C
C  CHECK THAT DATE IS WITHIN RETENTION PERIOD
C
      IDF = IDDTDR(4,IX)
      IF(IDDTDR(8,1).NE.0.OR.IDDTDR(9,1).NE.0) GO TO 150
      WRITE(LPE,130) JULDA
  130 FORMAT(' **WARNING** FILES ARE EMPTY FOR THIS DAY ',I6)
      IWRIT = 1
      GO TO 475
  150 CONTINUE
      CALL UMEMOV(IDDTDR(8,IX),IEDATE,1)
      CALL UMEMOV(IDDTDR(11,IX),LDATE,1)
      IF(JULDA.GE.IEDATE.AND.JULDA.LE.LDATE) GO TO 165
      WRITE(LPE,160)
  160 FORMAT(' **WARNING** DATE IS OUT OF RANGE FOR MDR DATA PRESENTLY',
     1       ' ON FILE.')
      IWRIT = 1
      GO TO 475
  165 CONTINUE
      JHR = MOD(JULHR,24)
      MZHR = MOD(JHR,6)
      IF(MZHR.EQ.0) GO TO 175
      WRITE(LPE,170)
  170 FORMAT(' **ERROR** IN PDEMDR. INVALID HOUR ON DATE CARD. ')
      IWRIT = 1
      GO TO 475
  175 IF(NOHR.EQ.1) JHR = 18
      IF(IPDDB.GT.0) WRITE(IOGDB,178) JHR,NOHR
  178 FORMAT(' JHR,NOHR =',2I4)
C
C  GET PERIOD FROM JULIAN HOUR
C
      IF(JHR.EQ.12)  IPER = 4
      IF(JHR.EQ.6)   IPER = 3
      IF(JHR.EQ.0)   IPER = 2
      IF(JHR.EQ.18)  IPER = 1
      IF(IPDDB.GT.0) WRITE(IOGDB,179) IPER
  179 FORMAT(' PERIOD = ',I2)
C
C  CHECK FOR VALID PERIOD
C
      IF(IPER.NE.0) GO TO 200
      WRITE(LPE,180) IPER,IMO,IDAY,IYR,IHR,TIME(3)
  180 FORMAT(' **ERROR** INVALID PERIOD ',I2,'  FOR MDR DATE ',
     1        2(I2,'/'),I4,' HR ',I2,1X,A3)
      IWRIT = 1
      GO TO 475
  200 CONTINUE
C
C  GET COORDINATES FOR BOX NUMBER
C
      CALL RWCARD(ISTAT)
      IF(ISTAT.NE.0) GO TO 950
      IF = 1
      IF(IFTYPE(IF).EQ.1) GO TO 225
      WRITE(LPE,210) IF
  210 FORMAT(' FIELD ',I2,' IS NOT AN INTEGER')
      IWRIT = 1
  225 CONTINUE
      IF(NFIELD.EQ.3) GO TO 250
      WRITE(LPE,230)
  230 FORMAT( '**ERROR** INVALID NUMBER OF FIELDS ON MDR EDIT DATA ',
     1        'CARD.')
      IWRIT = 1
  250 CONTINUE
      CALL UINTFX(NROW,IFSTRT(IF),IFSTOP(IF),JERR)
      IF(JERR.EQ.0) GO TO 275
      WRITE(LPE,280)
  280 FORMAT(' INVALID CHARACTER IN FIELD ',I2)
      NWRIT = 1
  275 IF = IF + 1
      CALL UINTFX(NCOLM,IFSTRT(IF),IFSTOP(IF),JERR)
      IF(JERR.EQ.0) GO TO 300
      WRITE(LPE,280) IF
      NWRIT = 1
      GO TO 375
  300 CONTINUE
C
C  READ STATE BOUNDARY PARAMETER RECORD TO OBTAIN PLOT BOUNDARY RANGE
C
      CALL RPPREC(IDSTA,ISTBN,IPTR,LARRAY,ARRAY,NFILL,PTRNXT,ISTAT)
      IF(ISTAT.EQ.0) GO TO 310
      GO TO (950,525,550) , ISTAT
  310 CONTINUE
      ISROW = ARRAY(4)
      IWCOLM = ARRAY(2)
      NUMROW = ARRAY(5)
      NUMCOL = ARRAY(3)
C
C  CONVERT ROW AND COLUMN TO MDR BOX NUMBER
C
      IF(NROW.GE.ISROW.AND.NROW.LE.ISROW+(NUMROW-1)) GO TO 325
      WRITE(LPE,315) NROW
  315 FORMAT('**ERROR** MDR ROW LOCATION ',I2,' OUT OF STATE BOUNDARY ',
     1        'RANGE.')
      IWRIT = 1
  325 IF(NCOLM.GE.IWCOLM.AND.NCOLM.LE.IWCOLM+(NUMCOL-1)) GO TO 350
      WRITE(LPE,330) NCOLM
  330 FORMAT(' **ERROR** MDR COLUMN LOCATION ',I3,' OUT OF STATE ',
     1       'BOUNDARY RANGE.')
      IWRIT = 1
      GO TO 375
  350 CONTINUE
      MDRBOX = (NROW - ISROW) * NUMCOL + NCOLM - (IWCOLM-1)
      IF(IPDDB.GT.0) WRITE(IOGDB,360) MDRBOX
  360 FORMAT(' MDRBOX = ',I6)
  375 CONTINUE
C
C  GET MDR VALUE
C
      IF = IF + 1
      CALL UINTFX(IVAL,IFSTRT(IF),IFSTOP(IF),JERR)
      IF(JERR.EQ.0) GO TO 400
      WRITE(LPE,280) IF
      WRITE(LPE,610) IVAL,IPER,IMO,IDAY,IYR,IHR,TIME(3)
      IWRIT = 1
      GO TO 475
  400 MDATA = IVAL
C
C  GET DATA ARRAY FOR THIS DATE
C
      CALL RPDDLY(MDR,JULDA,0,LPNTRS,IPNTRS,LPFILL,LDATA,DATA,LDFILL,
     1           NUMSTA,MSNG,1,DATES,ISTAT)
      IF(NUMSTA.EQ.0) GO TO 650
      IF(ISTAT.EQ.0) GO TO 425
      GO TO (425,700,700,725,750,950) , ISTAT
  425 CONTINUE
      IF(LDFILL.EQ.0) GO TO 775
C
C  SET DATA ARRAY TO NEW VALUES
C
      NVALS = IDDTDR(16,IX)
      IOFF = IDDTDR(16,IX) * (IPER - 1) + 1
      DATA(IOFF + MDRBOX - 1) = MDATA
      IF(IPDDB.GT.0) WRITE(IOGDB,430) IOFF
  430 FORMAT(' IOFF = ',I6)
C
C  CALL WRITE MDR ROUTINE TO UPDATE THIS RECORD
C
      IF(IWRIT.EQ.1) GO TO 600
      CALL WPDMDR(JULDA,IPER,NVALS,LDFILL,DATA(IOFF),ISTAT)
      IF(ISTAT.EQ.0) GO TO 450
      IF(ISTAT.GE.30) GO TO 850
      GO TO (800,825,950) , ISTAT
  450 CONTINUE
      WRITE(LP,460) IPER,IMO,IDAY,IYR,IHR,TIME(3)
  460 FORMAT(' MDR DATA VALUE EDITED FOR PERIOD ',I2,' ON ',2(I2,'/'),
     1      I4,' HR ',I2,1X,A3)
      IWRIT = 0
  475 CONTINUE
C
C  MORE VALUES FOR THIS PERIOD ??
C
      CALL RWCARD(ISTAT)
      IF(ISTAT.NE.0) GO TO 950
      IF = 1
      IF(IFTYPE(IF).EQ.1.AND.NFIELD.EQ.3) GO TO 250
      NUM = IFSTOP(IF) - IFSTRT(IF) + 1
      IF(NUM.GT.8) NUM = 8
      CALL UPACK1(IBUF(IFSTRT(IF)),NCARD,NUM)
      CALL UNAMCP(NCARD,LEND,JERR)
      IF(JERR.EQ.0) GO TO 995
      WRITE(LPE,480)
  480 FORMAT(' **ERROR** INVALID END CARD FOR MDR EDIT. END ASSUMED')
      GO TO 995
  525 CONTINUE
      WRITE(LPE,530)
  530 FORMAT(' **ERROR** IN PDEMDR. PARAMETER RECORD FOR STBN NOT ',
     1       'FOUND.')
      IWRIT = 1
      GO TO 475
  550 CONTINUE
      WRITE(LPE,560) NFILL
  560 FORMAT(' **ERROR** IN PDEMDR. WORK ARRAY TOO SMALL FOR PARAMETER',
     1       ' DATA. ENLARGE TO ',I4)
      IWRIT = 1
      GO TO 475
  600 CONTINUE
      WRITE(LPE,610) MDATA,IPER,IMO,IDAY,IYR,IHR,TIME(3)
  610 FORMAT(' **WARNING** MDR VALUE ',I4,' NOT EDITED FOR PER ',I2,
     1       ' ON ',2(I2,'/'),I4,' HR ',I2,1X,A3,' DUE TO INPUT ERROR.')
      IWRIT = 0
      GO TO 475
  650 CONTINUE
      WRITE(LPE,660)
  660 FORMAT(' **WARNING** NO STATIONS DEFINED FOR MDR6 ON PPDB.')
      IWRIT = 1
      GO TO 475
  700 CONTINUE
      WRITE(LPE,710)
  710 FORMAT(' **ERROR** IN PDEMDR. DATA ARRAY TO SMALL. ENLARGE.')
      IWRIT = 1
      GO TO 475
  725 CONTINUE
      WRITE(LPE,730) IMO,IDAY,IYR,IHR,TIME(3)
  730 FORMAT(' **ERROR** IN PDEMDR. NO MDR DATA FOUND ON PPDB FOR ',
     1       2(I2,'/'),I4,' HR ',I2,1X,A3)
      IWRIT = 1
      GO TO 475
  750 CONTINUE
      WRITE(LPE,760) IPTYPE
  760 FORMAT(' **ERROR** IN PDEMDR INVALID DATA TYPE ',A4)
      IWRIT = 1
      GO TO 475
  775 CONTINUE
      WRITE(LPE,780) IMO,IDAY,IYR,IHR,TIME(3)
  780 FORMAT(' NO MDR DATA ON FILE FOR ',2(I2,'/'),I4,' HR ',I2,1X,A3)
      IWRIT = 1
      GO TO 475
  800 CONTINUE
      WRITE(LPE,810)
  810 FORMAT(' **ERROR** IN PDEMDR. NUMBER OF EDIT VALUES EXCEEDS ',
     1       ' NUMBER OF VALUES ON FILE.')
      IWRIT = 1
      GO TO 475
  825 CONTINUE
      WRITE(LPE,830)
  830 FORMAT(' **ERROR** IN PDEMDR. DATE IS NOT WITHIN THE PRESENT ',
     1        ' RANGE OF MDR DATA ON FILE.')
      IWRIT = 1
      GO TO 475
  850 CONTINUE
      WRITE(LPE,860) MDATA
  860 FORMAT(' **ERROR** IN PDEMDR. VALUE ',I4,' IS OUT OF THE ALLOW',
     1       'ABLE RANGE (-36 TO +36) FOR MDR DATA.')
      GO TO 600
  950 CONTINUE
      WRITE(LPE,960)
  960 FORMAT(' **ERROR** IN PDEMDR. SYSTEM ERROR.')
      GO TO 999
  995 CONTINUE
C
C DEBUG
C
      IF(IPDDB.GT.0) WRITE(IOGDB,998)
  998 FORMAT(' EXIT PDEMDR')
  999 CONTINUE
      RETURN
      END
