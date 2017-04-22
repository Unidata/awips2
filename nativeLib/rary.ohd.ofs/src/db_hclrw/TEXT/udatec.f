C MEMBER UDATEC
C  (from old member UDATEA)
C.......................................................................
C.......................................................................
C
      SUBROUTINE UDATEC(ISF,IEF,NHRADD,NHSWCH,IPRINT,JULDA,INTHR,JULHR,
     1                  ISTAT)
C.......................................................................
C.......................................................................
C
C      UDATEC GETS THE DATE FROM ARRAY IBUF IN COMMON BLOCK /UFREEI/.
C.......................................................................
C
C     ARGUMENT LIST
C
C     ISF    - START OF THE DATE FIELD IN /UFREEI/
C     IEF    - END OF THE DATE FIELD IN /UFREEI/
C     NHRADD - THE NUMBER OF HOURS TO ADD TO THE INTERNAL
C              HOUR AND JULIAN HOUR IF NO HOUR IS DECODED
C     NHSWCH - SWITCH SPECIFYING IF INTERNAL HOUR SHOULD BE IN
C              00-23 OR 01-24 HOUR RANGE
C               =0, 00 TO 23
C               =1, 01 TO 24
C     IPRINT - SWITCH SPECIFYING WHETHER OR NOT TO PRINT
C              A WARNING MESSAGE IF FIELD DECODED IS NOT
C              A VALID DATE
C               =0, DO NOT PRINT MESSAGE
C               NE 0, PRINT MESSAGE AND CALL WARN
C     JULDA  - JULIAN DAY RETURNED
C     INTHR  - INTERNAL HOUR RETURNED
C     JULHR  - JULIAN HOUR RETURNED
C     ISTAT  - STATUS FLAG
C               =0, OK
C               =1, INVALID DATE FIELD
C               =2, ERROR IN ARGUMENT INPUT
C                   ONE OF ISF, IEF, NHRADD OR NHSWCH NOT
C                   IN VALID RANGE
C.......................................................................
C
      COMMON/UFREEI/ NFIELD,IFTYPE(40),IFCNT(40),IFSTRT(40),IFSTOP(40),
     *               IBUF(80),KOUNT
      COMMON/IONUM/IN,IPR,IPU
      COMMON/MDFLTD/INHOUR
C
      DIMENSION IDARAY(7)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/udatec.f,v $
     . $',                                                             '
     .$Id: udatec.f,v 1.1 1995/09/17 18:43:22 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C
C     SET STATUS TO ZERO
C
      ISTAT=0
C
C.......................................................................
C
C     CHECK VALUES FOR ISF AND IEF
C
      IF(ISF.GT.80.OR.ISF.LT.1.OR.IEF.GT.80.OR.IEF.LT.1)GO TO 45
      IF(ISF.LE.IEF)GO TO 47
C
C     PROBLEMS WITH ISF AND/OR IEF - ASSUME BOTH EQUAL 1 AND CONTINUE
C
   45 WRITE(IPR,605)ISF,IEF
  605 FORMAT(1H0,10X,'**WARNING** IN UDATEC - VALUES FOR START AND ',
     1 'END OF DATE FIELD ARE ',I3,' AND ',I3,'.'/11X,'THEY HAVE BEEN ',
     2 'RESET TO 1.')
      CALL WARN
      ISTAT=2
C
      IS=1
      IE=1
      GO TO 60
C
   47 IS=ISF
      IE=IEF
C.......................................................................
C     CALL HCKDAT TO DECODE DATE FIELD
C
   60 CALL HCKDAT(IS,IE,IDARAY,IER)
C
      IF(IER.EQ.0)GO TO 70
C
C     ERROR DECODING DATE - DO NOT PRINT MESSAGE IF IPRINT EQ 0
C
      ISTAT=1
      IF(IPRINT.EQ.0)GO TO 999
C
C     IPRINT NE 0 SO PRINT MESSAGE AND CALL WARN
C
      WRITE(IPR,608)(IBUF(I),I=IS,IE)
  608 FORMAT(1H0,10X,'**WARNING** INVALID DATE FIELD DECODED'/
     1  11X,'FIELD ENTERED IS ',80A1)
      WRITE(IPR,609)
  609 FORMAT(11X,'SEE USER''S MANUAL SECTION VI.5.2B FOR VALID DATE ',
     1 'FORMATS.')
      CALL WARN
      GO TO 999
C.......................................................................
C
   70 INHOUR=0
      IF(IDARAY(5).EQ.-1)INHOUR=1
C
C     CALL HSETDY TO EXPAND DATE AND GET DEFAULT HOUR AND TZC IF NEEDED
C
      CALL HSETDY(IDARAY)
C.......................................................................
C
C     NOW COMPUTE JULIAN DAY AND INTERNAL HOUR FROM JULIAN HOUR
C      JULIAN HOUR IS FIRST LOCATION IN IDARAY
C
      JULHR=IDARAY(1)
C
      JULDA=JULHR/24
      INTHR=JULHR-JULDA*24
      JULDA=JULDA+1
C
C     INTHR IS NOW IN 00 TO 23 RANGE - CHECK TO SEE IF 01 TO 24 WANTED
C
      IF(NHSWCH.EQ.0.OR.NHSWCH.EQ.1)GO TO 90
C
C     NHSWCH OUT OF VALID RANGE - ASSUME = 0
C
      WRITE(IPR,606)NHSWCH
  606 FORMAT(1H0,10X,'**WARNING** - IN UDATEA OR UDATEC - ',
     1 'ARGUMENT NHSWCH MUST BE 0 OR 1 - VALUE ENTERED IS ',I11/
     2  11X,'VALUE OF ZERO IS ASSUMED.')
      CALL WARN
      ISTAT=2
C
      JHSWCH=0
      GO TO 91
C
   90 JHSWCH=NHSWCH
C
   91 IF(JHSWCH.EQ.0)GO TO 100
C
C     01 TO 24 HOUR RANGE WANTED
C
      IF(INTHR.NE.0)GO TO 100
C
      JULDA=JULDA-1
      INTHR=24
C.......................................................................
C
C     CHECK IF NHRADD NEEDS TO BE ADDED TO INTHR AND JULHR
C     ADD NHRADD IF NO HOUR DECODED IN DATE FIELD
C      IF NHRADD EQUAL TO 0 JUST RETURN
C
  100 IF(NHRADD.EQ.0)GO TO 1000
C
C     NHRADD MUST BE IN RANGE -24 TO 24
C
      IF(NHRADD.GE.-24.AND.NHRADD.LE.24)GO TO 110
C
C     NHRADD OUT OF RANGE - SET TO 0
C
      WRITE(IPR,607)NHRADD
  607 FORMAT(1H0,10X,'**WARNING** - IN UDATEA OR UDATEC - ',
     1 'NUMBER OF HOURS TO ADD MUST BE IN RANGE -24 TO 24.'/
     2 11X,'VALUE ENTERED IS ',I11,' - ZERO IS ASSUMED.')
      CALL WARN
      ISTAT=2
      GO TO 1000
C.......................................................................
C
C     CHECK INHOUR TO SEE IF HOURS WERE DECODED
C       INHOUR = 0 IF HOURS DECODED
C       INHOUR = 1 IF NO HOURS DECODED
C
  110 IF(INHOUR.EQ.0)GO TO 1000
C
C     NO HOURS ENTERED - ADD NHRADD TO JULHR AND INTHR
C
      JULHR=JULHR+NHRADD
C
      INTHR=INTHR+NHRADD
C
      IF(INTHR.GT.0)GO TO 120
C
C     NEGATIVE OR ZERO INTHR
C
      IF(INTHR.EQ.0.AND.NHSWCH.EQ.0)GO TO 1000
C
      INTHR=INTHR+24
      JULDA=JULDA-1
C
  120 IF(INTHR.LT.24)GO TO 1000
      IF(INTHR.EQ.24.AND.NHSWCH.EQ.1)GO TO 1000
C
      INTHR=INTHR-24
      JULDA=JULDA+1
      GO TO 1000
C
  999 JULDA=0
      INTHR=0
      JULHR=0
C
 1000 RETURN
      END
