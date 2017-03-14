C MEMBER PRP23
C  (from old member FCPRP23)
C
C  DESC -- PRINT PARAMETER SUBROUTINE FOR STAGE-Q OPERATION
C
C.......................................................................
C
      SUBROUTINE PRP23(PO)
C
C.......................................................................
C
C  THIS IS THE PRINT PARAMETER SUBROUTINE FOR STAGE-Q OPERATION
C
C    ARGUMENT LIST
C    PO     - THE PARAMETER ARRAY
C             CONTENTS:
C               POSITION     DESCRIPTION
C                  1         VERSION NUMBER
C                 2-6        20-CHAR NAME OF GAGING STATION OR FORECAST
C                             POINT
C                  7         CONVERSION INDICATOR
C                            =1, STAGE TO DISCHARGE
C                            =2, DISCHARGE TO STAGE
C                            (DEFAULT IS 1)
C                 8-9        8-CHAR STAGE T.S. IDENTIFIER
C                  10        4-CHAR STAGE T.S. DATA TYPE CODE
C                  11        TIME INTERVAL IN HOURS FOR STAGE T.S.
C                12-13       8-CHAR DISCHARGE T.S. IDENTIFIER
C                  14        4-CHAR DISCHARGE T.S. DATA TYPE CODE
C                  15        TIME INTERVAL IN HOURS FOR DISCHARGE T.S.
C                16-17       8-CHAR RATING CURVE IDENTIFIER
C                  18        DEFAULT CARRYOVER INDICATOR
C                            =0, DEFAULT CARRYOVER (ALL ZEROES) USED
C                            =1, INITIAL CARRYOVER READ IN
C
C
C.......................................................................
C
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      JONATHAN WETMORE - HRL - 810415
C  THIS IS THE FIRST VERSION OF THIS OPERATION
C
C***********************************************************************
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1),SUBNAM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp23.f,v $
     . $',                                                             '
     .$Id: prp23.f,v 1.1 1995/09/17 18:50:06 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SUBNAM/4HPRP2,4H3   /
      DATA LTRACE,NOP/1,23/
C
C
      CALL FPRBUG(SUBNAM,LTRACE,NOP,IBUG)
C
C  DEF PARMS
      INTVLS=PO(11)
      INTVLQ=PO(15)
      ICONV=PO(7)
C
      IF(ICONV.GT.1) GO TO 20
      WRITE (IPR,1)(PO(I),I=2,6)
 1    FORMAT(1H0,10X,34HSTAGE TO DISCHARGE CONVERSION FOR ,5A4)
      GO TO 30
 20   WRITE(IPR,2)(PO(I),I=2,6)
 2    FORMAT(1H0,10X,34HDISCHARGE TO STAGE CONVERSION FOR ,5A4/)
 30   WRITE(IPR,3)(PO(I),I=16,17)
 3    FORMAT(1H0,21X,19HUSING RATING CURVE ,2A4)
      WRITE(IPR,4)(PO(J),J=8,10),INTVLS,(PO(K),K=12,14),INTVLQ
  4   FORMAT(1H0,26X,16HTIME SERIES USED //14X,8HCONTENTS,5X,4HI.D.,
     15X,4HTYPE,4X,13HTIME INTERVAL //15X,5HSTAGE,5X,2A4,3X,A4,6X,
     1I2,6H HOURS /13X,9HDISCHARGE,3X,2A4,3X,A4,6X,I2,6H HOURS //)
C
C
 999   CONTINUE
       IF(ITRACE.GE.1) WRITE(IODBUG,6002)
 6002  FORMAT(1H ,19H *** EXIT PRP23 ***)
C
C
       RETURN
       END
