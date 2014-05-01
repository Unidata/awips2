C MEMBER PRGTIX
C  (from old member PRDHUTIL)
C-----------------------------------------------------------------------
C
      SUBROUTINE PRGTIX (LUNIT,IPOS,ISTAT)
C
C  ROUTINE TO FIND THE LOCATION OF THE SPECIFIED UNIT NUMBER IN THE
C  TIME SERIES FILE CONTROL INFORMATION COMMON BLOCK.
C
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urtscl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/prgtix.f,v $
     . $',                                                             '
     .$Id: prgtix.f,v 1.1 1995/09/17 18:45:47 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10) LUNIT
10    FORMAT (' *** ENTER PRGTIX : LUNIT=',I2)
C
      ISTAT=0
      NUNIT=LUNIT
      IF (IAMORD.EQ.1) NUNIT=LUNIT+KUPRDO
C
C  CHECK COMMON BLOCK FOR UNIT NUMBER
      DO 20 IPOS=1,5
         IF (IAMORD.EQ.0.AND.TSCNTR(1,IPOS).EQ.NUNIT) GO TO 30
         IF (IAMORD.EQ.1.AND.ITSCNT(1,IPOS).EQ.NUNIT) GO TO 30
20       CONTINUE
C
C  UNIT NUMBER NOT FOUND
      IPOS=0
      ISTAT=1
C
30    IF (IPRTR.GT.0) WRITE (IOGDB,40) NUNIT,IPOS,ISTAT
40    FORMAT (' *** EXIT PRGTIX : NUNIT=',I2,3X,'IPOS=',I2,3X,
     *   'ISTAT=',I2)
C
      RETURN
C
      END
