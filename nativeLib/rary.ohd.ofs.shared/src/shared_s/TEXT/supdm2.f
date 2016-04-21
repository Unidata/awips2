C MODULE SUPDM2
C  PART OF OLD SUPDMP
C
      SUBROUTINE SUPDM2 (LARRAY,IARRAY,NVAL)
C
      INTEGER*2 IARRAY(LARRAY)
C
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/supdm2.f,v $
     . $',                                                             '
     .$Id: supdm2.f,v 1.1 1995/09/17 19:22:07 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  DUMP IN INTEGER*2 FORMAT
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,30)
      CALL SULINE (LP,1)
      WRITE (LP,40)
      CALL SULINE (LP,1)
      NPER=10
      NUM=NVAL
      IF (NUM.GT.NPER) NUM=NPER
      NUM1=1
      WRITE (LP,50) NUM1,(IARRAY(I),I=1,NUM)
      CALL SULINE (LP,1)
      IF (LARRAY.LE.NPER) GO TO 20
         NTIME=(LARRAY-NPER)/NPER
         IF (MOD(LARRAY,NPER).NE.0) NTIME=NTIME+1
         NUM1=NPER+1
         NUM2=NPER*2
         DO 10 J=1,NTIME
            IF (NUM2.GT.LARRAY) NUM2=LARRAY
            WRITE (LP,50) NUM1,(IARRAY(I),I=NUM1,NUM2)
            CALL SULINE (LP,1)
            NUM1=NUM1+NPER
            NUM2=NUM2+NPER
10          CONTINUE
C
20    RETURN
C
C-----------------------------------------------------------------------
C
30    FORMAT (' *--> INTEGER*2 DUMP IN 10I8   FORMAT ')
40    FORMAT (' ',5X,10('-------+'))
50    FORMAT (' ',I4,1X,10I8)
C
      END
