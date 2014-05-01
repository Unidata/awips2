C MODULE URELFX
C-----------------------------------------------------------------------
C
      SUBROUTINE URELFX (ANS,ISTRT,IEND,ISTAT,ID)
C
C  ROUTINE TO GET REAL VALUE FROM FIELD IN CHARACTER ARRAY IBUF.
C
      INCLUDE 'uiox'
      INCLUDE 'ufreei'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/urelfx.f,v $
     . $',                                                             '
     .$Id: urelfx.f,v 1.2 2001/06/13 13:50:00 dws Exp $
     . $' /
C    ===================================================================
C
C
      ITYPE=2
      CALL UFIXED (IBUF,ANS,ISTRT,IEND,ITYPE,ID,IERR)
      IF (IERR.NE.0) THEN
         IF (ISTRT.EQ.IEND) WRITE (LP,10) ISTRT
         IF (ISTRT.NE.IEND) WRITE (LP,20) ISTRT,IEND
10    FORMAT ('0**ERROR** REAL VALUE EXPECTED IN COLUMN ',I2,'.')
20    FORMAT ('0**ERROR** REAL VALUE EXPECTED IN COLUMNS ',I2,'-',I2,
     *   '.')
         ISTAT=1
         ENDIF
C
      RETURN
C
      END
