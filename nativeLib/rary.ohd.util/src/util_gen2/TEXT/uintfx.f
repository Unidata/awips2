C MEMBER UINTFX
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE UINTFX (IANS,ISTRT,IEND,ISTAT)
C      
C  ROUTINE TO CONVERT CHARACTERS IN ARRAY IBUF TO INTEGER VALUE.     
C   
      INCLUDE 'uio'   
      INCLUDE 'ufreei'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uintfx.f,v $
     . $',                                                             '
     .$Id: uintfx.f,v 1.2 1997/09/22 17:46:51 page Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C      
      CALL UFIXED (IBUF,IANS,ISTRT,IEND,1,0,IERR)
      IF (IERR.NE.0) THEN 
         IF (ISTRT.EQ.IEND) WRITE (LP,10) ISTRT
10    FORMAT ('0**ERROR** INTEGER VALUE EXPECTED IN ',
     *   'COLUMN ',I2,'.')
         IF (ISTRT.NE.IEND) WRITE (LP,20) ISTRT,IEND
20    FORMAT ('0**ERROR** INTEGER VALUE EXPECTED IN ',
     *   'COLUMNS ',I2,' THROUGH ',I2,'.')   
         ISTAT=1
         ENDIF   
C     
      RETURN
C      
      END
