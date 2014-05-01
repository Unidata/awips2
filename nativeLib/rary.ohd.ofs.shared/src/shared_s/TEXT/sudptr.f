C MODULE SUDPTR
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT THE POINTER AND DATA ARRAYS RETURNED
C  FROM THE PREPROCESSOR DATA BASE ROUTINE RPDDLY.
C
      SUBROUTINE SUDPTR (TYPE,IPNTRS,LPFILL)
C
      CHARACTER*4 TYPE
      CHARACTER*20 HEADR
      INTEGER*2 IPNTRS(*)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sudptr.f,v $
     . $',                                                             '
     .$Id: sudptr.f,v 1.3 1998/04/07 18:11:27 page Exp $
     . $' /
C    ===================================================================
C
C
C
C  CHECK TRACE LEVEL
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,50)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (LPFILL.EQ.0) GO TO 40
C
C  SET MAXIMUM NUMBER OF VALUES PER LINE
      MAXPER=20
      HEADR='(9X,??(''-----+''))'
      IF (TYPE.EQ.'PP24') THEN
         MAXPER=15
         HEADR='(9X,??(''------+''))'
         ENDIF
C
      NVAL=LPFILL
      IF (NVAL.GT.MAXPER) NVAL=MAXPER
C
C  PRINT POINTERS
      I1=1
10    I2=I1+(MAXPER-1)
      IF (I2.GT.LPFILL) I2=LPFILL
      IF (I1.EQ.1.OR.ISNWPG(LP).EQ.1) GO TO 20
      GO TO 30

20    WRITE (LP,60) TYPE,LPFILL
      CALL SULINE (LP,2)
      IF (MAXPER.EQ.15) THEN
         WRITE (LP,70) (I,I=1,NVAL)
         CALL SULINE (LP,2)
         ENDIF
      IF (MAXPER.EQ.20) THEN
         WRITE (LP,80) (I,I=1,NVAL)
         CALL SULINE (LP,2)
         ENDIF
      IBEG=5
      NCHAR=2
      IPRERR=1
      CALL UFI2A (NVAL,HEADR,IBEG,NCHAR,IPRERR,LP,IERR)
      WRITE (LP,HEADR)
      CALL SULINE (LP,1)
C
30    IF (MAXPER.EQ.15) THEN
         WRITE (LP,90) I1,(IPNTRS(I),I=I1,I2)
         CALL SULINE (LP,1)
         ENDIF
      IF (MAXPER.EQ.20) THEN
         WRITE (LP,100) I1,(IPNTRS(I),I=I1,I2)
         CALL SULINE (LP,1)
         ENDIF
      IF (I2.EQ.LPFILL) GO TO 40
         I1=I2+1
         GO TO 10
C
40    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,110)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER SUDPTR')
60    FORMAT ('0DUMP OF PREPROCESSOR DATA BASE POINTER ARRAY : ',
     *   'TYPE=',A,5X,'LENGTH OF ARRAY=',I6)
70    FORMAT ('0POSITION',1X,15(I6,1X))
80    FORMAT ('0POSITION',1X,20(I5,1X))
90    FORMAT (' ',2X,I5,2X,15(I6,1X))
100   FORMAT (' ',2X,I5,2X,20(I5,1X))
110   FORMAT (' *** EXIT SUDPTR')
C
      END
