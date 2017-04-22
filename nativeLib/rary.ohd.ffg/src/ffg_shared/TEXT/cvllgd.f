C  CVLLGD
C ORIGINALLY MEMBER SBLLGD
C-----------------------------------------------------------------------
C                             LAST UPDATE: 1997-11-17 BY DWS
C
C @PROCESS LVL(77)
C
      SUBROUTINE CVLLGD (FLON,FLAT,NPAIR,X,Y,ILLGD,ISTAT)
C
C  THIS ROUTINE CONVERTS FROM LONGITUDE AND LATITUDE COORDINATE
C  LOCATION TO THE HRAP GRID SYSTEM LOCATION (AND VISA-VERSA).
C  NORTH POLE IS ASSUMED TO BE GRID COORDINATES 401,1601.
C
C    NOTE, THE ALGORITMS HAVE BEEN PLACED INTO ROUTINE LLGD AND
C    THIS ROUTINE CONTAINS DEBUG AND ERROR STATEMENTS ONLY. 971117
C
C  IF ILLGD=0, LAT/LONG ARE COMPUTED FROM GRID POINTS.
C  IF ILLGD=1, GRID POINTS ARE DETERMINED FROM LAT/LONG.
C
C    ARGUMENT LIST:
C
C       NAME     TYPE  I/O   DIM   DESCRIPTION
C       ----     ----  ---   ---   -----------
C       FLON      R*4    I    1    LONGITUDE (DECIMAL DEGREES)
C       FLAT      R*4    I    1    LATITUDE (DECIMAL DEGREES)
C       NPAIR     I      I    1    NUMBER OF LAT/LON PAIRS
C       X         R      O    1    CONVERTED LONGITUDE
C       Y         R      O    1    CONVERTED LATITUDE
C       ILLGD     I      I    1    CONVERSION CODE
C                                   0=CONVERT GRID POINTS TO LAT/LON
C                                   1=CONVERT LAT/LON TO GRID POINTS
C       ISTAT     I      O    1    STATUS INDICATOR
C
      character*8 sname
      INTEGER  NPAIR,ILLGD,NUMERR,ISTAT,ISBUG,II
      REAL     X(1),Y(1),FLON(1),FLAT(1)
C
      INCLUDE 'ffg_inc/iuws'
      INCLUDE 'ffg_inc/gdebug'
C
      data sname / 'cvllgd  ' /
c
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/cvllgd.f,v $
     . $',                                                             '
     .$Id: cvllgd.f,v 1.1 2001/08/16 17:42:39 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      call prbug (sname,1,3,ibug)
C
      NUMERR=0
C
C  CALL ROUTINE TO DO CONVERSIONS
C
      CALL LLGD (FLON,FLAT,NPAIR,X,Y,ILLGD,ISTAT)
C
C  CHECK NUMBER OF VALUES TO BE PROCESSED
C
      IF (ISTAT .EQ. -1) THEN
         WRITE (iue,160) NPAIR
         nerr = nerr + 1
C
C  CHECK CONVERSION OPTION
C
      ELSEIF (ISTAT .EQ. -2) THEN
         WRITE (iue,170) ILLGD
         nerr = nerr + 1
C
C  CHECK FOR BAD LONG/LAT INPUT
C
      ELSEIF (ILLGD.EQ.1 .AND. ISTAT.GT.0) THEN
         DO 50 II=1,NPAIR
            IF (FLAT(II).LT.10. .OR. FLAT(II).GT.80. .OR.
     $          FLON(II).LT.40. .OR. FLON(II).GT.180.) THEN
               WRITE (iue,190) FLAT(II),FLON(II),ISTAT
               nerr = nerr + 1
            ENDIF
   50    CONTINUE
C
C  CHECK FOR BAD GRID POINTS
C
      ELSEIF (ILLGD.EQ.0 .AND. ISTAT.GT.0) THEN
         DO 100 II=1,NPAIR
            IF (X(II).LT.-300. .OR. X(II).GT.1661. .OR.
     $          Y(II).LT.1 .OR. Y(II).GT.1601.) THEN
               WRITE (iue,220) X(II),Y(II),ISTAT
               nerr = nerr + 1
            ENDIF
  100    CONTINUE

      ENDIF
C
      IF (ibug.GT.0) THEN
         DO 130 II=1,NPAIR
            WRITE (iud,230) II,FLON(II),FLAT(II),X(II),Y(II)
c            CALL SULINE (IOSDBG,1)
130         CONTINUE
         ENDIF
C
c140   IF (ISTRCE.GT.0) WRITE (IOSDBG,240)
c      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT ('0*** ERROR - IN CVLLGD - NUMBER OF PAIRS TO BE ',
     *   'PROCESSED (',I4,') IS LESS THAN OR EQUAL TO ZERO.')
170   FORMAT ('0*** ERROR - IN CVLLGD - ',I4,' IS AN INVALID ',
     *   'CONVERSION OPTION VALUE. VALID VALUES ARE 0 AND 1.')
180   FORMAT (' NPAIR=',I4,3X,'ILLGD=',I4)
190   FORMAT ('0*** ERROR - IN CVLLGD - THE SPECIFIED LATITUDE/',
     *   'LONGITUDE (',F6.2,',',F6.2,') EXCEED THE VALID RANGE.',
     *   ' FIRST OF',I6,' ERRORS.')
220   FORMAT ('0*** ERROR - IN CVLLGD - THE SPECIFIED HRAP/NWSRFS ',
     *   'COORDINATES (',F8.2,',',F8.2,') EXCEED THE VALID RANGE.',
     *   ' FIRST OF',I6,' ERRORS.')
230   FORMAT (' I=',I4,3X,
     *   'FLON(I)=',F6.2,3X,
     *   'FLAT(I)=',F6.2,3X,
     *   'X(I)=',F7.2,3X,
     *   'Y(I)=',F7.2)
C
      END
