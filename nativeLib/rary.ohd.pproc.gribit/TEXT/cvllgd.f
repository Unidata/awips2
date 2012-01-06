C MODULE CVLLGD
C-----------------------------------------------------------------------
C
      SUBROUTINE CVLLGD (FLON,FLAT,NPAIR,X,Y,ILLGD,ISTAT)
C
C  THIS ROUTINE CONVERTS FROM LONGITUDE AND LATITUDE COORDINATE
C  LOCATION TO THE HRAP GRID SYSTEM LOCATION AND VISA-VERSA.
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
      DIMENSION X(NPAIR),Y(NPAIR),FLON(NPAIR),FLAT(NPAIR)
C
      INCLUDE 'ffg_inc/iuws'
      INCLUDE 'ffg_inc/gdebug'
c
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/cvllgd.f,v $
     . $',                                                             '
     .$Id: cvllgd.f,v 1.1 2006/05/03 13:43:58 gsood Exp $
     . $' /
C    ===================================================================
C
C
      IBUG=0
C
C  CONVERT VALUES
      CALL LLGD (FLON,FLAT,NPAIR,X,Y,ILLGD,ISTAT)
      IF (ISTAT.EQ.-1) WRITE (IUE,10) NPAIR
10    FORMAT (' ERROR: IN CVLLGD - NUMBER OF PAIRS TO BE ',
     *   'PROCESSED (',I4,') IS LESS THAN OR EQUAL TO ZERO.')
      IF (ISTAT.EQ.-2) WRITE (IUE,20) ILLGD
20    FORMAT (' ERROR: IN CVLLGD - ',I4,' IS AN INVALID ',
     *   'CONVERSION OPTION VALUE. VALID VALUES ARE 0 AND 1.')
      IF (ISTAT.GT.0) THEN
         IF (ILLGD.EQ.1) THEN
            DO 40 II=1,NPAIR
               IF (FLAT(II).LT.10. .OR. FLAT(II).GT.80. .OR.
     $             FLON(II).LT.40. .OR. FLON(II).GT.180.) THEN
                  WRITE (IUE,30) FLAT(II),FLON(II),ISTAT
30    FORMAT (' ERROR: IN CVLLGD - THE SPECIFIED LATITUDE/',
     *   'LONGITUDE (',F6.2,',',F6.2,') EXCEED THE VALID RANGE.',
     *   ' FIRST OF',I6,' ERRORS.')
                  ENDIF
40             CONTINUE
            ENDIF
         IF (ILLGD.EQ.0) THEN
            DO 60 II=1,NPAIR
               IF (X(II).LT.-300. .OR. X(II).GT.1661. .OR.
     $             Y(II).LT.1 .OR. Y(II).GT.1601.) THEN
                  WRITE (IUE,50) X(II),Y(II),ISTAT
50    FORMAT (' ERROR: IN CVLLGD - THE SPECIFIED HRAP/NWSRFS ',
     *   'COORDINATES (',F8.2,',',F8.2,') EXCEED THE VALID RANGE.')
                  ENDIF
60             CONTINUE
            ENDIF
         ENDIF
C
      IF (IBUG.GT.0) THEN
         DO 80 II=1,NPAIR
            WRITE (IUD,70) II,FLON(II),FLAT(II),X(II),Y(II)
70    FORMAT (' I=',I4,3X,
     *   'FLON(I)=',F6.2,3X,
     *   'FLAT(I)=',F6.2,3X,
     *   'X(I)=',F7.2,3X,
     *   'Y(I)=',F7.2)
80          CONTINUE
         ENDIF
C
      RETURN
C
      END
