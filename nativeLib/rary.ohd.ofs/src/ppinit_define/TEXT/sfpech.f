C MEMBER SFPECH
C  (from old member SFMAPE)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/03/95.13:33:26 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SFPECH (PE,PECHG,ISTAT)
C
C     THIS ROUTINE COMPUTES THE DAILY CHANGE IN MEAN PE FROM
C     THE 16TH OF EACH MONTH TO THE 16TH OF THE NEXT MONTH.
C      (THE I VALUE OF PECHG REPRESENTS THE CHANGE FROM
C       THE I VALUE OF PE TO THE I+1 VALUE OF PE)
C      VERSION 1.0   2/7/83   WRITTEN BY LEB
C
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION PE(1),PECHG(1),NDAYS(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfpech.f,v $
     . $',                                                             '
     .$Id: sfpech.f,v 1.1 1995/09/17 19:11:29 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,20)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      ISTAT=0
C
      LDEBUG=ISBUG('MAPE')
      IF (LDEBUG.GT.0) WRITE (IOSDBG,30) (PE(I),I=1,12)
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
C     COMPUTE PECHG VALUES.
C
      DO 10 I=1,12
         J=I+1
         IF (I.EQ.12) J=1
         PECHG(I)=(PE(J)-PE(I))/NDAYS(I)
10       CONTINUE
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,30) (PECHG(I),I=1,12)
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,40) ISTAT
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' ***ENTER SFPECH')
30    FORMAT (' ',(12F10.2,1X))
40    FORMAT (' *** EXIT SFPECH - ISTAT=',I2)
C
      END
