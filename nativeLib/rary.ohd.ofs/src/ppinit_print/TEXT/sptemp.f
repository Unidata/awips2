C MODULE SPTEMP
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATION TEMP PARAMETERS.
C
      SUBROUTINE SPTEMP (IPRNT,UNITS,LEVEL,IVTEMP,STAID,NUMBER,
     *   DESCRP,STATE,ITYOBS,IMTN,TEMPCF,IFMM,TEMPFE,ITNTWK,IPMMMT,
     *   IPDMMT,WTMMT,IPDINS,WTINS,IPDFMM,WTFMM,ITTAVR,UNUSED,ISTAT)
C
      CHARACTER*4 UNITS
      CHARACTER*4 TUNITS,UCODE,XFMM
      CHARACTER*8 ESTID
      CHARACTER*20 ESTDSC
C
      INCLUDE 'scommon/dimsta'
      INCLUDE 'scommon/dimtemp'      
C
      DIMENSION VALMAX(MAXTMP),VALMIN(MAXTMP)
      DIMENSION VALCOR(MAXCF)
      DIMENSION UNUSED(1)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/sptemp.f,v $
     . $',                                                             '
     .$Id: sptemp.f,v 1.3 2002/02/11 21:04:07 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,*) 'ENTER SPTEMP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('TEMP')
C
      ISTAT=0
C
C  DECODE UNITS
      CALL SUDCDU (UNITS,TUNITS,DEGMIN,IERR)
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      IF (IPRNT.EQ.0) GO TO 10
         IF (IPRNT.EQ.0) THEN
            WRITE (LP,250)
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.1) THEN
            WRITE (LP,280) STAID
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.2) THEN
            WRITE (LP,300) DESCRP
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.3) THEN
            WRITE (LP,290) NUMBER
            CALL SULINE (LP,2)
            ENDIF
         WRITE (LP,270)
         CALL SULINE (LP,1)
         GO TO 20
C
10    WRITE (LP,250)
      CALL SULINE (LP,2)
      WRITE (LP,270)
      CALL SULINE (LP,1)
      GO TO 30
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
20    IF (LDEBUG.GT.0) THEN
         WRITE (LP,320) IVTEMP
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT TEMP STATION IDENTIFIER, DESCRIPTIVE INFORMATION AND
C  STATION NUMBER
      IF (IPRNT.EQ.0) GO TO 30
         IF (NUMBER.GT.0) THEN
            WRITE (LP,330) STAID,DESCRP,STATE,NUMBER
            CALL SULINE (LP,2)
            ENDIF
         IF (NUMBER.EQ.0) THEN
            WRITE (LP,340) STAID,DESCRP,STATE
            CALL SULINE (LP,2)
            ENDIF
C
C  PRINT TYPE OF DATA OBSERVED BY STATION AND INDICATOR IF
C  STATION HAS MAX/MIN DATA
30    XFMM='FMM'
      IF (IFMM.EQ.0) XFMM='NFMM'
      IF (ITYOBS.LT.0.OR.ITYOBS.GT.4) THEN
         WRITE (LP,350) ITYOBS
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 80
         ENDIF
      IF (ITYOBS.EQ.1) THEN
          WRITE (LP,360) 'MXMN',XFMM
          CALL SULINE (LP,2)
         GO TO 80
          ENDIF
      IF (ITYOBS.EQ.2) THEN
         WRITE (LP,360) 'INST',XFMM
         CALL SULINE (LP,2)
         GO TO 80
         ENDIF   
      IF (ITYOBS.EQ.3) THEN
         WRITE (LP,360) 'BOTH (MXMN AND INST)',XFMM
         CALL SULINE (LP,2)
         GO TO 80
         ENDIF   
      IF (ITYOBS.EQ.4) THEN
         WRITE (LP,360) 'SYN',XFMM
         CALL SULINE (LP,2)
         GO TO 80
         ENDIF
C
C  PRINT TIME INTERVAL OF INSTANTANEOUS DATA
80    IF (ITYOBS.EQ.2.OR.ITYOBS.EQ.3) THEN 
         WRITE (LP,460) ITTAVR
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT ELEVATION WEIGHTING FACTOR AND MOUNTAINOUS INDICATOR
      VALUE=TEMPFE
      IF (TUNITS.EQ.'ENGL') THEN
C     GET CONVERION FACTORS
         ICONV=2
         NVAL=1
         CALL UDUCNV ('KM  ','MI  ',ICONV,NVAL,CONV1,TCONV,IERR)
         CALL UDUCNV ('M   ','FT  ',ICONV,NVAL,CONV2,TCONV,IERR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*)
     *         ' CONV1=',CONV1,
     *         ' CONV2=',CONV2,
     *         ' CONV1/CONV2=',CONV1/CONV2,
     *         ' '
            ENDIF
C     CONVERT VALUE
         VALUE=TEMPFE*(CONV1/CONV2)
         ENDIF
      IF (VALUE.GT.0.) GO TO 110
         IF (IMTN.EQ.0) THEN
            WRITE (LP,400) VALUE
            CALL SULINE (LP,2)
            GO TO 120
            ENDIF
110   WRITE (LP,390) VALUE
      CALL SULINE (LP,2)
C
C  PRINT MAX AND MIN CORRECTION FACTORS
120   IF (TEMPCF(1).EQ.0..AND.TEMPCF(2).EQ.0.) THEN
         WRITE (LP,380)
         CALL SULINE (LP,2)
         GO TO 130
         ENDIF
      CALL UMEMOV (TEMPCF,VALCOR,2)
      UCODE='DEGF'
      IF (TUNITS.EQ.'METR') THEN
         ICONV=1
         CALL UDUCNV ('DEGF','DEGC',ICONV,MAXCF,VALCOR,VALCOR,IERR)
         UCODE='DEGC'
         ENDIF
      WRITE (LP,370) VALCOR,UCODE
      CALL SULINE (LP,2)
C
C  PRINT MAX AND MIN MEAN TEMPERATURES
130   CALL RPP1MT (IPMMMT,TMPMAX,TMPMIN,IERR)
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.1) THEN
            WRITE (LP,510)
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 150
            ENDIF
         IF (IERR.EQ.2) THEN
            WRITE (LP,520)
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 150
            ENDIF
         IF (IERR.EQ.3) THEN
            WRITE (LP,530) IPMMMT
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 150
            ENDIF
         WRITE (LP,540) IERR
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 150
         ENDIF
      WRITE (LP,410)
      CALL SULINE (LP,2)
      WRITE (LP,415)
      CALL SULINE (LP,1)
      IF (TUNITS.EQ.'ENGL') THEN
         UCODE='DEGF'
         CALL UMEMOV (TMPMAX,VALMAX,MAXTMP)
         CALL UMEMOV (TMPMIN,VALMIN,MAXTMP)
         GO TO 140
         ENDIF
      UCODE='DEGC'
      ICONV=1
      CALL UDUCNV ('DEGF','DEGC',ICONV,MAXTMP,TMPMAX,VALMAX,IERR)
      CALL UDUCNV ('DEGF','DEGC',ICONV,MAXTMP,TMPMIN,VALMIN,IERR)
140   WRITE (LP,420) 'MAX',VALMAX,UCODE
      CALL SULINE (LP,2)
      WRITE (LP,420) 'MIN',VALMIN,UCODE
      CALL SULINE (LP,2)
C
150   IF (ITNTWK.EQ.0) GO TO 190
      IF (LEVEL.NE.2) GO TO 190
C
C  PRINT ARRAY LOCATIONS, WEIGHTS AND QUADRANTS OF 3 CLOSEST
C  STATIONS WITH MAX/MIN TEMPERATURE DATA IN EACH QUADRANT
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,430) 'MXMN','TM24'
      CALL SULINE (LP,3)
      N=0
      DO 160 I=1,4
         DO 160 J=1,3
            IF (IPDMMT(J,I).EQ.0) GO TO 160
            N=N+1
            ITM=N
            CALL SUGTID (ITM,'TM24',IPDMMT(J,I),ESTID,ESTDSC,ESTST,
     *         IESTCH,LSWORK,SWORK(1),IERR)
            WRITE (LP,440) N,IPDMMT(J,I),ESTID,ESTDSC,WTMMT(J,I),I
            CALL SULINE (LP,1)
160         CONTINUE
      IF (N.EQ.0) THEN
         WRITE (LP,450)
         CALL SULINE (LP,1)
         ENDIF
C
C  PRINT ARRAY LOCATIONS, WEIGHTS AND QUADRANTS OF 3 CLOSEST
C  STATIONS WITH INSTANTANEOUS TEMP DATA IN EACH QUADRANT
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,430) 'INST','TAVR'
      CALL SULINE (LP,3)
      N=0
      DO 170 I=1,4
         DO 170 J=1,3
            IF (IPDINS(J,I).EQ.0) GO TO 170
            N=N+1
            ITM=N
            CALL SUGTID (ITM,'TAVR',IPDINS(J,I),ESTID,ESTDSC,ESTST,
     *         IESTCH,LSWORK,SWORK(1),IERR)
            WRITE (LP,440) N,IPDINS(J,I),ESTID,ESTDSC,WTINS(J,I),I
            CALL SULINE (LP,1)
170         CONTINUE
      IF (N.EQ.0) THEN
         WRITE (LP,450)
         CALL SULINE (LP,1)
         ENDIF
C
C  PRINT ARRAY LOCATIONS, WEIGHTS AND QUADRANTS OF 2 CLOSEST
C  STATIONS WITH FORECAST TEMP IN EACH QUADRANT
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,430) 'FMM','TF24'
      CALL SULINE (LP,3)
      N=0
      DO 180 I=1,4
         DO 180 J=1,2
            IF (IPDFMM(J,I).EQ.0) GO TO 180
            N=N+1
            ITM=N
            CALL SUGTID (ITM,'TF24',IPDFMM(J,I),ESTID,ESTDSC,ESTST,
     *         IESTCH,LSWORK,SWORK(1),IERR)
            WRITE (LP,440) N,IPDFMM(J,I),ESTID,ESTDSC,WTFMM(J,I),I
            CALL SULINE (LP,1)
180         CONTINUE
      IF (N.EQ.0) THEN
         WRITE (LP,450)
         CALL SULINE (LP,1)
         ENDIF
C
C  PRINT NETWORK STATUS
190   IF (ITNTWK.LT.0.OR.ITNTWK.GT.2) THEN
         WRITE (LP,550) ITNTWK
         CALL SUERRS (LP,2,-1)
         GO TO 230
         ENDIF
      IF (ITNTWK.EQ.0) THEN
         WRITE (LP,470)
         CALL SULINE (LP,2)
         GO TO 230
         ENDIF
      IF (ITNTWK.EQ.1) THEN
         WRITE (LP,480)
         CALL SULINE (LP,2)
         GO TO 230
         ENDIF
      IF (ITNTWK.EQ.2) THEN
         WRITE (LP,490)
         CALL SULINE (LP,2)
         GO TO 230
         ENDIF
C
C  PRINT NUMBER OF UNUSED POSITIONS
230   NUNUSD=2
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,500) NUNUSD
         CALL SULINE (LP,2)
         ENDIF
C
      WRITE (LP,260)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,*) 'EXIT SPTEMP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
250   FORMAT ('0*-->  TEMP PARAMETERS')
260   FORMAT ('0',132('-'))
270   FORMAT (' ')
280   FORMAT ('0',59('-'),' ID=',A,2X,59('-'))
290   FORMAT ('0',59('-'),' NUMBER=',I4,2X,59('-'))
300   FORMAT ('0',52('-'),' DESC=',A,2X,52('-'))
320   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
330   FORMAT ('0*--> TEMP PARAMETERS :  ',
     *   'IDENTIFIER = ',A,5X,
     *   'DESCRIPTION = ',A,5X,
     *   'STATE = ',A2,5X,
     *   'NUMBER = ',I4)
340   FORMAT ('0*--> TEMP PARAMETERS :  ',
     *   'IDENTIFIER = ',A,5X,
     *   'DESCRIPTION = ',A,5X,
     *   'STATE = ',A2,5X,
     *   'NUMBER = ** NONE **')
350   FORMAT ('0*** ERROR - IN SPTEMP - INVALID VALUE OF ITYOBS : ',I3)
360   FORMAT ('0TYPE OF OBSERVED DATA = ',A,T55,'FORECAST MAX/MIN',
     *   ' DATA INDICATOR = ',A)
370   FORMAT ('0CORRECTION FACTORS:  ',
     *   'MAX=',F6.2,3X,
     *   'MIN=',F6.2,3X,
     *   A)
380   FORMAT ('0CORRECTION FACTORS = ** NOT DEFINED **')
390   FORMAT ('0ELEVATION WEIGHTING FACTOR = ',F7.2,2X,'(MOUNTAINOUS)')
400   FORMAT ('0ELEVATION WEIGHTING FACTOR = ',F7.2,2X,
     *   '(NON-MOUNTAINOUS)')
410   FORMAT ('0MEAN TEMPERATURES:',T26,
     *   '  JAN  ',
     *   '  FEB  ',
     *   '  MAR  ',
     *   '  APR  ',
     *   '  MAY  ',
     *   '  JUN  ',
     *   '  JUL  ',
     *   '  AUG  ',
     *   '  SEP  ',
     *   '  OCT  ',
     *   '  NOV  ',
     *   '  DEC  ')
415   FORMAT (' ',T26,
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ',
     *   ' ----- ')
420   FORMAT ('0',19X,1X,A,2X,12(F5.1,2X),A)
430   FORMAT ('0',A,' ESTIMATOR STATIONS',
     *   T35,A,' POINTER',5X,
     *      'IDENTIFIER',5X,
     *      'DESCRIPTION',14X,
     *      'WEIGHT',7X,
     *      'QUADRANT' /
     *   T35,12('-'),5X,
     *      10('-'),5X,
     *      20('-'),5X,
     *      8('-'),5X,
     *      8('-'))
440   FORMAT (T31,I2,4X,I5,10X,A,7X,A,5X,G8.3,9X,I1)
450   FORMAT (T35,'**NONE**')
460   FORMAT ('0DATA TIME INTERVAL OF INSTANTANEOUS DATA = ',I2,1X,
     *   'HOURS')
550   FORMAT ('0*** ERROR - IN SPTEMP - INVALID VALUE OF NETWORK ',
     *   'INDICATOR : ITNTWK=',I2)
470   FORMAT ('0STATUS:  NETWORK NOT RUN - ',
     *   'CANNOT USE IN AN MAT AREA')
480   FORMAT ('0STATUS:  NETWORK RUN PREVIOUSLY - ',
     *   'CANNOT USE IN NEW MAT AREA')
490   FORMAT ('0STATUS:  NETWORK RUN - ',
     *   'CAN USE IN AN MAT AREA')
500   FORMAT('0NUMBER OF UNUSED POSITIONS = ',I2)
510   FORMAT ('0*** ERROR - IN SPTEMP - SYSTEM ERROR ACCESSING ',
     *   'MEAN MAX/MIN TEMPERATURES FILE.')
520   FORMAT ('0*** ERROR - IN SPTEMP - MAX/MIN TEMPERATURES NOT ',
     *   'DEFINED.')
530   FORMAT ('0*** ERROR - IN SPTEMP - ',I5,' IS AN INVALID VALUE ',
     *   'FOR THE POINTER TO MEAN MAX/MIN TEMPERATURES.')
540   FORMAT ('0*** ERROR - IN SPTEMP - STATUS CODE FROM ROUTINE ',
     *   'RPP1MT NOT RECOGNIZED : ',I3)
C
      END
