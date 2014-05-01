C MODULE SPPCPN
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATION PCPN PARAMETERS.
C
      SUBROUTINE SPPCPN (IPRNT,UNITS,LEVEL,IPREST,IVPCPN,UNUSED,
     *   STAID,NUMBER,DESCRP,STATE,STALOC,STACOR,
     *   IPPROC,IPTIME,MDRBOX,PCPNCF,IPTWGT,IPNTWK,IPSWGT,IPCHAR,
     *   IPD5PT,STA5WT,STASID,IPDSPT,STASWT,IPD3PT,STA3WT,
     *   ISTAT)
C
      CHARACTER*4 UNITS,DEGMIN
      CHARACTER*4 TUNITS,UNITSP,PWGT,PCHR,PMDR,PCODE
      CHARACTER*8 TYPMSG
      CHARACTER*8 ESTID
      CHARACTER*20 STASDS,ESTDSC
C
      DIMENSION UNUSED(1)
      DIMENSION MINDEG(4)
      DIMENSION STASCO(2)
C      
      INCLUDE 'scommon/dimstan'
      INCLUDE 'scommon/dimpcpn'
      DIMENSION PXCHRN(MPXCHR)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/sppcpn.f,v $
     . $',                                                             '
     .$Id: sppcpn.f,v 1.2 1998/04/10 16:18:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,320)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('PCPN')
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
      IF (IPRNT.EQ.0.AND.LDEBUG.EQ.0) GO TO 10
         IF (IPRNT.EQ.0) THEN
            WRITE (LP,330)
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.1) THEN
            WRITE (LP,360) STAID
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.2) THEN
            WRITE (LP,380) DESCRP
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.3) THEN
            WRITE (LP,370) NUMBER
            CALL SULINE (LP,2)
            ENDIF
         WRITE (LP,350)
         CALL SULINE (LP,2)
         GO TO 20
C
10    WRITE (LP,330)
      CALL SULINE (LP,2)
      WRITE (LP,350)
      CALL SULINE (LP,2)
      GO TO 50
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
20    IF (LDEBUG.EQ.0) GO TO 30
         WRITE (LP,390) IVPCPN
         CALL SULINE (LP,2)
C
30    IF (LDEBUG.GT.0) GO TO 40
         IF (IPRNT.EQ.0) GO TO 50
C
C  PRINT PCPN STATION IDENTIFIER, DESCRIPTIVE INFORMATION AND
C  STATION NUMBER
40    IF (NUMBER.GT.0) THEN
         WRITE (LP,400) STAID,DESCRP,STATE,NUMBER
         CALL SULINE (LP,2)
         ENDIF
      IF (NUMBER.EQ.0) THEN
         WRITE (LP,410) STAID,DESCRP,STATE
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT STATION LOCATION AND WEIGHTING INDICATOR
50    PWGT='WGT'
      IF (IPSWGT.EQ.0) PWGT='NWGT'
      IF (IPRNT.EQ.0) GO TO 60
         IF (DEGMIN.EQ.'NO') THEN
            WRITE (LP,420) STALOC,PWGT
            CALL SULINE (LP,2)
            ENDIF
         IF (DEGMIN.EQ.'YES') THEN
            CALL SUDMDD ('DM  ',2,STALOC,MINDEG,IERR)
            WRITE (LP,430) MINDEG,PWGT
            CALL SULINE (LP,2)
            ENDIF
         GO TO 70
60    WRITE (LP,440) PWGT
      CALL SULINE (LP,2)
C
C  PRINT STATION COORDINATES (NWSRFS/HRAP)
70    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,450) STACOR
         CALL SULINE (IOSDBG,2)
         ENDIF
C
C  PRINT DATA TIME INTERVAL, PROCESSING CODE AND MDR USAGE OPTION
      PCODE='????'
      IF (IPPROC.EQ.0) PCODE='NORM'
      IF (IPPROC.EQ.1) PCODE='ZERO'
      IF (IPPROC.EQ.2) PCODE='SYN'
      PMDR='NMDR'
      IF (MDRBOX.GT.0) THEN
         PMDR='MDR'
         CALL SBNUMM (MDRBOX,NCOL,NROW,IERR)
         WRITE (LP,470) IPTIME,PCODE,PMDR,NROW,NCOL
         CALL SULINE (LP,2)
         GO TO 80
         ENDIF
      WRITE (LP,460) IPTIME,PCODE,PMDR
      CALL SULINE (LP,2)
C
C  PRINT TYPE OF 24-HOUR STATION STATION WEIGHTS
80    IF (IPTWGT.GT.0) GO TO 110
C
C  CHECK IF NETWORK HAS BEEN RUN ON STATION
      IF (IPNTWK.EQ.0) THEN
         WRITE (LP,720)
         CALL SULINE (LP,2)
         GO TO 130
         ENDIF
C
      IF (IPREST.EQ.0.AND.LDEBUG.EQ.0) GO TO 130
C
C  PRINT TYPE OF 24-HOUR WEIGHTS, PP24 POINTERS, WEIGHTS, QUADRANTS (D2)
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,480)
      CALL SULINE (LP,3)
      N=0
      DO 105 I=1,4
         DO 100 J=1,5
            IF (IPD5PT(J,I).EQ.0) GO TO 100
            N=N+1
            ITM=N
            CALL SUGTID (ITM,'PP24',IPD5PT(J,I),ESTID,ESTDSC,ESTST,
     *         IESTCH,LSWORK,SWORK(1),IERR)
            PCHR='NO'
            IF (IESTCH.GT.0) PCHR='YES'
            IF (IERR.GT.0) PCHR='????'
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,480)
               CALL SULINE (LP,3)
               ENDIF
            WRITE (LP,490) N,IPD5PT(J,I),ESTID,ESTDSC,STA5WT(J,I),I,
     *         PCHR
            CALL SULINE (LP,1)
100         CONTINUE
105      CONTINUE
      GO TO 130
C
C  PRINT 24-HOUR SIGNIFICANCE WEIGHTS, IDENTIFIER, WEIGHTS
110   IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,500)
      CALL SULINE (LP,3)
      NUMSTA=0
      DO 120 I=1,IPTWGT
         IF (STASID(I).EQ.' ') GO TO 120
         NUMSTA=NUMSTA+1
         TYPMSG='NOTE'
         CALL SUGTDS (STASID(I),STASDS,STATE,STASCO,STASEL,LSWORK,
     *      SWORK(1),TYPMSG,IERR)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,500)
            CALL SULINE (LP,3)
            ENDIF
         WRITE (LP,510) I,STASID(I),STASDS,STASWT(I)
         CALL SULINE (LP,1)
120      CONTINUE
C
C  PRINT <24-HOUR TIMING STATIONS IF NETWORK RUN
130   IF (IPNTWK.EQ.0) GO TO 160
      IF (LEVEL.NE.2) GO TO 160
      IF (IPTIME.EQ.24) GO TO 160
C
C  PRINT PPVR POINTER, WEIGHT, QUADRANT
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,520)
      CALL SULINE (LP,3)
      M=0
      DO 145 I=1,4
         DO 140 J=1,3
            IF (IPD3PT(J,I).EQ.0) GO TO 140
            M=M+1
            ITM=M
            CALL SUGTID (ITM,'PPVR',IPD3PT(J,I),ESTID,ESTDSC,ESTST,
     *         IESTCH,LSWORK,SWORK(1),IERR)
            IF (ISNWPG(LP).EQ.1) THEN
               WRITE (LP,520)
               CALL SULINE (LP,3)
               ENDIF
            WRITE (LP,490) M,IPD3PT(J,I),ESTID,ESTDSC,STA3WT(J,I),I
            CALL SULINE (LP,1)
140         CONTINUE
145      CONTINUE
C
C  PRINT ARRAY LOCATIONS OF POINTERS FOR PCPN DATA FOR STATION
      IF (LDEBUG.EQ.0) GO TO 160
      IF (IPTWGT.EQ.0) GO TO 160
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,610)
         CALL SULINE (LP,4)
         DO 150 I=1,IPTWGT
            WRITE (LP,620) I,IPDSPT(I)
            CALL SULINE (LP,1)
150         CONTINUE
C
C  PRINT CORRECTION FACTORS
160   IF (PCPNCF(1).EQ.1.) THEN
         WRITE (LP,550)
         CALL SULINE (LP,2)
         GO TO 190
         ENDIF
      IF (PCPNCF(2).EQ.-999.) THEN
         WRITE (LP,540) PCPNCF(1)
         CALL SULINE (LP,2)
         GO TO 190
         ENDIF
      WRITE (LP,530) PCPNCF
      CALL SULINE (LP,2)
C
C  PRINT ARRAY LOCATION OF CHARACTERISTICS FOR THIS STATION
190   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IPCHAR=',IPCHAR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT PRECIPITATION CHARACTERISTICS
      IF (IPCHAR.EQ.0) THEN
         WRITE (LP,640)
         CALL SULINE (LP,2)
         GO TO 250
         ENDIF
      CALL RPP1CH (IPCHAR,PXCHR,IERR)
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.1) THEN
            WRITE (LP,700)
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 250
            ENDIF
         IF (IERR.EQ.2) THEN
            WRITE (LP,710) IPCHAR
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 250
            ENDIF
         WRITE (LP,630) IERR
         CALL SUERRS (LP,2,-1)
         GO TO 250
         ENDIF
      NUMDEC=2
      ICHECK=0
      DO 220 I=1,MPXCHR
         PXCHRN(I)=PXCHR(I)
         IF (TUNITS.EQ.'METR') THEN
            CALL UDUCNV ('IN  ','MM  ',1,1,PXCHR(I),PXCHRN(I),IERR)
            ENDIF
         IF (PXCHRN(I).GT.99.999) THEN
            ICHECK=1
            NUMDEC=0
            GO TO 220
            ENDIF
         IF (ICHECK.EQ.0.AND.PXCHRN(I).LT.0.1) NUMDEC=3
220      CONTINUE
      UNITSP='IN'
      IF (UNITS.EQ.'METR') UNITSP='MM'
      IF (ICHECK.EQ.1) THEN
         WRITE (LP,680)
         CALL SULINE (LP,2)
         WRITE (LP,690) PXCHRN,UNITSP
         CALL SULINE (LP,2)
         ENDIF
      IF (NUMDEC.EQ.2.OR.NUMDEC.EQ.3) THEN
         WRITE (LP,650)
         CALL SULINE (LP,2)
         WRITE (LP,655)
         CALL SULINE (LP,1)
         IF (NUMDEC.EQ.2) THEN
            WRITE (LP,660) PXCHRN,UNITSP
            CALL SULINE (LP,2)
            ENDIF
         IF (NUMDEC.EQ.3) THEN
            WRITE (LP,670) PXCHRN,UNITSP
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  PRINT NETWORK STATUS
250   IF (IPNTWK.LT.0.OR.IPNTWK.GT.3) THEN
         WRITE (LP,555) IPNTWK
         CALL SUERRS (LP,2,-1)
         GO TO 310
         ENDIF
      IF (IPNTWK.EQ.0) THEN
         WRITE (LP,570)
         CALL SULINE (LP,2)
         GO TO 310
         ENDIF
      IF (IPNTWK.EQ.1) THEN
         WRITE (LP,575)
         CALL SULINE (LP,2)
         GO TO 310
         ENDIF
      IF (IPNTWK.EQ.2) THEN
         WRITE (LP,580)
         CALL SULINE (LP,2)
         GO TO 310
         ENDIF
      IF (IPNTWK.EQ.3) THEN
         WRITE (LP,590)
         CALL SULINE (LP,2)
         GO TO 310
         ENDIF
C
C  PRINT NUMBER OF UNUSED POSITIONS
310   NUNUSD=2
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,730) NUNUSD
         CALL SULINE (LP,2)
         ENDIF
C
      WRITE (LP,340)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,750) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
320   FORMAT (' *** ENTER SPPCPN')
330   FORMAT ('0*--> PCPN PARAMETERS')
340   FORMAT ('0',132('-'))
350   FORMAT (' ')
360   FORMAT ('0',59('-'),' ID=',A,2X,59('-'))
370   FORMAT ('0',59('-'),' NUMBER=',I4,2X,59('-'))
380   FORMAT ('0',52('-'),' DESC=',A,2X,52('-'))
390   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
400   FORMAT ('0*--> PCPN PARAMETERS :  ',
     *   'IDENTIFIER = ',A,5X,
     *   'DESCRIPTION = ',A,5X,
     *   'STATE = ',A2,5X,
     *   'NUMBER = ',I4)
410   FORMAT ('0*--> PCPN PARAMETERS :  ',
     *   'IDENTIFIER = ',A,5X,
     *   'DESCRIPTION = ',A,5X,
     *   'STATE = ',A2,5X,
     *   'NUMBER = ** NONE **')
420   FORMAT ('0LATITUDE = ',F6.2,3X,
     *   'LONGITUDE = ',F7.2,5X,
     *   'WEIGHTING INDICATOR = ',A)
430   FORMAT ('0LATITUDE = ',I2,'-',I2,3X,
     *   'LONGITUDE = ',I3,'-',I2,5X,
     *   'WEIGHTING INDICATOR = ',A)
440   FORMAT ('0WEIGHTING INDICATOR = ',A)
450   FORMAT ('0NWSRFS/HRAP COORDINATES: (',F5.1,',',F5.1,')')
460   FORMAT ('0DATA TIME INTERVAL = ',I2,' HOURS',6X,
     *   'PROCESSING CODE = ',A,6X,
     *   'MDR USAGE OPTION = ',A)
470   FORMAT ('0DATA TIME INTERVAL = ',I2,' HOURS',6X,
     *   'PROCESSING CODE = ',A,6X,
     *   'MDR USAGE OPTION = ',A,':  ROW=',I4,3X,'COL=',I3)
480   FORMAT ('0TYPE OF 24-HOUR WEIGHTS = D2',
     *    T35,'PP24 POINTER',5X,
     *      'IDENTIFIER',5X,
     *      'DESCRIPTION',14X,
     *      'WEIGHT',7X,
     *      'QUADRANT',5X,
     *      'CHARACTERISTICS' / 
     *   T35,12('-'),5X,
     *      10('-'),5X,
     *      20('-'),5X,
     *      8('-'),5X,
     *      8('-'),5X,
     *      15('-'))
490   FORMAT (T31,I2,4X,I5,10X,A,7X,A,5X,G8.3,9X,I1,14X,A4)
500   FORMAT ('0TYPE OF 24-HOUR WEIGHTS = SIG',T35,
     *      'IDENTIFIER',5X,
     *      'DESCRIPTION',14X,
     *      'WEIGHT' /
     *   T35,10('-'),5X,
     *       20('-'),5X,
     *       6('-'))
510   FORMAT (T31,I2,2X,A,7X,A,5X,F6.3)
520   FORMAT ('0TIMING WEIGHTS',T35,
     *   T35,'PPVR POINTER',5X,
     *      'IDENTIFIER',5X,
     *      'DESCRIPTION',14X,
     *      'WEIGHT',7X,
     *      'QUADRANT' /
     *   T35,12('-'),5X,
     *      10('-'),5X,
     *      20('-'),5X,
     *      8('-'),5X,
     *      8('-'))
530   FORMAT ('0CORRECTION FACTORS:  WINTER=',F5.2,3X,'SUMMER=',F5.2)
540   FORMAT ('0CORRECTION FACTOR = ',F5.2)
550   FORMAT ('0CORRECTION FACTOR : ** NOT DEFINED **')
555   FORMAT ('0*** ERROR - IN SPPCPN - INVALID VALUE OF NETWORK ',
     *   'INDICATOR : IPNTWK=',I2)
570   FORMAT ('0STATUS:  NETWORK NOT RUN - ',
     *   'CANNOT USE IN MAP AREA')
575   FORMAT ('0STATUS:  NETWORK RUN PREVIOUSLY - ',
     *   'CANNOT USE NEW MAP AREA')
580   FORMAT ('0STATUS:  NETWORK RUN - ',
     *   'CAN USE 24-HOUR DATA IN MAP AREA')
590   FORMAT ('0STATUS:  NETWORK RUN - ',
     *   'CAN USE BOTH 24-HOUR AND LESS THAN 24-HOUR DATA IN MAP AREA')
610   FORMAT ('0ARRAY LOCATION OF POINTERS FOR PCPN DATA:',10X,
     *   'POINTER' / 52X,7('-'))
620   FORMAT (45X,I2,6X,I5)
630   FORMAT ('0*** ERROR - IN SPPCPN - STATUS CODE FROM RPP1CH IS ',
     *   I3,'.')
640   FORMAT ('0PRECIPITATION CHARACTERISTICS : ** NOT DEFINED **')
650   FORMAT ('0CHARACTERISTICS:',T18,
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
655   FORMAT (' ',T18,
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
660   FORMAT ('0',T18,12(F6.2,1X),1X,A)
670   FORMAT ('0',T18,12(F6.3,1X),1X,A)
680   FORMAT ('0CHARACTERISTICS:  JAN',7X,'FEB',7X,'MAR',5X,'APR',7X,
     *   'MAY',5X,'JUN',7X,
     *   'JUL',5X,'AUG',7X,'SEP',5X,'OCT',7X,'NOV',6X,'DEC')
690   FORMAT ('0',17X,12(E8.3,1X),1X,A)
700   FORMAT ('0*** ERROR - IN SPPCPN - SYSTEM ERROR ACCESSING ',
     *   'PRECIPITATION CHARACTERISTICS FILE.')
710   FORMAT ('0*** ERROR - IN SPPCPN - ',I5,' IS AN INVALID VALUE ',
     *   'FOR THE LOCATION OF CHARACTERISTICS.')
720   FORMAT ('0TYPE OF 24-HOUR WEIGHTS = D2')
730   FORMAT ('0NUMBER OF UNUSED POSITIONS = ',I2)
750   FORMAT (' *** EXIT SPPCPN - ISTAT=',I2)
C
      END
