C MODULE SFADRV
C-----------------------------------------------------------------------
C
      SUBROUTINE SFADRV (AREAID,ARRAY,LARRAY,IPARM,ITYPE,POWER,STMNWT,
     *   NSEGS,LFACTR,IY,IXB,IXE,XC,YC,MSTAS,NSTAS,STAID,STAWT,IPT,
     *   STACC,ISTAT)
C
C  THIS ROUTINE IS THE DRIVER ROUTINE FOR THE COMPUTATION OF STATION 
C  WEIGHTS FOR THE MAP, MAT AND MAPE PREPROCESSORS.
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwkx'
C
      CHARACTER*8 AREAID
      DIMENSION ARRAY(LARRAY)
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
      DIMENSION STAID(2,MSTAS),STAWT(MSTAS),IPT(MSTAS),STACC(2,MSTAS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfadrv.f,v $
     . $',                                                             '
     .$Id: sfadrv.f,v 1.2 1999/07/06 11:47:16 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SFADRV'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('WGHT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IPARM=',IPARM,' ITYPE=',ITYPE,
     *      ' POWER=',POWER,' NSEGS=',NSEGS,' LFACTR=',LFACTR,
     *      ' MSTAS=',MSTAS
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,'(2(1X,A,F7.2))') 'XC=',XC,'YC=',YC
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NUMERR=0
C
C  CHECK IF NETWORK COMMON BLOCK HAS BEEN FILLED
      IF (INWTYP.NE.2) GO TO 10
      IF (INWFIL.GT.0) GO TO 30
      IF (INWFIL.EQ.0) GO TO 10
         WRITE (LP,110)
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 90
C
C  FILL NETWORK COMMON BLOCK
10    IRTYPE=2
      CALL SNSTAN (IRTYPE,LARRAY,ARRAY,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,120)
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 90
         ENDIF
C
C  CHECK IF ANY STATIONS STORED IN COMMON BLOCK
      IF (INWFIL.EQ.0) THEN
         WRITE (LP,130)
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 90
         ENDIF
C
30    IF (LDEBUG.GT.0) THEN
         DO 40 I=1,INWFIL
            WRITE (LP,140) STIDNW(1,I),STIDNW(2,I),
     *         CORDNW(1,I),CORDNW(2,I),
     *         PP24NW(I),PPVRNW(I),
     *         TA24NW(I),TAINNW(I),TF24NW(I),
     *         EA24NW(I)
            CALL SULINE (LP,1)
40          CONTINUE
         ENDIF
C
C  CHECK FOR INVALID IPARM VALUE
      IF (IPARM.GT.7) THEN
         WRITE (LP,150) 'IPARM',IPARM
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 90
         ENDIF
C
C  CHECK FOR INVALID ITYPE VALUE
      IF (ITYPE.GT.4) THEN
         WRITE (LP,150) 'ITYPE',ITYPE
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 90
         ENDIF
C
C  CALL ROUTINE TO PERFORM STATION WEIGHT COMPUTATIONS
      IRTYPE=2
      CALL SFWGHT (IRTYPE,AREAID,IPARM,ITYPE,NSEGS,LFACTR,IY,IXB,IXE,
     *   MSTAS,XC,YC,POWER,STMNWT,NSTAS,STAID,STAWT,IPT,STACC,ISTAT)
C
      IF (LDEBUG.GT.0) THEN
         DO 80 I=1,NSTAS
            WRITE (IOSDBG,
     *         '(1X,A,I4,1X,A,2A4,
     *           1X,A,F8.5,1X,A,I5,2(1X,A,F7.2))'
     *           )
     *         'I=',I,'STAID=',STAID(1,I),STAID(2,I),
     *         'STAWT(I)=',STAWT(I),'IPT(I)=',IPT(I),
     *         'STACC(1,I)=',STACC(1,I),'STACC(2,I)=',STACC(2,I)
            CALL SULINE (IOSDBG,1)
80          CONTINUE
         ENDIF
C
90    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SFADRV'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT ('0*** ERROR - IN SFADRV - MEAN AREAL PROCESSING CANNOT ',
     *   'BE COMPLETED BECAUSE NETWORK ARRAYS ARE TOO SMALL.')
120   FORMAT ('0*** ERROR - IN SFADRV - NETWORK ARRAY COULD NOT BE ',
     *   'FILLED.')
130   FORMAT ('0*** ERROR - IN SFADRV - NO STATIONS WITH COMPLETE ',
     *   'DEFINITIONS FOUND. STATION WEIGHT PROCESSING STOPPED.')
140   FORMAT (' ',2A4,8I10)
150   FORMAT ('0*** ERROR - IN SFADRV - INVALID ',A,' VALUE : ',I4)
C
      END
