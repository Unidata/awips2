C MODULE SNTEMP
C-----------------------------------------------------------------------
C
C  ROUTINE TO PERFORM NETWORK COMPUTATIONS FOR TEMP STATIONS.
C
      SUBROUTINE SNTEMP (IRTYPE,IALL,IOPTN,LARRAY,ARRAY,INWFLG,
     *   IUEND,ISTAT)
C
      CHARACTER*4 WDISP,UNITS
      CHARACTER*8 TYPERR
      CHARACTER*8 TSTAID
      CHARACTER*50 STRING
      CHARACTER*100 HEADER/' '/
C
      REAL*4 RSTAID(2)    
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION INWFLG(*)
      DIMENSION INWPTR(4,3),UNUSED(5)
C      
      INCLUDE 'scommon/dimsta'
      INCLUDE 'scommon/dimtemp'
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_network/RCS/sntemp.f,v $
     . $',                                                             '
     .$Id: sntemp.f,v 1.3 2002/02/11 21:03:55 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('NTWK','NTWKTEMP','SNTEMP  ',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SNTEMP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('NTWK','NTWKTEMP','SNTEMP  ',LDEBUG)
C
      ISTAT=0
      NUMERR=0
C
C  PRINT HEADER
      WRITE (LP,390)
      CALL SULINE (LP,1)
      IF (INWFLG(3).EQ.0.AND.INWFLG(4).EQ.0.AND.INWFLG(5).EQ.0) THEN
         WRITE (LP,400)
         CALL SULINE (LP,2)
         GO TO 80
         ENDIF
      NPOS=1
      IF (INWFLG(3).EQ.1) THEN
         CALL SUBSTR ('3',1,1,HEADER,NPOS)
         NPOS=NPOS+2
         CALL SUBSTR ('CLOSEST',1,7,HEADER,NPOS)
         NPOS=NPOS+8
         CALL SUBSTR ('MXMN',1,4,HEADER,NPOS)
         NPOS=NPOS+5
         ENDIF
      IF (INWFLG(4).EQ.1) THEN
         IF (INWFLG(3).EQ.1.OR.INWFLG(5).EQ.1) THEN
            IF (INWFLG(5).EQ.1) THEN
               NPOS=NPOS-1
               CALL SUBSTR (',',1,1,HEADER,NPOS)
               NPOS=NPOS+2
               ELSE
                  IF (INWFLG(3).EQ.1) THEN
                     CALL SUBSTR ('AND',1,3,HEADER,NPOS)
                     NPOS=NPOS+4
                     ENDIF
               ENDIF
            ENDIF
         CALL SUBSTR ('3',1,1,HEADER,NPOS)
         NPOS=NPOS+2
         CALL SUBSTR ('CLOSEST',1,7,HEADER,NPOS)
         NPOS=NPOS+8
         CALL SUBSTR ('INST',1,4,HEADER,NPOS)
         NPOS=NPOS+5
         ENDIF
      IF (INWFLG(5).EQ.1) THEN
         IF (INWFLG(3).EQ.1.OR.INWFLG(4).EQ.1) THEN
            CALL SUBSTR ('AND',1,3,HEADER,NPOS)
            NPOS=NPOS+4
            ENDIF
         CALL SUBSTR ('2',1,1,HEADER,NPOS)
         NPOS=NPOS+2
         CALL SUBSTR ('CLOSEST',1,7,HEADER,NPOS)
         NPOS=NPOS+8
         CALL SUBSTR ('FMM ',1,4,HEADER,NPOS)
         NPOS=NPOS+5
         ENDIF
      STRING='STATIONS PER QUADRANT.'
      CALL SUBSTR (STRING,1,LENSTR(STRING),HEADER,NPOS)
      WRITE (LP,420) HEADER
      CALL SULINE (LP,2)
C
80    WRITE (LP,390)
      CALL SULINE (LP,1)
C
      NSTA=0
C
C  CHECK IF ANY TEMP PARAMETERS DEFINED
      CALL SUPPPN ('TEMP',NUMDEF,IERR)
      IF (NUMDEF.EQ.0) THEN
         WRITE (LP,410)
         GO TO 370
         ENDIF
C
      IPTR=0         
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF SUFFICIENT CPU TIME AVAILABLE
100   ICKRGN=0
      MINRGN=0
      ICKCPU=1
      MINCPU=10
      IPRERR=1
      IPUNIT=LP
      TYPERR='ERROR'
      INCLUDE 'clugtres'
      IF (IERR.GT.0) THEN
         CALL SUFATL
         IUEND=1
         GO TO 375
         ENDIF
C
C  READ TEMP PARAMETER RECORD
      CALL UREPET (' ',STAID,8)
      IREAD=1
      INCLUDE 'scommon/callsrtemp'
      IPTRN1=IPTRNX
      IF (IERR.EQ.2.AND.NSTA.EQ.0) GO TO 370
      IF (IERR.EQ.6) GO TO 370
      IF (IERR.GT.0) THEN
         CALL SRPPST (STAID,'TEMP',IPTR,LARRAY,0,IPTRNX,IERR)
         WRITE (LP,430) IERR
         CALL SUERRS (LP,2,NUMERR)
         GO TO 360
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
C     PRINT TEMP PARAMETERS
         IPRNT=1
         UNITS='ENGL'
         LEVEL=1
         IF (LDEBUG.GT.0) LEVEL=2
         INCLUDE 'scommon/callsptemp'
         ENDIF
C
C  FIND STATION IN NTWK COMMON BLOCK
      DO 140 IPOS=1,INWFIL
         CALL UMEMOV (STAID,RSTAID,2)
         IF (RSTAID(1).EQ.STIDNW(1,IPOS).AND.
     *       RSTAID(2).EQ.STIDNW(2,IPOS)) GO TO 150
140      CONTINUE
         WRITE (LP,440) STAID
         CALL SULINE (LP,1)
         GO TO 350
C
150   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,450) STAID,IPOS,TA24NW(IPOS)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF TA24 POINTER IS ZERO
      IF (TA24NW(IPOS).NE.0) GO TO 160
         WRITE (LP,460) STAID
         CALL SUWRNS (LP,2,-1)
         WRITE (LP,470)
         CALL SULINE (LP,1)
         GO TO 350
C
C  SET STATION LAT/LON AND ELEVATION
160   LOCX=CORDNW(1,IPOS)
      LOCY=CORDNW(2,IPOS)
      IELEV=ELEVNW(IPOS)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      ITYPE=4
C
      IF (INWFLG(3).EQ.0.AND.IALL.EQ.-1.AND.IOPTN.EQ.0) GO TO 220
C
C  FIND 3 CLOSEST MAX/MIN TEMP STATIONS PER QUADRANT
      NSTQ=3
      INPARM=4
C
C  INITIALIZE WORK ARRAY
      DO 170 I=1,INWFIL
         WORKNW(I)=0.0
170      CONTINUE
C
      CALL SFQUAD (IRTYPE,LOCX,LOCY,STAID,NSTQ,INPARM,ITYPE,IMTN,
     *   TEMPFE,IELEV,INWPTR,IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,480)
         CALL SULINE (IOSDBG,2)
         WRITE (IOSDBG,510)
         CALL SULINE (IOSDBG,2)
         DO 185 J=1,4
            DO 180 I=1,NSTQ
               K=INWPTR(J,I)
               IF (K.EQ.0) GO TO 180
               WRITE (IOSDBG,520) (STIDNW(N,K),N=1,2),
     *            (CORDNW(N,K),N=1,2),
     *            PP24NW(K),PPVRNW(K),TA24NW(K),TAINNW(K),TF24NW(K),
     *            ELEVNW(K),EA24NW(K),PCHRNW(K)
               CALL SULINE (IOSDBG,1)
180            CONTINUE
185         CONTINUE   
         ENDIF         
C
C  INITIALIZE POINTERS AND WEIGHTS
      DO 205 I=1,NSTQ
         DO 200 J=1,4
            IPDMMT(I,J)=0
            WTMMT(I,J)=0.0
200         CONTINUE
205      CONTINUE
C
C  STORE POINTERS AND WEIGHTS
      DO 215 I=1,NSTQ
         DO 210 J=1,4
            INWLOC=INWPTR(J,I)
            IF (INWLOC.EQ.0) GO TO 210
               IPDMMT(I,J)=TA24NW(INWLOC)
               IF (LDEBUG.GT.0.AND.IPDMMT(I,J).EQ.441) THEN
                  CALL UMEMOV (STIDNW(1,INWLOC),TSTAID,2)
                  WRITE (IOSDBG,*) 'IN SNTEMP -',
     *               ' IPDMMT(I,J)=',IPDMMT(I,J),
     *               ' INWLOC=',INWLOC,' TSTAID=',TSTAID
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               WTMMT(I,J)=WORKNW(INWLOC)
210         CONTINUE
215      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
220   IF (INWFLG(4).EQ.0.AND.IALL.EQ.-1.AND.IOPTN.EQ.0) GO TO 280
C
C  FIND 3 CLOSEST INST TEMP STATIONS PER QUADRANT
      NSTQ=3
      INPARM=5
C
C  INITIALIZE WORK ARRAY.
      DO 230 I=1,INWFIL
         WORKNW(I)=0.0
230      CONTINUE
C
      CALL SFQUAD (IRTYPE,LOCX,LOCY,STAID,NSTQ,INPARM,ITYPE,IMTN,
     *   TEMPFE,IELEV,INWPTR,IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,490)
         CALL SULINE (IOSDBG,2)
         WRITE (IOSDBG,510)
         CALL SULINE (IOSDBG,2)
         DO 245 J=1,4
            DO 240 I=1,NSTQ
               K=INWPTR(J,I)
               IF (K.EQ.0) GO TO 240
               WRITE (IOSDBG,520) (STIDNW(N,K),N=1,2),
     *            (CORDNW(N,K),N=1,2),
     *            PP24NW(K),PPVRNW(K),TA24NW(K),TAINNW(K),TF24NW(K),
     *            ELEVNW(K),EA24NW(K),PCHRNW(K)
               CALL SULINE (IOSDBG,1)
240            CONTINUE
245         CONTINUE                        
         ENDIF
C
C  INITIALIZE POINTERS AND WEIGHTS
      DO 265 I=1,NSTQ
         DO 260 J=1,4
            IPDINS(I,J)=0
            WTINS(I,J)=0.0
260         CONTINUE
265      CONTINUE
C
C  STORE POINTERS AND WEIGHTS
      DO 275 I=1,NSTQ
         DO 270 J=1,4
            INWLOC=INWPTR(J,I)
            IF (INWLOC.EQ.0) GO TO 270
               IPDINS(I,J)=TAINNW(INWLOC)
               IF (LDEBUG.GT.0.AND.IPDINS(I,J).EQ.1) THEN
                  CALL UMEMOV (STIDNW(1,INWLOC),TSTAID,2)
                  WRITE (IOSDBG,*) 'IN SNTEMP -',
     *               'IPDINS(I,J)=',IPDINS(I,J),
     *               ' INWLOC=',INWLOC,' TSTAID=',TSTAID
                  CALL SULINE (IOSDBG,1)
                  ENDIF
               WTINS(I,J)=WORKNW(INWLOC)
270         CONTINUE
275      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
280   IF (INWFLG(5).EQ.0.AND.IALL.EQ.-1.AND.IOPTN.EQ.0) GO TO 340
C
C  FIND 2 CLOSEST FCST TEMP STATIONS PER QUADRANT
      NSTQ=2
      INPARM=6
C
C  INITIALIZE WORK ARRAY.
      DO 290 I=1,INWFIL
290   WORKNW(I)=0.0
C
      CALL SFQUAD (IRTYPE,LOCX,LOCY,STAID,NSTQ,INPARM,ITYPE,IMTN,
     *   TEMPFE,IELEV,INWPTR,IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,500)
         CALL SULINE (IOSDBG,2)
         WRITE (IOSDBG,510)
         CALL SULINE (IOSDBG,2)
         DO 305 J=1,4
            DO 300 I=1,NSTQ
               K=INWPTR(J,I)
               IF (K.EQ.0) GO TO 300
               WRITE (IOSDBG,520) (STIDNW(N,K),N=1,2),
     *            (CORDNW(N,K),N=1,2),
     *            PP24NW(K),PPVRNW(K),TA24NW(K),TAINNW(K),TF24NW(K),
     *            ELEVNW(K),EA24NW(K),PCHRNW(K)
               CALL SULINE (IOSDBG,1)
300            CONTINUE
305         CONTINUE
         ENDIF
C
C  INITIALIZE POINTERS AND WEIGHTS
      DO 325 I=1,NSTQ
         DO 320 J=1,4
            IPDFMM(I,J)=0
            WTFMM(I,J)=0.0
320         CONTINUE
325      CONTINUE
C
C  STORE POINTERS AND WEIGHTS
      DO 335 I=1,NSTQ
         DO 330 J=1,4
            INWLOC=INWPTR(J,I)
            IF (INWLOC.EQ.0) GO TO 330
               IPDFMM(I,J)=TF24NW(INWLOC)
               WTFMM(I,J)=WORKNW(INWLOC)
330         CONTINUE
335      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  UPDATE PARAMETERS
340   ITNTWK=2
      UNSD=-999.
      WDISP='OLD'
      NSPACE=0
      IWRITE=1
      INCLUDE 'scommon/callswtemp'
      IF (IERR.GT.0) GO TO 350
C
      NSTA=NSTA+1
C
C  UPDATE NETWORK INDICATOR IN NTWK COMMON BLOCK
      IPNTWK=SFLGNW(IPOS)/10
      SFLGNW(IPOS)=IPNTWK*10+ITNTWK
C
      IF (LDEBUG.GT.0) THEN
C     READ PARAMETERS
         IREAD=1
         INCLUDE 'scommon/callsrtemp'
         IF (IERR.GT.0) GO TO 350
C     PRINT PARAMETERS
         INCLUDE 'scommon/callsptemp'
         ENDIF
C
350   IF (IPTRN1.EQ.0) GO TO 370
         IPTR=IPTRN1
         GO TO 100
C
360   ISTAT=1
C
370   WRITE (LP,530) NSTA
      CALL SULINE (LP,2)
C
375   IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SNTEMP : ISTAT=',I2ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
390   FORMAT (' ')
400   FORMAT ('0*--> FIND 3 CLOSEST MXMN, 3 CLOSEST INST ',
     *   'AND 2 CLOSEST FCST TEMP STATIONS PER QUADRANT.')
410   FORMAT ('0*** NOTE - NO TEMP PARAMETERS ARE DEFINED.')
420   FORMAT ('0*--> FIND ',A)
430   FORMAT ('0*** ERROR - IN SNTEMP - UNSUCCESSFUL CALL TO SRTEMP : ',
     *   'STATUS CODE=',I2)
440   FORMAT ('0*** NOTE - STATION ',A,' NOT FOUND IN NETWORK ',
     *   'COMMON BLOCK. STATION MAY BE INCOMPLETE.')
450   FORMAT (' STAID=',A,3X,'IPOS=',I5,3X,'TA24NW(IPOS)=',I5)
460   FORMAT ('0*** WARNING - TA24 POINTER FOR STATION ',A,' ',
     *   'IS ZERO. TEMP PARAMETERS EXIST BUT STATION GENL ',
     *   'PARAMETERS')
470   FORMAT (T16,'DO NOT CONTAIN TEMP AS A DATA GROUP.')
480   FORMAT ('0CONTENTS OF NETWORK ARRAYS FOR 3 CLOSEST MXMN TEMP ',
     *   'STATIONS PER QUADRANT')
490   FORMAT ('0CONTENTS OF NETWORK ARRAYS FOR 3 CLOSEST INST TEMP ',
     *   'STATIONS PER QUADRANT')
500   FORMAT ('0CONTENTS OF NETWORK ARRAYS FOR 2 CLOSEST FCST TEMP ',
     *   'STATIONS PER QUADRANT')
510   FORMAT ('0',T5,'STIDNW  ',4X,'CORDNW',12X,
     *   'PP24NW',3X,'PPVRNW',3X,
     *   'TA24NW',3X,'TAINNW',3X,'TF24NW',3X,'ELEVNW',3X,
     *   'EA24NW',3X,'PCHRNW')
520   FORMAT (T5,2A4,10(3X,I6))
530   FORMAT ('0*** NOTE - ',I4,' STATIONS WITH TEMP PARAMETERS ',
     *   'SUCCESSFULLY PROCESSED.')
C
      END
