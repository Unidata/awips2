C MODULE SFWGHT
C-----------------------------------------------------------------------
C
      SUBROUTINE SFWGHT (IRTYPE,AREAID,IPARM,ITYPE,NSEGS,LFACTR,IY,IXB,
     *   IXE,MSTAS,XC,YC,POWER,STMNWT,NSTAS,STAID,STAWT,IPT,STACC,
     *   ISTAT)
C
C  THIS ROUTINE COMPUTES GRID POINT, THIESSEN, 1/D**POWER OR 1/D**2 
C  WEIGHTS.
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwkx'
C
      CHARACTER*8 AREAID
      DIMENSION IY(NSEGS),IXB(NSEGS),IXE(NSEGS)
      DIMENSION STAID(2,MSTAS),STAWT(MSTAS),IPT(MSTAS),STACC(2,MSTAS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfwght.f,v $
     . $',                                                             '
     .$Id: sfwght.f,v 1.3 2000/03/13 16:27:57 page Exp $
     . $' /
C    ===================================================================
C
      DATA BLNK/4H    /
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SFWGHT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('WGHT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      'IPARM=',IPARM,' ITYPE=',ITYPE,' NSEGS=',NSEGS,
     *      ' LFACTR=',LFACTR,' MSTAS=',MSTAS
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NUMERR=0
      NUMWRN=0
      IMNT=0
      FE=0.0
      IELEV=0.0
      STAID(1,1)=BLNK
      STAID(2,1)=BLNK
      STMIN=STMNWT
      DO 10 I=1,INWFIL
         WORKNW(I)=0.0
10       CONTINUE
C
C  DESCRIPTION OF STATION WEIGHTING VARIABLE VALUES
C
C     IPARM     ITYPE     DESCRIPTION
C     -----     -----     -----------
C       1         3       MAP TIMING - 1/D**POWER
C       1         4       PCPN - 1/D**2
C       2         1       MAP STATION - GRID
C       2         2       MAP STATION - THIESSEN
C       2         3       MAP STATION - 1/D**POWER
C       2         4       PCPN 1/D**2
C       3         1       MAT STATION - GRID
C       3         3       MAT STATION - 1/D**POWER
C       4         4       TEMP MAX/MIN - 1/D+FE*DE
C       5         4       TEMP INST - 1/D+FE*DE
C       6         4       TEMP FUT - 1/D+FE*DE
C       7         3       MAPE STATION - 1/D**POWER
C
C  DETERMINE NUMBER OF STATIONS TO BE STORED
      NN=0
      IF (IPARM.EQ.1.OR.IPARM.EQ.2) NN=NPP24
      IF (IPARM.EQ.3) NN=NTA24
      IF (IPARM.EQ.4) NN=NTA24
      IF (IPARM.EQ.5) NN=NTAIN
      IF (IPARM.EQ.6) NN=NTF24
      IF (IPARM.EQ.7) NN=NEA24
      IF (NN.EQ.0) NN=INWFIL
      IF (ITYPE.NE.3) GO TO 50
         IF (IPARM.EQ.1.OR.IPARM.EQ.2) GO TO 20
            GO TO 30
20    NN=20
      GO TO 60
30    IF (IPARM.EQ.3.OR.IPARM.EQ.7) GO TO 40
         GO TO 60
40    IF (IPARM.EQ.3) NN=10
      IF (IPARM.EQ.7) NN=5
50    IF (ITYPE.NE.4) GO TO 60
         NN=4
60    IF (ITYPE.EQ.1.AND.IPARM.EQ.2) NN=99
      IF (ITYPE.EQ.2.AND.IPARM.EQ.2) NN=99
      IF (ITYPE.EQ.1.AND.IPARM.EQ.3) NN=50
      IPM=IABS(IPARM)
C
C  SET NUMBER OF STATIONS PER QUADRANT
      NSTATQ=1
C
C  CALL ROUTINE TO COMPUTE WT OR BRANCH TO LOOPING ALGORITHM
      IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) GO TO 80
C
C  CHANGE CENTROID TO INTEGER
      IXC=XC*10.+0.5
      IYC=YC*10.+0.5
C
      IF (ITYPE.EQ.3) GO TO 70
C
      CALL SFQUAD (IRTYPE,IXC,IYC,STAID,NSTATQ,IPM,ITYPE,IMNT,FE,
     *   IELEV,IPT,ISTAT)
      GO TO 120
C
70    CALL SFPOWR (IXC,IYC,IPM,ITYPE,POWER,ISTAT)
      GO TO 120
C
C  PROCESS EACH GRID SEGMENT GRID
80    DO 110 J=1,NSEGS
         IXXB=IXB(J)
         IXXE=IXE(J)
         DO 100 I=IXXB,IXXE,LFACTR
            IX=I*10
            IYY=IY(J)*10
            IF (ITYPE.NE.1) GO TO 90
               CALL SFQUAD (IRTYPE,IX,IYY,STAID,NSTATQ,IPM,ITYPE,IMNT,
     *            FE,IELEV,IPT,ISTAT)
               GO TO 100
90          CALL SFPOWR (IX,IYY,IPM,ITYPE,POWER,ISTAT)
100         CONTINUE
110      CONTINUE
C
120   IF (MSTAS.GE.NN) GO TO 130
         WRITE (LP,480) MSTAS,AREAID,NN
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 440
C
C  DETERMINE TOP NN WEIGHTS
130   DO 140 I=1,NN
         STAWT(I)=0.0
         IPT(I)=0
140      CONTINUE
C
      NSTAS=0
      DO 160 I=1,INWFIL
         IF (WORKNW(I).LE.STAWT(NN)) GO TO 160
            STAWT(NN)=WORKNW(I)
            IPT(NN)=I
            NSTAS=NSTAS+1
            NNN=NN-1
            DO 150 J=1,NNN
               K=NN-J
               L=NN+1-J
               IF (WORKNW(I).LE.STAWT(K)) GO TO 160
                  TEMP=STAWT(K)
                  STAWT(K)=STAWT(L)
                  STAWT(L)=TEMP
                  ITEMP=IPT(K)
                  IPT(K)=IPT(L)
                  IPT(L)=ITEMP
150               CONTINUE
160      CONTINUE
C
      SUM=0.0
      DO 170 I=1,NN
         SUM=SUM+STAWT(I)
170      CONTINUE
C
      IF (SUM.GT.0.0) GO TO 180
         WRITE (LP,500) AREAID
         CALL SUERRS (LP,2,NUMERR)
         WRITE (LP,520)
         CALL SULINE (LP,1)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,510) IRTYPE,ITYPE,IPARM
            CALL SULINE (IOSDBG,1)
            ENDIF
         ISTAT=2
         GO TO 440
C
180   IF (ITYPE.EQ.4) GO TO 270
      IF (ITYPE.NE.1.AND.ITYPE.NE.2) GO TO 200
      IF (IPARM.NE.2.AND.IPARM.NE.3) GO TO 200
C
C  CHECK IF WEIGHTING LIMIT NEEDS TO BE CHANGED FOR LARGE AREAS
      NWT=0
      DO 190 I=1,INWFIL
         IF (WORKNW(I).GT.0.) NWT=NWT+1
190      CONTINUE
      WTLMT=.5/STMIN
      IF (NWT.LE.WTLMT) GO TO 200
         STMIN=.5/NWT
         WRITE (LP,530) AREAID,STMNWT,STMIN
         CALL SUWRNS (LP,2,NUMWRN)
         WRITE (LP,540) NN
         CALL SULINE (LP,1)
C
C  NORMALIZE TOP NN WEIGHTS
200   NSTAS=0
      DO 220 I=1,NN
         STAWT(I)=STAWT(I)/SUM
         IF (IPARM.EQ.-2) GO TO 210
            IF (STAWT(I).LT.STMIN) GO TO 230
210      IF (STAWT(I).LT.0.0) GO TO 230
         NSTAS=NSTAS+1
220      CONTINUE
C
C  RENORMALIZE WEIGHTS IF ANY WEIGHTS WERE DROPPED
230   IF (NSTAS.EQ.NN) GO TO 270
      SUM=0.0
C
      DO 240 I=1,NSTAS
         SUM=SUM+STAWT(I)
240      CONTINUE
C
      DO 250 I=1,NSTAS
         STAWT(I)=STAWT(I)/SUM
250      CONTINUE
C
C  ZERO WEIGHTS FOR STATIONS DROPPED
      N1=NSTAS+1
      IF (N1.GT.NN) GO TO 270
         DO 260 I=N1,NN
            STAWT(I)=0.0
260         CONTINUE
C
270   IF (IPARM.EQ.-2) GO TO 420
C
      DO 280 I=1,NSTAS
         STAID(1,I)=STIDNW(1,IPT(I))
         STAID(2,I)=STIDNW(2,IPT(I))
         STACC(1,I)=CORDNW(1,IPT(I))/10.
         STACC(2,I)=CORDNW(2,IPT(I))/10.
280      CONTINUE
C
C  STORE POINTERS FOR STATION DATA
C
      GO TO (290,310,340,340,360,380,400),IPARM
C
290   DO 300 I=1,NSTAS
         IPT(I)=PPVRNW(IPT(I))
300      CONTINUE
      GO TO 420
C
310   DO 330 I=1,NSTAS
         IF (PP24NW(IPT(I)).GT.99999) THEN
            IPT(I)=PP24NW(IPT(I))-99999
            GO TO 330
            ENDIF
         IPM=PP24NW(IPT(I))
         ILOC=IABS(IPM)
         IPT(I)=ILOC
330      CONTINUE
      GO TO 420
C
340   DO 350 I=1,NSTAS
         IPM=TA24NW(IPT(I))
         ILOC=IABS(IPM)
         IPT(I)=ILOC
350      CONTINUE
      GO TO 420
C
360   DO 370 I=1,NSTAS
         IPT(I)=TAINNW(IPT(I))
370      CONTINUE
      GO TO 420
C
380   DO 390 I=1,NSTAS
         IPT(I)=TF24NW(IPT(I))
390      CONTINUE
      GO TO 420
C
400   DO 410 I=1,NSTAS
         IPT(I)=EA24NW(IPT(I))
410      CONTINUE
C
420   IF (LDEBUG.GT.0) THEN
         DO 430 I=1,NSTAS
            WRITE (IOSDBG,
     *         '(1X,A,I4,1X,A,2A4,
     *           1X,A,F8.5,1X,A,I5,2(1X,A,F7.2))'
     *           )
     *         'I=',I,'STAID=',STAID(1,I),STAID(2,I),
     *         'STAWT(I)=',STAWT(I),'IPT(I)=',IPT(I),
     *         'STACC(1,I)=',STACC(1,I),'STACC(2,I)=',STACC(2,I)
            CALL SULINE (IOSDBG,1)
430         CONTINUE
         ENDIF
C
440   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SFWGHT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
480   FORMAT ('0*** ERROR - IN SFWGHT - ARRAY DIMENSION (',
     *   I5,') TOO SMALL TO PERFORM STATION WEIGHTING ',
     *   'FOR AREA ',A,'. ',I5,' WORDS ARE NEEDED.')
500   FORMAT ('0*** ERROR - IN SFWGHT - NO STATION WEIGHTS COULD BE ',
     *   'COMPUTED FOR AREA ',A,' BECAUSE NO STATIONS WITH WEIGHT ',
     *   'WERE FOUND.')
510   FORMAT (T16,'IRTYPE=',I2,3X,'IPARM=',I2,3X,'ITYPE=',I2)
520   FORMAT (T26,'THIS ERROR MAY BE CAUSED BECAUSE NETWORK HAS NOT ',
     *   'BEEN RUN OR NO STATIONS OF NEEDED TYPE ARE DEFINED.')
530   FORMAT ('0*** WARNING - MINIMUM WEIGHT OF STATIONS FOR STATION ',
     *   'WEIGHTING FOR AREA ',A,' (',F5.3,') CHANGED TO ',F6.4)
540   FORMAT (T16,'BECAUSE AREA IS LARGE AND HAS MANY STATIONS. ',
     *   'A MAXIMUM OF ',I3,' STATIONS WILL BE USED.')
C
      END
