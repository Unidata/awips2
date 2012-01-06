C MODULE NCLMPX
C-----------------------------------------------------------------------
C
C  ROUTINE TO CALCULATE MAPX FOR EACH AREA.
C
      SUBROUTINE NCLMPX (MAPXCO,NMXFL,XGRD,NXGRD,IRADAR,
     &   IHRAP,IX,IY,NXA,IRELHR,NHRS,ITEMP,NITEMP)
C
C----------------------------------------------------------
C     WRITTEN BY R SHEDD (HRL) 11/90
C----------------------------------------------------------
C
      CHARACTER*8 OLDOPN,AREAID
      REAL*4     MAPXCO(NMXFL)
      INTEGER*2  XGRD(NXGRD),IRADAR(IX,IY),ITEMP(NITEMP)
      INTEGER    IHRAP(4)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mapx/RCS/nclmpx.f,v $
     . $',                                                             '
     .$Id: nclmpx.f,v 1.4 1999/07/06 15:52:15 page Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM = -1
      CALL FSTWHR ('NCLMPX  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'ENTER NCLMPX'
C
      IBUG = 0
      ICPBUG = 0
      IF (IPBUG('NCLX') .EQ. 1) IBUG = 1
      IF (IPBUG('NCPU') .EQ. 1) ICPBUG = 1
C
      IF (ICPBUG.GT.0) THEN
        ITOTCP = 1
        CALL URTIMR (LAPSCP,ITOTCP)
        TOTCP = FLOAT(ITOTCP)/100.
        WRITE (IOPDBG,60) 'START',NXA,IRELHR,TOTCP
        ENDIF
C
      IERR = 0
      IF (NXA.LT.1) THEN
         WRITE (IPR,70) NXA
         CALL ERROR
         IERR = 1
         ENDIF
      NCHK=NXA*3
      IF (NCHK.GT.NMXFL) THEN
         WRITE (IPR,80) NCHK,NMXFL
         CALL ERROR
         IERR = 1
         ENDIF
      IF (IRELHR.LT.1) THEN
         WRITE (IPR,90) IRELHR
         CALL ERROR
         IERR = 2
         ENDIF
      NCHK=(NXA-1)*NHRS+IRELHR
      IF (NCHK.GT.NITEMP) THEN
         WRITE (IPR,100) NCHK,NITEMP
         CALL ERROR
         IERR = 3
         ENDIF
      IF (IERR.GT.0) GO TO 50
C
C   CALCULATE MAPX FOR EACH AREA
C
      DO 40 IAREA = 1,NXA
C     CALCULATE POSITION IN ITEMP ARRAY WHERE MAPX VALUE FOR THIS HOUR
C     FOR THIS AREA IS TO BE PLACED
         LOCTMP = (IAREA-1)*NHRS + IRELHR
         CALL UMEMOV (MAPXCO(3*IAREA-2),AREAID,2)
         IPNT = MAPXCO(3*IAREA) - 8
         IF (IPNT.LT.1.OR.IPNT.GT.NXGRD) THEN
            WRITE (IPR,110) 'STARTING',IPNT,NXGRD,AREAID
            CALL ERROR
            GO TO 50
            ENDIF
         NSEGS = XGRD(IPNT)
         NCHK=IPNT+NSEGS
         IF (NCHK.GT.NXGRD) THEN
            WRITE (IPR,110) 'ENDING',NCHK,NXGRD,AREAID
            CALL ERROR
            GO TO 50
            ENDIF
         IF (IBUG.GE.1) WRITE (IOPDBG,*)
     &      ' AREAID=',AREAID,
     &      ' IPNT=',IPNT,
     &      ' NSEGS=',NSEGS,
     &      ' LOCTMP=',LOCTMP,
     &      ' '
         NGRDS = 0
         ISUM = 0
C     CALCULATIONS FOR EACH LINE SEGMENT WITHIN AREA
         DO 20 LSEG = 1,NSEGS
            NP = IPNT + 1 + (LSEG-1)*3
            NROW = XGRD(NP)
            NCBEG = XGRD(NP+1)
            NCEND = XGRD(NP+2)
            IF (IBUG.GE.1) WRITE (IOPDBG,*)
     &         ' NROW=',NROW,
     &         ' NCBEG=',NCBEG,
     &         ' NCEND=',NCEND,
     &         ' '
            ILR = NROW - IHRAP(3) + 1
            NCHK=NCBEG-IHRAP(1)+1
            IF (NCHK.LT.1) THEN
               WRITE (IPR,120) 'COLUMN',NCHK,AREAID
               CALL ERROR
               GO TO 50
               ENDIF
            NCHK=NCEND-IHRAP(1)+1
            IF (NCHK.GT.IX) THEN
               WRITE (IPR,130) 'COLUMN',NCHK,'COLUMNS',IX,AREAID
               CALL ERROR
               GO TO 50
               ENDIF
            IF (ILR.LT.1) THEN
               WRITE (IPR,120) 'ROW',ILR,AREAID
               CALL ERROR
               GO TO 50
               ENDIF
            IF (ILR.GT.IY) THEN
               WRITE (IPR,130) 'ROW',ILR,'ROWS',IY,AREAID
               CALL ERROR
               GO TO 50
               ENDIF
C        SUM VALUE IN EACH GRID BOX WITHIN SEGMENT
            DO 10 INC = NCBEG,NCEND
               ILC = INC - IHRAP(1) + 1
               NGRDS = NGRDS + 1
               ISUM = ISUM + IRADAR(ILC,ILR)
               IF (IBUG.GE.1)  WRITE (IOPDBG,*)
     &            ' IRADAR(ILC,ILR)=',IRADAR(ILC,ILR),
     &            ' NROW=',NROW,
     &            ' INC=',INC,
     &            ' '
               IF (IRADAR(ILC,ILR).GE.0) GO TO 10
                  ITEMP(LOCTMP) = -999
                  GO TO 30
10             CONTINUE
20          CONTINUE
         ITEMP(LOCTMP) = ISUM/NGRDS
30       IF (IBUG.GE.1) WRITE (IOPDBG,*)
     &      ' AREAID=',AREAID,
     &      ' IRELHR=',IRELHR,
     &      ' LOCTMP=',LOCTMP,
     &      ' ITEMP(LOCTMP)=',ITEMP(LOCTMP),
     &      ' '
         IF (ICPBUG.GT.0) THEN
            CALL URTIMR (LAPSCP,ITOTCP)
            CPLAPS = FLOAT(LAPSCP)/100.
            WRITE (IOPDBG,140) AREAID,IRELHR,CPLAPS
            ENDIF
40       CONTINUE
C
50    IF (ICPBUG.GT.0) THEN
         CALL URTIMR (LAPSCP,ITOTCP)
         TOTCP = FLOAT(ITOTCP)/100.
         WRITE (IOPDBG,60) 'END',NXA,IRELHR,TOTCP
         ENDIF
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'EXIT NCLMPX'
C
      RETURN
C
60    FORMAT (' ',A,' MAPX CALCULATIONS FOR ',I3,' AREAS ',
     &   'FOR HOUR ',I3,
     &   ' - ',
     &   'TOTAL CPU TIME USED IS ',F7.2,' SECONDS')
70    FORMAT ('0**ERROR** NUMBER OF MAPX AREAS (',I2,
     &   ') IS LESS THAN ONE.')
80    FORMAT ('0**ERROR** NUMBER OF POSITIONS IN MXCO ARRAY ',
     &   'TO BE PROCESSED (',I5,') EXCEEDS ARRAY SIZE (',I5,').')
90    FORMAT ('0**ERROR** RELATIVE HOUR TO BE PROCESSED (',I2,
     &   ') IS LESS THAN ONE.')
100   FORMAT ('0**ERROR** NUMBER OF POSITIONS IN ARRAY ITEMP ',
     &   'TO BE PROCESSED (',I5,') EXCEEDS ARRAY SIZE (',I5,').')
110   FORMAT ('0**ERROR** POINTER TO ',A,' POSITION IN XGRD ARRAY (',
     &      I6,') ','IS LESS THAN ONE OR GREATER THAN ARRAY SIZE (',
     &      I6,')' /
     &   11X,'FOR AREA ',A,'.')
120   FORMAT ('0**ERROR** ',A,' NUMBER ',I4,' TO BE PROCESSED ',
     &   'FOR AREA ',A,' IS LESS THAN ONE.')
130   FORMAT ('0**ERROR** ',A,' NUMBER ',I4,' TO BE PROCESSED ',
     &      'EXCEEDS NUMBER OF ',A,' SPECIFIED IN UGNL PARAMETERS (',I4,
     &      ') ' /
     &   11X,'FOR AREA ',A,'.')
140   FORMAT (' END OF MAPX CALCULATIONS FOR AREA ',A,' ',
     &   'FOR HOUR ',I3,
     &   ' - ',
     &   'TOTAL CPU TIME USED IS ',F7.2,' SECONDS')
C
      END
