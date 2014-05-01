C MODULE SUGTTS
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ TIME SERIES IDENTIFIERS FROM THE INDEX AND SORT
C
      SUBROUTINE SUGTTS (ITYPE,ISORT,NWORDS,MAXID,IARRAY,NUMID,ISTAT)
C
      INTEGER FMAP/4HFMAP/
      INTEGER IBLNK4/4H    /
C
      DIMENSION IARRAY(NWORDS,1)
      DIMENSION IBUF(4)
      DIMENSION TSID(2),FTSID(2),IHEAD(22)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pmaxdm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugtts.f,v $
     . $',                                                             '
     .$Id: sugtts.f,v 1.3 2001/06/13 14:04:56 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUGTTS'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,170) ITYPE,ISORT,NWORDS,MAXID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
      NUMID=0
      NUMERR=0
C
C  CHECK SORT VALUE : 0=DO NOT SORT, 1=SORT BY ID, 2=SORT BY DESCRIPTION
      IF (ISORT.GE.0.OR.ISORT.LE.2) GO TO 10
         WRITE (LP,180) ISORT
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 150
C
10    CALL SUDOPN (1,'PRD ',IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (LP,200)
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 150
C
20    NUMADD=0
      MAXREC=MAXTMS*2
      DO 60 IREC=1,MAXREC
         CALL UREADT (KINDEX,IREC,IBUF,IERR)
         IF (IERR.EQ.0) GO TO 30
            WRITE (LP,190) IREC,KINDEX
            CALL SUERRS (LP,2,NUMERR)
            ISTAT=1
            GO TO 150
30       IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,210) ITYPE,IREC,(IBUF(N),N=1,3)
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IBUF(1).EQ.IBLNK4.AND.IBUF(2).EQ.IBLNK4) GO TO 60
         IF (IBUF(3).NE.ITYPE) GO TO 60
            NUMID=NUMID+1
            IF (NUMID.LE.MAXID) GO TO 40
               NUMADD=NUMADD+1
               IF (NUMADD.GT.1) GO TO 60
                  WRITE (LP,230) MAXID
                  CALL SUERRS (LP,2,NUMERR)
                  ISTAT=1
                  GO TO 60
40          CALL SUBSTR (IBUF,1,8,IARRAY(1,NUMID),1)
            IF (ISORT.NE.2) GO TO 60
               CALL SUBSTR (IBUF,1,8,TSID,1)
               MAXX=1
               IF (ITYPE.EQ.FMAP)
     *            CALL RPRDFH (TSID,'MAP ',MAXX,IHEAD,NUMX,XBUF,IERR)
               IF (ITYPE.NE.FMAP)
     *            CALL RPRDH (TSID,ITYPE,MAXX,IHEAD,NUMX,XBUF,FTSID,
     *               IERR)
               IF (IERR.NE.0) THEN
                  CALL SRPRST ('RPRDFH  ',TSID,ITYPE,MAXX,NUMERR,IERR)
                  ISTAT=1
                  GO TO 60
                  ENDIF
               CALL SUBSTR (IHEAD,69,20,IARRAY(3,NUMID),1)
60       CONTINUE
C
      IF (NUMADD.GT.0) THEN
         WRITE (LP,240) NUMADD
         CALL SULINE (LP,2)
         GO TO 150
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,260) NUMID
         CALL SULINE (LP,1)
         endif
C
      NUM=NUMID-1
      IF (NUM.GT.0) GO TO 120
         IF (NUMID.EQ.1) GO TO 150
            WRITE (LP,220) ITYPE
            CALL SULINE (LP,2)
            ISTAT=2
            GO TO 150
C
120   IF (ISORT.EQ.0) GO TO 150
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IWORD1=1
      IWORD2=2
      IF (ISORT.EQ.2) IWORD1=3
      IF (ISORT.EQ.2) IWORD2=7
      ISPTR=0
      CALL SUSOR2 (NWORDS,NUMID,IWORD1,IWORD2,IARRAY,IARRAY,ISPTR,IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,270)
         CALL SULINE (IOSDBG,1)
         DO 130 I=1,NUMID
            WRITE (IOSDBG,280) I,(IARRAY(N,I),N=1,NWORDS)
            CALL SULINE (IOSDBG,1)
130         CONTINUE
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   WRITE (LP,290) NUMID,ITYPE,MAXID
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUGTTS : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
170   FORMAT (' ITYPE=',A4,3X,'ISORT=',I2,3X,'NWORDS=',I3,3X,
     *   'MAXID=',I5)
180   FORMAT ('0*** ERROR - IN SUGTTS - INVALID SORT VALUE : ',I3)
190   FORMAT ('0*** ERROR - IN SUGTTS - DAIO ERROR AT RECORD ',I5,
     *   ' OF UNIT ',I2,'.')
200   FORMAT ('0*** ERROR - PROCESSED DATA BASE NOT ALLOCATED.')
210   FORMAT (' ITYPE=',A4,3X,'IREC=',I5,3X,'IDENTIFIER=',2A4,3X,
     *   'TYPE=',A4)
220   FORMAT ('0*** NOTE - IN SUGTTS - NO IDENTIFIERS FOUND TO ',
     *   'MATCH TYPE ',A4,' IN PROCESSED DATA BASE INDEX.')
230   FORMAT ('0*** ERROR - IN SUGTTS - MAXIMUM IDENTIFIERS THAT CAN ',
     *   'BE PROCESSED (',I5,') EXCEEDED.')
240   FORMAT ('0*** NOTE - IN SUGTTS - ',I5,' STATION NAMES COULD NOT ',
     *   'BE PROCESSED.')
260   FORMAT (' NUMID=',I4)
270   FORMAT (' SORTED LIST')
280   FORMAT (' ',I4,3X,10(A4,1X))
290   FORMAT ('0*** NOTE - ',I4,1X,A4,' IDENTIFIERS PROCESSED ',
     *   'FROM PROCESSED DATA BASE. ',
     *   'A MAXIMUM OF ',I5,' CAN BE PROCESSED.')
C
      END
