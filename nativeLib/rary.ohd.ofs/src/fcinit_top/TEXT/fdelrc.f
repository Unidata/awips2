C  MODULE FDELRC
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE RATING CURVES.
C
      SUBROUTINE FDELRC (LDRC,NDRC)
C
C  SECOND, ALL SEGMENT DEFINITIONS ARE LOADED INTO CORE, ONE AT A TIME,
C  AND THE P AND T ARRAYS ARE SCANNED FOR ANY RATING CURVE USE.
C  ANY THAT ARE FOUND IN THE SEGMENT ARE COMPARED TO THE LIST PASSED
C  TO THE SUBROUTINE. ANY THAT MATCH ARE NOTED AS UNDELETABLE.
C
C  ARGUMENT LIST:
C    INPUT:
C      LDRC - ARRAY OF RATING CURVE IDENTIFIERS
C      NDRC - NUMBER OF IDENTIFIERS IN THE ARRAY
C
C  SUBROUTINE ORIGINALLY WRITTEN BY -- JOE OSTROWSKI -- HRL -- 7/1981
C
      CHARACTER*8 RTNNAM,OPNOLD
C
      DIMENSION LDRC(2,NDRC)
      PARAMETER (MUSE=50)
      DIMENSION INUSE(3,MUSE),LRCSEG(2,MUSE)
      DIMENSION IRTCV(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fsglst'
      INCLUDE 'common/fratng'
      INCLUDE 'common/frcptr'
C
      EQUIVALENCE (RTCVID(1),IRTCV(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fdelrc.f,v $
     . $',                                                             '
     .$Id: fdelrc.f,v 1.3 2000/03/14 11:57:37 page Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4h    /
      DATA IOBSO,LETE/4hOBSO,4hLETE/
C
C
      RTNNAM='FDELRC'
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      IBUG=IFBUG('DLTE')+IFBUG('RTCV')
C
      IF (IBUG.GT.0) WRITE (IODBUG,10) NDRC,
     *  ((LDRC(K,J),K=1,2),J=1,NDRC)
10    FORMAT (' IN FDELRC: NDRC=',I2,' IDENTIFIERS: ',
     *  5(/' ',5(2A4,' ')))
C
      NUSE=0
      IDSEGN(1)=BLANK
      IDSEGN(2)=BLANK
      DO 20 I=1,MUSE
         INUSE(1,I) = 0
20       CONTINUE
C
C  CHECK IF ANY RATING CURVES EXIST
      IF (NRC.EQ.0) THEN
         WRITE (IPR,30)
30    FORMAT ('0**WARNING** NO RATING CURVES EXIST.')
         CALL WARN
         GO TO 190
         ENDIF
C
C  CHECK IF EACH RATING CURVE EXISTS
      DO 50 I=1,NDRC
         CALL FINDRC (LDRC(1,I),IREC,ISW)
         IF (ISW.GT.0) GO TO 50
            WRITE (IPR,40) (LDRC(J,I),J=1,2)
40    FORMAT ('0**WARNING** RATING CURVRE ',2A4,' NOT FOUND.')
            CALL WARN
            LDRC(1,I)=BLANK
            LDRC(2,I)=BLANK
50       CONTINUE
C
      IF (IBUG.GT.0) WRITE (IODBUG,10) NDRC,
     *  ((LDRC(K,J),K=1,2),J=1,NDRC)
C
C  CHECK IF RATING CURVE IS USED BY ANY SEGMENT
      DO 150 IRSEG=1,NS
         CALL FGETSG (IDSEGN,IRSEG,MP,P,MT,T,MTS,TS,1,0,IER)
         IF (IER.NE.0) THEN
            WRITE (IPR,60) IRSEG
60    FORMAT ('0**ERROR** UNABLE TO READ SEGMENT STATUS FILE RECORD ',
     *   I6,'.')
            CALL ERROR
            GO TO 190
            ENDIF
C     CHECK IF IF SEGMENT IS OBSOLETE
         IF (IDSEGN(1).EQ.IOBSO.AND.IDSEGN(2).EQ.LETE) GO TO 150
C     FIND ALL RATING CURVES USED BY THE SEGMENT
         CALL FRCTP (LRCSEG,NRCSEG,T,MT,P,MP,IERC)
         IF (IERC.NE.0) THEN
            WRITE (IPR,70) IDSEGN
70    FORMAT ('0**ERROR** UNABLE TO GET LIST OF RATING CURVES USED BY ',
     *   'SEGMENT ',2A4,'.')
            CALL ERROR
            GO TO 190
            ENDIF
         IF (IBUG.GT.0) WRITE (IODBUG,80) IDSEGN,NRCSEG
80    FORMAT (' IDSEGN=',2A4,' NRCSEG=',I2)
C     CHECK IF NO RATING CURVES FOUND
         IF (NRCSEG.EQ.0) GO TO 150
C     CHECK IF RATING CURVE USED BY THIS SEGMENT
         IF (IBUG.GT.0) WRITE (IODBUG,90) NRCSEG,
     *     ((LRCSEG(K,J),K=1,2),J=1,NRCSEG)
90    FORMAT (' NRCSEG=',I2,' LRCSEG=:' / (1X,2A4))
         DO 140 J=1,NDRC
            IF (LDRC(1,J).EQ.BLANK.AND.LDRC(2,J).EQ.BLANK) GO TO 140
            DO 130 K=1,NRCSEG
               IF (LRCSEG(1,K).NE.LDRC(1,J).OR.LRCSEG(2,K).NE.LDRC(2,J))
     *            GO TO 130
               NUSE=NUSE+1
               IF (NUSE.GT.MUSE) THEN
                  WRITE (IPR,100) MUSE
100   FORMAT ('0**ERROR** MORE THAN ',I2,' RATING CURVES ',
     *   'TO BE DELETED ARE IN SEGMENTS.')
                  CALL ERROR
                  GO TO 190
                  ENDIF
               IF (IBUG.GT.0) WRITE (IODBUG,110) NUSE,(LDRC(M,J),M=1,2)
110   FORMAT (' NUSE=',I3,' LDRC=',2A4)
               INUSE(1,NUSE)=J
               INUSE(2,NUSE)=IDSEGN(1)
               INUSE(3,NUSE)=IDSEGN(2)
               WRITE (IPR,120) (LDRC(L,J),L=1,2),(INUSE(L,NUSE),L=2,3)
120   FORMAT ('0**ERROR** RATING CURVE ',2A4,' IS USED IN SEGMENT ',
     *   2A4,'.')
               CALL ERROR
               GO TO 140
130            CONTINUE
140         CONTINUE
150      CONTINUE
C
      IF (IBUG.GT.0) WRITE (IODBUG,10) NDRC,
     *  ((LDRC(K,J),K=1,2),J=1,NDRC)
C
C  DELETE RATING CURVES
      DO 180 I=1,NDRC
C     SKIP ANY RATING CURVES NOT FOUND OR USED BY A SEGMENT
         IF (LDRC(1,I).EQ.BLANK.AND.LDRC(2,I).EQ.BLANK) GO TO 180
         IF (NUSE.GT.0) THEN
            DO 160 J=1,NUSE
               IF (INUSE(1,J).EQ.I) GO TO 180
160            CONTINUE
            ENDIF
C     SET RATING CURVE TO OBSOLETE
         CALL FGETRC (LDRC(1,I),IERR)
         CALL FINDRC (LDRC(1,I),IREC,ISW)
         CALL UMEMOV ('OBSOLETE',IRTCV,2)
         CALL FCWTRC (IREC,IRTCV,1)
         WRITE (IPR,170) (LDRC(K,I),K=1,2)
170   FORMAT ('0**NOTE** RATING CURVE ',2A4,' DELETED.')
180      CONTINUE
C
190   CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
