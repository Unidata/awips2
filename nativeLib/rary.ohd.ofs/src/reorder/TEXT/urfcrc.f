C MODULE URFCRC
C-----------------------------------------------------------------------
C
C  ROUTINE TO REORDER THE RATING CURVE FILES.
C
      SUBROUTINE URFCRC (LSTRC,NLSTRC,MLSTCK,LSTCK,ISTAT)
C
      REAL OBSLT(2)/4HOBSO,4HLETE/
      INTEGER LSTCK(MLSTCK)
C
      DIMENSION LSTRC(2,NLSTRC)
      DIMENSION ZBUF(300)
      DIMENSION IBUF(3),BUF(3)
C
      EQUIVALENCE (BUF(1),IBUF(1))
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/frcptr'
      INCLUDE 'common/frcpt2'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urfcrc.f,v $
     . $',                                                             '
     .$Id: urfcrc.f,v 1.3 2002/02/11 21:15:04 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER URFCRC'
C
C  SET DEBUG LEVEL
      IBUG=IFBUG('FCRC')
C
      ISTAT=0
C
      NRC2=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY RATING CURVES USED IN SEGMENT
C
      CALL SULINE (IPR,2)
      WRITE (IPR,80)
C
      NCOPY=0
C
      IF (NLSTRC.EQ.0) GO TO 20
C
C  COPY RATING CURVE FROM OLD TO NEW FILES
      DO 10 I=1,NLSTRC
         CALL FINDRC (LSTRC(1,I),IREC,ISW)
         IF (ISW.EQ.0) THEN
C        RATING CURVE NOT FOUND
            WRITE (IPR,100) (LSTRC(N,I),N=1,2)
            CALL SUERRS (IPR,2,-1)
            ISTAT=1
            GO TO 10
            ENDIF
         NRC2=NRC2+1
         IF (NRC2.GT.MRCF2) THEN
            WRITE (IPR,120) MRCF2
            CALL SUERRS (IPR,2,-1)
            ISTAT=1
            GO TO 60
            ENDIF
C     UPDATE RATING CURVE FILE
         CALL UREADT (KFRTCV,IREC,ZBUF(1),IERR)
         CALL UWRITT (LFRTCV,NRC2,ZBUF(1),IERR)
C     UPDATE RATING CURVE POINTER FILE
         IBUF(1)=LSTRC(1,I)
         IBUF(2)=LSTRC(2,I)
         IBUF(3)=NRC2
         M2=NRC2+1
         CALL UWRITT (LFRCPT,M2,IBUF(1),IERR)
         IF (IREC.GT.MLSTCK) THEN
            WRITE (IPR,110) MLSTCK,(LSTRC(N,I),N=1,2)
            CALL SUERRS (IPR,2,-1)
            ISTAT=1
            GO TO 10
            ENDIF
         LSTCK(IREC)=1
         NCOPY=NCOPY+1
10       CONTINUE
C
20    CALL SULINE (IPR,2)
      WRITE (IPR,130) NCOPY
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY NON-OBSLOETE RATING CURVES NOT USED IN SEGMENT
C
      CALL SULINE (IPR,2)
      WRITE (IPR,90)
C
      NCOPY=0
C
      IF (NRC.EQ.0) GO TO 40
C
      NOBSLT=0
C
      DO 30 I=1,NRC
         IF (LSTCK(I).EQ.0) THEN
            CALL UREADT (KFRTCV,I,ZBUF(1),IERR)
            IF (ZBUF(1).EQ.OBSLT(1).AND.ZBUF(2).EQ.OBSLT(2)) THEN
               NOBSLT=NOBSLT+1
               GO TO 30
               ENDIF
            NRC2=NRC2+1
            IF (NRC2.GT.MRCF2) THEN
               WRITE (IPR,120) MRCF2
               CALL SUERRS (IPR,2,-1)
               ISTAT=1
               GO TO 60
               ENDIF
            CALL UWRITT (LFRTCV,NRC2,ZBUF(1),IERR)
            BUF(1)=ZBUF(1)
            BUF(2)=ZBUF(2)
            IBUF(3)=NRC2
            M2=NRC2+1
            CALL UWRITT (LFRCPT,M2,IBUF(1),IERR)
            IF (I.GT.MLSTCK) THEN
               WRITE (IPR,110) MLSTCK,(LSTRC(N,I),N=1,2)
               CALL SUERRS (IPR,2,-1)
               ISTAT=1
               GO TO 30
               ENDIF
            LSTCK(I)=1
            NCOPY=NCOPY+1
            ENDIF
30       CONTINUE
C
      CALL SULINE (IPR,2)
      WRITE (IPR,130) NCOPY
C
      CALL SULINE (IPR,2)
      WRITE (IPR,140) NOBSLT
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  UPDATE RECORD 1 OF POINTER FILE
40    CALL UWRITT (LFRCPT,1,NRC2,IERR)
C
      IF (IBUG.GT.0) THEN
         IF (NRC2.LT.NRC) THEN
            DO 50 I=1,NRC
               IF (LSTCK(I).EQ.0) THEN
                  CALL UREADT (KFRTCV,I,ZBUF(1),IERR)
                  CALL SULINE (IODBUG,1)
                  WRITE (IODBUG,150) ZBUF(1),ZBUF(2),I
                  ENDIF
50             CONTINUE
            ENDIF
         ENDIF
C
60    IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT URFCRC'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT ('0*** NOTE - BEGIN TO COPY RATING CURVES USED IN ',
     *   'SEGMENT DEFINITIONS.')
90    FORMAT ('0*** NOTE - BEGIN TO COPY RATING CURVES NOT USED IN ',
     *   'SEGMENT DEFINITIONS.')
100   FORMAT ('0*** ERROR - IN URFCRC - RATING CURVE ',2A4,
     *   ' IS USED BY A SEGMENT BUT IS NOT DEFINED.')
110   FORMAT ('0*** ERROR - IN URFCRC - MAXIMUM NUMBER OF RATING ',
     *   'CURVES THAT CAN BE PROCESSED (',I4,') EXCEEDED FOR ',
     *   'RATING CURVE ',2A4,'.')
120   FORMAT ('0*** ERROR - IN URFCRC - MAXIMUM NUMBER OF RATING ',
     *   'CURVES THAT CAN BE STORED IN NEW FILES (',I4,') EXCEEDED.')
130   FORMAT ('0*** NOTE - ',I4,' RATING CURVES SUCCESSFULLY COPIED.')
140   FORMAT ('0*** NOTE - ',I4,' OBSOLETE RATING CURVES NOT COPIED.')
150   FORMAT (' RATING CURVE ',2A4,' AT RECORD ',I4,' NOT COPIED.')
C
      END
