C MODULE ESPRDF
C-----------------------------------------------------------------------
C
      SUBROUTINE ESPRDF (ICODE,ICHKID,ISGREC,TSESP,MTSESP,PESP,MPESP,
     *   SPESP,MSPESP,IER)
C
C   THIS ROUTINE FILLS COMMON BLOCK ESPSEG. IF ICODE=1 IT ALSO FILLS
C   ARRAYS TSESP, PESP AND SPESP. IF ICHKID=1 ALSO CHECKS SEGMENT NAMES
C   ON THE ESP AND FORECAST FILES (ID VS.IDSEGN) TO SEE IF THEY
C   ARE THE SAME.
C
C   ORIGINALY WRITTEN BY JAY DAY
C
      CHARACTER*8 OLDOPN
C
      DIMENSION TSESP(*),PESP(*),SPESP(*)
C
      INCLUDE 'common/espseg'
      INCLUDE 'common/esprec'
      INCLUDE 'common/espfle'
      INCLUDE 'common/eunit'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcsegn'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/esprdf.f,v $
     . $',                                                             '
     .$Id: esprdf.f,v 1.3 2001/06/13 11:29:05 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=0
      CALL FSTWHR ('ESPRDF  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER ESPRDF'
C
      IBUG=IFBUG('EARY')
C
      IER=0
      IREC=ISGREC
C
C  READ ESP FILE
      CALL UREADT (KEPARM,IREC,ESPDAT,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,5) IREC
5     FORMAT ('0**ERROR** READING RECORD ',I5,
     * ' FROM THE ESP PARAMETER FILE.')
         CALL ERROR
         IER=1
         GO TO 50
         ENDIF
C
      CALL UMEMOV (ESPDAT,ID,2)
C
      IF (ICHKID.EQ.1) THEN
         IF (ID(1).EQ.IDSEGN(1).AND.ID(2).EQ.IDSEGN(2)) GO TO 20
         WRITE (IPR,10) ID,IDSEGN
10    FORMAT ('0**ERROR** ESP SEGMENT ID (',2A4,
     * ') DOES NOT MATCH THE FORECAST COMPONENT SEGMENT ID (',
     * 2A4,').')
         CALL ERROR
         IER=1
         GO TO 50
         ENDIF
C
C  FILL COMMON BLOCK ESPSEG
20    NSREC=ESPDAT(3)
      DO 30 I=1,5
         IECRDT(I)=ESPDAT(I+3)
         IECKDT(I)=ESPDAT(I+8)
30       CONTINUE
      LTSESP=ESPDAT(14)
      LPESP=ESPDAT(15)
      LSPESP=ESPDAT(16)
C
      IF (ICODE.EQ.0) GO TO 50
C
      IF (LTSESP.GT.MTSESP.OR.
     *    LPESP.GT.MPESP.OR.
     *    LSPESP.GT.MSPESP) THEN
         WRITE (IPR,40) ID,LTSESP,LPESP,LSPESP,MTSESP,MPESP,MSPESP
40    FORMAT ('0**ERROR** NOT ENOUGH SPACE TO DEFINE TSESP, PESP OR ',
     *     ' SPESP ARRAYS FOR ESP SEGMENT ',2A4,':' /
     * 11X,'LTSESP=',I5,' LPESP=',I5,' LSPESP=',I5,
     *     'MTSESP=',I5,' MPESP=',I5,' MSPESP=',I5)
         CALL ERROR
         IER=1
         GO TO 50
         ENDIF
C
C  SET COUNTER OF LOCATION ON RECORD
      NXWD=17
C
C  MAKE CALLS TO EREAD TO FILL THE ARRAYS BY READING THE ESP FILE.
      IF (LTSESP.GT.0) THEN
         CALL EREAD (TSESP,LTSESP,IREC,NXWD)
         ENDIF
      IF (LPESP.GT.0) THEN
         CALL EREAD (PESP,LPESP,IREC,NXWD)
         ENDIF
      IF (LSPESP.GT.0) THEN
         CALL EREAD (SPESP,LSPESP,IREC,NXWD)
         ENDIF
C
50    IF (IBUG.GT.0) THEN
         WRITE(IODBUG,60) LTSESP,LPESP,LSPESP
60    FORMAT (' LTSESP=',I5,' LPESP=',I5,'LSPESP=',I5)
         IF (ICODE.EQ.1) THEN
            WRITE(IODBUG,70)
70    FORMAT (' TSESP ARRAY:')
            WRITE(IODBUG,80) (TSESP(I),I=1,LTSESP)
80    FORMAT (1X,10F10.0)
            WRITE(IODBUG,90)
90    FORMAT (' PESP ARRAY:')
            WRITE(IODBUG,80) (PESP(I),I=1,LPESP)
            WRITE(IODBUG,100)
100   FORMAT (' SPESP ARRAY:')
            WRITE(IODBUG,80) (SPESP(I),I=1,LSPESP)
            ENDIF
         ENDIF
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
