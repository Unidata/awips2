C MODULE ESPWTF
C-----------------------------------------------------------------------
C
      SUBROUTINE ESPWTF (ISGREC,TSESP,MTSESP,PESP,MPESP,SPESP,MSPESP,
     *   IER)
C
C   THIS ROUTINE WRITES A SEGMENT DEFINITION TO THE FILE ESPPARM.
C
C   THIS SUBROUTINE WAS WRITTEN BY GERALD N DAY.
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/espseg'
      INCLUDE 'common/esprec'
      INCLUDE 'common/espfle'
      INCLUDE 'common/eunit'
C
      CHARACTER*8 OLDOPN
C
      DIMENSION TSESP(MTSESP),PESP(MPESP),SPESP(MSPESP)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/espwtf.f,v $
     . $',                                                             '
     .$Id: espwtf.f,v 1.4 2001/06/13 12:56:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=0
      CALL FSTWHR ('ESPWTF  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER ESPWTF'
C
      IREC=ISGREC
      CALL UMEMOV (ID,ESPDAT,2)
      ESPDAT(3)=NSREC+.01
      DO 40 I=1,5
         ESPDAT(3+I)=IECRDT(I)+.01
         ESPDAT(8+I)=IECKDT(I)+.01
   40    CONTINUE
      ESPDAT(14)=LTSESP+.01
      ESPDAT(15)=LPESP+.01
      ESPDAT(16)=LSPESP+.01
C
C  SET COUNTER OF LOCATION ON RECORD
      NXWD=17
C
C  WRITE THE ARRAYS TO THE ESP FILE.
      IF (LTSESP.GT.0) THEN
         CALL EWRIT (TSESP,LTSESP,IREC,NXWD)
         ENDIF
      IF (LPESP.GT.0) THEN
         CALL EWRIT (PESP,LPESP,IREC,NXWD)
         ENDIF
      IF (LSPESP.EQ.GT) THEN
         CALL EWRIT (SPESP,LSPESP,IREC,NXWD)
         ENDIF
C
C  WRITE THE LAST PARTIALLY FILLED RECORD
      IF (NXWD.NE.1) THEN
         CALL UWRITT (KEPARM,IREC,ESPDAT,ISTAT)
         ENDIF
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
