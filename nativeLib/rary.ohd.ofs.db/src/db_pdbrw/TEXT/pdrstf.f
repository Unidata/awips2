C MODULE PDRSTF
C-----------------------------------------------------------------------
C
      SUBROUTINE PDRSTF (IDAY,IDATES,IPOINT,IRCOFS,IFILE,NDTWDS,LDATE,
     *   ISTAT)
C
C  ROUTINE TO SET FUTURE DATA VALUES TO MISSING FOR A NEW DAY.
C
      DIMENSION IDATES(*)
      INTEGER*2 IDTREC(32)
C
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdsifc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdrstf.f,v $
     . $',                                                             '
     .$Id: pdrstf.f,v 1.2 2001/06/13 12:44:51 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) 'ENTER PDRSTF'
C
      ISTAT=0
C
      LRCPD2=LRCPDD*2
      J=IDAY+1
C
      DO 20 I=J,LDATE
         CALL PDFNDD (I,IDATES,IDD)
         IF (IDD.EQ.0) GO TO 20
C     COMPUTE RECORD AND INDEX WITHIN RECORD
         IDREC=IDATES(IDD+1)
         IRCRED=IDREC+IRCOFS-1
         IDX=IPOINT-(IRCOFS-1)*LRCPD2
         IF (IPDDB.GT.0) WRITE (IOGDB,*) 'J=',J,' IRCRED=',IRCRED,
     *      ' IDX=',IDX
         CALL UREADT (KPDDDF(IFILE),IRCRED,IDTREC,ISTAT)
         IF (ISTAT.NE.0) GO TO 30
         DO 10 N=1,NDTWDS
            IDTREC(IDX+N-1)=MISSNG
10          CONTINUE
         CALL UWRITT (KPDDDF(IFILE),IRCRED,IDTREC,ISTAT)
         IF (ISTAT.NE.0) GO TO 30
20       CONTINUE
C
30    IF (IPDTR.GT.0) WRITE (IOGDB,*) 'EXIT PDRSTF - ISTAT=',ISTAT
C
      RETURN
C
      END
