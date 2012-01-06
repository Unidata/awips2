C MODULE HPASTA
C-----------------------------------------------------------------------
C
      SUBROUTINE HPASTA (TECHNM,LIARGS,ITVAL,NIARGS,IARGS,ISTAT)
C
C  ROUTINE TO RETURN THE VALUE OF A TECHNIQUE AND ANY ARGUMENTS.
C
C    ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM   DESCRIPTION
C       ------   ----   ---   ---   -----------
C       TECHNM    A8     I     1    TECHNIQUE NAME
C       LIARGS     I     I     1    LENGTH OF ARRAY IARGS
C       ITVAL      I     O     1    VALUE OF TECHNIQUE
C       NIARGS     I     O     1    NUMBER OF WORDS FILLED IN IARGS
C       IARGS      I     O   LIARGS ARGUMENT VALUES
C       ISTAT      I     O     1    STATUS:
C                                     0=OKAY
C                                     1=LIARGS TOO SMALL
C                                     2=TECHNIQUE NOT FOUND
C                                     3=NO TECHNIQUES IN ARRAY IPTRAY
C
      CHARACTER*8 TECHNM
      CHARACTER*8 RTNNAM,RTNOLD
C
      DIMENSION IARGS(LIARGS)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hgtech'
      INCLUDE 'hclcommon/htechn'
      INCLUDE 'hclcommon/hgargm'
      INCLUDE 'hclcommon/hargmn'
      INCLUDE 'hclcommon/hptray'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hpasta.f,v $
     . $',                                                             '
     .$Id: hpasta.f,v 1.3 2001/06/13 12:07:46 mgm Exp $
     . $' /
C    ===================================================================
C
C   
      RTNNAM='HPASTA'
C
      IF (IHCLTR.GT.1) WRITE (LP,*) 'ENTER ',RTNNAM
C
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,RTNOLD,IOLDOP)
C
      ISTAT=0
C
      ITVAL=0
      NARGS=0
C
C  CHECK IF TECHNIQUE DEFINED AND HAS ARGUMENTS
      CALL HPAST (TECHNM,ITVAL,ISTAT)
C
C  IF ISTAT=0, TECHNIQUE FOUND AND NO ARGUMENTS
C  IF ISTAT=1, TECHNIQUE FOUND AND HAS ARGUMENTS
C  IF ISTAT=2, TECHNIQUE NOT FOUND
C
      IF (ISTAT.NE.1) GO TO 70
C
      ISTAT=0
C
      IF (NPTRAY.EQ.0) THEN
         ISTAT=3
         GO TO 70
         ENDIF
C
C  CHECK FOR TECHNIQUE NAME
      DO 10 I=2,NPTRAY,7
         CALL UNAMCP (IPTRAY(I),TECHNM,IMATCH)
         IF (IMATCH.EQ.0) GO TO 30
10       CONTINUE
C
C  TECHNIQUE NOT FOUND
      ISTAT=2
      GO TO 70
C
C  CHECK IF ANY ARGUMENTS
30    NARGS=IPTRAY(I+5)
      IF (NARGS.EQ.0) GO TO 70
      IF (NARGS.GT.LIARGS) THEN
         WRITE (LP,40) NARGS,LIARGS,TECHNM
40    FORMAT ('0**WARNING** NUMBER OF WORDS NEEDED IN ARGUMENT ARRAY (',
     *   I5,') EXCEEDS ARRAY SIZE (',I5,') FOR TECHNIQUE ',A,'.')
         CALL WARN
         NARGS=LIARGS
         ISTAT=1
         ENDIF
C
C  GET ARGUMENTS
      IAP=IPTRAY(I+6)
      IF (IPTRAY(I+4).LT.0) GO TO 60
         CALL UMEMOV (IARG(IAP),IARGS,NARGS)
         GO TO 70
60    CALL UMEMOV (IGARG(-IAP),IARGS,NARGS)
C
70    NIARGS=NARGS
C
      IF (IHCLDB.GT.0) THEN
         WRITE (IOGDB,*) 'EXIT HPASTA -',
     *   ' ISTAT=',ISTAT,
     *   ' TECHNM=',TECHNM,
     *   ' ITVAL=',ITVAL,
     *   ' NARGS=',NARGS
         IF (NIARGS.GT.0) THEN
            WRITE (IOGDB,90) (IARGS(I),I=1,NIARGS)
90    FORMAT (' IN HPASTA - IARGS IN I5 FORMAT:' / (1X,20(1X,I5)))
            WRITE (IOGDB,95) (IARGS(I),I=1,NIARGS)
95    FORMAT (' IN HPASTA - IARGS IN A4 FORMAT:' / (1X,20(1X,A4,1X)))
            ENDIF
         ENDIF
C
      CALL FSTWHR (RTNOLD,IOLDOP,RTNOLD,IOLDOP)
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT ',RTNNAM,' - ISTAT=',ISTAT
C
      RETURN
C
      END
