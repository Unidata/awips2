C MODULE HPAST
C-----------------------------------------------------------------------
C
      SUBROUTINE HPAST (TECHNM,ITVAL,ISTAT)
C
C  THIS ROUTINE RETURNS THE VALUE OF A TECHNIQUE.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE   I/O   DIM   DESCRIPTION
C       ------    ----   ---   ---   ---------
C       TECHNM     A8     I     1    TECHNIQUE NAME
C       ITVAL       I     O     1    VALUE OF TECHNIQUE
C       ISTAT       I     O     1    STATUS:
C                                      0=OKAY
C                                      1=TECHNIQUE HAS ARGUMENTS
C                                      2=TECHNIQUE NOT FOUND
C                                      3=NO TECHNIQUES IN ARRAY IPTRAY
C
      CHARACTER*8 TECHNM
      CHARACTER*8 RTNNAM,RTNOLD
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hgtech'
      INCLUDE 'hclcommon/htechn'
      INCLUDE 'hclcommon/hptray'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hpast.f,v $
     . $',                                                             '
     .$Id: hpast.f,v 1.3 2001/06/13 12:07:31 mgm Exp $
     . $' /
C    ===================================================================
C
C  
      RTNNAM='HPAST'
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER ',RTNNAM
C
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,RTNOLD,IOLDOP)
C
      ISTAT=0
C
      ITVAL=0
      NARGS=0
C
      IF (NPTRAY.EQ.0) THEN
         ISTAT=3
         GO TO 60
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
      GO TO 60
C
C  GET TECHNIQUE VALUE
30    IT=IPTRAY(I+4)
      IF (IT.LT.0) GO TO 40
         ITVAL=ITECH(IT)
         GO TO 50
40    ITVAL=IGTECH(-IT)
C
50    NARGS=IPTRAY(I+5)
      IF (NARGS.NE.0) ISTAT=1
C
60    IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'IN ',RTNNAM,' -',
     *   ' TECHNM=',TECHNM,
     *   ' ITVAL=',ITVAL,
     *   ' NARGS=',NARGS
C
      CALL FSTWHR (RTNOLD,IOLDOP,RTNOLD,IOLDOP)
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT ',RTNNAM,' - ISTAT=',ISTAT
C
      RETURN
C
      END
