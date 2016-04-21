C MEMBER PDSTP2
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/28/95.16:01:30 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDSTP2 (NODAY,ISTAIX,IRCOFS,IVAL2,IDAY2,ISTAT)
C
C FIND THE LARGEST VALUE IN FILE FOR STATION BUT NOT FOR DAY NODAY
C
C  NODAY IS DAY NOT TO FIND
C ISTAIX IS SUBSCRIPT IN STATION IN RECORD
C IRCOFS IS RECORD OFFSET IN DATA RECORD
C IVAL2 IS NEW LARGEST
C IDAY2 IS THE DAY
C ISTAT IS THE STAT
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdunts'
C
      INTEGER*2 IDATA(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdstp2.f,v $
     . $',                                                             '
     .$Id: pdstp2.f,v 1.2 1996/01/16 22:54:31 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA LPP24/4HPP24/
C
C
C GET SUBSCRIPT OF PP24 IN DIRECTORY
C
      IVAL2=0
      ITX=IPDCKD(LPP24)
C
C GET DATES FOR SEARCH
C
      CALL UMEMOV (IDDTDR(8,ITX),ID1,1)
      IDAY2=0
      IREC=IDDTDR(10,ITX)+IRCOFS-1
      CALL UMEMOV (IDDTDR(11,ITX),IDL,1)
      IFILE=KPDDDF(IDDTDR(4,ITX))
C
C GET NUMBER OF RECORDS FOR 1 DAY AND LAST REC
C
      CALL PDVALS(ITX,N1DAY,NALL,LSTREC)
C
C LOOP ON EACH DAY EXCEPT NODAY
C
      DO 20 J=ID1,IDL
         IF (J.EQ.NODAY) GO TO 10
         CALL UREADT (IFILE,IREC,IDATA,ISTAT)
         IF (ISTAT.NE.0) GO TO 30
          CALL PDGTPP (IDATA(ISTAIX),PP24,IHR,IPP)
         IF (IPP.LE.IVAL2) GO TO 10
C
C GOT A BIGGIE
C
         IVAL2=IPP
         IDAY2=J
C
C TRY NEXT, IF RECORD IS LAST GO TO BEGINNING
C
10       IREC=IREC+N1DAY
         IF (IREC.GT.LSTREC) IREC=IDDTDR(15,ITX)+IRCOFS-1
20    CONTINUE
C
30    IF (IPDDB.GT.0) WRITE (IOGDB,40) IVAL2,IDAY2
40    FORMAT (' *** EXIT PDSTP2 - IVAL2,IDAY2',2I6)
C
      RETURN
C
      END
