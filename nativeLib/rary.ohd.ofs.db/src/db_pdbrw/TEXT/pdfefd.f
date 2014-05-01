C MODLUE PDFEFD
C-----------------------------------------------------------------------
C
      SUBROUTINE PDFEFD (IDATES,IDTXX)
C
C  ROUTINE TO FIND OLDEST JULIAN DATE OF FUTURE DATA
C
      DIMENSION IDATES(*)
C
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdfefd.f,v $
     . $',                                                             '
     .$Id: pdfefd.f,v 1.2 2001/06/13 12:44:39 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) 'ENTER PDFEFD'
C
      IDTXX=0
      NDTS=IDATES(1)-1
      IF (IPDTR.GT.0) WRITE (IOGDB,*) 'NDTS=',NDTS
C
C  CHECK IF ANY DATES
      IF (NDTS.LT.0) GO TO 30
C
      IDTXX=2
      IF (NDTS.LT.1) GO TO 30
C
      IDTOLD=IDATES(2)
      IPOS=4
      DO 20 I=1,NDTS
         IF (IPDTR.GT.0) WRITE (IOGDB,*) 'IPOS=',IPOS,
     *      ' IDATES(IPOS)=',IDATES(IPOS),' IDTOLD=',IDTOLD
         IF (IDATES(IPOS).GE.IDTOLD) GO TO 10
            IDTOLD=IDATES(IPOS)
            IDTXX=IPOS
10       IPOS=IPOS+2
20       CONTINUE
C
30    IF (IPDTR.GT.0) WRITE (IOGDB,*) 'EXIT PDFEFD - IDTXX=',IDTXX

      RETURN
C
      END
