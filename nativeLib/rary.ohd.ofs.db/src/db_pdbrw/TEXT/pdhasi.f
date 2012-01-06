C MEMBER PDHASI
C  (from old member PDPDFNDI)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDHASI (IHOLD,INTID,IHASH)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdhasi.f,v $
     . $',                                                             '
     .$Id: pdhasi.f,v 1.1 1995/09/17 18:44:00 dws Exp $
     . $' /
C    ===================================================================
C
C
C  THIS ROUTINE WILL COMPUTE A HASH VALUE.
C  THE INPUT IS AN INTEGER WORD SO JUST DO A MOD ON IT WITH IHOLD.
C
C
      IHASH=MOD(INTID,IHOLD)
      IF (IHASH.EQ.0) IHASH=IHOLD
C
      RETURN
C
      END
