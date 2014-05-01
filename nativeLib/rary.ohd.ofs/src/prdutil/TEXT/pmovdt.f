C MEMBER PMOVDT
C  (from old member PRDFUNC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/22/95.08:22:39 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PMOVDT (NEWSLT)
C
C  THIS ROUTINE WILL MOVE THE LAST ENTRY IN DATFIL TO POSITION NEWSLT.
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/pmovdt.f,v $
     . $',                                                             '
     .$Id: pmovdt.f,v 1.1 1995/09/17 19:16:38 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  CHECK NUMBER OF DATA TYPES DEFINED
      IF (NUMDTP.EQ.NEWSLT) GO TO 10
C
      CALL UMEMOV (DATFIL(1,NUMDTP),DATFIL(1,NEWSLT),18)
C
C  UPDATE CONTROLS
      IF (DATFIL(7,NEWSLT).EQ.0) GO TO 10
      J=IABS(DATFIL(7,NEWSLT))
      IF (DATFIL(7,J).LT.0) DATFIL(7,J)=-NEWSLT
      IF (DATFIL(7,J).GT.0) DATFIL(7,J)=NEWSLT
C
10    NUMDTP=NUMDTP-1
      J=NUMDTP+1
C
      IF (IPRTR.GT.0) WRITE (IOGDB,20) J,NEWSLT
20    FORMAT (' *** EXIT PMOVDT : J=',I4,3X,'NEWSLT=',I4)
C
      RETURN
C
      END
