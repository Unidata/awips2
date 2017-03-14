C MEMBER PPUTTS
C  (from old member PRDWPRDH)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/20/95.09:15:52 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PPUTTS (ITSID,ITYPE,IREC,IXREC,INDEX,ISTAT)
C
C   THIS ROUTINE WILL ENTER A TIME SERIES INTO THE TIME SERIES INDEX
C   IN THE POSITION IXREC WHICH WAS DETERMINED BY PSERCH.
C
C
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urmaxm'
C
      DIMENSION ITSID(2),IXBUF(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pputts.f,v $
     . $',                                                             '
     .$Id: pputts.f,v 1.1 1995/09/17 18:45:45 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10) ITSID,ITYPE
10    FORMAT (' *** ENTER PPUTTS - ITSID=',2A4,3X,'ITYPE=',A4)
C
C  UPDATE NUMBER OF TIME SERIES DEFINED AND GET UNIT NUMBER
      IF (IAMORD.EQ.0) GO TO 20
         NMTIMS=NMTIMS+1
         IDATFL(15,INDEX)=IDATFL(15,INDEX)+1
         LUIX=KUPRIX
         GO TO 40
20    NUMTMS=NUMTMS+1
      DATFIL(15,INDEX)=DATFIL(15,INDEX)+1
      LUIX=KINDEX
C
      IF (IPRTR.GT.0) WRITE (IOGDB,30) ISTAT
30    FORMAT (' *** EXIT PPUTTS - ISTAT=',I2)
C
C  WRITE ENTRY TO INDEX
40    CALL UMEMOV (ITSID,IXBUF(1),2)
      IXBUF(3)=ITYPE
      IXBUF(4)=IREC
      CALL UWRITT (LUIX,IXREC,IXBUF,ISTAT)
C
      RETURN
C
      END
