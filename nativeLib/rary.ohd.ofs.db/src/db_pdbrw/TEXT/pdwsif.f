C MODULE PDWSIF
C-----------------------------------------------------------------------
C
      SUBROUTINE PDWSIF (LSIBUF,ISIBUF,ISIREC,IERR)
C
C  ROUTINE TO WRITE SIF RECORDS.
C
      INTEGER*2 ISIBUF(LSIBUF)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdwsif.f,v $
     . $',                                                             '
     .$Id: pdwsif.f,v 1.2 2001/06/14 18:27:04 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET NUMBER OF WORDS
      NWORD=ISIBUF(1)
C
C  COMPUTE NUMBER OF RECORDS
      LRCPD2=LRCPDI*2
      NREC=IUNRCD(NWORD,LRCPD2)
C
C  WRITE RECORDS
      CALL WVLRCD (KPDSIF,ISIREC,NREC,ISIBUF,LRCPDI,IERR)
C
      RETURN
C
      END
