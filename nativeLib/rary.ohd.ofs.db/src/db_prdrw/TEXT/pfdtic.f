C MEMBER PFDTIC
C  (from old member PRDINCOR)
C-----------------------------------------------------------------------
C
      SUBROUTINE PFDTIC (ITYPE,INDXD,INDXIC)
C
C THIS ROUTINE LOOKS FOR A DATA TYPE IN THE INCORE DATA TYPE MANAGEMENT
C TABLE AND RETURNS ITS INDEX IN THE DATA DICTIONARY AND THE TABLE
C  IT RETURNS ZEROS IF NOT FOUND
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pfdtic.f,v $
     . $',                                                             '
     .$Id: pfdtic.f,v 1.1 1995/09/17 18:45:35 dws Exp $
     . $' /
C    ===================================================================
C
C
      INDXIC=0
      CALL PFDTYP (ITYPE,INDXD)
      IF (INDXD.NE.0) GO TO 10
      WRITE (LPE,2000) ITYPE
2000  FORMAT (' **NOTE** PFDTIC WAS PASSED A DATA TYPE THAT IS NOT',
     *  ' IN THE DATA DICTIONARY: ',A4)
      GO TO 999
C
C SHOULD THIS GO IN CORE
C
10    IF (DATFIL(3,INDXD).EQ.0) GO TO 999
C
C ARE THERE ANY IN CORE
C
      IF (DATFIL(10,INDXD).EQ.0) GO TO 999
C
C YES THERE ARE
C
      INDXIC=DATFIL(10,INDXD)
      IF (IPRTR.GT.0) WRITE (IOGDB,2001) ITYPE,INDXIC
2001  FORMAT (' *** EXIT PFDTIC - ITYPE=',A4,3X,'INDXIC=',I4)
C
999   RETURN
C
      END
