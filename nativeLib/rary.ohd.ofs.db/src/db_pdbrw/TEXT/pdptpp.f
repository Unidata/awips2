C MEMBER PDPTPP
C  (from old member PP24DATA)
C
C                             LAST UPDATE: 02/06/95.17:23:03 BY $WC21DT
C
      SUBROUTINE PDPTPP(PP24,IHOUR,IDATA)
C
      INCLUDE 'udebug'
      INTEGER*2 IDATA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdptpp.f,v $
     . $',                                                             '
     .$Id: pdptpp.f,v 1.1 1995/09/17 18:44:11 dws Exp $
     . $' /
C    ===================================================================
C
C
      IPP=PP24*100.0 +0.5
      IDATA = (IPP-3000)*10
      IF(IDATA.GT.0) IDATA=IDATA + IHOUR/3
      IF(IDATA.LE.0) IDATA=IDATA - IHOUR/3
      IF(IPDDB.EQ.1) WRITE(IOGDB,2000) PP24,IHOUR,IDATA
2000  FORMAT(' PDPTPP PP24,IHOUR,IDATA=',F10.3,2I8)
      RETURN
      END
