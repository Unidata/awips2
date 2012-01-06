C MEMBER PDGTPP
C  (from old member PP24DATA)
C
C                             LAST UPDATE: 02/06/95.17:23:03 BY $WC21DT
C
      SUBROUTINE PDGTPP(IDATA,PP24,IHOUR,IPP)
C
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdbdta'
      INTEGER*2 IDATA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdgtpp.f,v $
     . $',                                                             '
     .$Id: pdgtpp.f,v 1.1 1995/09/17 18:43:59 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF(IDATA.EQ.-9) GO TO 100
      PP24=IDATA/10 + 3000
      IPP=PP24 + .001
       IHOUR=3*IABS(((IPP-3000)*10) - IDATA)
      PP24=PP24/100.0
      GO TO 999
C
100   PP24=-9999.00
      IHOUR=0
      IPP=MISSPP
999   CONTINUE
      IF(IPDDB.EQ.1) WRITE(IOGDB,2000) IDATA,PP24,IHOUR,IPP
2000  FORMAT(' PDGTPP IDATA,PP24,IHOUR,IPP=',I8,F10.3,I3,I8)
      RETURN
      END
