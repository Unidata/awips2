C MEMBER EFCLST
C  (from old member EEFCLST)
C
      SUBROUTINE EFCLST(JDFC,KHFC,JDLST,KHLST)
C
C   THIS SUBROUTINE CONVERTS A JULIAN DAY AND HOUR IN
C   FORECAST INTERNAL TIME TO A JULIAN DAY AND HOUR IN
C   LOCAL STANDARD TIME.
C
C   THIS SUBROUTINE WAS WRITTEN BY GERALD N DAY.
C
      INCLUDE 'common/fctime'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/etime'
C
      DIMENSION SBNAME(2),OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/efclst.f,v $
     . $',                                                             '
     .$Id: efclst.f,v 1.1 1995/09/17 19:18:39 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SBNAME/4HEFCL,4HST  /
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** EFCLST ENTERED)
C
      LOCAL=FLOCAL
C
      JDLST=JDFC
      KHLST=KHFC+LOCAL
      IF(KHLST.LE.24) GO TO 50
      KHLST=KHLST-24
      JDLST=JDLST+1
      GO TO 999
   50 IF(KHLST.GT.0) GO TO 999
      KHLST=KHLST+24
      JDLST=JDLST-1
  999 CONTINUE
C
      LOCAL=0
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
