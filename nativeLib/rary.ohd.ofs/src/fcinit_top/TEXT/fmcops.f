C MEMBER FMCOPS
C  (from old member FCFMCOPS)
C
C DESC 'SCAN C ARRAY TO FIND HOW MANY OPERATIONS SAVE CARRYOVER'
C.......................................................................
C                             LAST UPDATE: 07/27/95.12:18:27 BY $WC21DT
C
      SUBROUTINE FMCOPS(NCOPS,C,NC)
C
C  NCOPS = RETURNED VALUE = NUMBER OF OPERATIONS SAVING CARRYOVER
C
      DIMENSION C(NC)
C  ROUTINE ORIGINALLY WRITTEN BY --
C    ED JOHNSON -- HRL -- 19 OCT 1979
C.......................................................................
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fmcops.f,v $
     . $',                                                             '
     .$Id: fmcops.f,v 1.2 1996/01/17 18:54:01 page Exp $
     . $' /
C    ===================================================================
C
C
C  TRACE LEVEL=2
C
      IF(ITRACE.GE.2)WRITE(IODBUG,901)
 901  FORMAT(18H ** FMCOPS ENTERED)
      NCOPS=0
      IF(NC.LE.0)RETURN
      ILOC=1
 100  IOP=C(ILOC)
      IF(IOP.EQ.-1)RETURN
      NCOPS=NCOPS+1
      ILOC=C(ILOC+1)
      IF(ILOC.LE.NC)GO TO 100
      WRITE(IPR,900)
 900  FORMAT('0**ERROR**  SCAN OF C ARRAY BY ',
     .  26HSUBROUTINE FMCOPS ABENDED.,/,
     .  50H          COUNT OF OPERATIONS SAVING CARRYOVER IS ,
     .  31HSUSPECT.  PROCESSING CONTINUES.)
      CALL ERROR
      RETURN
      END
