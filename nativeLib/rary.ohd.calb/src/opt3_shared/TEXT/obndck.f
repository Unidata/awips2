C     MEMBER OBNDCK
C
      SUBROUTINE OBNDCK(PARM,OPID,OPNEW,VALUE,DELTA,CHECKL,CHECKU,
     *NPER,ICLOSE)
C
C......................................
C     THIS SUBROUTINE CHECKS TO BE SURE THAT A PARAMETER'S
C     INITIAL VALUE IS NOT OUTSIDE ITS BOUNDS.
C......................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL   JUNE 1981   VERSION 1
C......................................
C
      INCLUDE 'common/ionum'
C
      DIMENSION PARM(2),OPID(2),OPNEW(2)
      DIMENSION OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/obndck.f,v $
     . $',                                                             '
     .$Id: obndck.f,v 1.2 1996/07/11 20:46:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      CALL FSTWHR('OBNDCK  ',0,OLDOPN,IOLDOP)
C
      IF(VALUE.GE.CHECKL) GO TO 26
C
      WRITE(IPR,900) VALUE,PARM,OPID,OPNEW,CHECKL
  900 FORMAT(1H0,10X,12H**WARNING** ,
     *20HAN INITIAL VALUE OF ,F9.3,15H FOR PARAMETER ,
     *2A4,13H (OPERATION: ,2A4,8H ,NAME: ,2A4,1H),
     */11X,30HIS LESS THAN ITS LOWER BOUND (,F9.3,
     *28H) AND WILL NOT BE OPTIMIZED.)
      ICLOSE=1
      CALL WARN
C
   26 IF(VALUE.LE.CHECKU) GO TO 40
C
      WRITE(IPR,902) VALUE,PARM,OPID,OPNEW,CHECKU
  902 FORMAT(1H0,10X,12H**WARNING** ,
     *20HAN INITIAL VALUE OF ,F9.3,15H FOR PARAMETER ,
     *2A4,13H (OPERATION: ,2A4,8H ,NAME: ,2A4,1H),
     */11X,33HIS GREATER THAN ITS UPPER BOUND (,F9.3,
     *28H) AND WILL NOT BE OPTIMIZED.)
      ICLOSE=1
      CALL WARN
C
   40 CALL FSTWHR(OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
      END
