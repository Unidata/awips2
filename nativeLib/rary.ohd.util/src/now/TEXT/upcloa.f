C MODULE UPCLOA
C  =====================================================================
C  pgm: UPCLOA() .. close all open files
C
C  use:     CALL UPCLOA()
C
C  rqd: common:  UPDAIO
C  rqd: subrtn:  UPCLOS
C  =====================================================================
      SUBROUTINE UPCLOA()

      EXTERNAL      UPCLOS

      INTEGER       IUN,IRECL,ISTAT
 
      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upcloa.f,v $
     . $',                                                             '
     .$Id: upcloa.f,v 1.2 2001/06/13 09:44:06 mgm Exp $
     . $' /
C    ===================================================================
C

        IUN = 0
  100   IF (IUN .GE. 99) GO TO 120
          IUN = IUN+1
          IRECL = UPRECL(IUN)
          IF (IRECL.GE.0 .AND. IUN.NE.UU .AND. IUN.NE.UE) THEN
            IF (IUN.NE.UR .AND. IUN.NE.UW) CALL UPCLOS (IUN,' ',ISTAT)
          ENDIF
          GOTO 100
  120   CONTINUE

      RETURN
      END
