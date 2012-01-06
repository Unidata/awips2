C$PRAGMA C (UDATL)
C MEMBER FSETNW
C  (from old member FCFSETNW)
C
C DESC 'SET WALL CLOCK TIME IN COMMON BLOCK /FCTIME/'
      SUBROUTINE FSETNW
C.......................................................................
C
C  SET VALUE OF NOW IN COMMON BLOCK FCTIME
C
C  ROUTINE ORIGINALLY WRITTEN BY --
C    ED JOHNSON -- HRL -- 11 OCT 1979
C
C.......................................................................
      INCLUDE 'common/fctime'
      DIMENSION INTIME(6)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fsetnw.f,v $
     . $',                                                             '
     .$Id: fsetnw.f,v 1.3 2002/02/11 13:15:34 michaelo Exp $
     . $' /
C    ===================================================================
C
      CALL UDATL(INTIME)
      NOW(1)=INTIME(3)
      NOW(2)=INTIME(4)
      NOW(3)=INTIME(1)
      NOW(4)=INTIME(5)
      NOW(5)=INTIME(6)
      RETURN
      END
