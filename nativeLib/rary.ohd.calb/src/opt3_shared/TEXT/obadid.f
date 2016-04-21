C     MEMBER OBADID
C
      SUBROUTINE OBADID(OPID)
C
C...........................................
C     THIS SUBROUTINE PRINTS THE WARNING THAT THE SPECIFIED
C     OPERATION IDENTIFIER IS NOT VALID.
C...........................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL   JUNE 1981   VERSION 1
C...........................................
C
      DIMENSION OPID(2)
      DIMENSION OLDOPN(2)
C
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/obadid.f,v $
     . $',                                                             '
     .$Id: obadid.f,v 1.2 1996/07/11 20:46:19 dws Exp $
     . $' /
C    ===================================================================
C
C
      CALL FSTWHR('OBADID  ',0,OLDOPN,IOLDOP)
C
      WRITE(IPR,900) OPID
  900 FORMAT(1H0,10X,12H**WARNING** ,2A4,1X,42HIS NOT AN ACCEPTABLE OPER
     *ATION IDENTIFIER./,10X,35HTHIS PARAMETER WILL NOT BE CHANGED.)
C
      CALL WARN
C
      CALL FSTWHR(OLDOPN,IOLDOP,OLDOPN,IOLDOP)
      RETURN
      END
