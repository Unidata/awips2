C  =====================================================================
C  pgm: UDOE(IUNIT,CZ,STMT) .. Error output rtn for UDOPEN
C
C   in: IUNIT ...... unit number identifying the desired file - INT
C   in: CZ ......... i/o error number if applicable - INT
C   in: STMT ....... Short descriptive error statmenet - CHAR*24
C   in:                (Note, end of statement must be blank filled)
C   in: (common) ... block common UPDAIO contains unit numbers for
C   in:              i/o routine messages (see subrtn UPRIMO)
C
C  rqd: common:  UPDAIO
C  =====================================================================
      SUBROUTINE UDOE (IUNIT,CZ,STMT)

      INTEGER       IUNIT,CZ
      CHARACTER*24  STMT

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/udoe.f,v $
     . $',                                                             '
     .$Id: udoe.f,v 1.2 2001/06/12 18:55:23 dws Exp $
     . $' /
C    ===================================================================
C

        IF( UE.GE.0 ) WRITE(UE,1111) IUNIT,CZ,STMT
 1111   FORMAT(' un-sys ',I3,' **ERROR** ',I5,',  DAIO - ',A)

      RETURN
      END
