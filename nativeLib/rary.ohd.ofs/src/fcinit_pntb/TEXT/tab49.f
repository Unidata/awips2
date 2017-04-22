C MEMBER TAB49
C  (from old member FCTAB49)
C
C @PROCESS LVL(77)
C                             LAST UPDATE: 04/25/95.13:32:09 BY $WC20SV
C
       SUBROUTINE TAB49(TO, LEFT, IUSET, NXT, LWORK )
       INTEGER TO(*)
       DIMENSION ANAME(2), SNAME(2)
       INTEGER LEFT, IUSET, NXT, LWORK
       INCLUDE 'common/ionum'
       INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab49.f,v $
     . $',                                                             '
     .$Id: tab49.f,v 1.1 1995/09/17 18:49:26 dws Exp $
     . $' /
C    ===================================================================
C
       DATA ANAME /4HBEGA,4HSSIM/
       DATA SNAME /4HTAB4,4H9   /

       CALL FOPCDE(ANAME, IEX49)
       CALL FPRBUG( SNAME, 1, IEX49, IBUG )

       IF (LEFT.LT.5) THEN
          WRITE(IPR, 901)
  901     FORMAT( 1H0, 10X, 9H**ERROR**,
     1 42HTHERE IS INSUFFICIENT SPACE IN THE T ARRAY,
     2 19HFOR THIS OPERATION. )
          CALL ERROR
          RETURN
       END IF

       IUSET = 4
       TO(1) = 49
       TO(2) = 4 + NXT
       TO(3) = 0
       TO(4) = 0

       END
