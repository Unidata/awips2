C     MEMBER OPTYPE
C
      SUBROUTINE OPTYPE(A,MA,OA,MOA,ILOCOA,MILOC,IZY,IPASS1,IFINSH,
     *NPARM,NCOUN,NN,IPMOVE)
C
C.......................................
C     THIS SUBROUTINE CALLS THE SPECIFIED OPTIMIZATION SCHEME.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL   MAY 1981   VERSION 1
C     REVISED FOR INCLUSION OF SCE OPTIMIZATION SCHEME BY
C            QINGYUN DUAN - UNIVERSITY OF ARIZONA OCTOBER 1991
C.......................................
C
      DIMENSION A(MA),OA(MOA),ILOCOA(MILOC)
C
      INCLUDE 'ocommon/opschm'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/optype.f,v $
     . $',                                                             '
     .$Id: optype.f,v 1.2 1996/07/11 20:53:18 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF(ITRACE.GE.1) WRITE(IODBUG,1000)
 1000 FORMAT(1H0,17H** OPTYPE ENTERED)
C
C     DETERMINE TYPE OF OPTIMIZATION SCHEME.
      GO TO (100,200,300), ISCHEM
C
C     CALL PATTERN SEARCH ROUTINE.
  100 CALL OPSRCH(A,MA,IFINSH,IPASS1,OA,MOA,ILOCOA,MILOC,NPARM,IZY,
     *NCOUN,NN,IPMOVE)
C
      GO TO 99
C
C     CALL ARS ROUTINE
  200 CONTINUE
      CALL OARSCH(A,MA,IFINSH,IPASS1,OA,MOA,ILOCOA,MILOC,NPARM)
C
      GO TO 99
C
C     CALL SCE-UA ROUTINE
  300 CONTINUE
      CALL OSCEUA(A,MA,IFINSH,IPASS1,OA,MOA,ILOCOA,MILOC,NPARM)
C
   99 CONTINUE
      IF(ITRACE.GE.1) WRITE(IODBUG,1002)
 1002 FORMAT(1H0,14H** EXIT OPTYPE)
      RETURN
      END
