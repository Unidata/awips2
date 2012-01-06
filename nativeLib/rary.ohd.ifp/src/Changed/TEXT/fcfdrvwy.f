C$PRAGMA C (CEX18)
C MEMBER FDRVWY
C  (from old member FCFDRVWY)
C
      SUBROUTINE FDRVWY(P,MP,C,MC,T,MT,D,MD,IHZERO,NUMOP,IERR)
C.......................................
C     DRIVWY CALLS EXECUTION SUBROUTINES FOR OPERATIONS THAT CAN USE
C        THE WATER YEAR SCRATCH FILE.  THESE ARE OPERATIONS NUMBER
C        18.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL     APRIL 1980
C.......................................
      DIMENSION P(MP),C(MC),D(MD)
      INTEGER T(MT)

      Integer event_loop_exit_status
      Common /cex_exit_status/ event_loop_exit_status
C
      INCLUDE 'common/fts'
      INCLUDE 'common/fratng'
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     1   LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/FCOPPT/LOCT,LPM,LCO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Changed/RCS/fcfdrvwy.f,v $
     . $',                                                             '
     .$Id: fcfdrvwy.f,v 1.1 2002/05/15 13:44:46 hank Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** FDRVWY ENTERED)
C.......................................
C     GO TO THE PROPER SECTION FOR THE OPERATION.
      IF(NUMOP.EQ.18) THEN
        GO TO 118
      ELSE
        GO TO 190
      END IF 
C.......................................
C     EXECUTE PLOT TIME SERIES OPERATION.
  118 continue
C   CHECK IF OUTPUT IS REQUESTED
      IF(NOPROT.EQ.1) GO TO 99
      IDT=P(LPM+10)
      IHR=IHZERO+IDT
      LT1=LOCT+4
      NPLOTS=P(LPM+7)
      NTTS=P(LPM+8)
      LD1=T(LOCT+3)
      LD2=LD1+NPLOTS
      LD3=LD2+12
      LD4=LD3+121
      LD5=LD4+NTTS
      LD6=LD5+NTTS
      LD7=LD6+NPLOTS
CC      CALL CEX18(P(LPM),D,T(LT1),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),
CC     1D(LD6),D(LD7))
CC      call cex18 (P,D,T(LT1),IDARUN,IHRRUN, LDARUN, LHRRUN,
CC     1            IDA,IDADAT,IHR,NOUTZ,NOUTDS,IPLHY,LPM,
CC     2            event_loop_exit_status)
      ifirst_time = 1
      Call cex18(P, P, TS, TS, LPM,
     1           RTCVID, RTCVID, RTCVID, D, T(LT1),
     2           IDARUN,IHRRUN, LDARUN, LHRRUN,LDACPD,lhrcpd,
     3           idadat, T, ifirst_time,
     4           event_loop_exit_status, mp, c,
     5           IDA,IHR,NOUTZ,NOUTDS,IPLHY)
C      write(*,*)'event_loop_exit_status',event_loop_exit_status

      GO TO 99
C.......................................
C     OPERATION NOT INCLUDED.
  190 IERR=1
C.......................................
   99 CONTINUE
      RETURN
      END
