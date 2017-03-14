C MEMBER IFRUN
C  (from old member FCIFRUN)
C
C DESC SEE IF A DATE AND TIME ARE WITHIN THE RUN PERIOD
C
C...............................................................
C
      FUNCTION IFRUN(IFRDAY,IFRTIM)
C
C..............................................................
C  FUNCTION IFRUN CHECKS TO SEE IF A PASSED DATE AND TIME ARE
C  WITHIN THE RUN PERIOD (I.E. - THEY ARE GREATER THAN THE
C  STARTING DATE AND TIME AND < OR = TO THE ENDING DATE AND TIME).
C
C  VALUE RETURNED = 0 IF DATE/TIME ARE NOT WITHIN THE RUN PERIOD,
C                 = 1 IF DATE/TIME ARE WITHIN THE RUN PERIOD
C
C......................................................................
C
C  ORIGINALLY PROGRAMMED BY --
C         JTOSTROWSKI -- HRL -- 122282
C
C.............................................................
C
      INCLUDE 'common/fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/ifrun.f,v $
     . $',                                                             '
     .$Id: ifrun.f,v 1.1 1995/09/17 19:08:40 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IGT,ILT,IEQ/2HGT,2HLT,2HEQ/
C
      IFRUN = 0
C
      CALL FDATCK(IFRDAY,IFRTIM,IDARUN,IHRRUN,IGT,ISTART)
      IF (ISTART.EQ.0) GO TO 99
      CALL FDATCK(IFRDAY,IFRTIM,LDARUN,LHRRUN,ILT,IBEND)
      CALL FDATCK(IFRDAY,IFRTIM,LDARUN,LHRRUN,IEQ,IEQEND)
      IF (IBEND.EQ.1.OR.IEQEND.EQ.1) GO TO 50
      GO TO 99
C
   50 IFRUN = 1
   99 RETURN
      END
