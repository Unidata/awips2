C MEMBER FWTCAL
C  (from old member FCFWTCAL)
C
      SUBROUTINE FWTCAL(EXTLOC,DTYPE,IDT,NPDT,UNITS,DIM,LD,NWORK,D,MD,
     1  MO,IYEAR,IERR)
C.......................................
C     THIS SUBROUTINE WRITES DATA TO THE NWSRFS CALIBRATION DATA FILES.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY ....
C           ERIC ANDERSON - HRL  APRIL 1981
C.......................................
      DIMENSION D(MD)
      DIMENSION EXTLOC(1),SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/WHERE/ISEG(2),IOPNUM,OPNAME(2)
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     1LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/CRWCTL/ISTOP,IERROR,IPRINT,ITSH,NTSH,ISFN,IRTSH
      COMMON/CPRCES/IPROC,MYSEQ,LSTSEQ(30)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/fwtcal.f,v $
     . $',                                                             '
     .$Id: fwtcal.f,v 1.1 1995/09/17 19:08:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HFWTC,4HAL  /
C.......................................
C     TRACE LEVEL=1
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** FWTCAL ENTERED)
C.......................................
C     INITIAL VALUES
      OPNAME(1)=SNAME(1)
      OPNAME(2)=SNAME(2)
C.......................................
C     CHECK VALUE OF LOCAL.
      IF(LOCAL.EQ.0) GO TO 103
      WRITE(IPR,901)
  901 FORMAT(1H0,10X,80H**ERROR** 'CALB' FILES CANNOT BE USED UNLESS LOC
     1AL=0 IN THE FCTIME COMMON BLOCK.)
      CALL ERROR
      IERR=1
      RETURN
C
C     CHECK PROGRAM BEING EXECUTED.
  103 IF(MAINUM.NE.3) GO TO 105
C.......................................
C     MANUAL CALIBRATION PROGRAM
      LENGTH=(24/IDT)*NPDT*31
      CALL MDYH1(LDA,LHR,MO,IDUM1,IYEAR,IDUM2,100,0,ZCODE)
      MYSEQ=EXTLOC(1)
C
C     CHECK IF MONTH NEEDS TO BE FILLED WITH MISSING DATA.
      IF(IDA.EQ.IDADAT) GO TO 115
      L=(IDA-IDADAT)*(24/IDT)*NPDT
      DO 114 I=1,L
      J=I-1
  114 D(LD+J)=-999.0
  115 IF(IDUM1.EQ.31) GO TO 119
      J=(LDA-IDADAT+1)*(24/IDT)*NPDT-1
      L=(31-IDUM1)*(24/IDT)*NPDT
      DO 116 I=1,L
  116 D(LD+J+I)=-999.0
  119 CONTINUE
C
      CALL WTFILE(DTYPE,IDT,MO,IYEAR,1,1,LENGTH,D(LD),0)
C
      IF(IERROR.EQ.0) RETURN
      WRITE(IPR,902)
  902 FORMAT(1H0,10X,76H**ERROR** PRECEDING ERRORS OCCURRED DURING WRITI
     1NG TO THE CALIBRATION FILES.)
      CALL ERROR
      IERR=1
      IERROR=0
      RETURN
  105 IF(MAINUM.NE.1) GO TO 110
C.......................................
C     OPERATION PROGRAM
      IERROR=0
      IER=0
      LEFTD=MD-NWORK+1
      CALL FWCLEX(EXTLOC,D(LD),D(NWORK),LEFTD,DTYPE,IDT,NPDT,UNITS,DIM,
     1IER)
      OPNAME(1)=SNAME(1)
      OPNAME(2)=SNAME(2)
      IF(IER.EQ.0) RETURN
      IERR=1
      RETURN
C.......................................
C     PROGRAM NOT INCLUDED.
  110 WRITE(IPR,903)
  903 FORMAT(1H0,10X,76H**ERROR** A CALIBRATION FILE WRITE ROUTINE IS NO
     1T INCLUDED FOR THIS PROGRAM.)
      CALL ERROR
      IERR=1
C.......................................
      RETURN
      END
