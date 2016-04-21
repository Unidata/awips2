C MODULE FWTPDB
C-----------------------------------------------------------------------
C
      SUBROUTINE FWTPDB (EXTLOC,IDT,UNITS,NPDT,DATA,LWKBUF,WKBUF,IERR)
C
C  THIS ROUTINE WRITES OUTPUT AND UPDATE TIME SERIES TO THE
C  PROCESSED DATA BASE USING ROUTINE WRPDD.
C
C   WRITTEN BY ERIC ANDERSON - HRL  JUNE 1981
C
      DIMENSION EXTLOC(*),DATA(*),WKBUF(LWKBUF)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/fcfutp'
      COMMON/PRDQPF/IRDQPF
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/fwtpdb.f,v $
     . $',                                                             '
     .$Id: fwtpdb.f,v 1.3 2000/12/18 23:20:10 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.2) WRITE (IODBUG,*) 'ENTER FWTPDB'
C
      IBUG=IFBUG('FPDB')
C
      IERR=0
      CALL UMEMOV ('FWTPDB  ',OPNAME,2)
      IOPNUM=0
C
C  COMPUTE PDB JULIAN HOUR OF FIRST DATA VALUE TO BE WRITTEN
      IHR=IHRRUN+IDT
      IF (IHR.GT.24)IHR=24
      JHOUR=(IDA-1)*24+IHR+NHOPDB
C
C  COMPUTE NUMBER OF TIME PERIODS TO BE WRITTEN AND NUMBER OF VALUES
      NUM=LDA*24/IDT+LHR/IDT-(IDA*24/IDT+(IHR-1)/IDT)
      NVAL=NUM*NPDT
C
C  COMPUTE LOCATION OF FIRST VALUE IN DATA() TO BE WRITTEN
      L=((IDA-IDADAT)*24/IDT+(IHR-1)/IDT)*NPDT+1
C
C  COMPUTE JULIAN HOUR OF FIRST FUTURE TIME PERIOD
      JHRFUT=0
      IRDQPF=0
      IF (IFPR.EQ.-1) IRDQPF=IFPR
      IF (IFPR.GE.0) THEN
         IF (MOD(IFPR,IDT).EQ.0) IRDQPF=IFPR
         IF (MOD(IFPR,IDT).NE.0) THEN
            IRDQPF=((IFPR+(IDT/2))/IDT)*IDT
            ENDIF
         ENDIF
      IF (LDACPD*24+LHRCPD.GE.LDA*24+(LHR/IDT)*IDT) GO TO 10
         JHRFUT=(LDACPD-1)*24+(LHRCPD/IDT)*IDT+IDT+NHOPDB
         IF (JHRFUT.LT.JHOUR) JHRFUT=JHOUR
C
C  WRITE DATA
10    LDATA=L+NVAL
      ICALL=1
      IREC=EXTLOC(4)
      CALL WPRDD (EXTLOC(1),EXTLOC(3),JHOUR,IDT,NUM,UNITS,NVAL,
     *  LDATA,DATA(L),JHRFUT,ICALL,LWKBUF,WKBUF,IREC,IER)
      IF (IER.NE.0) THEN
         CALL FWPRDE (EXTLOC(1),EXTLOC(3),JHOUR,IDT,NUM,UNITS,NVAL,
     *     LDATA,ICALL,LWKBUF,IER)
         IF (IER.EQ.2.OR.IER.EQ.7) THEN
C        TIME SERIES TRUNCATED OR CANNOT PRESERVE MINDAYS
            ELSE
               IERR=1
            ENDIF
         ENDIF
C
      IF (IBUG.EQ.1) THEN
         WRITE (IODBUG,20) (EXTLOC(I),I=1,4),IDT,UNITS,IHR,JHOUR,
     *     NUM,L,NVAL,JHRFUT
20    FORMAT (' FWTPDB DEBUG=',2A4,1X,A4,F8.2,1X,I4,1X,A4,6I8)
         LVAL=NVAL+L-1
         WRITE (IODBUG,30) (DATA(I),I=L,LVAL)
30    FORMAT(' ',12F10.2)
         ENDIF
C
      IF (ITRACE.GE.2) WRITE (IODBUG,*) 'EXIT FWTPDB'
C
      RETURN
C
      END
