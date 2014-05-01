C MEMBER PIN63
C
      SUBROUTINE PIN63(PO,LEFTP,IUSEP,TS,MTS)
C.......................................................................
C  THIS IS THE INPUT SUBROUTINE FOR THE SET-TS OPERATION.
C.......................................................................
C  SUBROUTINE INITIALLY WRITTEN BY. . .
C            JASON SPERFSLAGE - HRC   DECEMBER 2000
C.......................................................................
      DIMENSION PO(1),TS(MTS),TSID(2)
      REAL*4 VAL
      REAL*4 OPVER
C  
      DIMENSION SNAME(2)
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin63.f,v $
     . $',                                                             '
     .$Id: pin63.f,v 1.1 2002/05/15 13:41:06 hank Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C  SHOULD ALWAYS BE THE 1ST EXECUTABLE STATEMENT IN PRIMARY SUBROUTINE
      CALL FPRBUG (SNAME,1,63,IBUG)
C.......................................................................
C  TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,16H** PIN63 ENTERED)
C.......................................................................
C  INITIALIZE DATA VARIABLES
C  NUMBER OF SPACES REQUIRED FOR THIS OPERATION IN THE P ARRAY
      IUSEP=6
C  INTIAL VALUE OF TIME SERIES CONSTANT
      VAL=0.
C  VERSION NUMBER FOR THIS OPERATION
      OPVER=1.0
C.......................................................................
C  CHECK TO SEE IF ENOUGH SPACE IS LEFT IN P()
      CALL CHECKP(IUSEP,LEFTP,IERR)
      IF (IERR.EQ.1) THEN
         WRITE (IPR,905) (SNAME(I),I=1,2),IUSEP,LEFTP
905      FORMAT (1H0,10X,'**ERROR** ',2A4,' NEEDS',I4,
     +     ' WORDS IN P ARRAY,',/,10X,' BUT ONLY',I4,' ARE AVAILABLE.')
         CALL ERROR
         IUSEP=0
         RETURN
      ENDIF
C.......................................................................
C  READ PARAMETERS FROM SET-TS INPUT CARD
      READ (IN,901) TSID,TYPE,IDT,VAL
  901 FORMAT (2X,2A4,1X,A4,3X,I2,3X,E10.3)
C.......................................................................
C  CHECK IF THE SPECIFIED TIME SERIES EXISTS
      CALL FINDTS(TSID,TYPE,IDT,LOCD,LOCTS,DIM)
      IF (LOCTS.LE.0) GO TO 200
C.......................................................................
C  STORE PARAMETRIC DATA IN THE P ARRAY
      PO(1)=OPVER+0.01
      PO(2)=TSID(1)
      PO(3)=TSID(2)
      PO(4)=TYPE
      PO(5)=IDT+0.01
      PO(6)=VAL
C.......................................................................
C  P ARRAY LOADED AS REQUIRED, EXIT PIN63
C
      GO TO 300
C.......................................................................
C  TIME SERIES DOES NOT EXIST
  200 WRITE (IPR,902) TSID,TYPE,IDT
  902 FORMAT (1H0,10X,69H**ERROR** THE TIME SERIES TO BE SET BY SET-TS
     1HAS NOT BEEN DEFINED.,3X,5HI.D.=,2A4,3X,5HTYPE=,A4,3X,3HDT=,I2,
     2/1X,5HHOURS,16X,40HTHUS, THIS OPERATION CANNOT BE EXECUTED.)
      CALL ERROR
      IUSET=0
C.......................................................................
  300 IF (ITRACE.GE.1) WRITE(IODBUG,903)
  903 FORMAT(1H0,13H** EXIT PIN63)
      RETURN
      END
