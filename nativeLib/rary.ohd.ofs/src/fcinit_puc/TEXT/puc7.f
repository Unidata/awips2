C MODULE PUC7
C-----------------------------------------------------------------------
C
      SUBROUTINE PUC7 (P,MC,LCO,C)
C
C.......................................................................
C
C     THIS ROUTINE WRITES CARD IMAGES TO UNIT IPU
C     IN A FORMAT THAT ROUTINE PIN7 CAN READ TO
C     REPRODUCE THE EXISTING LAG/K OPERATION.
C.......................................................................
C
C      ROUTINE ORIGINALLY PROGRAMMED BY
C                 GEORGE F. SMITH  HRL - NOVEMBER 1979.
C                     UPDATED MARCH 1982 TO PUNCH IN ENGLISH OR
C                       METRIC UNITS
C                     UPDATED JUNE 1989 FOR FT. WORTH TRANSMISSION
C                       LOSS COMPUTATIONS
C.......................................................................
C
C
      CHARACTER*20 STRING
C
      include 'common/ionum'
      COMMON/PUDFLT/IPDFLT
      COMMON/FENGMT/METRIC
C
      DIMENSION P(1),C(1),SUBN(2),IPMETR(2)
C
      LOGICAL FOP7,MEANQ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc7.f,v $
     . $',                                                             '
     .$Id: puc7.f,v 1.4 1999/04/23 17:33:15 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBN,LT,NOP/4HPUC7,4H    ,1,7/
      DATA L3/4HL3  /
      DATA IPMETR/4HENGL,4HMETR/
      DATA RES,SNGL/4HRES-,4HSNGL/
C
      CALL FPRBUG (SUBN,LT,NOP,IB)
C
      DTYPIN=P(4)
      CALL FDCODE (DTYPIN,STDUNT,INDIMS,MSNG,NUPDT,TSCALE,NEXTRA,IER)
      MEANQ=.FALSE.
      IF (INDIMS.EQ.L3) MEANQ=.TRUE.
C
      IMETR=P(10)
      IF (METRIC.EQ.0) IMETR=0
      IF (METRIC.EQ.1) IMETR=1
      IF (IMETR.EQ.1) GO TO 10
      CALL FCONVT ('CMS ','L3/T',IU,CFSM,CFSA,IER)
      CALL FCONVT ('CMSD','L3  ',IU,CFDM,CFDA,IER)
C
10    NP=P(16)
      NC=C(1)
C
      IF (IB.EQ.1) CALL FPRPC7 (NP,P,NC,C)
C
      ITA=P(5)
      ITB=P(9)
      TLRC=P(11)
      QBNTL=P(12)
      IF (IMETR.EQ.0) QBNTL=QBNTL*CFSM
      JLAG=P(19)
      IBK=P(18)
      IBK1=IBK+1
      JK=P(IBK)
C
C  CHECK IF QBNTL IS A REAL VALUE
      IBEG=1
      NCHAR=LEN(STRING)
      NDEC=0
      IPRERR=0
      CALL UFF2A (QBNTL,STRING,IBEG,NCHAR,NDEC,IPRERR,IPR,IERR)
      IF (IERR.NE.0.OR.STRING.EQ.'NaN') THEN
         WRITE (IPR,20)
20    FORMAT ('0**WARNING** IN PUC7 - VALUE OF VARIABLE QBNTL IS NOT ',
     *   'A REAL NUMBER AND WILL BE SET TO ZERO.')
         CALL WARN
         QBNTL=0.0
         ENDIF
C
      IF (P(2).NE.RES.OR.P(3).NE.SNGL) GO TO 40
C
      WRITE (IPU,30) (P(I),I=2,4),ITA,JLAG,JK,
     1  IPMETR(IMETR+1),TLRC,QBNTL
30    FORMAT (2A4,1X,A4,1X,I2,18X,I5,1X,I5,1X,A4,F5.2,F10.1)
      GO TO 60
C
40    WRITE (IPU,50) (P(I),I=2,4),ITA,(P(I),I=6,8),ITB,JLAG,JK,
     1  IPMETR(IMETR+1),TLRC,QBNTL
50    FORMAT(2A4,1X,A4,1X,I2,1X,2A4,1X,A4,1X,I2,1X,I5,1X,I5,1X,A4,
     1   F5.2,F10.1)
C
60    IX=1
      IF (JLAG.GT.0) IX=JLAG*2
C
      ICONV=0
      IF (IX.EQ.1.OR.IMETR.EQ.1) GO TO 80
      ICONV=1
      DO 70 I=2,IX,2
         P(19+I)=P(19+I)*CFSM
70       CONTINUE
C
80    CALL FFPURL (P(20),IX,IB)
C
      IF (ICONV.EQ.0) GO TO 100
C
      DO 90 I=2,IX,2
         P(19+I)=P(19+I)/CFSM
90       CONTINUE
C
100   IX=1
      IF (JK.GT.0)IX=JK*2
C
      ICONV=0
      IF (IX.EQ.1.OR.IMETR.EQ.1) GO TO 120
      ICONV=1
      DO 110 I=2,IX,2
         P(IBK1+I-1)=P(IBK1+I-1)*CFSM
110      CONTINUE
C
120   CALL FFPURL (P(IBK1),IX,IB)
C
      IF (ICONV.EQ.0) GO TO 140
C
      ICONV=0
      DO 130 I=2,IX,2
         P(IBK1+I-1)=P(IBK1+I-1)/CFSM
130      CONTINUE
C
140   IZ=0
      MXLCO=C(5)
C
      IX=1
      IF (IPDFLT.EQ.0) GO TO 160
C
150   CALL FFPUIN (IZ,IX,IB)
C
      GO TO 280
C
160   IF (FOP7(P(19),P(20))) GO TO 180
      IF (FOP7(P(IBK),P(IBK1))) GO TO 170
C
      GO TO 150
C
170   IX=1
      I1=1
      CALL FFPUIN (I1,IX,IB)
      GO TO 240
C
180   IX=1
      CALL FFPUIN (MXLCO,IX,IB)
C
      IX=MXLCO*2
      IF (IX+LCO.GT.MC) THEN
         WRITE (IPR,190) IX,LCO,MC
190   FORMAT ('0**ERROR** IN PUC7 - NUMBER OF CARRYOVER VALUES (',I6,
     *   ') PLUS STARTING POSITION IN C ARRAY (',I5,
     *   ') EXCEEDS SIZE OF C ARRAY (',I5,').')
         CALL ERROR
         GO TO 280
         ENDIF
C
      ICONV=0
      IF (IMETR.EQ.1) GO TO 210
C
      ICONV=1
      DO 200 I=1,IX,2
         C(5+I)=C(5+I)*CFSM
200      CONTINUE
C
210   CALL FFPURL(C(6),IX,IB)
C
      IF (ICONV.EQ.0) GO TO 230
      ICONV=0
      DO 220 I=1,IX,2
         C(5+I)=C(5+I)/CFSM
220      CONTINUE
C
230   IF (FOP7(P(IBK),P(IBK1))) GO TO 240
C
      IX=1
      IF (IMETR.EQ.0) C(3)=C(3)*CFSM
      CALL FFPURL (C(3),IX,IB)
      IF (IMETR.EQ.0) C(3)=C(3)/CFSM
      GO TO 280
C
240   IX=3
      IY=2
      IF (MEANQ)IX=2
      IF (MEANQ)IY=3
      ICONV=0
      IF (IMETR.EQ.1) GO TO 260
C
      ICONV=1
      IF (MEANQ) GO TO 250
      C(2)=C(2)*CFSM
      C(3)=C(3)*CFSM
      C(4)=C(4)*CFDM
      GO TO 260
C
250   C(3)=C(3)*CFSM
      C(4)=C(4)*CFDM
C
260   CALL FFPURL (C(IY),IX,IB)
C
      IF (IMETR.EQ.1) GO TO 280
C
      IF (MEANQ) GO TO 270
C
      C(2)=C(2)/CFSM
      C(3)=C(3)/CFSM
      C(4)=C(4)/CFDM
      GO TO 280
C
270   C(3)=C(3)/CFSM
      C(4)=C(4)/CFDM
C
280   RETURN
C
      END
