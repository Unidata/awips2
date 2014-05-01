C MODULE PINA7
C-----------------------------------------------------------------------
C
      SUBROUTINE PINA7 (P,LP,IP,C,LC,IC,IBUG,IER)
C.......................................................................
C
C     THIS SUBROUTINE COMPUTES THE 2*S/DT+O VS O AND 2*S/(DT/4)+O VS O
C     TABLES NEEDED BY THE ATLANTA K PROCEDURE, AND STORES THESE
C     TABLES IN THE P ARRAY.
C.......................................................................
C
C     SUBROUTINE ORIGINALLY PROGRAMMED BY
C                GEORGE F. SMITH - HRL   DECEMBER 1979
C     UPDATED MARCH 1982 TO ALLOW METRIC AND ENGLISH UNITS
C.......................................................................
C
C     VARIABLES IN ARGUMENT LIST
C
C        1. P    - THE P ARRAY
C        2. LP   - TOTAL AVAILABLE LENGTH OF P ARRAY
C        3. IP   - USED SPACE IN THE P ARRAY
C        4. C    - THE C ARRAY
C        5. LC   - TOTAL AVAILABLE LENGTH OF THE C ARRAY
C        6. IC   - USED SPACE IN THE C ARRAY
C        7. IBUG - PRINT DEBUG FLAG, PRINT IF IBUG = 1
C.......................................................................
C
      INCLUDE 'common/fdbug'
      COMMON/FATLGK/IATL,C1,C2
C
      DIMENSION P(1),C(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pina7.f,v $
     . $',                                                             '
     .$Id: pina7.f,v 1.3 2006/03/16 16:35:58 xfan Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GT.0) WRITE(IODBUG,*) 'ENTER PINA7'
C
      XITA=P(5)-0.01
      IBK=P(18)
      IBK1=IBK+1
      IPASS=1
C
    5 IP=IP+1
      CALL CHECKP(IP,LP,IER)
      IF(IER.EQ.1)GO TO 991
C
      IF(IPASS.EQ.1)IBOS=P(16)+1
      IF(IPASS.EQ.2)IBOS=IP
      IBOS1=IBOS+1
C
      NPKQ=P(IBK)
C
      IF(NPKQ.GT.0)GO TO 10
C
      P(IBOS)=2.01
      IP=IP+2
      CALL CHECKP(IP,LP,IER)
      IF(IER.EQ.1)GO TO 992
      P(IBOS1)=0.
      P(IBOS1+1)=0.
C
      IP=IP+2
      CALL CHECKP(IP,LP,IER)
      IF(IER.EQ.1)GO TO 992
      P(IBOS1+2)=1.E6
      S=1.E6*P(IBK1)
      P(IBOS1+3)=2.*S/XITA+1.E6
      GO TO 1000
C
   10 NPOS=0
C
      S=0.
      Q1=0.
C
      DO 90 I=1,NPKQ
      IF(I+1.GT.NPKQ)GO TO 100
      J=I+1
C
      DELK=ABS(P(IBK+J*2-1) - P(IBK+I*2-1))
      DELQ=ABS(P(IBK+J*2) - P(IBK+I*2))
C
      ISEGS=1
      IF(DELK.NE.0)ISEGS=(DELQ+C1*DELK)/C2 + 1.5
      IF(ISEGS.GT.20)ISEGS=20
C
      IPART=0
C
   20 Q2=P(IBK+I*2) + DELQ*IPART/ISEGS
      IP=IP+2
      CALL CHECKP(IP,LP,IER)
      IF(IER.EQ.1)GO TO 992
      P(IBOS+NPOS*2+1)=Q2
      QBAR=(Q1+Q2)/2.
      S=FSERC7(LXXXX,QBAR,NPKQ,P(IBK1))*(Q2-Q1) + S
      P(IBOS+NPOS*2+2)=2.*S/XITA + Q2
      Q1=Q2
      NPOS=NPOS+1
      IPART=IPART+1
      IF(IPART.LT.ISEGS)GO TO 20
C
   90 CONTINUE
C
  100 Q2=P(IBK+I*2)
      IP=IP+2
      CALL CHECKP(IP,LP,IER)
      IF(IER.EQ.1)GO TO 992
      P(IBOS+NPOS*2+1)=Q2
      QBAR=(Q1+Q2)/2.
      S=FSERC7(LXXXX,QBAR,NPKQ,P(IBK1))*(Q2-Q1) + S
      P(IBOS+NPOS*2+2)=2.*S/XITA + Q2
      Q1=Q2
      NPOS=NPOS+1
C
      IF(P(IBK+2).EQ.0.0)GO TO 200
C
      IP=IP+2
      CALL CHECKP(IP,LP,IER)
      IF(IER.EQ.1)GO TO 992
      LEND=IBOS1 + NPOS*2
      DO 110 I=IBOS1,LEND
      J=LEND - I + IBOS1
      P(J+2)=P(J)
  110 CONTINUE
C
      P(IBOS1)=0.
      P(IBOS1+1)=0.
      NPOS=NPOS+1
C
  200 Q2=1.E6
      IP=IP+2
      CALL CHECKP(IP,LP,IER)
      IF(IER.EQ.1)GO TO 992
      P(IBOS+NPOS*2+1)=Q2
      QBAR=(Q1+Q2)/2.
      S=FSERC7(LXXXX,QBAR,NPKQ,P(IBK1))*(Q2-Q1) + S
      P(IBOS+NPOS*2+2)=2.*S/XITA + Q2
      NPOS=NPOS+1
C
      P(IBOS)=NPOS + .01
C
 1000 IF(IPASS.EQ.2)GO TO 1100
      IF (NPKQ .EQ. 0) GO TO 1050
      DO 1010 I=1,NPKQ
      IF(XITA/2.0.GT.P(IBK+I*2-1))GO TO 1020
 1010 CONTINUE
      GO TO 1050
C
 1020 IPASS=2
      XITA=XITA/4.
      GO TO 5
C
  991 IP=IP-1
      GO TO 999
  992 IP=IP-2
      GO TO 999
C
 1050 CONTINUE
      IP = IP + 1
      CALL CHECKP(IP,LP,IER)
      IF (IER.EQ.1) GO TO 991
C
C  INDICATE THAT THERE ARE NO VALUES IN 'QUARTER' TABLE.
      P(IP) = 0.01
C
 1100 IF (IBUG.GT.0) THEN
         WRITE (IODBUG,603) LP,IP
  603 FORMAT(' OF THE ',I6,' AVAILABLE LOCATIONS IN THE P ARRAY ',
     1   I5,' ARE USED BY THIS OPERATION')

C dws    P(16) was placed into an integer to replace it in the next
C dws     couple of statements to avoid compiler warnings ... 2006-01-23

         NUMP16 = P(16)

         NPOS=P(NUMP16+1)
         WRITE (IODBUG,601) NPOS
  601 FORMAT (' NUMBER OF PAIRS OF O AND 2*S/DT+O VALUES = ',I3)
         CALL PROS7 (P(NUMP16+2),NPOS,IODBUG)
         IF (IPASS.EQ.2) THEN
            NPOS=P(IBOS)
            WRITE (IODBUG,602) NPOS
  602 FORMAT(' FOR ONE QUARTER OF THE ORIGINAL TIME INTERVAL ',
     1   ' THE NUMBER OF PAIRS OF O AND 2*S/DT+O VALUES = ',I3)
            CALL PROS7 (P(IBOS1),NPOS,IODBUG)
	    ENDIF
         WRITE (IODBUG,604) (P(I),I=1,IP)
  604 FORMAT (' THE P ARRAY CONTAINS: ' / (1X,10G10.4))
	 ENDIF
C
999   RETURN
C
      END
