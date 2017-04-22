C MODULE PUC2
C-----------------------------------------------------------------------
C
      SUBROUTINE PUC2 (PO,CO)
C
C  THIS ROUTINE PUNCHES PARAMETERS AND CARRYOVER VALUES FOR THE 
C  THE UNIT HYDROGRAPH OPERATION IN THE FORMAT REQUIRED BY THE
C  INPUT ROUTINE PIN2.
C
C  ROUTINE INITIALLY WRITTEN BY - LARRY BRAZIL - HRL -8/1979
C
      CHARACTER*8 RTNNAM,OLDNAM
      DIMENSION PO(*),CO(*)
      DIMENSION DCARRY(2),CARRY(2),P(7)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/pudflt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc2.f,v $
     . $',                                                             '
     .$Id: puc2.f,v 1.2 2002/02/11 13:54:01 michaelo Exp $
     . $' /
C    ===================================================================
C
      DATA DCARRY/4H   C,4HARRY/,BLANK/4H    /
      DATA METR/4HMETR/,IENG/4HENGL/
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER PUC2'
C
      RTNNAM='PUC2'
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OLDNAM,IOLDOP)
C
      NCARD=0
C
C  PUNCH CARD 1
      NCARD=NCARD+1
      IF (IPDFLT.EQ.0) GO TO 40
         CARRY(1)=BLANK
         CARRY(2)=BLANK
         GO TO 42
   40 CARRY(1)=DCARRY(1)
      CARRY(2)=DCARRY(2)
   42 NV=PO(10)
      NRO=PO(21)
      IF (NRO.GT.0) GO TO 45
         CARRY(1)=BLANK
         CARRY(2)=BLANK
  45  IF (METRIC.EQ.1) LMETR=0
      IF (METRIC.EQ.0) LMETR=1
      IF (METRIC.EQ.-1) LMETR=PO(23)
      METENG=METR
      IF (LMETR.EQ.1) METENG=IENG
      BASE=PO(24)
      IF (LMETR.EQ.1) BASE=BASE/(.3048)**3
      IF (BASE.LE.0.01) BASE=0.0
      AREA=PO(7)
      IF (LMETR.EQ.1) AREA=AREA/((.3048*5280./1000.)**2.)
      WRITE (IPU,901) (PO(I),I=2,6),AREA,NV,CARRY,METENG,BASE
  901 FORMAT (5A4,5X,F10.1,2X,I3,2X,2A4,1X,A4,F10.3)
C
C  PUNCH CARD 2
      NCARD=NCARD+1
      IDTR=PO(16)
      IDTQ=PO(20)
      WRITE (IPU,902) PO(13),PO(14),PO(15),IDTR,PO(17),PO(18),PO(19),
     *   IDTQ
  902 FORMAT (2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2)
C
C  PUNCH CARD 3
      NCARD=NCARD+1
      IORD=PO(22)-1
      XPO=((0.3048)**3)/25.4
      DO 60 I=1,NV,7
         J=I+6
         IF (J.GT.NV) J=NV
         II=0
         IF (LMETR.EQ.1) GO TO 54
            DO 52 L=I,J
               II=II+1
               P(II)=PO(IORD+L)
               VALMAX=999999.0
               IF (P(II).GT.VALMAX) THEN
                  WRITE (IPR,65) II,NCARD,P(II),VALMAX
65    FORMAT ('0**ERROR** VALUE ',I3,' FOR CARD ',I1,' (',G10.2,
     *   ') EXCEEDS ',F10.1,' AND WILL BE SET TO ZERO.')
                  CALL ERROR
                  P(II)=0.0
                  ENDIF
   52          CONTINUE
            WRITE (IPU,904) (P(K),K=1,II)
  904 FORMAT(7F10.4)
            GO TO 60
   54    DO 56 L=I,J
            II=II+1
            P(II)=PO(IORD+L)/XPO
            VALMAX=99999999.0
            IF (P(II).GT.VALMAX) THEN
               WRITE (IPR,65) II,NCARD,P(II),VALMAX
               CALL ERROR
               P(II)=0.0
               ENDIF
   56       CONTINUE
         WRITE (IPU,903) (P(K),K=1,II)
  903 FORMAT (7F10.1)
   60    CONTINUE
C
      IF (IPDFLT.EQ.1) GO TO 100
      IF (NRO.EQ.0) GO TO 100
C
C  PUNCH CARD 4
      NCARD=NCARD+1
      DO 80 I=1,NRO,7
         J=I+6
         IF (J.GT.NRO) J=NRO
         II=0
         IF (LMETR.EQ.1) GO TO 74
            DO 72 L=I,J
               II=II+1
               P(II)=CO(L)
               VALMAX=999999.0
               IF (P(II).GT.VALMAX) THEN
                  WRITE (IPR,65) II,NCARD,P(II),VALMAX
                  CALL ERROR
                  P(II)=0.0
                  ENDIF
   72          CONTINUE
            GO TO 78
   74    DO 76 L=I,J
            II=II+1
            P(II)=CO(L)/25.4
            VALMAX=999999.0
            IF (P(II).GT.VALMAX) THEN
               WRITE (IPR,65) II,NCARD,P(II),VALMAX
               CALL ERROR
               P(II)=0.0
               ENDIF
   76       CONTINUE
   78    WRITE (IPU,904) (P(K),K=1,II)
   80    CONTINUE
C
100   CALL FSTWHR (OLDNAM,IOLDOP,OLDNAM,IOLDOP)
C
      RETURN
C
      END
