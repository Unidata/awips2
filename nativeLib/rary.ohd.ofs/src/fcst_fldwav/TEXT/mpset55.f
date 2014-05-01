      SUBROUTINE MPSET55(JCT,JRV,JN,MRV,JCK,IMAP,NB,M,NJUN,I1,I2,
     .  K1,K2,K30)

C jgg modified argument list above for MR 1954

c  this subroutine orders all 1st order tribs to be mapped with the
c  current river

c ... jct is the main stem river to be mapped
c ... jck contains the order of the rivers to be mapped


C jgg modified the following line for MR 1954
C      DIMENSION MRV(K1),NB(K1),JCK(K1),IMAP(K2,K1,K30)

      DIMENSION MRV(K1),NB(K1),JCK(K1),IMAP(K2,K1,K30),NJUN(K1)

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/mpset55.f,v $
     . $',                                                             '
     .$Id: mpset55.f,v 1.2 2004/09/24 20:52:41 jgofus Exp $
     . $' /
C    ===================================================================
C

      DO 100 J=1,JN

C jgg modified the following line for MR 1954
C        IF(MRV(J).EQ.JCT) THEN
       IF(MRV(J).EQ.JCT.AND.NJUN(J).GE.I1.AND.NJUN(J).LE.I2) THEN
          N=NB(J)
          JCK(JRV)=J
          DO 50 I=1,N
            IMAP(I,J,M)=JRV
   50     CONTINUE
          JRV=JRV+1
        ENDIF
  100 CONTINUE
      RETURN
      END
