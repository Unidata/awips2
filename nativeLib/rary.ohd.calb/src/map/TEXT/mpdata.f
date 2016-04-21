C   MEMBER MPDATA
C ----------------------------------------------------------------------
C
      SUBROUTINE MPDATA (BMO,BYR,EMO,EYR,NUM,OPT8,KSTART,K1,N,OUTMO,
     *   OUTYR,IEND,NCDX,MOX,MYR,OBTMO,OBTMOX,C,IND,LT,LS,ITUNIT,
     *   IFIRST)
C
      INTEGER      BMO,BYR,EMO,EYR,OUTMO,OUTYR,OBTMO,OBTMOX,OPT8
      INTEGER      DEBUG,DEBUGA
      DIMENSION    OBTMO(M1),OBTMOX(M1),IND(M1),C(M1,817)
C
      INCLUDE 'common/ionum'
      COMMON /MAP/ DEBUG,DEBUGA
      COMMON /MAP3/ B(745)
      COMMON /DIM/ M1,M2,M3,M4,M5,M6
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mpdata.f,v $
     . $',                                                             '
     .$Id: mpdata.f,v 1.2 1999/01/19 15:30:22 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (DEBUG.EQ.1.OR.DEBUGA.EQ.1) WRITE (IPR,170)
C
      IF (IFIRST.EQ.0) GO TO 20
C
      IEND=0
      NOMO=13-BMO
      NOYR=(EYR-BYR-1)*12
      NOMO=NOMO+NOYR+EMO
      LT=NOMO*NUM
      LS=0
      MOX=BMO
      MYR=BYR
      KSTART=25
      DO 10 J=1,NUM
         IND(J)=0
         DO 10 K=1,24
            C(J,K)=0.0
10       CONTINUE
      GO TO 30
C
20    IF (LS.EQ.LT) GO TO 150
      KSTART=73
      MOX=MOX+1
      IF (MOX.LT.13) GO TO 30
      MOX=1
      MYR=MYR+1
30    OUTMO=MOX
      OUTYR=MYR
C
      GO TO (80,40,80,70,80,70,80,80,70,80,70,80),MOX
40    JX=MYR+1900
      XX=JX
      JX=JX/4
      XX=XX*0.25
      YY=JX
      IF (XX-YY)50,60,50
50    N=28
      NCDX=672
      GO TO 90
60    N=29
      NCDX=696
      GO TO 90
70    N=30
      NCDX=720
      GO TO 90
80    N=31
      NCDX=744
C
C  READ INTO ARRAY ONE MONTH OF DATA FOR EACH STATION
c
   90 continue
      K1=N*24
c
      DO 140 J=1,NUM
      L=LS+J
      CALL UREADT (ITUNIT,L,B,IERR)
      KF=KSTART-24
      if(ierr.ne.0) write(ipr,12) ierr
C
      DO 100 K=KF,K1
      C(J,K+24)=B(K)
100   CONTINUE
c
      L2=LS+J+NUM
      C(J,817)=B(745)
      OBTMO(J)=B(745)
      IF (L2.LE.LT) GO TO 120
C
      DO 110 K=1,48
      KX=K1+K+24
      C(J,KX)=0.0
110   CONTINUE
      GO TO 140
C
  120 CONTINUE
      CALL UREADT (ITUNIT,L2,B,IERR)
      if(ierr.ne.0) write(ipr,12) ierr
      DO 130 K=1,48
      KX=K1+K+24
      C(J,KX)=B(K)
130   CONTINUE
      OBTMOX(J)=B(745)
C
140   CONTINUE
C
      LS=LS+NUM
      K1=K1+72
C
      GO TO 160
C
150   IEND=1
C
160    RETURN
C
   12 format(/,2x,'*** error return code from ureadt=',i3)
170   FORMAT (' *** ENTER MPDATA')
      END
