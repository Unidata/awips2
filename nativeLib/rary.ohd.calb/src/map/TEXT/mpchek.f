C   MEMBER MPCHEK
C-----------------------------------------------------------------------
      SUBROUTINE MPCHEK (NUM,RSTA,BMO,BYR,EMO,EYR,NSTART,DUNITS,OPT8,
     *   OPT9,NUMM5,PXMASS,MT,C,STAID,MO,MC,hourly_flag,monthly_flag)
C
C
C  THIS ROUTINE PERFORMS the following FUNCTIONS -
C
C        1)  STORES THE MONTHLY PRECIPITATION SUM FOR EACH STATION
C            INCLUDED IN THE ANALYSIS.  THIS DATA MAY THEN BE USED IF
C            CONSISTENCY PLOTS ARE MADE.
C
C        2)  CHECKS ALL HOURLY PRECIPITATION DATA AND DAILY SUMS FOR
C            EXTRAORDINALLY LARGE VALUES.
C
c        3)  generates the monthly_flag array for use by IDMA
c
C
      INCLUDE 'common/ionum'
      COMMON /DIM/ M1,M2,M3,M4,M5,M6
C
      DIMENSION    PXMASS(M1,M3)
      DIMENSION    C(M1,817),STAID(7,M1)
      DIMENSION    PXMAX(24),MO(M3)
C
      integer*2    hourly_flag(200,817),monthly_flag(200,600)
      INTEGER      RSTA,BMO,BYR,EMO,EYR,OPT8,OPT9
      INTEGER      DEBUG,DEBUGA
      REAL         INCHES,MMETER
C
      COMMON  /MAP/  DEBUG,DEBUGA
      COMMON  /MAP2/  LDA(2,12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mpchek.f,v $
     . $',                                                             '
     .$Id: mpchek.f,v 1.3 1999/01/19 15:29:55 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA         INCHES/4HIN  /,MMETER/4HMM  /
C
      IF (DEBUG.EQ.1.OR.DEBUGA.EQ.1) WRITE (IPR,140)
C
c   nstart = 0 -- first time calling MPCHEK
c          = 1 -- all calls after first
c
      IF (NSTART.eq.0) then
         NSTART=1
C
C        SET VARIABLES FOR CHECKING PRECIPITATION VALUES.
c
         IF (DUNITS.EQ.INCHES)  PXBASH=1.0
         IF (DUNITS.EQ.MMETER)  PXBASH=25.4
         IF (DUNITS.EQ.INCHES)  PXBASD=2.5
         IF (DUNITS.EQ.MMETER)  PXBASD=63.5
C
C        CALCULATE NUMBER OF MONTHS IN PERIOD OF RECORD
c
         K=EMO-BMO+1
         MT=(EYR-BYR)*12+K
C
         IF (DEBUG.GT.0) WRITE (IPR,10) EMO,BMO,K,EYR,BYR,MT,M3
         MOS=BMO
         LYR=BYR
C
C        CALCULATE DAYS IN MONTH
c
         DO 30 I=1,MT
         IF (MOS.EQ.2) then
            LY=0
            IF ((LYR-4*(LYR/4)).EQ.0)LY=1
            MO(I)=LDA((LY+1),2)
            MOS=3
         else
            MO(I)=LDA(1,MOS)
            MOS=MOS+1
            IF (MOS.NE.13) GO TO 30
            MOS=1
            LYR=LYR+1
         end if
30       CONTINUE
C
         IF (DEBUG.GT.0) WRITE (IPR,40) NUM,MT
C
C        INITIALIZE STATION PRECIPITATION SUM ARRAY
c
         DO 50  I=1,NUM
         DO 50  J=1,MT
         PXMASS(I,J)=0.0
         monthly_flag(i,j)=0
   50    continue
C
         MOS=BMO
         LYR=BYR
         MC=0
      end if
C
c   M = number of hours in the month
c
      MC=MC+1
      M=MO(MC)*24
C
      IF (DEBUG.GT.0) WRITE (IPR,70) MC,MO(MC),M
C
C  PERFORM PRECIPITATION DATA CHECK FOR EACH STATION
c
      DO 120 I=1,NUM
      IF (OPT9.EQ.2.AND.I.EQ.NUMM5)  GO TO 120
      IHEAD=0
      LL=0
      L=24
      SUMPX=0.0
      mflag=0
C
C  PERFORM PRECIPITATION DATA CHECK FOR EACH DAY IN MONTH
c
      DO 100 J=1,M,24
      LL=LL+1
      PXDT=0.0
      MPX=0
C
C  SUM HOURLY PRECIPITATION DATA TO FIND DAILY TOTAL AND CHECK FOR
C  EXTRAORDINALLY LARGE HOURLY VALUES.
c
      DO 80 N=1,24
      L=L+1
      PX=C(I,L)
      SUMPX=SUMPX+PX
      PXMAX(N)=PX
      PXDT=PXDT+PX
      if(hourly_flag(i,l).eq.1) mflag=1
      IF (PX.GE.PXBASH ) MPX=1
80    CONTINUE
C
C  CHECK FOR EXTRAORDINALLY LARGE DAILY PRECIPITATION SUM.
      IF (PXDT.GE.PXBASD ) MPX=1
      IF (MPX.EQ.0) GO TO 100
      IF (OPT8.NE.0)  GO TO 100
C
C  PRINT PRECIPITATION VALUES FOR DAYS WITH EXTRAORDINALLY LARGE VALUES.
      WRITE(IPR,170) I,(STAID(JJ,I),JJ=1,7),MOS,LYR
      IF (IHEAD.EQ.0) then
         WRITE(IPR,180) DUNITS
         IHEAD=1
      end if
c
      IF (DUNITS.EQ.INCHES)  WRITE(IPR,150) LL,(PXMAX(KK),KK=1,24),PXDT
      IF (DUNITS.EQ.MMETER)  WRITE(IPR,160) LL,(PXMAX(KK),KK=1,24),PXDT
C
100   CONTINUE
C
      IF (DEBUG.GT.0) WRITE (IPR,110) I,MC
      PXMASS(I,MC)=SUMPX
      if(mflag.eq.1) monthly_flag(i,mc)=1
120   CONTINUE
C
      MOS=MOS+1
      IF (MOS.eq.13) then
         MOS=1
         LYR=LYR+1
      end if
c
c   check for number of months exceeding max allowed
c
      IF (MC.LE.MT) return
      IUSTOP=0
      CALL USTOP (IPR,IUSTOP)
C
      return
c
10    FORMAT (' EMO=',I4,3X,'BMO=',I4,3X,'K=',I4,3X,
     *   'EYR=',I4,3X,'BYR=',I4,3X,'MT=',I5,3X,'M3=',I5)
40    FORMAT (' NUM=',I3,3X,'MT=',I4)
70    FORMAT (' MC=',I3,3X,'MO(MC)=',I3,3X,'M=',I4)
110   FORMAT (' I=',I5,3X,'MC=',I5)
140   FORMAT (' *** ENTER MPCHEK')
150   FORMAT (1X,I2,1X,24F5.2,F6.2)
160   FORMAT (1X,I2,1X,24F5.1,F6.2)
170   FORMAT(/10X, 9HSTATION  ,I3,5X,6A4,A1,10X,7HMONTH  ,I3,5X,6HYEAR
     *,I6)
180   FORMAT (1X,3HDAY,53X,14HHOURLY--TOTALS,2H (,A4,2H) ,45X,5HDAILY  )
      END
