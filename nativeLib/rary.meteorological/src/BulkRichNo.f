      SUBROUTINE RICHNO(HT,HW,UW,VW,RHO,NLVLS,NW,BUOY,RICHNUM)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C Compute the dimensionless bulk Richardson number as defined by
C Weisman and Klemp (1982).
C
C History.
C --------
C Tom Schlatter  Late 1982    Original code based on MWR article by
C                             Weisman and Klemp (1982).
C D. Baker       01 Jun 84    Removed computation of positive energy...
C                             made it an input argument.
C D. Baker       01 Jul 85    Updated code for documentation.
C J. Ramer	 16 Jun 92	Added divide-by-zero prevention.
C D. Perry       10 Oct 96    Adapted code for WFO
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C HT          Real Array    Sounding heights (m asl).
C HW          Real Array    Heights of wind reports (m asl).
C UW          Real Array    Wind u-components (m/s).
C VW          Real Array    Wind v-components (m/s).
C RHO         Real Array    Air density at each sounding level (kg/m**3).
C NLVLS       Integer       Number of sounding levels passed.
C NW          Integer       Number of wind levels passed.
C BUOY        Real          Positive buoyant energy (J/kg).
C
C On output:
C ----------
C RICHNUM     Real          Dimensionless bulk Richardson number.
C
C
C Local array dimension.
C
      integer MNL
      PARAMETER (MNL=500)
C
C Input arguments.
C
      INTEGER NLVLS,NW,qc
      REAL HT(NLVLS),HW(NW),UW(NW),VW(NW),RHO(NLVLS)
      REAL BUOY
C
C Output arguments.
C
      REAL RICHNUM
C
C Local variables/arrays.
C
      REAL RULAY,RVLAY,RLAY,DZ,UL,VL,U6,V6,SUMU,SUMV
      REAL SUMR,SUMUL,SUMVL,SUMRL,HBL,HTOP,SKE
      REAL RHOW(MNL)
      INTEGER I
C
C Constants.
C
      integer FLAG, HALFKM, SIXKM
      PARAMETER (FLAG=99999.0)
      PARAMETER (HALFKM=500.0)
      PARAMETER (SIXKM=6000.0)
      RICHNUM=FLAG
C
C Interpolate an air density value to each reported wind level.
C
      IF (NLVLS.NE.NW) THEN
          CALL WNDRHO(RHO,HT,NLVLS,HW,NW,RHOW)
      ELSE
         DO 3 I=1,NLVLS
            RHOW(I)=RHO(I)
3        CONTINUE
      END IF
C
C do some QC.
C
      qc=1
         DO 4 I=2,NW
         If (UW(I).ne.UW(1) .and. VW(I).ne.VW(1)) qc=0
4        CONTINUE
      If (qc.eq.1)Write (*,*) 'Monotonic winds in RICH'
      If (NLVLS.lt.3 .or. NLVLS.gt.500) Then
          Write (*,*) NLVLS,' sounding levels in RICH.'
          qc=1
      End If
      If (NW.lt.3 .or. NW.gt.500) Then
          Write (*,*) NW,' wind levels in RICH.'
          qc=1
      End If
      DO 5 I=1,NW
      If (RHOW(I).le.0.0) Then
          Write (*,*) 'Bad value for RHOW ',I,RHOW(i)
          qc=1
          Goto 6
      End If
5     CONTINUE
6     CONTINUE
      DO 7 I=2,NW
      If (HW(I)-HW(I-1).le.0.0) Then
          Write (*,*) 'Bad wind heights ',I,HW(I-1),HW(I)
          qc=1
          Goto 8
      End If
7     CONTINUE
8     CONTINUE
      DO 15 I=1,NLVLS
      If (RHO(I).le.0.0) Then
      Write (*,*) 'Bad value for RHO ',I,RHOW(i)
          qc=1
          Goto 16
      End If
15    CONTINUE
16    CONTINUE
      DO 17 I=2,NLVLS
      If (HT(I)-HT(I-1).le.0.0) Then
          Write (*,*) 'Bad sounding heights ',I,HT(I-1),HT(I)
          qc=1
          Goto 18
      End If
17    CONTINUE
18    CONTINUE

      If (qc.eq.1) Return

C
C Initialize sums.
C
      SUMU=0.
      SUMV=0.
      SUMR=0.
      SUMUL=0.
      SUMVL=0.
      SUMRL=0.
C
C Define shear layer bounds (above ground level).
C
      HBL=HW(1)+HALFKM
      HTOP=HW(1)+SIXKM
      IF (HW(NW).LT.HTOP .OR. HW(2).GT.HTOP) GO TO 999
C
C Loop to calculate shear terms.
C
c   initialize RULAY, RVLAY, RLAY, DZ
      i=1
      RULAY=0.5*(RHOW(I)*UW(I))
      RVLAY=0.5*(RHOW(I)*VW(I))
      RLAY=0.5*(RHOW(I))
      DZ=HW(I)

      DO 100 I=2,NW
         RULAY=0.5*(RHOW(I)*UW(I)+RHOW(I-1)*UW(I-1))
         RVLAY=0.5*(RHOW(I)*VW(I)+RHOW(I-1)*VW(I-1))
         RLAY=0.5*(RHOW(I)+RHOW(I-1))
         DZ=HW(I)-HW(I-1)
         IF (HW(I).GT.HTOP) GO TO 101
         SUMU=SUMU+RULAY*DZ
         SUMV=SUMV+RVLAY*DZ
         SUMR=SUMR+RLAY*DZ
         IF (HW(I).GT.HBL .AND. I.GT.2) GO TO 100
         SUMUL=SUMUL+RULAY*DZ
         SUMVL=SUMVL+RVLAY*DZ
         SUMRL=SUMRL+RLAY*DZ
100   CONTINUE

101   SUMU=SUMU+RULAY*DZ
      SUMV=SUMV+RVLAY*DZ
      SUMR=SUMR+RLAY*DZ
      IF (SUMR.LE.0.0) THEN
          U6=0.0
          V6=0.0
      ELSE
          U6=SUMU/SUMR
          V6=SUMV/SUMR
      END IF
      IF (SUMRL.LE.0.0) THEN
          UL=0.0
          VL=0.0
      ELSE
          UL=SUMUL/SUMRL
          VL=SUMVL/SUMRL
      END IF
c      Write (*,*) 'IN RICH BUOY,UL,VL,U6,V6'
c      Write (*,*) BUOY,UL,VL,U6,V6
C
C Calculate one-half the square of the shear vector in the
C lowest 6 km.
C
      U6=U6-UL
      V6=V6-VL
      SKE=0.5*(U6*U6+V6*V6)
C
C Compute the bulk richardson number.
C
      if (SKE.gt.0) RICHNUM=BUOY/SKE
C
C Exit.
C
999   RETURN
      END


































