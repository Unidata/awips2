      SUBROUTINE PXCON(IOPT,NSTA,NMO,PX,STNAME,IMO,IYR,UNITS,IMOW,IMOS,
     1ND,MD,IGS,IGROUP,BASE,PRINTD,STID,IORDCN,NG,NPG,jflag)
C     SUBROUTINE COMPUTES AND PRINTS CONSISTENCY PLOTS

      INCLUDE 'common/ionum'
c
c specify the maximum number of stations allowed
c
      PARAMETER (NSX=500)    ! change NSX from 200 to 500 xfan 03/15/02
      PARAMETER (MaxMonth=1200) !Change 600 to MaxMonth.kwz.08/9/04
      DIMENSION NPG(3),IGS(5,ND),NPLUS(5),IGROUP(ND),PX(ND,MD),
     1STNAME(ND,5),BASE(5,MD),BSTA(5),ACCUM(5),IGN(5),S(11),
     2SYM(5),ORD(101,101),BSTAKP(nsx,MaxMonth),MOP(101),JYRP(101)
      DIMENSION IMINST(50),IORDCN(3,nsx)
      integer*2 jflag(nsx,MaxMonth),iflag(5)
      CHARACTER*4 PRNTOP(nsx),PRINTD,STID(nsx,2)
      CHARACTER*8 STPS,STIDIN(nsx),STIN(5)
      CHARACTER*1 SIGN(nsx),SI(5)
      character*132 newnam
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pxpp/RCS/pxcon.f,v $
     . $',                                                             '
     .$Id: pxcon.f,v 1.5 2004/08/17 14:24:02 wkwock Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK,DOT,ASTER/1H ,1H.,1H*/
      DATA SYM/1H1,1H2,1H3,1H4,1H5/
C
C
C  SET UNIT NUMBER FOR IDMA OUTPUT
      LIDMA=99
C
C  SET FILE NAME FOR IDMA OUTPUT
      CALL USUFIT (IPR,'_dma',NEWNAM,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (LP,385) 'USUFIT',ISTAT
385   FORMAT ('0*** WARNING - IN MEPCON - STATUS FROM ROUTINE ',A,
     *   ' IS ',I2,'.')
         CALL UWARN (LP,0,-1)
         LIDMA=0
         ENDIF
C
      IF (LIDMA.GT.0) THEN
C     OPEN IDMA OUTPUT FILE
         CALL UPOPEN (LIDMA,NEWNAM,0,'F',ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (LP,385) 'UPOPEN',ISTAT
            CALL UWARN (LP,0,-1)
            LIDMA=0
            ENDIF
         ENDIF

      READ (IN,900) NG,(NPG(IG),IG=1,NG)
  900 FORMAT(15I5)
      IF(IOPT.EQ.0)RETURN
      LMOW=IMOS-1
      LMOS=IMOW-1
      DO 10 IG=1,NG
      NUM=NPG(IG)
      NPLOTS=(NUM-1)/5+1
      DO 899 J=1,NPLOTS
         PRNTOP(J)='YES '
         READ (IN,898) (STIN(II),SI(II),II=1,5),PRNTOP(J)
  898    FORMAT(5(A8,A1,1X),A4)
      DO 897 IJ=1,5
         JN=(J-1)*5+IJ
         STIDIN(JN)=STIN(IJ)
         SIGN(JN)=SI(IJ)
  897 CONTINUE
  899 CONTINUE
      K=1
      DO 9 J=1,NUM
         DO 8 IX=1,NSTA
            STPS=STID(IX,1)//STID(IX,2)
            IF(STIDIN(J).EQ.STPS) THEN
               IGS(IG,J)=IX
               IORDCN(IG,K)=IX
               K=K+1
               IF(SIGN(J).EQ.'-') IGS(IG,J)=-IX
               GO TO 6
            END IF
    8    CONTINUE
    6    N=IGS(IG,J)
         N=IABS(N)
         IGROUP(N)=IG
    9 CONTINUE
   10 CONTINUE
C     REMOVE STATIONS WITH MISSING DATA.
      DO 20 N=1,NSTA
      DO 21 M=1,NMO
      IF(PX(N,M).LT.0.0)GO TO 25
   21 CONTINUE
      GO TO 20
   25 IG=IGROUP(N)
      NUM=NPG(IG)
      DO 26 I=1,NUM
      NN=IGS(IG,I)
      NN=IABS(NN)
      IF(NN.NE.N)GO TO 26
      IGS(IG,I)=0
      IGROUP(N)=0
   26 CONTINUE
   20 CONTINUE
C     CHECK GROUPS NOW THAT STATIONS HAVE BEEN REMOVED.
      IG=1
   30 NNZ=0
      NP=0
      NUM=NPG(IG)
      DO 31 I=1,NUM
      N=IGS(IG,I)
      IF(N.EQ.0)GO TO 31
      NNZ=NNZ+1
      IF(N.GT.0)NP=NP+1
   31 CONTINUE
      IF(NNZ.GT.1)GO TO 32
C     ONLY ONE OR ZERO STATIONS LEFT IN GROUP.
      IF(IG.EQ.NG)GO TO 33
      NN=IG+1
      DO 34 II=NN,NG
      NUM=NPG(II)
      IM=II-1
      NPG(IM)=NUM
      DO 34 I=1,NUM
      IGS(IM,I)=IGS(II,I)
   34 CONTINUE
   33 NG=NG-1
      IF(IG.GT.NG)GO TO 39
      GO TO 30
   32 IF(NP.GT.1)GO TO 35
C     ONLY ONE POSITIVE STATION-MAKE ALL POSITIVE.
      DO 36 I=1,NUM
      N=IGS(IG,I)
      IF(N.LT.0)IGS(IG,I)=-N
   36 CONTINUE
      NP=NNZ
   35 NPLUS(IG)=NP
      IF(NNZ.EQ.NUM)GO TO 38
C     SOME STATIONS WITH MISSING DATA IN GROUP.
      II=1
      DO 37 I=1,NUM
      IF(IGS(IG,I).EQ.0)GO TO 37
      IGS(IG,II)=IGS(IG,I)
      II=II+1
   37 CONTINUE
      NPG(IG)=NNZ
   38 IG=IG+1
      IF(IG.GT.NG)GO TO 39
      GO TO 30
   39 IF(NG.EQ.0)RETURN
C     GENERATE CONSISTENCY PLOTS.
C     PRINT TITLE PAGE.
      write(IPR,901)
  901 FORMAT(1H1,/////,34X,32HPRECIPITATION CONSISTENCY CHECK.)
      write(IPR,902)
  902 FORMAT(1H0,56HSTATIONS WITH POSITIVE NUMBERS CONSTITUTE THE GROUP
     1BASE,1X,60HAND ARE PLOTTED AGAINST THE OTHER STATIONS IN THE GROUP
     2 BASE)
      write(IPR,903)
  903 FORMAT(1H0,69HSTATIONS WITH NEGATIVE RUN NUMBERS ARE PLOTTED AGAIN
     1ST THE GROUP BASE)
      write(IPR,904)
  904 FORMAT(1H0)
      DO 40 IG=1,NG
      write(IPR,905) IG
  905 FORMAT(1H0,10X,17HSTATIONS IN GROUP,I2)
      write(IPR,906)
  906 FORMAT(1H0,12HSTA. RUN NO.,8X,12HSTATION NAME)
      NUM=NPG(IG)
      DO 40 I=1,NUM
      N=IGS(IG,I)
      NN=IABS(N)
      write(IPR,907) N,(STNAME(NN,J),J=1,5)
  907 FORMAT(1H ,5X,I3,12X,5A4)
   40 CONTINUE
C     COMPUTE BASE FOR EACH GROUP.
      DO 50 IG=1,NG
      NUM=NPG(IG)
      FNP=NPLUS(IG)
      FNP=1.0/FNP
      BS=0.0
      DO 51 M=1,NMO
      BMO=0.0
      DO 52 I=1,NUM
      N=IGS(IG,I)
      IF(N.LT.0)GO TO 52
      BMO=BMO+PX(N,M)*FNP
   52 CONTINUE
      BS=BS+BMO
      BASE(IG,M)=BS
   51 CONTINUE
   50 CONTINUE
C     SET-UP PLOTS.
      DO 60 IG=1,NG
      NUM=NPG(IG)
      FNP=NPLUS(IG)
      NPLOTS=(NUM-1)/5+1
      DO 61 IP=1,NPLOTS
      II=(IP-1)*5+1
      IL=IP*5
      IF(IL.GT.NUM)IL=NUM
      NN=IL-II+1
      DO 59 I=II,IL
      J=I-II+1
      N=IGS(IG,I)
      IGN(J)=IABS(N)
   59 CONTINUE
      IPASS=1
   81 IF(PRNTOP(IP).EQ.'YES ') write(IPR,908)
  908 FORMAT(1H1,68HMONTHLY LISTING OF ACCUMULATED PRECIPITATION AND BAS
     1E PRECIPITATION.)
      IF(PRNTOP(IP).EQ.'YES ') then
      write(IPR,909) ((STNAME(IGN(J),I),I=1,5),J=1,NN)
      IF (LIDMA.GT.0) write(LIDMA,909) ((stname(ign(j),i),i=1,5),j=1,nn)
      endif
  909 FORMAT(1H0,10X,5(3X,5A4))
      IF(PRNTOP(IP).EQ.'YES ') write(IPR,910)
  910 FORMAT(1H ,7HMO/YEAR,3X,5(6X,7HACC. PX,3X,7HBASE PX))
C     COMPUTE PLOT SCALES AND PRINT ACCUMULATIONS
      DMAX=0.0
      DMIN=0.0
      BMAX=0.0
      BP=0.0
      ICOUNT=0
      DO 62 M=1,NMO
      BMO=BASE(IG,M)
      JYR=(M-1)/12
      MO=M-JYR*12
      MO=IMO+MO-1
      JYR=IYR+JYR
      IF(MO.GT.12) THEN
         MO=MO-12
         JYR=JYR+1
      END IF
      IPRT=1
      IF(IOPT.EQ.2) THEN
         IF((IPASS.EQ.2).AND.((MO.LT.IMOS).OR.(MO.GT.LMOS))) IPRT=0
         IF((IPASS.EQ.1).AND.(MO.GT.LMOW).AND.(MO.LT.IMOW)) IPRT=0
      END IF
      DO 63 I=II,IL
      J=I-II+1
      IF(M.GT.1)GO TO 64
      BSTA(J)=0.0
c
c initialize flag for identifcation of missing, estimated, or observed precipitation
c
      iflag(j)=0

      ACCUM(J)=0.0
      RMONSM=0.0
   64 N=IGS(IG,I)
      BS=BMO-BP
      NA=IABS(N)
      PXS=PX(NA,M)
      IF(N.GT.0)BS=(BS*FNP-PXS)/(FNP-1.0)
      IF(IPRT.EQ.0)GO TO 63
c
c set flag for identification of missing, estimated, or observed precipitation
c
      iflag(j)=jflag(na,m)

      ACCUM(J)=ACCUM(J)+PXS
      BSTA(J)=BSTA(J)+BS
      BSTAKP(I,M)=BSTA(J)
      DEV=ACCUM(J)-BSTA(J)
      IF(DEV.GT.DMAX)DMAX=DEV
      IF(DEV.LT.DMIN)DMIN=DEV
      IF(BSTA(J).GT.BMAX)BMAX=BSTA(J)
   63 CONTINUE
      BP=BMO
      IF((PRNTOP(IP).EQ.'YES ').AND.(IPRT.NE.0)) then
      write(IPR,911) MO,JYR,(ACCUM(J),BSTA(J),J=1,NN)
  911 FORMAT(1H ,I2,1H/,I4,3X,5(3X,2F10.1))
c
c write for GUI-based double mass analysis
c
      IF (LIDMA.GT.0) write(LIDMA,919) mo,jyr,(accum(j),iflag(j),
     *   bsta(j),j=1,nn)
  919 format(1H ,i2,1H/,i4,3x,5(3x,f10.1,i3,f10.1))
      endif

      IF(M.EQ.NMO) THEN
      NLAST=IL-II+1
      DO 65 IB=1,NLAST
         RMONSM=RMONSM+BSTA(IB)
   65 CONTINUE
      IF(IOPT.EQ.2) THEN
         IF((IPASS.EQ.2).AND.(IPRT.EQ.0)) IPRT=1
         IF((IPASS.EQ.1).AND.(IPRT.EQ.0)) IPRT=1
      END IF
      DEVMIN=9999.
         DO 66 I=II,IL
            IF(IPRT.EQ.0)GO TO 66
            J=I-II+1
            DEVLST=ABS((RMONSM/(IL-II+1))-BSTA(J))
            LMO=MO
            LYR=JYR
            IF(DEVLST.LT.DEVMIN) THEN
               DEVMIN=DEVLST
               IMINST(IP)=I
               JKP=J
            END IF
            IF(SIGN(I).EQ.'-')JKP=1
   66    CONTINUE
      END IF
   62 CONTINUE
            SAVBAS=BSTA(JKP)
C     PRINT PLOT TITLE.
      IF(PRNTOP(IP).EQ.'YES ') write(IPR,912) IG
  912 FORMAT(1H1,69HDEVIATION OF ACCUMULATED PRECIPITATION FROM THE GROU
     1P BASE-----GROUP=,I1)
      IF(PRNTOP(IP).EQ.'YES ') write(IPR,913)
  913 FORMAT(1H0,5X,12HSTA. RUN NO.,8X,13HSTA. PLOT NO.,7X,12HSTATION NA
     1ME)
      DO 70 I=II,IL
      J=I-II+1
      N=IGS(IG,I)
      NA=IABS(N)
      IF(PRNTOP(IP).EQ.'YES ') write(IPR,914) N,J,(STNAME(NA,K),K=1,5)
  914 FORMAT(1H ,9X,I3,19X,I2,12X,5A4)
   70 CONTINUE
C     COMPUTE AND PRINT DEVIATION SCALE.
      T=DMAX-DMIN
      T25=0.25*BMAX
      IF (T.LT.T25) T=T25
      KA=T*0.001+2.0
      K=T*0.1
      K=K+KA
      T=K*10.0
      DPL=T*0.01
      K=DMAX/DPL+1.0
      DMAX=K*DPL
      DMIN=DMAX-T
C     K IS LOCATION OF ZERO DEVIATION.
      K=101-K
      IF(PRNTOP(IP).EQ.'YES ') THEN
         IF(PRINTD.EQ.'YES ') write(IPR,915) UNITS
  915    FORMAT(1H0,10X,33HASTERISKS INDICATE ZERO DEVIATION,15X,9HUNITS
     1 ARE,1X,A4,49X,9HDATES FOR)
         IF(PRINTD.NE.'YES ') write(IPR,1015) UNITS
 1015    FORMAT(1H0,10X,33HASTERISKS INDICATE ZERO DEVIATION,15X,9HUNITS
     1 ARE,1X,A4)
      END IF
      KA=0
      DO 71 IY=1,101,10
      KA=KA+1
      S(KA)=DMIN+DPL*(IY-1)
   71 CONTINUE
      IF(PRNTOP(IP).EQ.'YES ') THEN
         IF(PRINTD.EQ.'YES ') write(IPR,916) S,JKP
  916    FORMAT(1H0,7HBASE PX,11F10.1,4X,7HSTATION,I3)
         IF(PRINTD.NE.'YES ') write(IPR,1016) S
 1016    FORMAT(1H0,7HBASE PX,11F10.1)
      END IF
C     COMPUTE BASE PX SCALE.
      KA=BMAX*0.1+1.0
      T=KA*10.0
      FINC=T*0.01
C     INITIALIZE GRID ARRAY.
      DO 72 IX=1,101
      DO 73 IY=2,100
   73 ORD(IX,IY)=BLANK
      DO 74 IY=1,101,10
   74 ORD(IX,IY)=DOT
      ORD(IX,K)=ASTER
   72 CONTINUE
C     FILL IN GRID ARRAY.
      BP=0.0
      MOP(1)=IMO
      JYRP(1)=IYR
      FINC1=0
      FINC1=FINC1+FINC
      FINC2=FINC1+FINC
      NX=2
      DO 75 M=1,NMO
      BMO=BASE(IG,M)
      JYR=(M-1)/12
      MO=M-JYR*12
      MO=IMO+MO-1
      JYR=IYR+JYR
      IF(MO.GT.12) THEN
         MO=MO-12
         JYR=JYR+1
      END IF
      IPRT=1
      IF(IOPT.EQ.2) THEN
         IF((IPASS.EQ.2).AND.((MO.LT.IMOS).OR.(MO.GT.LMOS))) IPRT=0
         IF((IPASS.EQ.1).AND.(MO.GT.LMOW).AND.(MO.LT.IMOW)) IPRT=0
      END IF
      DO 78 I=II,IL
      J=I-II+1
      IF(M.GT.1)GO TO 79
      BSTA(J)=0.0
      ACCUM(J)=0.0
   79 N=IGS(IG,I)
      BS=BMO-BP
      NA=IABS(N)
      PXS=PX(NA,M)
      IF(N.GT.0)BS=(BS*FNP-PXS)/(FNP-1.0)
      IF(IPRT.EQ.0)GO TO 78
      ACCUM(J)=ACCUM(J)+PXS
      BSTA(J)=BSTA(J)+BS
      DEV=ACCUM(J)-BSTA(J)
      IY=(DPL*0.5+DEV)/DPL
      IY=IY+K
      IF(DEV.LT.-0.5*DPL)IY=IY-1
      IX=BSTA(J)/FINC+1.5
      ORD(IX,IY)=SYM(J)
   78 CONTINUE
      IF(IPRT.EQ.0)GO TO 82
      IF(PRINTD.EQ.'YES ') THEN
      IMP=IMINST(IP)
      IF(BSTAKP(IMP,M).GE.FINC1) THEN
   87    IF(BSTAKP(IMP,M).LT.FINC2) THEN
            MOP(NX)=MO
            JYRP(NX)=JYR
            NX=NX+1
            FINC1=FINC1+FINC
            FINC2=FINC1+FINC
         ELSE
            MOP(NX)=MOP(NX-1)
            JYRP(NX)=JYRP(NX-1)
            NX=NX+1
            FINC1=FINC1+FINC
            FINC2=FINC1+FINC
            GO TO 87
         END IF
      END IF
      END IF
   82 BP=BMO
   75 CONTINUE
C     PRINT GRID ARRAY.
      DO 80 IX=1,101
      FIX=IX-1
      BS=FIX*FINC
      IF(MOP(IX).EQ.0) THEN
         MOP(IX)=LMO
         JYRP(IX)=LYR
      END IF
      IF(PRNTOP(IP).EQ.'YES ') THEN
         IF(PRINTD.EQ.'YES ') THEN
            write(IPR,917) BS,(ORD(IX,IY),IY=1,101),MOP(IX),JYRP(IX)
         ELSE
            write(IPR,918) BS,(ORD(IX,IY),IY=1,101)
         END IF
      END IF
  917 FORMAT(1H ,F10.1,5X,101A1,3X,I4,1H/,I4)
  918 FORMAT(1H ,F10.1,5X,101A1)
      MOP(IX)=0.
      JYRP(IX)=0.
   80 CONTINUE
      IF(IOPT.EQ.1)GO TO 61
      IF(IPASS.EQ.2)GO TO 61
      IPASS=2
      GO TO 81
   61 CONTINUE
   60 CONTINUE
C
      IF (LIDMA.GT.0) THEN
C     CLOSE IDMA OUTPUT FILE
         CALL UPCLOS (LIDMA,' ',ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (LP,385) 'UPCLOS',ISTAT
            CALL UWARN (LP,0,-1)
            ENDIF
         ENDIF
C
      RETURN
C
      END
