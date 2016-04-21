      SUBROUTINE WRTMAP55(PO,NB,MPRV,XXLAT,XXLON,Y,HS,BS,BSS,X,HLV,
     .  NJFM,NIFM,NJTO,NITO,NQL,LQ1,LQN,MPLOC,JCK,IMAP,MRV,NJUN,NSTR,
     .  NST,STONAM,GZO,DTMAP,DTH,IOPNMAP,
     .  K1,K2,K9,K10,K14,K22,K27,K30)

c  this subroutine writes out the data needed for FLDVIEW
c  data is written to four files:
c     (1) scenario info, cross section info - rvr mile, invert, lat, long
c     (2) wsel profile - rvr mile, peak wsel, topwidth, wsel on town side of levee
c     (3) same as (2) for animation; one file per animation step
c     (4) cross section info - rvr mile, t.s. id, b vs h curve

c... MPFRST=0 => initialize everything
c...        1 => animation mode; store at selected time step
c...        2 => animation mode; done with time step, store peak

C
C  ROUTINE WAS WRITTEN ORIGINALLY BY: JANICE SYLVESTRE - HL - 6/2001
C
C  MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C     determine which data should be written for each scenario - make sure
C     the files are actually written 
C     
CC      CHARACTER FILIN*12,ATIM*6
      CHARACTER*6 ATIM
      CHARACTER*4 STONAM
      CHARACTER*8 TSID
      CHARACTER*20 FILNAM
      CHARACTER*20 FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,FILEFORM_U
      CHARACTER*100 ENVVAR1,ENVVAR2
      CHARACTER*150 FILNM
      CHARACTER*150 FILANIM,DIRNAME,PATHNAME,UNIXCMD

      COMMON/IONUM/IN,IPR,IPU
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/LEV55/NLEV,DHLV,NPOND,DTHLV,IDTHLV
cc      COMMON/FLDMAP55/NMAP,FILANIM,FILNAM,MPTIM
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/ofs55'
      INCLUDE 'common/opfil55'
      INCLUDE 'common/fldmap55'

      DIMENSION PO(1),NB(K1),XXLAT(K2,K1),XXLON(K2,K1),Y(K2,K1)
      DIMENSION X(K2,K1),HS(K9,K2,K1),HLV(K22),NJFM(K22),NIFM(K22)
      DIMENSION NJTO(K22),NITO(K22),NQL(K1),LQ1(K10,K1),LQN(K10,K1)
      DIMENSION MPRV(K30),MPLOC(2,K30),JCK(K1),IMAP(K2,K1,K30),MRV(K1)
      DIMENSION NJUN(K1),NSTR(K1),NST(K14,K1),BS(K9,K2,K1),GZO(K14,K1)
      DIMENSION BSS(K9,K2,K1),STONAM(3,K27),DTMAP(K30)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/wrtmap55.f,v $
     . $',                                                             '
     .$Id: wrtmap55.f,v 1.2 2004/09/24 20:52:53 jgofus Exp $
     . $' /
C    ===================================================================
C

      IF(DTMAP(1).LT.1.0.AND.MPFRST.EQ.1) GO TO 400
  
      TOL=0.0000001

c.... initialize imap parameter
      DO 250 M=1,NMAP
        DO 210 J=1,JN
          N=NB(J)
          DO 205 I=1,N
            IMAP(I,J,M)=0
  205     CONTINUE
  210   CONTINUE
  250 CONTINUE

c.... determine which reaches are used in each scenario
      DO 15 M=1,NMAP
        DO 1 J=1,JN
          jck(j)=0
    1   CONTINUE

        DO 2 J=1,JN
          IF(MPRV(M).EQ.J) GO TO 4
    2   CONTINUE

c.... find the main river reach (jrv is the 1st river in scenario)
    4   I1=MPLOC(1,M)
        I2=MPLOC(2,M)
        JRV=1
        DO 6 I=I1,I2
          IMAP(I,J,M)=JRV
    6   CONTINUE
        LRV=1
        JCK(1)=J

c.... find the first order trib in the reach (jct is the junction)
        IF(JN.EQ.1) GO TO 18
        JRV=JRV+1
        DO 8 J2=1,JN
          JCT=MRV(J2)
          IF(JCT.EQ.J.AND.NJUN(J2).GE.I1.AND.NJUN(J2).LE.I2) GO TO 10
    8   CONTINUE
        GO TO 15
   10   N=NB(J2)
cc         DO 12 I=1,N
cc          IMAP(I,J2,M)=JRV
cc   12   CONTINUE
cc        JCK(2)=J2
cc        JRV=JRV+1
        CALL MPSET55(JCT,JRV,JN,MRV,JCK,IMAP,NB,M,NJUN,I1,I2,K1,K2,K30)

c.... find all other associated tribs
        IF(JN.LE.2) GO TO 18
        DO 14 J3=2,JN
          JCT=JCK(J3)
          IF(JCT.GT.0) THEN
            I1=1
            I2=NB(JCT)
            CALL MPSET55(JCT,JRV,JN,MRV,JCK,IMAP,NB,M,
     .                             NJUN,I1,I2,K1,K2,K30)
         ENDIF
   14   CONTINUE
   15 CONTINUE

   18 IF(MPFRST.EQ.2) WRITE(JFSCEN,480) NMAP

CC      DO 25 M=1,NMAP
        DO 23 J=1,JN
          N=NB(J)
          DO 20 I=1,N
            ISKIP=1
            DO 19 M=1,NMAP
              IF(IMAP(I,J,M).GT.0) ISKIP=0
   19       CONTINUE
cc            WRITE(JFSCEN,490) X(I,J),(IMAP(I,J,M), M=1,NMAP)
cc            WRITE(JFXY,500) X(I,J),XXLAT(I,J),XXLON(I,J),HS(1,I,J)
            IF(ISKIP.EQ.0.AND.MPFRST.EQ.2) THEN
              WRITE(JFSCEN,490) X(I,J),(IMAP(I,J,M),M=1,NMAP)
              WRITE(JFXY,510) X(I,J),XXLAT(I,J),XXLON(I,J),HS(1,I,J)
            ENDIF
   20     CONTINUE
   23   CONTINUE
CC   25 CONTINUE

c.... write the forecast file
   24 IF(MPFRST.EQ.2) WRITE(JFPRF,470)
      IF(MPFRST.LT.2.AND.IOPNMAP.NE.1.AND.DTMAP(1).GE.1.0) THEN

c.... determine the files to be stored for animation
        TIM=TT-DTH
cc        IF(TIM.LT.0.) TIM=0.
        DTANIM=(TIM)/DTMAP(1)
        LANIM=DTANIM
        MTIM=MPTIM*LANIM
        DIFF=TIM-FLOAT(MTIM)
        IF(ABS(DIFF).GT.TOL) GO TO 200
        CALL TOCHAR55(MTIM,ATIM,ICOL)
        LFILANIM=LENSTR(FILANIM)
        PATHNAME=FILANIM(1:LFILANIM)//'AN'//ATIM//'.fcs'
        FILNM=FILNAM(1:LENSTR(FILNAM))//'AN'//ATIM
cc        K=INDEX(FILNM,'.')
cc        IF(K.NE.0) LN=K-1

        FILETYPE='FLDWAV-FLDVIEW '
        FILEACCS='SEQUENTIAL'
        FILESTAT='UNKNOWN'
        FILEFORM_F='FORMATTED'
        FILEFORM_U='UNFORMATTED'
        LRECL=0
C
cc        CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
cc     *     LRECL,JFANIM,IERR)
cc        IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)


cc        IF(MTIM.GT.0) THEN
          CLOSE(JFANIM)
          OPEN(JFANIM,FILE=PATHNAME)
cc        ENDIF
cc        WRITE(JFANIM,'(A)') PATHNAME(1:LENSTR(PATHNAME))
        WRITE(JFANIM,'(A)') FILNM(1:LENSTR(FILNM))
      ENDIF

      LS=1
      IF(MPFRST.EQ.2) WRITE(JFXSEC,480) NCS
cc      WRITE(JFPRF,470)
      DO 100 J=1,JN
CC        IF(KMAP(J).EQ.1) THEN
          N=NB(J)
          DO 50 I=1,N

c.... check to see if cross section I is to be mapped
            ISKIP=1
            DO 25 M=1,NMAP
              IF(IMAP(I,J,M).GT.0) ISKIP=0
   25       CONTINUE
            IF(ISKIP.EQ.1) THEN
              DO 32 L=1,NSTR(J)
                IF(NST(L,J).EQ.I) THEN
                  LS=LS+1
                  GO TO 50
                ENDIF
   32         CONTINUE
              GO TO 50
            ENDIF

            CALL SECT55(PO(LCPR),PO(LOAS),BS,HS,PO(LOASS),
     .          BSS,J,I,Y(I,J),PO(LCHCAV),PO(LCIFCV),K1,K2,K9)

c.... find max elev on levee side
            HLEV=-999.
            DO 28 L=1,NLEV
              IF(NJFM(L).EQ.J.AND.NIFM(L).EQ.I) THEN
                HLEV=HLV(L)
                DO 26 L2=1,NLEV
                  IF(NJTO(L2).EQ.NJTO(L).AND.NITO(L2).EQ.NITO(L).AND.
     .               HLV(L2).GT.HLEV) HLEV=HLV(L2)
   26           CONTINUE
                IF(HLEV.LT.HS(1,I,J)) HLEV=-999.
                GO TO 40
              ENDIF
   28       CONTINUE

c.... triple the width at the lateral flow reach
            NQ=NQL(J)
            IF(NQ.GT.0) THEN
              DO 35 L=1,NQ
                L1=LQ1(L,J)
                LN=LQN(L,J)
                DO 30 LL=L1,LN
                  IF(I.EQ.LL) THEN
                    BT=BT*3
                    GO TO 40
                  ENDIF
   30           CONTINUE
   35         CONTINUE
            ENDIF

c.... write the forecast file
   40       IF(MPFRST.EQ.2) WRITE(JFPRF,500) X(I,J),Y(I,J),BT,HLEV
            IF(MPFRST.LT.2.AND.IOPNMAP.NE.1.AND.DTMAP(1).GT.0.) 
     .          WRITE(JFANIM,500) X(I,J),Y(I,J),BT,HLEV

            IF(MPFRST.EQ.2) THEN
              TSID='NONE'
              DO 45 L=1,NSTR(J)
                IF(NST(L,J).EQ.I) THEN
                  TSID=STONAM(1,LS)//STONAM(2,LS)
                  LS=LS+1
                  GO TO 47
                ENDIF
   45         CONTINUE
   47         WRITE(JFXSEC,520) X(I,J),GZO(I,J),TSID,(BS(K,I,J)+
     .             BSS(K,I,J),K=1,NCS),(HS(K,I,J), K=1,NCS)
            ENDIF
   50     CONTINUE
CC        ENDIF
  100 CONTINUE


C  CLOSE FILES
  200 IF(MPFRST.EQ.2) THEN
        IUNIT=0
        CALL CLFILE ('FLDWAV-FLDVIEW',IUNIT,IERR)
cc        CLOSE(JFXY)
cc        CLOSE(JFPRF)
      ELSE
        CLOSE(JFANIM)
        MPFRST=1
      ENDIF

CC      CLOSE(50)
CC      CLOSE(51)

  470 FORMAT('Peak')
  480 FORMAT(I5)
  490 FORMAT(F14.4,',',20(I5,','),$)
  500 FORMAT(F15.4,',',F15.4,',',F15.4,',',F15.4)
  510 FORMAT(20(F15.4,','),F15.4,',',F15.4,',',F15.4,',',F15.4)
  520 FORMAT(F10.4,F10.2,2X,A8,100F10.2)
  400 RETURN
      END






