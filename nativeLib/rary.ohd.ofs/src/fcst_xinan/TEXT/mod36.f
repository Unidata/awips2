C MEMBER MOD36
C  (from old member FCEX36)
C
      SUBROUTINE MOD36(KDA,KHR,NOUTZ,NOUTDS,N,NUNIT,CO,BADJ,IBUG,IPRINT,
     1IOUT)
C.......................................
C     SUBROUTINE MAKES MOD FOR XIN-SMA OPERATION.
C.......................................
C     WRITTEN BY QINGPING ZHU  -YRCC CHINA   SEP.   1988
C.......................................
      DIMENSION CO(1),BADJ(1)
      REAL K,IMP,KG,KSS,KSSD,KGD
C
C     COMMON BLOCKS
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FPM36/K,IMP,WM,WUM,WLM,WDM,WMM,SM,SMM,B,EX,C,KSS,KG,
     1KSSD,KGD,CI,CG,CID,CGD
      COMMON/MOD136/NDT36,IDT36(10),IUT36(10),VAL36(8,10)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_xinan/RCS/mod36.f,v $
     . $',                                                             '
     .$Id: mod36.f,v 1.1 1995/09/17 18:58:29 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
      IF(IUT36(N).EQ.0) GO TO 100
C     CHANGE NEEDED ONLY FOR SPECIFIED SUB-BASIN.
      J=7*(IUT36(N)-1)
      NCHNG=1
      GO TO 105
C     STATE VARIABLES NEED CHANGED FOR ALL SUB-BASINS.
  100 J=0
      NCHNG=NUNIT
C.......................................
C     BEGINNING OF SUB-BASIN LOOP.
  105 DO 120 I=1,NCHNG
C     STORE STARTING STATE VARIABLES FOR CURRENT SUB-BASIN.
      WU=CO(J+1)
      WL=CO(J+2)
      WD=CO(J+3)
      S=CO(J+4)
      QI=CO(J+5)
      QG=CO(J+6)
      FR=CO(J+7)
      NCRT=IUT36(N)
      IF(IUT36(N).EQ.0) NCRT=I
C     NCRT IS THE NUMBER OF CURRENT SUB-BASIN.
C.......................................
C     MAKE MOD CHANGE
      DO 110 M=1,6
      IF(VAL36(M,N).GE.0.0) CO(J+M)=VAL36(M,N)
  110 CONTINUE
      IF(VAL36(7,N).LT.0.0) GO TO 130
      CO(J+1)=CO(J+1)*VAL36(7,N)
      CO(J+2)=CO(J+2)*VAL36(7,N)
      CO(J+3)=CO(J+3)*VAL36(7,N)
  130 IF(VAL36(8,N).GE.0.0) CO(J+6)=CO(J+6)*VAL36(8,N)
C.......................................
C     CHECK IF DEBUG OUTPUT OR DETAIL OUTPUT ARE NEEDED.
      IF((IBUG.EQ.0).AND.(IPRINT.EQ.0)) GO TO 115
      CALL MDYH1(KDA,KHR,MO,ID,IY,IH,NOUTZ,NOUTDS,TZ)
      WRITE (IOUT,911) MO,ID,IY,IH,TZ
  911 FORMAT (1H0,20X,I2,1H/,I2,1H/,I4,2H--,I2,1X,A4)
      WRITE(IOUT,900) NCRT
  900 FORMAT(1H ,1X,9HSUB-BASIN,1X,I2,1X,30HSTATE VARIABLES WERE CHANGED
     1: )
      WRITE(IOUT,901) WU,WL,WD,S,QI,QG,FR
  901 FORMAT(1H ,5X,3HOLD,2X,4F7.1,2F7.2,F7.3)
      WRITE(IOUT,902) (CO(J+M),M=1,7)
  902 FORMAT(1H ,5X,3HNEW,2X,4F7.1,2F7.2,F7.3)
C.......................................
C     STORE VALUES AFTER MOD.
  115 WUC=CO(J+1)
      WLC=CO(J+2)
      WDC=CO(J+3)
      SC=CO(J+4)
      QIC=CO(J+5)
      QGC=CO(J+6)
      FRC=CO(J+7)
C.......................................
C     CHECK STATE VARIABLES FOR ILLEGITIMATE VALUES.
      L=0
      IF (WUC.LE.WUM) GO TO 20
      CO(J+1)=WUM
      L=1
   20 IF (WLC.LE.WLM) GO TO 21
      CO(J+2)=WLM
      L=1
   21 IF (WDC.LE.WDM) GO TO 22
      CO(J+3)=WDM
      L=1
   22 IF(SC.LE.SM) GO TO 23
      CO(J+4)=SM
      L=1
   23 IF (L.EQ.0) GO TO 150
      CALL MDYH1(KDA,KHR,MO,ID,IY,IH,NOUTZ,NOUTDS,TZ)
      WRITE (IPR,912) MO,ID,IY,IH,TZ
  912 FORMAT (1H0,10X,11H**WARNING**,10X,I2,1H/,I2,1H/,I4,2H--,I2,1X,A4)
      WRITE(IPR,922) NCRT
  922 FORMAT (1H0,10X,35H  INITIAL STATE VARIABLES FOR UNIT ,
     1 I2,27H CONTAIN IMPOSSIBLE VALUES.)
      WRITE(IPR,923)
  923 FORMAT(1H0,41X,5H WUC ,2X,5H WLC ,2X,5H WDC ,2X,5H SC  ,2X,
     15H QIC ,2X,5H QGC ,1X,5H FRC )
      WRITE(IPR,924) WUC,WLC,WDC,SC,QIC,QGC,FRC
  924 FORMAT (1H0,16X,19HINITIAL VALUES WERE,3X,4F7.1,2F7.2,F7.3)
      WRITE(IPR,925) (CO(J+M),M=1,7)
  925 FORMAT (1H0,25X,10HCHANGED TO,3X,4F7.1,2F7.2,F7.3)
      CALL WARN
C.......................................
C     ADJUST WATER BALANCE DUE TO CHANGES OF THE STATE VARIABLES.
  150 BADJ(NCRT)=BADJ(NCRT)+CO(J+1)+CO(J+2)+CO(J+3)-(WU+WL+WD)
     1+(CO(J+4)-S)*FR
      IF(IUT36(N).NE.0) GO TO 160
      J=J+7
  120 CONTINUE
  160 RETURN
      END
