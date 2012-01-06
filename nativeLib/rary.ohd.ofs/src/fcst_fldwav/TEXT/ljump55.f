      SUBROUTINE LJUMP55(PO,JNK,NCS,IS,IND,INN,KJP,J,YD,YU,QU,HS,
     * DDX,IFR,YCR,KRCH,NQCM,K1,K2,K7,K8,K9)
C
C  THE SUBROUTINE LOCATES LOCATION OF HYDRAULIC JUMP
      COMMON/METR55/METRIC
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/fdbug'
      INCLUDE 'common/ofs55'

      DIMENSION PO(*)
      DIMENSION YD(K2,K1),YU(K2,K1),QU(K2,K1),HS(K9,K2,K1),DDX(K2,K1)
      DIMENSION IFR(K2,K1),YCR(K2,K1),KRCH(K2,K1),NQCM(K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/ljump55.f,v $
     . $',                                                             '
     .$Id: ljump55.f,v 1.2 2000/12/19 15:53:43 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'LJUMP55 ' /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      NCML=ABS(NQCM(J))
      IF(NCML.EQ.0) NCML=NCS
C       CHECK FOR JUMP MOVING UPSTREAM
      KJPS=KJP
      N1=IND-IS
      HTOL=0.2
      DO 70 II=1,N1
      I=IND-II+1
      YSV=YU(I,J)
      QI=QU(I,J)
      YMN=YCR(I,J)
      YMX=HS(NCS,I,J)
      CALL HSEQ55(PO,JNK,J,I,QI,YSV,YS,YMN,YMX,ITS,K1,K2,K9)
      YEXT=YU(I+1,J)
      IF(JNK.GE.11) WRITE(IPR,68) KJPS,I,YEXT,YS,ITS,IFR(I,J)
   68 FORMAT(5X,5HKJPS=,I5,5X,2HI=,I5,5X,5HYEXT=,F10.2,5X,3HYS=,F10.2,
     *  3X,4HITS=,I5,3X,4HIFR=,I2)
      IF(YEXT.LT.YS+HTOL .OR. IFR(I,J).EQ.2) GO TO 75
      YU(I,J)=YEXT
      YD(I,J)=YU(I,J)
      IFR(I,J)=0
      KJP=I-1
   70 CONTINUE
   75 IF(KJP.EQ.KJPS) GO TO 80
      IF(JNK.GE.9) WRITE(IPR,76) KJPS,KJP
   76 FORMAT(/10X,30HJUMP MOVED UPSTREAM FROM SECT=,I5,5X,8HTO SECT=,
     *  I5/)
      GO TO 100
C      CHECK FOR JUMP MOVING DOWNSTREAM
   80 IS=KJP
      INN=IS+3
      IF(INN.LT.IS) INN=IS
      DQDT=0.0
      DO 90 I=IS,INN
      IF(I.LE.2) GO TO 90
      KRA=IABS(KRCH(I,J))
      IF(KRA.GE.10 .AND. KRA.LE.30) GO TO 90
      IL=I-1
      QIL=QU(IL,J)
      QI=QU(I,J)
      YIL=YU(IL,J)
      YSV=YU(I,J)
      DX=DDX(IL,J)
      YMN=HS(1,I,J)
      YMX=YCR(I,J)
      Y1=0.5*(YMN+YMX)
      CALL DWATR55(PO,JNK,NCML,NQCM,J,IL,I,QIL,QI,YIL,YMN,DX,Y1,YMX,
     1 DQDT,ITD,K1,K2,K7,K8,K9)
      YII=YMN
      ITS=0
      IF(ABS(YMN-YCR(I,J)).LT.0.001) YS=YMN
      IF(ABS(YMN-YCR(I,J)).LT.0.001) GO TO 85
      YMN=YCR(I,J)
      YMX=HS(NCS,I,J)
      CALL HSEQ55(PO,JNK,J,I,QI,YII,YS,YMN,YMX,ITS,K1,K2,K9)
   85 YIIP=YII
      YSP=YS
      YSVP=YSV
      IF(METRIC.EQ.0) GO TO 86
      YIIP=YII/3.281
      YSP=YS/3.281
      YSVP=YSV/3.281
   86 IF(JNK.GE.11) WRITE(IPR,94) I,YIIP,YSP,YSVP,ITS,IFR(I,J)
   94 FORMAT(10X,2HI=,I5,5X,4HYII=,F10.2,5X,3HYS=,F10.2,5X,4HYSV=,F10.2,
     *  3X,4HITS=,I5,3X,4HIFR=,I2)
      IF(YSV.GE.YS-HTOL) GO TO 95
      IFR(I,J)=1
      YU(I,J)=YII
      YD(I,J)=YU(I,J)
      KJP=I
   90 CONTINUE
   95 IF(KJP.EQ.KJPS) GO TO 100
      IF(JNK.GE.9) WRITE(IPR,96) KJPS,KJP
   96 FORMAT(/10X,32HJUMP MOVED DOWNSTREAM FROM SECT=,I5,5X,8HTO SECT=,
     *  I5/)
  100 RETURN
      END
