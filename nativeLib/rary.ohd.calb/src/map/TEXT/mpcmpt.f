C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (CHECK_EXIST)
CC AV -- 6/29 this is not a c routine !$PRAGMA C (DIRNAME)
C MODULE MPCMPT
C-----------------------------------------------------------------------
C
      SUBROUTINE MPCMPT (NBASIN,K1,N,NUM,OPT3,OPT8,OUTMO,OUTYR,
     * DTYPE,IT,DUNITS,FUNITS,DIMF,IPUNCH,MOX,MYR,ANAME,AREAID,SWX,
     * C,PXSUM,BMO,BYR,EMO,EYR,IMOW,IMOS,OPT1,IUNIT,FULLN,ICNT,BD,FN)
C
C  ROUTINE  MPCMPT  COMPUTES MEAN AREAL PRECIPITATION VALUES
C  FOR EACH BASIN SUBAREA.
C
      CHARACTER OFORMAT*12,DIRNAME*80,CT*2
      CHARACTER*12 BD(50),FN(50)
      CHARACTER FCSTGNM*20,FC1*4,FC2*4,FC3*4,FILE1*100
      CHARACTER*112 FULLN(50)
      CHARACTER*12 AREAID(M2)
      CHARACTER*20 ANAME(M2)
      CHARACTER*150 PATHNAME,UNIXCMD
      DIMENSION SWX(M2,M1,2),C(M1,817)
      DIMENSION PXSUM(M2,M6,12),IUNIT(*)
      DIMENSION NDUMST(200),FLAT(3000),FLON(3000),FNAME(3)
C
      INTEGER EMO,EYR
      INTEGER OUTMO,OUTYR,BMO,BYR,OPT1,OPT3,OPT8
      INTEGER DEBUG,DEBUGA
C
      INCLUDE 'uiox'
      COMMON /DIM/ M1,M2,M3,M4,M5,M6
      COMMON /MAP/ DEBUG,DEBUGA
      COMMON /MAP2/ LASTDA(2,12)
      COMMON /MAP3/ B(745)
      COMMON /MNC1/ AREA,DUN,FLAT,FLON,FNAME,FUN,ILIST,NDUMST
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mpcmpt.f,v $
     . $',                                                             '
     .$Id: mpcmpt.f,v 1.7 2002/02/11 18:34:30 dws Exp $
     . $' /
C    ===================================================================
C
      EQUIVALENCE (FC1,FN1), (FC2,FN2), (FC3,FN3)
C
C
      IF (DEBUG.EQ.1) WRITE (LP,*) 'ENTER MPCMPT'

      IUSTOP=0
C
      OFORMAT='(6F9.3)'
      CT=' '
C
      NYR=MYR-BYR+1
      LMOW=IMOS-1
      ICNT=ICNT+1
C
      FN1=FNAME(1)
      FN2=FNAME(2)
      FN3=FNAME(3)
      FCSTGNM=FC1//FC2//FC3
c
      DO 110 MM=1,NBASIN
         DO 10 I=1,744
            B(I)=0.0
10          CONTINUE
C     CONSTRUCT FULL PATHNAME FOR OUTPUT FILE
         IF (ICNT.EQ.1) THEN
            CALL GET_APPS_DEFAULTS ('calb_area_ts_dir',16,DIRNAME,LENDR)
            IF (IT.EQ.6) THEN
               CT='06'
               ELSE IF (IT.EQ.3) THEN
                  CT='03'
               ELSE IF (IT.EQ.1) THEN
                  CT='01'
               ENDIF
            IF (FCSTGNM.EQ.' ') THEN
               FILE1=DIRNAME(1:LENDR)//'/'
               ELSE
                  LEN=INDEX(FCSTGNM,' ')-1
                  FILE1=DIRNAME(1:LENDR)//'/'//FCSTGNM(1:LEN)//'/'
               ENDIF
            L1=INDEX(FILE1,' ')-1
            L=INDEX(BD(MM),' ')-1
            IF (L.EQ.-1) L=12
            M=INDEX(FN(MM),' ')-1
            IF (M.EQ.-1) M=12
            IF (L.EQ.0) THEN
               M=INDEX(AREAID(MM),' ')-1
               IF (M.EQ.-1) M=12
               PATHNAME=FILE1(1:L1)
               UNIXCMD='mkdir -p '//PATHNAME
               CALL SYSTEM (UNIXCMD)
               FULLN(MM)=PATHNAME(1:LENSTR(PATHNAME))//
     *                   AREAID(MM)(1:M)//
     *                   '.MAP'//
     *                   CT
               ELSE
                  PATHNAME=FILE1(1:L1)//BD(MM)(1:L)
                  UNIXCMD='mkdir -p '//PATHNAME
                  CALL SYSTEM (UNIXCMD)
                  FULLN(MM)=PATHNAME(1:LENSTR(PATHNAME))//
     *                      '/'//
     *                      FN(MM)(1:M)//
     *                      '.MAP'//
     *                      CT
               ENDIF
C        CHECK IF DIRECTORY EXISTS
            IPRINT=1
            PATHNAME(LENSTR(PATHNAME)+1:LENSTR(PATHNAME)+1)=CHAR(0)
            CALL CHECK_EXIST (PATHNAME,'directory',IEXIST,IPRINT)
            IF (IEXIST.NE.1) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,185) PATHNAME(1:LENSTR(PATHNAME))
               CALL USTOP (LP,IUSTOP)
               ENDIF
            CALL CARDHD (FULLN(MM),DTYPE,FUNITS,DIMF,IT,BMO,BYR,
     *                  EMO,EYR,AREAID(MM),ANAME(MM),IUNIT(MM),OFORMAT,
     *                  IAM1,IAM2,IERR)
            IF (IERR.NE.0) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,190) 'CARDHD',IUNIT(MM)
               CALL USTOP (LP,IUSTOP)
               ENDIF
            endif
         IF (OPT8.NE.0) GO TO 20
            WRITE (LP,140) AREAID(MM),OUTMO,OUTYR
            WRITE (LP,150) ANAME(MM)
20       JX=1
         IP=1
         IF (OPT1.LT.3.OR.OPT1.EQ.4) GO TO 30
            IF (MOX.GT.LMOW.AND.MOX.LT.IMOW) IP=2
30       DO 60 J=24,K1,N
            XMBP=0.0
            DO 50 K=1,NUM
               IF (SWX(MM,K,IP).LT.0.001) GO TO 50
               XC=0.0
               DO 40 L=1,N
                  M=J+L
                  XC=XC+C(K,M)
40                CONTINUE
               XMBP=XMBP+XC*SWX(MM,K,IP)
50             CONTINUE
            B(JX)=XMBP
            JX=JX+1
60          CONTINUE
C     PRINT MAP
         NPER=24/N
         LEAPYR=0
         IF ((OUTYR-4*(OUTYR/4)).EQ.0) LEAPYR=1
         LASTD=LASTDA((LEAPYR+1),OUTMO)
         NPERMO=NPER*LASTD
         NLINE=NPER*(LASTD-1)/24+1
         SUMPX=0.0
         DO 80 I=1,NLINE
            MHR1=(I-1)*24+1
            MHR2=I*24
            IF (MHR2.GT.NPERMO) MHR2=NPERMO
            DO 70 MHR=MHR1,MHR2
               SUMPX=SUMPX+B(MHR)
70             CONTINUE
            IDA=((MHR1-1)/NPER)+1
            IF (OPT8.NE.0) GO TO 80
               WRITE (LP,160) IDA,(B(MHR),MHR=MHR1,MHR2)
80          CONTINUE
         IF (OPT8.NE.0) GO TO 90
            WRITE (LP,170) SUMPX,DUNITS
90       PXSUM(MM,NYR,MOX)=SUMPX
         IF (DEBUG.EQ.1) WRITE (LP,180) MM,NYR,MOX,PXSUM(MM,NYR,MOX)
         IF (OPT8.NE.0) GO TO 100
            IF ((N.EQ.1).AND.(MM.NE.NBASIN)) WRITE (LP,130)
C     WRITE MAP TO FILE - CONVRT IF NECESSARY
  100    NVAL=0
         IF (OPT3.EQ.1) NVAL=744
         IF (OPT3.EQ.2) NVAL=248
         IF (OPT3.EQ.3) NVAL=124
         IF (DUNITS.NE.FUNITS) THEN
            ICONV=1
            CALL UDUCNV (DUNITS,FUNITS,ICONV,NVAL,B,B,IERR)
            ENDIF
CCC         CALL WTFILE (DTYPE,IT,MOX,MYR,1,1,NVAL,B,IPUNCH)
         CALL CARDWT (DTYPE,IT,MOX,MYR,1,B,IUNIT(MM),OFORMAT,IERR)
         IF (IERR.GT.0) THEN
            CALL UEROR (LP,0,-1)
            WRITE (LP,190) 'CARDWT',IUNIT(MM)
            CALL USTOP (LP,IUSTOP)
            ENDIF
110      CONTINUE
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT ('1')
140   FORMAT ('0MAP FOR  ',A12,5X,I2.2,'/',I2.2)
150   FORMAT (5X,A20)
160   FORMAT (' ',I3,2X,24F5.2)
170   FORMAT (' ',100X,' MONTHLY TOTAL=',F7.2,2X,A4)
180   FORMAT (' ',3I10,F10.2)
185   FORMAT ('0*** ERROR - IN SXHRTM - DIRECTORY ',A,' NOT FOUND.')
190   FORMAT ('0*** ERROR - IN SXHRTM - CALLING ROUTINE ',A,
     *   ' FOR UNIT ',I2,'.')
C
      END
