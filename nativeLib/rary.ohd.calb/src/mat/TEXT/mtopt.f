C MODULE MTOPT
C-----------------------------------------------------------------------
C
      SUBROUTINE MTOPT (NAREAS,ICONS,IRGCK,IELEV,IGRID,ITYPE,POWER,
     *   IPRINT,IPUNCH,NOOUT,IFSUMT,IFSUMP,FILEN,ICTMP,ICTIM,
     *   STMNWT,UNITI,UNITO,ISFN)
C
C  ROUTINE TO PRINT THE OPTIONS FOR PROGRAM MAT.
C
      CHARACTER*4 UNITI,UNITO
C
      DIMENSION OPTEL(3),OMOUNT(3)
      DIMENSION OPTWGT(2)
      DIMENSION FILEN(3),FILE(3)
      DIMENSION PRED(2),DP(2)
      DIMENSION ENG(2),OPTUI(2),OPTUO(2)
C
      INCLUDE 'uiox'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mat/RCS/mtopt.f,v $
     . $',                                                             '
     .$Id: mtopt.f,v 1.3 2002/02/11 18:56:48 dws Exp $
     . $' /
C    ===================================================================
C
      DATA OPTMAT/4H OFF/,OPTCON/4H OFF/,OPTTEM/4H OFF/
      DATA OPTTMP/4H OFF/,OPTTIM/4H OFF/,OPTSFN/4H OFF/
C
      DATA OPTPRT/4H OFF/,OPTPNC/4H OFF/,OPTOUT/4H OFF/
      DATA OPTSMT/4H OFF/,OPTSMP/4H OFF/
      DATA FILE/4H    ,4H -- ,4H    /
      DATA OPMATP/4H ON /
C
      DATA PRED/4HPRED,4HETER/
      DATA DP/4HD**P,4HOWER/
      DATA ENG/4HENGL,4HISH /
      DATA OPTUI/4HMETR,4HIC  /,OPTUO/4HMETR,4HIC  /
C
      DATA OPTEL/4HNON-,4HMOUN,4HTAIN/,OMOUNT/4HMOUN,4HTAIN,4HOUS /
      DATA OPTWGT/4HGRID,4H PT /
      DATA ON/4H ON /,OFF/4H OFF/
C
C
      IF (ISFN.EQ.1)   OPTSFN=ON
      IF (NAREAS.GT.0) OPTMAT=ON
      IF (ICONS.EQ.1)  OPTCON=ON
      IF (IRGCK.GT.0)  OPTTEM=ON
C
      IF (ICTMP.EQ.1)  OPTTMP=ON
      IF (ICTIM.EQ.1)  OPTTIM=ON
      IF (IPRINT.EQ.1) OPTPRT=ON
      IF (IPUNCH.EQ.1) OPTPNC=ON
      IF (IFSUMT.EQ.1) OPTSMT=ON
      IF (IFSUMP.EQ.1) OPTSMP=ON
C-----------------------------------------------------------------------
C MOUNTAINOUS/NON-MOUNTAINOUS TERRAIN OPTION
      IF (IELEV.EQ.1) THEN
        DO 10 I=1,3
          OPTEL(I)=OMOUNT(I)
10      CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C PRINTING/FILE OUTPUT OPTIONS
      IF (NOOUT.EQ.1) GO TO 30
      IF (NOOUT.GE.2) OPMATP=OFF
      IF (NOOUT.EQ.3) GO TO 30
        OPTOUT=ON
        DO 20 I=1,3
          FILE(I)=FILEN(I)
20      CONTINUE
30    CONTINUE
C-----------------------------------------------------------------------
C WEIGHTING OPTIONS
      IF (IGRID.EQ.0) THEN
        DO 40 I=1,2
          OPTWGT(I)=PRED(I)
40      CONTINUE
        GO TO 60
      ENDIF
C
      IF (ITYPE.EQ.3) THEN
        DO 50 I=1,2
          OPTWGT(I)=DP(I)
50      CONTINUE
      ENDIF
60    CONTINUE
C-----------------------------------------------------------------------
C INPUT/OUTPUT UNITS
      IF (UNITI.NE.'METR') THEN
        DO 70 I=1,2
          OPTUI(I)=ENG(I)
70      CONTINUE
      ENDIF
C
      IF (UNITO.NE.'METR') THEN
        DO 80 I=1,2
          OPTUO(I)=ENG(I)
80      CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C PRINT OPTION TABLE
      WRITE (LP,110)
      WRITE (LP,120)
      WRITE (LP,130)
      WRITE (LP,140)
      WRITE (LP,150) OPTMAT,OPTCON,OPTTEM,OPTUI,OPTSFN
C
      WRITE (LP,160)
      WRITE (LP,170)
      WRITE (LP,180)
      WRITE (LP,190)
      WRITE (LP,200) (OPTEL(I),I=1,3),OPTTMP,OPTTIM,OPTSMT,OPTSMP
C
      WRITE (LP,210)
      WRITE (LP,220)
      WRITE (LP,230)
      WRITE (LP,240)
      WRITE (LP,250)
      WRITE (LP,260) OPTPRT,OPTPNC,OPTOUT,(FILE(I),I=1,3),OPMATP,OPTUO
C
      IF (IRGCK.GT.0) GO TO 100
         IF (ITYPE.EQ.3.AND.IGRID.EQ.1) GO TO 90
            WRITE (LP,270)
            WRITE (LP,280) STMNWT,(OPTWGT(I),I=1,2)
            GO TO 105
90       WRITE (LP,310) STMNWT,(OPTWGT(I),I=1,2),POWER
         GO TO 105
C
100   WRITE (LP,290)
      WRITE (LP,300) IRGCK
C      
105   RETURN
C      
C-----------------------------------------------------------------------
C
110   FORMAT(/,21X,'CONSISTENCY',19X,'INPUT',8X,'MULTIPLE')
120   FORMAT(' ',9X,'MAT',10X,'CHECK',9X,'TEMPCK',8X,'UNITS',7X,
     *   'CALB FILES')
130   FORMAT(' ',5(8X,'OPTION'))
140   FORMAT(' ',5(8X,6('-')))
150   FORMAT(' ',9X,A4,2(10X,A4),9X,2A4,7X,A4)
160   FORMAT(//,21X,'TEMPERATURE',6X,'TIME',9X,'SUMMARY',7X,'CARDS')
170   FORMAT(' ',7X,'ELEVATION',4X,'CORRECTION',4X,'CORRECTION',
     *           6X,'TABLE',8X,'@F @G @H')
180   FORMAT(' ',5(8X,'OPTION'))
190   FORMAT(' ',5(8X,6('-')))
200   FORMAT(' ',5X,3A4,6X,A4,3(10X,A4))
210   FORMAT(//,9X,'INPUT')
220   FORMAT(' ',8X,'TEMP',10X,'HEADER',9X,'MAT',10X,'MAT',10X,'MAT',
     *   10X,'OUTPUT')
230   FORMAT(' ',8X,'PRINT',9X,'PUNCH',9X,'OUTPUT',7X,'OUTPUT',7X,
     *   'PRINT',9X,'UNITS')
240   FORMAT(' ',3(8X,'OPTION'),8X,'FILE',8X,'OPTION',8X,'OPTION')
250   FORMAT(' ',3(8X,6('-')),8X,4('-'),2(8X,6('-')))
260   FORMAT(' ',9X,A4,10X,A4,10X,A4,5X,3A4,5X,A4,9X,2A4)
270   FORMAT(//,8X,'MINIMUM',8X,'TYPE',/,8X,'STATION',9X,'OF',
     *   /,9X,'WEIGHT',7X,'WEIGHT',/,9X,6('-'),7X,6('-'))
280   FORMAT(' ',9X,F5.3,6X,2A4)
290   FORMAT(//,8X,'TEMPCK',/,7X,'STATION',/,8X,'NUMBER',/,
     *   8X,6('-'))
300   FORMAT(' ',9X,I2)
310   FORMAT(//,8X,'MINIMUM',8X,'TYPE',/,8X,'STATION',9X,'OF',
     *   /,9X,'WEIGHT',7X,'WEIGHT',8X,'POWER',/,9X,6('-'),7X,
     *   6('-'),8X,6('-'),/,10X,F5.3,6X,2A4,8X,F4.2)
C
      END
