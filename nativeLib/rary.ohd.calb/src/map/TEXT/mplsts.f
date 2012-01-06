C   MEMBER MPLSTS
C ----------------------------------------------------------------------
C
      SUBROUTINE MPLSTS (FILE,MM,XDATA)
C
C  THIS ROUTINE LISTS THE DATA FOR A SINGLE OR A MULTIPLE TIME
C  SERIES
C
      character    FILE*112,OFORMAT*12
      INTEGER      BMO,BYR,EMO,EYR,DEBUG,DEBUGA
C
      DIMENSION    XDATA(744,M2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'clbcommon/bhtime'
      COMMON  /DIM/  M1,M2,M3,M4,M5,M6
      COMMON  /MAP/  DEBUG,DEBUGA
      COMMON  /LSDTA/  IDEC,BMO,BYR,EMO,EYR,UNITW,FILEN(3),DTYPEE,IT,
     *                 STAID(3),DESCRP(5),DESCR(5),JMO,JYR,DATA(744)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mplsts.f,v $
     . $',                                                             '
     .$Id: mplsts.f,v 1.2 1998/04/07 19:33:48 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA         SAME/4HSAME/,DTYPE/4HMAP /
C
      IF (DEBUG.EQ.1) WRITE (IPR,110)
C
      UNITW=SAME
      IMO=0
      IYR=0
      LMO=0
      LYR=0
      RUNITS=UNITW
      CALL CARDLO(IMO,IYR,LMO,LYR, IM1, IY1, IM2, IY2,
     +            RUNITS, UNITW, FILE, DTYPE, IT, STAID,
     +            DESCRP, IUNIT, OFORMAT, XMFAC, AFAC, IAM1, IAM2,
     +            IERROR)
      WRITE (IPR,80) UNITW,DESCRP
C
      NMO=(LMO+LYR*12)-(IMO+IYR*12)+1
C
C  READ DATA
      JMO=IMO
      JYR=IYR
      NVAL=744/IT
      IF (DEBUG.EQ.1) WRITE (IPR,140) NMO,JMO,JYR,NVAL
      WRITE (IPR,100)
C
      DO 50 I=1,NMO
      CALL CARDRD (1,IUNIT,OFORMAT,IAM1,IAM2,XMFAC,AFAC,IT,
     +             JMO,JYR,XDATA(1,mm),1,IERROR)
      if(ierror.ne.0) then
         write(ipr,1) file,jmo,jyr
         stop
      end if
      DO 30 K=1,NVAL
30    DATA(K)=XDATA(K,MM)
      WRITE (IPR,90)  JMO,JYR
c
      CALL MPFORM
      JMO=JMO+1
      IF (JMO.LE.12)  GO TO 50
      JMO=1
      JYR=JYR+1
50    CONTINUE
C
      RETURN
    1 format(/,2x,'*** error reading input time series for',1x,
     *  'file=',a,/,3x,
     *  'month=',i2,2x,'year=',i2,2x,'-- program stopping ***')
60    FORMAT (/   T10,8HCOLUMN 1,T25,2H10,T35,2H20,T45,2H30,T55,2H40,
     *            T65,2H50,T75,2H60,T85 ,2H70,T95 ,2H80/
     *            T17,16(5H----+))
70    FORMAT (' ',15X,A12,A4,I4,I6,'-',3A4,'-',
     *   I2.2,'/',I2.2,'/',I2.2,I4.4,'.',I4.4,2X,5A4)
80    FORMAT ('1- LISTING OF DATA (UNITS=',A,')',
     *   'FOR THE TIME SERIES ',5A4/)
90    FORMAT ('0',I2,'/',I4)
100   FORMAT (///)
110   FORMAT (' *** ENTER MPLSTS')
120   FORMAT (T10,3A4,5I10)
130   FORMAT (8H0TRACE 1 ,5I10)
140   FORMAT (8H0TRACE 2 ,5I10)
C
      END
