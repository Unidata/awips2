C MODULE XFMAPP
C-----------------------------------------------------------------------
C
      SUBROUTINE XFMAPP (ITSID,IFFHR,ILFHR,NDAYS,PX,NUM,UNITS,IWRTFL)
C
C  THIS PRINTS THE SIX HOUR FUTURE PRECIP ESTIMATES FOR ALL
C  FUTURE MAP AREAS WITH INPUT DATA.
C
C  IMPORTANT VARIABLES INCLUDE:
C
C      ITSID = TIME SERIES ID
C      NDAYS = NUMBER OF DAYS OF FUTURE DATA IN THE RUN
C         PX = 6 HOUR FMAP VALUES
C      UNITS = UNITS ABBREVIATIONS FOR THE PRECIP UNITS (IN OR MM)
C
      CHARACTER*4 UNITS
      CHARACTER*8 ITSID,OLDOPN
      PARAMETER (LDATA=4)
      CHARACTER*6 DATA(LDATA)
      CHARACTER*12 DUNITS
C
      DIMENSION PX(1)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      COMMON /XFDATE/ IFEM,IFED,IFEY,IFEH,ILEM,ILED,ILEY,ILEH,IM(31),
     $ ID(31),IY(31),IH(4),IOPTZC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fmap/RCS/xfmapp.f,v $
     . $',                                                             '
     .$Id: xfmapp.f,v 1.6 2000/08/01 14:06:19 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
      CALL FSTWHR ('XFMAPP  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.0)WRITE (IOPDBG,*) 'ENTER XFMAPP'
C
      IBUG=IPBUG('XFP ')
C
C  DETERMINE THE UNITS OF THE INPUT DATA
      DUNITS='INCHES'
      NDEC=2
      IF (UNITS.EQ.'MM') THEN
         DUNITS='MILLIMETERS'
         NDEC=0
	 ENDIF
C
C  WRITE COLUMN HEADINGS
      IF (IWRTFL.EQ.0) THEN
         WRITE (IPR,180) DUNITS
         WRITE (IPR,190) IH(1),IOPTZC,IH(2),IOPTZC,IH(3),IOPTZC,IH(4),
     $      IOPTZC
         ENDIF
C
C  FIND LAST DAY WITH NON-ZERO QPF VALUES
      DO 50 I=1,NUM
         J=NUM-I+1
         IF (PX(J).LT.0.001)GO TO 50
         L=J
         GO TO 60
50       CONTINUE
      GO TO 150
60    L=L+IFFHR/6
      NDA=(L-1)/4+1
C
C  WRITE SIX HOUR PRECIP DATA
      I1=1
      DO 140 N=1,NDA
         DO 70 I=1,LDATA
            DATA(I)=' '
70          CONTINUE
         IF (N.GT.1) GO TO 80
            NP=(24-IFFHR)/6
            L=(IFFHR/6)+1
            GO TO 110
80       IF (N.LT.NDAYS) GO TO 90
            NP=ILFHR/6
            IF (ILFHR.EQ.0) NP=4
            GO TO 100
90       NP=4
100      L=1
110      DO 120 I=1,NP
	    IBEG=1
	    NCHAR=LEN(DATA(L))
	    IPRERR=1
	    CALL UFF2A (PX(I1),DATA(L),IBEG,NCHAR,NDEC,IPRERR,IPR,ISTAT)
            IF (ISTAT.NE.0) THEN
               WRITE (IPR,170) I
               CALL ERROR
               ENDIF
            I1=I1+1
            L=L+1
120         CONTINUE
         IF (N.GT.1) GO TO 130
            WRITE (IPR,200) ITSID,IM(N),ID(N),IY(N),DATA
            GO TO 140
130      WRITE (IPR,210) IM(N),ID(N),IY(N),DATA
140      CONTINUE
C
150   CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.0) WRITE (IOPDBG,*) 'EXIT XFMAPP'
C
170   FORMAT('0**ERROR** REAL TO CHARACTER CONVERSION ',
     1 'CANNOT BE MADE FOR PERIOD ',I2)
180   FORMAT ('0',12X,' SIX HOUR PRECIPITATION IN ',A,
     $ ' (ALL VALUES NOT SHOWN ARE ZERO)')
190   FORMAT (
     $ '0',20X,'FMAPID  ',2X,'END DATE  ',2X,4(I2.2,A4,2X) /
     $ ' ',20X,'--------',2X,'----------',2X,4(6('-'),2X))
200   FORMAT (' ',20X,A8,2X,I2.2,'/',I2.2,'/',I4.4,2X,4(A,2X))
210   FORMAT (' ',20X,8X,2X,I2.2,'/',I2.2,'/',I4.4,2X,4(A,2X))
C
      RETURN
C
      END

