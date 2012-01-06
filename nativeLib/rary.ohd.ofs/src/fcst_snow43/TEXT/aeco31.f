C     Module AECO31
C
      SUBROUTINE AECO31 (X,CAESC)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE ADJUSTS SNOW MODEL CARRYOVER VALUES FOR A
C     CHANGE IN THE AREAL EXTENT OF THE SNOW COVER.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON-HRL APRIL 1981 FOR THE SNOW-17 OPERATION
C     MODIFIED BY...
C        JAY DAY, RTi, FOR USE IN THE 'SNOW-43' OPERATION
C        Comments added by Nalneesh Gaur, RTi, July 1995
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      X            I/O         Array containing updated states
C      CAESC         I          Computed Areal Extent of Snow Cover
C.......................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real x, caesc
C     --- L O C A L ---

C dws    Var J must be made an integer to avoid warnings ... 2006-01-23
C dws integer i, n
C dws real    r, fn, j, fj

      integer   i, j, n
      real      r, fn, fj

      real    taesc, weai, aescmn, cover, aescmx
C
C----- D I M E N S I O N --------------------------------
      DIMENSION X(5)
C
C----- C O M M O N  B L O C K S -------------------------
      include 'snow43/snco31'
      include 'snow43/snpm31'
      include 'snow43/snup31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/aeco31.f,v $
     . $',                                                             '
     .$Id: aeco31.f,v 1.2 2006/03/16 18:54:04 xfan Exp $
     . $' /
C    ===================================================================
C
C.......................................
C
C
      COVER=X(5)
      AESCMX=CAESC+sctol
      AESCMN=CAESC-sctol
      IF(COVER.GT.AESCMX.OR.COVER.LT.AESCMN) GO TO 50
      X(5)=CAESC
      GO TO 999
   50 WELIQW=X(1)+X(3)
C
C     DETERMINE IF CURRENTLY ON DEPLETION CURVE OR NEW SNOW LINE.
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      IF(WELIQW.GE.AI) GO TO 100
      IF(WELIQW.LE.SB) GO TO 100
C.......................................
C     CURRENTLY ON NEW SNOW LINE.
C
      IF (COVER.LE.SBAESC) GO TO 100
C
C   THIS NEXT SECTION KEEPS THE NEW SNOW LINE FROM BEING TOO STEEP.
C
      R=(WELIQW/AI)*10.+1.
      N=R
      FN=N
      R=R-FN
      TAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(COVER.LE.TAESC) GO TO 100
C
C     ADJUST SBWS , LEAVE AEADJ AS IS.
C
      SBWS=((1.0-SBAESC)/(COVER-SBAESC))*(WELIQW-SB)+SB
C
C   KEEP THE NEW SNOW LINE FORM BEING TOO FLAT.
C
      IF(SBWS.GT.SB+.75*SNOF) GO TO 999
C
      SB=SBWS-.75*SNOF
      R=(SB/AI)*10.+1.
      N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      GO TO 999
C.......................................
C     CURRENTLY OR SHOULD BE ON THE DEPLETION CURVE.
  100 DO 101 I=2,11
      IF (COVER.GE.ADC(I)) GO TO 101
      J=I-1
      FJ=J
      WEAI=0.1*(FJ+ (COVER-ADC(J))/(ADC(I)-ADC(J)))
      GO TO 105
  101 CONTINUE
      WEAI=1.0
  105 AEADJ=WELIQW/WEAI
      SBAESC=COVER
C.......................................
  999 RETURN
      END
