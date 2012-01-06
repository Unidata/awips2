	SUBROUTINE CV_DATE2JUL (YR,MON,DAY,JD,ISTATUS)
C
C	Subroutine CV_DATE2JUL converts input dates into Julian day.
C It is a "pirated" version of the ASCII2JUL.  The major
C difference is that CV_DATE2JUL does not utilize an ASCII string for
C the data.
C
C
C	INPUT:
C		YR		Year
C		MON		Month
C               DAY             Day
C
C	OUTPUT:
C		JD		An integer form of the Julian day
C				This form is currently [ddd]; it
C				can, however, be easily changed  to
C				the form [yyddd] (see below)
C
C
      implicit none

        INTEGER   YR,MON,DAY
	INTEGER*4 JD
        INTEGER   istatus
C
C ****	Calculate number of days  ****
C
	JD = 0
	IF(MON.EQ.1)GO TO 20
C
	JD = JD+31
	IF (MON.EQ.2)GO TO 20
C
	JD=JD+28
	IF (YR/4*4 .EQ. YR) JD=JD+1
	IF (MON.EQ.3)GO TO 20
C
	JD=JD+31
	IF(MON.EQ.4)GO TO 20
C
	JD=JD+30
	IF(MON.EQ.5)GO TO 20
C
	JD=JD+31
	IF(MON.EQ.6)GO TO 20
C
	JD=JD+30
	IF(MON.EQ.7)GO TO 20
C
	JD=JD+31
	IF(MON.EQ.8)GO TO 20
C
	JD=JD+31
	IF(MON.EQ.9)GO TO 20
C
	JD=JD+30
	IF(MON.EQ.10)GO TO 20
C
	JD=JD+31
	IF(MON.EQ.11)GO TO 20
C
	JD=JD+30
	IF(MON.EQ.12)GO TO 20
C
C
C ****	Getting to this point indicates an error.  ****
C
C
	GO TO 200
C
C
20	JD=JD+DAY	!This gives JD in [ddd] form.
C       JD=YR*1000+JD	!If this line is included, JD is returned in
			![yyddd] form.
C
C ****	Return JD as an integer value -- not as a character string  ****
C
	RETURN
C
C
c   `g77' doesn't support the I/O statements `TYPE' and `ACCEPT'.  
c200	TYPE *,'Error in retrieving Julian day from input;',
  200	print *,'Error in retrieving Julian day from input;',
     1	        ' exiting subroutine CV_DATE2JUL'
	Istatus = 0
        RETURN
	END
