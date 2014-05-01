C MEMBER IANSW
C  (from old member PPGIANSW)
C
      FUNCTION IANSW(IARG)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/iansw.f,v $
     . $',                                                             '
     .$Id: iansw.f,v 1.1 1995/09/17 19:02:51 dws Exp $
     . $' /
C    ===================================================================
C
C
C.....THIS FUNCTION RETURNS A VALUE OF 1 IF IARG IS 1.
C.....THIS FUNCTION RETURNS A VALUE OF 2 IF IARG IS SOMETHING OTHER
C.....THAN 1.
C
C.....THIS FUNCTION IS PRIMARILY INTENDED TO TEST VALUES OF THE VARIOUS
C.....YES/NO OR ON/OFF TECHNIQUES IN MARO. THE VALUE OF THE FUNCTION IS
C.....PRIMARILY USED AS A POINTER TO AN ARRAY THAT COVERS TWO PRINT
C.....CHOICES IN THE MARO TITLE PRINT SUBROUTINE.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN     WGRFC FT. WORTH, TEXAS     FEBRUARY 10, 1988
C
      IF(IARG .EQ. 0) GOTO 10
      IF(IARG .EQ. 1) GOTO 20
   10 IANSW = 2
      RETURN
C
   20 IANSW = 1
      RETURN
      END
