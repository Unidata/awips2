C  =====================================================================
C  pgm: W3AK18 .. dummy routine (dynamic allocation of data set)
C  =====================================================================
      SUBROUTINE W3AK18(DDNAME,DSNAME,KEY,ISTAT)
 
      CHARACTER*8    DDNAME,DSNAME
      INTEGER        KEY,ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_stubs/RCS/w3ak18.f,v $
     . $',                                                             '
     .$Id: w3ak18.f,v 1.1 1995/09/17 19:04:38 dws Exp $
     . $' /
C    ===================================================================
C
 
        ISTAT = 0
 
      RETURN
      END
