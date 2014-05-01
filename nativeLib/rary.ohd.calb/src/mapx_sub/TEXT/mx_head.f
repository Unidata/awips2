C  =====================================================================
C  pgm: MX_HEAD .. Output heading to the program status output unit
C
C  use:     CALL MX_HEAD(PGMVRN,PGMVRD)
C
C   in: PGMVRN ....... program version number as char str - char*(*)
C   in: PGMVRD ....... program version date as 'mm/dd/yy' - char*(*)
C  out: (file) ....... Output text goes to routine 'WLIN' which contains
C  out:                the output unit number information
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MX_HEAD(PGMVRN,PGMVRD)

      EXTERNAL       WLIN

      CHARACTER*(*)  PGMVRN,PGMVRD
      CHARACTER*200  LIN
      CHARACTER*25   MSG1
      CHARACTER*10   MSG2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mx_head.f,v $
     . $',                                                             '
     .$Id: mx_head.f,v 1.1 2001/06/13 09:18:39 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA  MSG1  / '  ========>  MAPX   ver: ' /
      DATA  MSG2  / '    date: ' /

        CALL WLIN('B',' ')
        LIN = ' '
        WRITE(LIN,'(A,A,A,A)',IOSTAT=JE) MSG1,PGMVRN,MSG2,PGMVRD
        IF (JE.EQ.0) CALL WLIN('M',LIN)
        IF (JE.EQ.0) CALL WLIN('B',' ')

      RETURN
      END
