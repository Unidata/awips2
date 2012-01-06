C$PRAGMA C (DDRMJL)
C MODULE MXC_INIT
C  =====================================================================
C  pgm: MXC_INIT .. Initialize mapx variables to their default values
C
C  rqd: DDRMJL
C  =====================================================================
      SUBROUTINE MXC_INIT(ISTART,IEND,LLRUN,NAMLLG,DIRGRD,
     $                    PREF,NYY,DT,ICV,RMISS,UNIT,
     $                    NCOL,DIRMAP,MAXBSN,NAREA,BASINS,XTIME)

      CHARACTER*(*)   NAMLLG,DIRGRD,DIRMAP
      CHARACTER*(*)   PREF,UNIT,XTIME
      CHARACTER*8     BASINS(*)
      INTEGER         ISTART,IEND,LLRUN,NYY,DT,ICV
      INTEGER         RMISS,NCOL,MAXBSN,NAREA
      INTEGER         II,IERR
      INTEGER         KJ1,KY1,KH1,KN1,KS1
      CHARACTER*3     KHSTR1
      CHARACTER*2     KHSTR2,KHSTR3
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxc_init.f,v $
     . $',                                                             '
     .$Id: mxc_init.f,v 1.2 2002/02/15 20:07:51 dws Exp $
     . $' /
C    ===================================================================
C

        ISTART = 0
        IEND   = 0
        LLRUN  = 0
        NYY    = 4
        DT     = 1
        ICV    = 0
        RMISS  = 0
        NCOL   = 1
        NAREA  = 0
        PREF   = 'xmrg'
        UNIT   = 'MM'
        NAMLLG = ' '
        DIRGRD = ' '
        DIRMAP = ' '

        DO 17 II=1,MAXBSN
          BASINS(II) = ' '
   17   CONTINUE

        CALL DDRMJL(KJ1,KY1,KH1,KN1,KS1)
         WRITE(KHSTR1,'(I3.3)',IOSTAT=IERR) KJ1
         WRITE(KHSTR2,'(I2.2)',IOSTAT=IERR) KH1
         WRITE(KHSTR3,'(I2.2)',IOSTAT=IERR) KN1
        XTIME = KHSTR1 // '_' // KHSTR2 // KHSTR3

      RETURN
      END
