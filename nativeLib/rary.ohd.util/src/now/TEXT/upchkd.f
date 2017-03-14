C  =====================================================================
C  pgm: UPCHKD .. check for existance of global input file directories
C
C  use:     CALL UPCHKD(GLODIR,STATUS)
C
C   in: GLODIR ........ string of words indicating which tokens to use
C   in:                 to see if its directory exists; capital letters
C   in:                 may be used; example: 'syst  OPER' - CHAR*(*)
C   in:                      word         token
C   in:                      ----         ---------------
C   in:                      syst         rfs_sys_dir
C   in:                      oper         ofs_fs5files
C   in:                      mods         ofs_mods_dir
C   in:                      grid         ofs_griddb_dir
C   in:                      reor         ofs_reorder_dir
C  out: STATUS ........ output status; - INT
C  out:                    0 = all pathname directories exist
C  out:                    1 = pathname does not exist for input word
C  out:                    2 = bad input word
C  out:                    4 = a pathname directory does not exist
C
C  rqd: UPPFIX,UPEXIS,KKNXWD,KKCAPS
C
C  cmt: The calling program should call routine UPINIO first to set
C  cmt:  variable UE to the error output unit number.
C  =====================================================================
      SUBROUTINE UPCHKD(GLODIR,STATUS)
 
      INTRINSIC      LEN
      EXTERNAL       UPPFIX,UPEXIS,KKNXWD,KKCAPS

      INCLUDE 'updaio'
 
      CHARACTER*(*)  GLODIR
      CHARACTER*6    NXWD
      INTEGER        STATUS,NOFC,IUN,ISTAT,IERR
      INTEGER        KLAS,KLIM,KWLM,KLEN,LEN
      CHARACTER*128  OTPATH
      CHARACTER*32   INFILE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upchkd.f,v $
     . $',                                                             '
     .$Id: upchkd.f,v 1.4 2001/06/13 12:06:22 mgm Exp $
     . $' /
C    ===================================================================
C
 
        STATUS = 0
 
        INFILE = 'ZZ'
        IUN = 0
 
        KLIM = LEN(GLODIR)
        KLAS = 0
        KWLM = 6
        CALL KKNXWD(GLODIR,KLAS,KLIM,NXWD,KWLM,KLEN)
   20   IF (STATUS.NE.0 .OR. KLEN.LE.0) GOTO 40
          CALL KKCAPS(NXWD)
 
          NOFC = 0
          IF (NXWD .EQ. 'SYST') THEN
            CALL UPPFIX('SYST',INFILE,OTPATH,NOFC)
          ELSEIF (NXWD .EQ. 'OPER') THEN
            CALL UPPFIX('OPER',INFILE,OTPATH,NOFC)
          ELSEIF (NXWD .EQ. 'MODS') THEN
            CALL UPPFIX('MODS',INFILE,OTPATH,NOFC)
          ELSEIF (NXWD .EQ. 'GRID') THEN
            CALL UPPFIX('GRID',INFILE,OTPATH,NOFC)
          ELSEIF (NXWD .EQ. 'REOR') THEN
            CALL UPPFIX('REOR',INFILE,OTPATH,NOFC)
          ENDIF

          IF (NOFC .EQ. 0) THEN
            STATUS = 2
          ELSEIF (NOFC .LE. 3) THEN
            STATUS = 1
          ELSE
            OTPATH(NOFC-2:NOFC) = '   '
            CALL UPEXIS(IUN,OTPATH,ISTAT)
            IF (ISTAT .NE. 0) STATUS = 4
          ENDIF
 
          IF (STATUS.EQ.0) CALL KKNXWD(GLODIR,KLAS,KLIM,NXWD,KWLM,KLEN)
            GOTO 20
   40   CONTINUE
 
        IF (STATUS.GT.0 .AND. UE.GE.0) WRITE(UE,240,IOSTAT=IERR) STATUS
  240   FORMAT(' upchkd      ** ERROR =',I3,', bad global dirs')
 
      RETURN
      END
