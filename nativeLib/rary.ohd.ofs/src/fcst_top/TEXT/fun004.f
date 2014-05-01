C MODULE FUN004
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR THE ESP FUNCTION.
C
      SUBROUTINE FUN004
C
C....................................................................
C
C   THIS ROUTINE WAS WRITTEN BY GERALD N. DAY
C
      INCLUDE 'common/where'
      INCLUDE 'common/killcd'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fpwarn'
      INCLUDE 'common/fsacpr'
      INCLUDE 'common/fsnw'
      INCLUDE 'common/ffgctl'
      COMMON /FSNWUP/ IUPWE,IUPSC
      INCLUDE 'clbcommon/crwctl'
C
      CHARACTER*8 TECHNAME,SEGOLD,OPNOLD
      PARAMETER (LIARGS=84)
      DIMENSION IARGS(LIARGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fun004.f,v $
     . $',                                                             '
     .$Id: fun004.f,v 1.5 2002/02/11 20:40:01 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOPOLD=IOPNUM
      CALL UMEMOV (SEGOLD,ISEG,2)
      CALL UMEMOV (OPNAME,OPNOLD,2)
      IOPNUM=0
      CALL UMEMOV ('NONE YET',ISEG,2)
      CALL UMEMOV ('FUN004  ',OPNAME,2)
C
C  SET LOCAL VARIABLE TO NUMBER OF FATAL ERRORS ON ENTRY
      KILSTR=NKILLS
C
C   SET VALUES NOT SET ELSEWHERE
C
cew modwrn is now set by technique modwarn 
cew changed for ssarr mods.
cew      MODWRN=1
      IRWWRN=1
      IDTWRN=1
C
      IUPWE=0
      IUPSC=0
C
C  SET INDICATOR THAT THIS IS NOT AN FFG RUN
      IFFG = 0
C
      NOSNOW=0
      NOFRZE=0
C
C  SET NWARN AND NERRS TO ZERO
      NWARN=0
      NERRS=0
C
C   SET VALUES IN CRWCTL COMMON BLOCK SO PROGRAM WILL RETURN FROM
C   CALIBRATION FILE READ/WRITE SUBS EVEN IF THERE IS AN ERROR.
      ISTOP=0
      IERROR=0
C
      IOUNT=0
      IF (IOUNT.EQ.1) THEN
C
C   GET I/O UNIT NUMBERS FOR PRINT,PUNCH AND DEBUG
      TECHNAME='PRINT'
      CALL HPAST (TECHNAME,IPR,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN(ISTAT,TECHNAME)
      CALL FTEKCK (IPR,TECHNAME,6,IPR,1,9)
      TECHNAME='PUNCH'
      CALL HPAST (TECHNAME,IPU,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN(ISTAT,TECHNAME)
      CALL FTEKCK (IPU,TECHNAME,7,IPU,1,9)
      TECHNAME='DEBUGPR'
      CALL HPAST (TECHNAME,IODBUG,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN(ISTAT,TECHNAME)
      CALL FTEKCK (IODBUG,TECHNAME,6,IODBUG,1,9)
C
C   SET I/O UNIT NUMBER FOR ERROR MESSAGES
      TECHNAME='ERRORPR'
      CALL HPAST (TECHNAME,IOERR,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN(ISTAT,TECHNAME)
      CALL FTEKCK (IOERR,TECHNAME,9,IOERR,1,9)
C
      ENDIF
C
C   SET TRACE LEVEL
      TECHNAME='FCDEBUG'
      CALL HPASTA (TECHNAME,LIARGS,IFCDBG,NWORDS,IARGS,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN(ISTAT,TECHNAME)
      ITRACE=IARGS(84)
C
      CALL UPAGE (IPR)
      WRITE (IPR,10)
10    FORMAT('0',45X,'ESP FUNCTION')
C
      IF (ITRACE.GT.0 )WRITE (IODBUG,*) 'EXIT FUN004'
C
      CALL EILUVO
C
      CALL ESPEX
C
      CALL UCLOSL
C
      IF (NKILLS.EQ.KILSTR) CALL STOPFN ('ESP     ')
C
      IOPNUM=IOPOLD
      CALL UMEMOV (SEGOLD,ISEG,2)
      CALL UMEMOV (OPNOLD,OPNAME,2)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT FUN014'
C
      RETURN
C
      END
