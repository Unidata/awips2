C MODULE UPRIMW
C  =====================================================================
C  pgm: UPRIMW .. Write user info
C
C   in: (common) ...... block common UPDAIO contains unit numbers for
C   in:                 i/o routine messages (see subrtn UPRIMO)
C
C  rqd: subrtns: KKSMLS,KKTRIM,UPEXIS,UPDELE,UPOPEN
C  rqd: common:  UPDAIO
C  =====================================================================
      SUBROUTINE UPRIMW(STMT,FMT,AVUN,IUN)


      EXTERNAL      KKSMLS,KKTRIM,UPEXIS,UPDELE,UPOPEN

      INTEGER       AVUN,IUN,JUN,JRECL,II,LBEG,LEND
      CHARACTER*(*) STMT
      CHARACTER*128 FILNAM,LN
      CHARACTER*4   FILCMD
      CHARACTER*1   FMT

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/uprimw.f,v $
     . $',                                                             '
     .$Id: uprimw.f,v 1.6 2004/05/03 21:39:21 hank Exp $
     . $' /
C    ===================================================================
C


        IUN = 0

        IF( UW .GE. 0 ) WRITE(UW,'(A)') STMT
        READ(UR,'(A)') FILNAM

        CALL KKTRIM(FILNAM,LBEG,LEND)
        FILCMD = FILNAM(LBEG:LEND)
        CALL KKSMLS(FILCMD)

CDWS    IF( FILCMD .EQ. '    ' ) GO TO 100
        IF( FILCMD .EQ. '    ' ) GO TO 140
        IF( FILCMD .EQ. 'quit' ) GO TO 900
        IF( FILCMD .EQ. 'q   ' ) GO TO 900
        IF( FILCMD .EQ. 'tty ' ) GO TO 140

          IUN = AVUN
          JUN = 0
          JRECL = 0
          CALL UPEXIS(JUN,FILNAM(LBEG:LEND),II)
          IF( II.EQ.0 ) CALL UPDELE(JUN,FILNAM(LBEG:LEND),II)
          IF( II.LE.0 ) CALL UPOPEN(IUN,FILNAM(LBEG:LEND),JRECL,FMT,II)
          IF( II.NE.0 ) IUN = -1
          
CHDH    Get the name of the file opened before writing it out. Move the
C       file to the /tmp directory if it is a TEMP.* file (pass in 1).
          LN = FILNAM
          CALL UPNOFI(LN,1)   
          CALL KKTRIM(LN,LBEG,LEND)                 
          IF(II.EQ.0 .AND. UW.GE.0) WRITE(UW,200) LN(LBEG:LEND),AVUN
          IF(II.NE.0 .AND. UW.GE.0) WRITE(UW,202) LN(LBEG:LEND),AVUN,II
          
C        IF(II.EQ.0 .AND. UW.GE.0) WRITE(UW,200) FILNAM(LBEG:LEND),AVUN
C        IF(II.NE.0 .AND. UW.GE.0) WRITE(UW,202) FILNAM(LBEG:LEND),AVUN
C     $                                            ,II
            GO TO 900

  140     CONTINUE
          UE  = UW
          UU  = -1
          IUN = UTW
 
  200     FORMAT('    ',A,' opened on unit',I4)
  202     FORMAT('     WARNING: Cannot open file ',A,' on unit ',
     $           I4,'  err:',I6)

  900 RETURN
      END
