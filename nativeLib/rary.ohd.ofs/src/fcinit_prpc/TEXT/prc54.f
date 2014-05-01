C MEMBER PRC54
C
      SUBROUTINE PRC54(PO,CO)
C.......................................
C     THIS SUBROUTINE PRINTS THE STATE VARIABLES (CARRYOVER VALUES)
C        FOR THE SIMPLE WATER BALANCE MODEL (SWB-NILE) OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        QINGYUN DUAN - GCIP CLIMATE PROJECT SEPTEMBER 1995 VERSION 1
C.......................................
C
      DIMENSION PO(1),CO(1)
C
C**********************************************************
C    COMMON BLOCKS
C**********************************************************
C                                           *--> FROM COMMON.FDBUG
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C                                           *--> FROM COMMON.IONUM
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc54.f,v $
     . $',                                                             '
     .$Id: prc54.f,v 1.1 1997/09/22 15:34:27 page Exp $
     . $' /
C    ===================================================================
C
C
C**********************************************************
C   CHECK TRACE LEVEL
C**********************************************************
C
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,16H** PRC54 ENTERED)
C
C**********************************************************
C    GET CONTROL VARIABLES
C**********************************************************
C
      NCO=PO(17)
      NXCO=PO(18)
      LFRZE=PO(22)
C
C**********************************************************
C    PRINT HEADING
C**********************************************************
C
      WRITE (IPR,901) (PO(I),I=2,6)
  901 FORMAT (1H0,10X,'SIMPLE WATER BALANCE (SWB-NILE) OPERATION FOR',
     +        1X,5A4)
C
C     PRINT CARRYOVERS
      WRITE (IPR,902)
  902 FORMAT (1H0,18X,'SOIL MOISTURE CONTENTS (MM)',
     +        /,24X,'SU',8X,'SB')
      WRITE (IPR,903) (CO(I),I=1,NCO)
  903 FORMAT (1H0,15X,2F10.3)
C
      IF (LFRZE.EQ.0) RETURN
      WRITE (IPR,904)
  904 FORMAT (1H0,18X,'FROZEN AND THAWING DEPTH, SNOW DEPTH (CM), SNOW',
     +        ' DENSITY (G/CM3), AND ICE CONTENT (MM)',/,20X,'FDP(1)',
     +        4X,'FDP(2)',4X,'TDP(1)',4X,'TDP(2)',7X,'SDP',7X,'SDN',3X,
     +        'WICE(1)',3X,'WICE(2)')
      IC=NCO+1
      LC=NCO+NXCO
      WRITE(IPR,905) (CO(I),I=IC,LC)
  905 FORMAT (1H ,15X,8F10.3)
C
  199 RETURN
      END
