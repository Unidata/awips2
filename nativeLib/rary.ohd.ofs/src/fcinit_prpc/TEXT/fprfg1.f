C MEMBER FPRFG1
C  (from old member FCPRP1)
C
      SUBROUTINE FPRFG1(PF)
C.......................................
C     THIS SUBROUTINE PRINTS FROZEN GROUND INFORMATION FOR
C     THE 'SAC-SMA ' OPERATION.
C.......................................
C     WRITTEN BY  ERIC ANDERSON-HRL   JUNE 1980
C.......................................
      DIMENSION PF(1)
C
C     COMMON BLOCK
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/fprfg1.f,v $
     . $',                                                             '
     .$Id: fprfg1.f,v 1.1 1995/09/17 18:49:31 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA FGIX/4HFGIX/
C.......................................
C     CONTROL VARIABLES
      LWE=PF(5)
      LFI=PF(6)
      LP=7
C.......................................
C     PRINT TIME SERIES USED.
      WRITE (IPR,900)
  900 FORMAT(1H0,20X,47HTIME SERIES USED FOR FROZEN GROUND COMPUTATIONS,
     1 //16X,8HCONTENTS,14X,4HI.D.,7X,4HTYPE,5X,13HTIME INTERVAL)
      IT=PF(4)
      WRITE (IPR,901) (PF(I),I=1,3),IT
  901 FORMAT(1H0,10X,15HAIR TEMPERATURE,10X,2A4,5X,A4,7X,I2,1X,5HHOURS)
      IF(LWE.EQ.0) GO TO 101
      IT=PF(LWE+3)
      WRITE(IPR,902) PF(LWE), PF(LWE+1), PF(LWE+2),IT
  902 FORMAT(1H ,10X,16HWATER-EQUIVALENT,9X,2A4,5X,A4,7X,I2,1X,5HHOURS)
  101 IF(LFI.EQ.0) GO TO 102
      IT=PF(LFI+2)
      WRITE(IPR,903) PF(LFI), PF(LFI+1), FGIX,IT
  903 FORMAT(1H ,10X,11HFROST INDEX,14X,2A4,5X,A4,7X,I2,1X,5HHOURS)
C.......................................
C     PRINT PARAMETERS
  102 WRITE(IPR,904)
  904 FORMAT(1H0,10X,24HFROZEN GROUND PARAMETERS)
      WRITE(IPR,905)
  905 FORMAT(1H0,13X,7HFGPM(1),3X,7HFGPM(2),3X,7HFGPM(3),3X,7HFGPM(4),
     13X,7HFGPM(5),3X,7HFGPM(6),3X,7HFGPM(7),3X,7HFGPM(8),3X,7HFGPM(9),
     22X,8HFGPM(10))
      WRITE(IPR,906) (PF(LP+I), I=1,10)
  906 FORMAT(1H ,10X,10F10.3)
C.......................................
      RETURN
      END
