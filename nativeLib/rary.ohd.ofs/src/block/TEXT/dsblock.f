C MEMBER DSBLOCK
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      BLOCK DATA DSBLOCK
C
C            BLOCK DATA
C
C             VERSION:  1.0.0
C
C                DATE:  AUGUST 25, 1983
C
C              AUTHOR:  PAUL MOORE
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    BLOCK DATA ROUTINE FOR SASM COMMON BLOCKS.
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'dscommon/dsunts'
      INCLUDE 'dscommon/dstdta'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSDSBLOCK       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/dsblock.f,v $
     . $',                                                             '
     .$Id: dsblock.f,v 1.1 1995/09/17 18:41:08 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C  COMMON /DSUNTS/
      DATA KDSHMF/13/,KDSRCF/14/,KDSSFC/45/,KDSADP/80/
      DATA KDSAID/30/,KDSMID/31/
C
C  COMMON /DSTDTA/
      DATA MSNG/99999/,PDMSNG/-999.0/,SDMSNG/99999.0/
      DATA IRPJHR/0/
      DATA IDTADM/4*0/
C
C
      END
