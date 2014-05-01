C @PROCESS LVL(77)
C MEMBER DGBLOCK
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/10/95.12:53:01 BY $WC20SV
C
C
      BLOCK DATA DGBLK
C
C
C  INITIALIZE GOES COMMON BLOCKS
C
C
C                                           *--> FROM DGCOMMON.DGUNTS           
C  GOES DATA FILE UNITS COMMON BLOCK                                            
      COMMON /DGUNTS/ KDGRCF                                                    
C                                                                               
C                                           *--> FROM DGCOMMON.DGVALU           
C  GOES SLR RECORD INFORMATION COMMON BLOCK                                     
      COMMON /DGVALU/ ISLRHD,MAXLDG,IDGDUM(4)                                   
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSDGBLOCK       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/dgblock.f,v $
     . $',                                                             '
     .$Id: dgblock.f,v 1.2 1995/10/02 13:37:49 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                               
C
C
      DATA KDGRCF/15/
C
      DATA ISLRHD/12/,MAXLDG/60/,IDGDUM/4*0/
C
C
      END
