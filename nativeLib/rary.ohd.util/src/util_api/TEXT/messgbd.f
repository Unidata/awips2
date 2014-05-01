C  =====================================================================
C  pgm: MESSGBD .. Blockdata routine to set mask values for message rtns
C  =====================================================================
      BLOCK DATA MESSGBD

      INCLUDE 'messenger_inc/messg'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSMESSGBD       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/messgbd.f,v $
     . $',                                                             '
     .$Id: messgbd.f,v 1.1 2002/10/10 20:13:13 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  EVT_UNKNOWN      /      0 /
      DATA  EVT_LOGGING      /   8192 /
      DATA  EVT_START_RTN    /  16384 /
      DATA  EVT_FINISH_RTN   /  24576 /
      DATA  EVT_PROGRESS     /  32768 /
      DATA  EVT_PARAMETERS   /  40960 /
      DATA  EVT_SOURCE_IO    /  49152 /
      DATA  EVT_DATA         /  57344 /
      DATA  EVT_MISC         /  65536 /
      DATA  EVT_REGRESSION   /  73728 /
      DATA  EVT_GUI          /  81920 /

      DATA  ALWAYS_PRINT     /      1 /
      DATA  TIER_1           /     20 /
      DATA  TIER_2           /     40 /
      DATA  TIER_3           /     60 /
      DATA  TIER_4           /     80 /
      DATA  TIER_5           /    100 /

      DATA  SEV_DEBUG        /      0 /
      DATA  SEV_INFO         /    128 /
      DATA  SEV_WARNING      /    256 /
      DATA  SEV_ERROR        /    384 /

      DATA  PS_UNKNOWN       /      0 /
      DATA  PS_NONE          /    512 /
      DATA  PS_CORRECT       /   1024 /
      DATA  PS_NON_CORRECT   /   1536 /

      DATA  DQ_UNKNOWN       /      0 /
      DATA  DQ_GOOD          /   2048 /
      DATA  DQ_QUESTIONABLE  /   4096 /
      DATA  DQ_BAD           /   6144 /

      END
