C MEMBER DFBLOCK
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/21/94.11:56:19 BY $WC20SV
C
C @PROCESS LVL(77)
C
       BLOCK DATA DFBLK
C
C
C  INITIALIZE SHEF COMMON BLOCKS
C
C
      INCLUDE 'dfcommon/dfunts'
C
      COMMON /CODES/ ICHA,ICHB,ICHC,ICHD,ICHE,ICHF,ICHG,ICHH,ICHI,
     1               ICHJ,ICHK,ICHL,ICHM,ICHN,ICHO,ICHP,ICHQ,ICHR,
     2               ICHS,ICHT,ICHU,ICHV,ICHW,ICHX,ICHY,ICHZ,ICH0,
     3               ICH1,ICH2,ICH3,ICH4,ICH5,ICH6,ICH7,ICH8,ICH9,
     4               IBLNK,ISLASH,ICOLON,IPLUS,IMINUS,IDOT,IARROW,
     5               ICOMMA
C
      COMMON /DCHARX/ LASTCH,MAXCH
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSDFBLOCK       / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/dfblock.f,v $
     . $',                                                             '
     .$Id: dfblock.f,v 1.1 1995/09/17 18:41:07 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET UNIT NUMBERS
      DATA KFPARM/20/,KFPPDB/21/,KFSOUT/22/
C
C  SET VALUES FOR CHARACTERS
      DATA ICHA    /65/
      DATA ICHB    /66/
      DATA ICHC    /67/
      DATA ICHD    /68/
      DATA ICHE    /69/
      DATA ICHF    /70/
      DATA ICHG    /71/
      DATA ICHH    /72/
      DATA ICHI    /73/
      DATA ICHJ    /74/
      DATA ICHK    /75/
      DATA ICHL    /76/
      DATA ICHM    /77/
      DATA ICHN    /78/
      DATA ICHO    /79/
      DATA ICHP    /80/
      DATA ICHQ    /81/
      DATA ICHR    /82/
      DATA ICHS    /83/
      DATA ICHT    /84/
      DATA ICHU    /85/
      DATA ICHV    /86/
      DATA ICHW    /87/
      DATA ICHX    /88/
      DATA ICHY    /89/
      DATA ICHZ    /90/
C
      DATA ICH0    /48/
      DATA ICH1    /49/
      DATA ICH2    /50/
      DATA ICH3    /51/
      DATA ICH4    /52/
      DATA ICH5    /53/
      DATA ICH6    /54/
      DATA ICH7    /55/
      DATA ICH8    /56/
      DATA ICH9    /57/
C
      DATA IBLNK   /32/
      DATA ISLASH  /47/
      DATA ICOLON  /58/
      DATA IPLUS   /43/
      DATA IMINUS  /45/
      DATA IDOT    /46/
      DATA IARROW  /60/
      DATA ICOMMA  /44/
C
C  SET MAXIMUM NUMBER OF COLUMNS OF SHEF MESSAGES TO BE PROCESSED
       DATA LASTCH/0/
       DATA MAXCH/80/
C
      END
