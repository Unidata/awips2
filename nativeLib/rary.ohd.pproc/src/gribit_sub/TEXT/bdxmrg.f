      block data bdxmrg_sub
c
c  initialize attributes used when creating GRIB files from xmrg file
c
      include 'xmrg_pm'
      character*62 xla(16),xlb(14)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSBDXMRG        / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob81/ohd/pproc/src/gribit_sub/RCS/bdxmrg.f,v $
     . $',                                                             '
     .$Id: bdxmrg.f,v 1.1 2006/10/19 16:06:04 dsa Exp $
     . $' /
C    ===================================================================
C
      equivalence (xline(1),xla(1)),(xline(17),xlb(1))

c -1:9:-1:xmrg         xmrg to GRIB library    updated 11/07/2000
c fields are:
c   xmrg_process
c   wmoid
c   ngrid
c   nturef
c   itrang
c   ibinf
c   GRIB_parm
c   modlid
c   itunit
c   ntufc
c   iwidth
c   idec
c   ipkflg
      data numx / 30 /
      data xla/
     +'QPE01   QPE01  ZETA98 152 240 1  0  1 4 4 3 0 0               ',
     +'QPE06   QPE06  ZETA98 154 240 1  0  6 4 4 3 0 0               ',
     +'QPE24   QPE24  ZETA98 155 240 1  0 24 4 4 3 0 0               ',
     +'P1A01    APCP  ZETA98 156 240 1  0  1 4 4 3 0 0               ',
     +'P1M01    APCP  ZETA98 157 240 1  0  1 4 4 3 0 0               ',
     +'P2A01    APCP  ZETA98 158 240 1  0  1 4 4 3 0 0               ',
     +'P2M01    APCP  ZETA98 159 240 1  0  1 4 4 3 0 0               ',
     +'MPA01    APCP  ZETA98 160 240 1  0  1 4 4 3 0 0               ',
     +'MPM01    APCP  ZETA98 161 240 1  0  1 4 4 3 0 0               ',
     +'PA*      APCP  ZETA98 170 240 1 -1 -1 4 4 3 0 0               ',
     +'XN*      APCP  ZETA98 171 240 1 -1 -1 4 4 3 0 0               ',
     +'MM*      APCP  ZETA98 172 240 1 -1 -1 4 4 3 0 0               ',
     +'SATA*    APCP  ZETA98 190 240 1 -1 -1 4 4 3 0 0               ',
     +'SATM*    APCP  ZEGA98 191 240 1 -1 -1 4 4 3 0 0               ',
     +'QPA*     APCP  YEIG98 180 218 1 -1 -1 4 4 3 0 0               ',
     +'QPM*     APCP  YEIG98 180 218 1 -1 -1 4 4 3 0 0               '/
c
      data xlb/
     +'FFG*     FFG*  ZEGZ98 151 240 1  0 24 2 4 0 0 0               ',
     +'FFR*     FFR*  ZEGZ98 151 240 1  0 24 2 4 0 0 0               ',
     +'gridff*  FFG*  ZEGZ98 151 240 1  0 24 2 4 0 0 0               ',
     +'gridro*  FFR*  ZEGZ98 151 240 1  0 24 2 4 0 0 0               ',
     +'stage2   APCP  ZETA98 152 240 1  0  1 4 4 3 0 0               ',
     +'autoS3   APCP  ZETA98 154 240 1  0  1 4 4 3 0 0               ',
     +'manualS3 APCP  ZETA98 155 240 1  0  1 4 4 3 0 0               ',
     +'P1       APCP  ZETA98 156 240 1  0  1 4 4 3 0 0               ',
     +'rfcwide  APCP  ZETA98 160 240 1  0  1 4 4 3 0 0               ',
     + 5*
     +'NONE                                                          '/
c
c -1:-1:-1:qpfwmo
      data xqpfwmo /
     + '001:YEIG98          ',
     + '002:YEIM98          ',
     + '003:YEIN98          ',
     + '004:YEIO98          ',
     + '005:YEIP98          ',
     + '006:YEIQ98          ',
     + '007:YEIR98          ',
     + '008:YEIS98          ',
     + '009:YEIS88          ',
     + '010:YEIT98          ',
     + '011:YEIT88          ',
     + '012:YEIU98          ' /
c
      end

