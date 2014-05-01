      SUBROUTINE plot_indices(stabmap, printsw, soundfile, soundtime,
     +           stnfilter,dev)
C***************************************************************
C* plot_indices
C*
C* This subroutine renders plan-view displays of indices
C* computed from nsharp_awc
C*
C* plot_indices(stabmap, printsw, soundfile, soundtime, stnfilter, dev)
C* Input parameters:
C*   stabmap CHAR*  - Values "Map1", "Map2" for two types of
C*     stability maps.
C*     Map1 - Displays these parameters:
C*       STID, LFC, LCL, CAPE, CIN, EL, Sweat
C*     Map2 - Displays these parameters:
C*       STID LIFT, K-Index, TOTL, PWAT, CAPE, BRCH
C*     Map3 - Displays these parameters:
C*       STID LIFT, K-Index, TOTL, CIN, CAPE, BRCH
C*
C*     The station model described in snmap.hlp is used
C*     Positions 1,2,4,6,7,8,9 are now being used.

C*   printsw CHAR*  - Values "display, "print".  Set the display
C*       graphics output mode to display or print.
C*
C*   soundfile - Name of sounding file
C*   soundtime - Date/Time of sounding, used for GDATTIM
C*   dev - PS device with specs of whether to use 17x11 or 8.5x11 paper
C* Adapted after SNMAP
C*
C* Author: Larry J. Hinson, AWC  July 2004
C*         lhinson 06/15/2005
C*         lhinson 05/24/2006 - Updated to add Map 3
C*****************************************************************
      INCLUDE 'GEMPRM.PRM'
      CHARACTER *(*) stabmap
      CHARACTER *(*) printsw
      CHARACTER *(*) soundfile
      CHARACTER *(*) soundtime
      CHARACTER *(*) stnfilter
      CHARACTER *(*) dev
      CHARACTER snfile*(LLMXLN), area*(LLMXLN), garea*(LLMXLN),
     +                  prminp*(LLMXLN), dattim*(LLMXLN),
     +                  colors*(LLMXLN), map*(LLMXLN), title*(LLMXLN),
     +                  device*(LLMXLN), proj*(LLMXLN), panel*(LLMXLN),
     +                  text*(LLMXLN), levels*(LLMXLN), vcoord*(LLMXLN),
     +                  latlon*(LLMXLN), filter*(LLMXLN),
     +                  shrttl*(LLMXLN), satfil*(LLMXLN),
     +                  radfil*(LLMXLN), lutfil*(LLMXLN),
     +                  stnplt*(LLMXLN), imcbar*(LLMXLN)
      LOGICAL           clear
C*
      CHARACTER snfcur*72, arecur*48, datcur*48, voutc*4
      CHARACTER         pmdset (MMPARM)*4, params*72, colrs*72
      CHARACTER parms (MMPARM)*4, times (LLMXTM)*20
      CHARACTER prcons (MMPARM)*16, chd (MMPARM)*8
      CHARACTER tstn*8, sta*8, ttlstr*80, ttt*72
      CHARACTER area1*48, area2*48, ttlinp*72, filnam*72
      CHARACTER imgfls(MXLOOP)*132, uprj*72, shrtin*72
      INTEGER           icolor (MMPARM), iscale (MMPARM)
      LOGICAL           respnd, done, proces, newfil, chrflg (MMPARM)
      LOGICAL           more, morlev, plot, wndflg
      REAL              offset (4), sxplt (LLSTFL), outd (MMPARM)
      REAL              syplt (LLSTFL), data (LLMXDT), rlevel (LLMXLV)
      REAL          filtfc
      dimension results(200)

      LOGICAL           first
      uprj = ''
      area = "us-"
      garea = '22.7;-117.5;47.7;-63.0'
      satfil = ''
      radfil=''
      imcbar=''
      dattim=soundtime
      levels='500'
      vcoord='pres'
      snfile=soundfile
      map='1/1'
      latlon=''
      title='1'
      clear=.true.
      panel='0'
      proj='str/90;-100;0/0;2;0;0'
      filter=stnfilter
      text='1'
      lutfil=''
      stnplt=''
C         device="ps|/tmp/planstnplot.ps|11.0;8.5|M"
      device=dev
      IF (stabmap .EQ. "map1") THEN
        prminp='stid;LFCT;SPAC;CAPE;SPAC;LCLT;CINS;EQLV;LIFT'
        colors='18;2;5;15;3;7;26'
        title='1/0/'//soundtime//
     +' LFC(hft AGL)  CAPE  LCL(hft AGL) CIN  EL(hft AGL)  SWEAT'

      ELSE IF (stabmap .EQ."map2") THEN
        prminp='stid;LIFT;SPAC;KINX;SPAC;TOTL;PWAT;CAPE;BRCH;SPAC;SPAC'
        colors='18;2;5;15;3;7;26'
        title='1/0/'//soundtime//' LIFT KINX TOTL PWAT CAPE BRCH'

      ELSE IF (stabmap .EQ."map3") THEN
        prminp='stid;LIFT;SPAC;KINX;SPAC;TOTL;CINS;CAPE;BRCH;SPAC;SPAC'
        colors='18;2;5;15;3;7;26'
        title='1/0/'//soundtime//' LIFT KINX TOTL CIN CAPE BRCH'
      ELSE IF (stabmap .EQ. "map4") THEN
        prminp='stid;CAPE;SPAC;CAPE;SPAC;CINS;CINS;PWAT;DWPT'
        colors='18;2;5;15;3;7;26'
        title='1/0/'//soundtime//
     +        ' MUCAPE MLCAPE MUCIN MLCIN PWAT(in*100) MLDWPT(F)'
      ENDIF

      proces = .true.

      CALL IN_BDTA (ier)
      IF ( PRINTSW .EQ. "print") THEN
        CALL GG_SDEV  ( device, iret )
            colors='1'
      ENDIF
C
C*              Set text.
C
C               CALL IN_TEXT ( text, ier )

      CALL GSMODE (2, ier)
      CALL IN_TEXT (text, iret)

      IF  ( iret .eq. 0 )  THEN
              CALL GSMFIL ('mepowo.gsf',ier)
              CALL GSMFIL ('hipowo.gsf',ier)
              CALL GG_MAPS  ( proj, garea, imgfls (1), idrpfl, iret )
      ELSE
              proces = .false.
      END IF
C
C*              Process filename, winds, and title.
C
      IF ( iret .eq. 0 ) THEN
         CALL FL_MFIL ( snfile, ' ', filnam, iret )
         IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret,
     +' ', ier )
         snfcur = ' '
         CALL SNMFIL ( filnam, snfcur, iflno, newfil, pmdset,
     +                            npmdst, ivert, iret )
      END IF
      IF  ( iret .ne. 0 )  proces = .false.


      CALL IN_FILT ( filter, filtfc, ier )
C
      CALL TB_PARM ( prminp, params, colrs, iret )
      IF ( iret .lt. 0 ) THEN
         CALL ER_WMSG ( 'TB', iret, ' ', ier )
         proces = .false.
      ELSE IF ( iret .eq. 2 ) THEN
         params = prminp
         colrs  = colors
      ELSE
         IF ( colors .ne. ' ' ) colrs = colors
      END IF
      IF  ( proces )  THEN
          CALL SNMLEV  ( iflno, levels, vcoord, ivert, nlev,
     +                         rlevel, voutc, lvert, iret )
          IF  ( ( iret .ne. 0 ) .or. ( nlev .eq. 0 ) )
     +                  proces = .false.
      END IF
C
      IF  ( proces )  THEN
C
C*              Process parameter names.
C
        CALL SNMPRM ( newfil, params, pmdset, npmdst,
     +                        parms, chrflg, ncprm,
     +                        prcons, wndflg, iret )
C
C*              Determine whether any data will be plotted.
C
        IF ( ncprm .eq. 0 ) THEN
             plot = .false.
        ELSE
             plot = .true.
        END IF
        IF  (  ncprm .gt. 0  )
     +    CALL SNMCLR (ncprm, parms, colrs, icolor, ier)
      END IF
C
C*          Get offsets for filtering
C
      IF  ( ( filtfc .ne. 0. ) .and. plot .and. proces )
     +           CALL SNMCOF ( ncprm, parms, wndflg,
     +                         filtfc, offset, ier )
C
C*          Set area and get times to be processed.
C
      IF  ( proces )  THEN
        ipos2 = INDEX ( area, '/' )
        IF ( area (1:1) .eq. '@' .and. ( ipos2 .gt. 4 ) ) THEN
                area1 = area ( :ipos2-1 )
                area2 = area ( ipos2+1: )
                iloop = 1
        ELSE
                area1 = area
                iloop = 2
        END IF
        CALL LC_UARE  ( area1, newfil, iflno, arecur, tstn,
     +                          ier )
        IF  ( ier .ne. 0 )  proces = .false.
C*
        CALL SNMDAT  ( dattim, iflno, newfil, datcur, ntime,
     +                         times, ier )
        IF  ( ier .ne. 0 )  proces = .false.
      END IF
C
C*            Begin processing if inputs are ok.
C
      IF  ( proces )  THEN
C
C*            For projection = SAT or RAD, make sure we only display as
C*            many times as we have images for.
C
        IF ( uprj (1:3) .eq. 'SAT' .or.
     +         uprj (1:3) .eq. 'RAD' )
     +         ntime = MIN ( ntime, numimg )
C
C*            Loop over times.
C
        ipass = 1
        itime = 1
        more  = .true.
C
        DO WHILE  ( more )
C
C*              Set the projection, garea for SAT (for each plot)
C
          IF ( uprj (1:3) .eq. 'SAT' .or.
     +                  uprj (1:3) .eq. 'RAD' ) THEN
                     CALL GG_MAPS  ( proj, garea, imgfls (itime),
     +                              idrpfl, iret )
                     IF (iret .ne. 0) more = .false.
          END IF
C
C*              Set the current pixmap.
C*              If this is the first time, go to the first pixmap.
C*              If it is not the first time, go to the next pixmap.
C
          IF ( more ) THEN
            IF  ( itime .eq. 1 )  THEN
              first = .true.
              CALL GSTANM ( iret )
            ELSE
              first = .false.
              CALL GSPLOT ( iret )
            END IF
          END IF

          nplot = 0
          CALL SN_STIM  ( iflno, times (itime), ier )
C
C*          Loop over levels.
C
          ilevl  = 1
          morlev = .true.
          DO WHILE  ( morlev )
            CALL SN_BEGS  ( iflno, ier )
            vlevel = rlevel ( ilevl )
            iopt=0
            ipass = ipass + 1
            IF  ( iopt .lt. 0 )  THEN
              more   = .false.
              morlev = .false.
            END IF
C
C*                Process clear, define panel, set up
C*                filtering and draw map.
C
            IF  ( more )  THEN
              IF  ( clear )  CALL GCLEAR  ( iret )
C
C*                  Set the panel
C
              CALL GG_PANL  ( panel, ier )
C
C*                  Apply LUT file
C
              IF ( itime .eq. 1 ) CALL IM_LUTF ( lutfil, ier )
C
C*                  Draw map, lat/lon lines, and station ID/marker.
C
              CALL GG_MAP  ( map, ier )
              CALL GG_LTLN ( latlon, ier )
              CALL GG_SPLT ( stnplt, ier )

C*                    Flush the graphics buffer.
C

C*                  Set up filtering.
C
C*                  Set up filtering.
C
              IF  ( ( filtfc .ne. 0. ) .and. plot )  THEN
                DO  m = 1, LLSTFL
                  sxplt (m) = RMISSD
                  syplt (m) = RMISSD
                END DO
              END IF
C
C*                  For special plotting, change the area on the
C*                  second time through.
C
              DO  lll = iloop, 2
                IF (( lll .eq. 2 ) .and. ( iloop .eq. 1 )) THEN
                  CALL LC_UARE ( area2, newfil, iflno,
     +                           arecur, tstn, ier )
                  IF  ( ier .ne. 0 ) plot = .false.
                END IF
C
C*                    Station loop.
C
                iout = 0
                DO WHILE  ( plot .and. ( iout .eq. 0 ))
                  CALL SN_SNXT ( iflno, sta, id, slat,
     +                                 slon, selv, iout )
                  IF  ( iout .eq. 0 )  THEN
C
C*                        Get the data.
C
                    CALL SN_RDAT  ( iflno, numlev, data,
     +                     ihhmm, ier )
C
C*                        Filter, first parm filter and
C*                        second sta filter, if requested.
C
                    IF  ( ier .eq. 0 )  THEN
                      ispri = 0
                      CALL PC_SSTN ( sta, id, slat, slon, selv,
     +                       ispri, ihhmm, numlev, ier )
                      CALL PC_CMVS ( vlevel, lvert, data,
     +                       outd, chd, ier )
                      IF  ( ier .eq. 0 )  THEN
                        CALL GTRANS ( 'M', 'P', 1, slat,
     +                                 slon, sx, sy, ier2 )
                      END IF
                    END IF
C*
                    IF  ( ( ier .eq. 0 ) .and. (filtfc .ne. 0.)
     +                             .and. ( lll .eq. 2 ) ) THEN
                      CALL SNMOVR  ( sx, sy, sxplt, syplt,
     +                                       nplot, offset, ier )
                    END IF
                    IF ( ier .eq. 0 .and. filtfc .ne. 0. ) THEN
C
C*                           Save x/y for no overlap.
C
                        nplot = nplot + 1
                        sxplt (nplot) = sx
                        syplt (nplot) = sy
                    END IF
C
C*                        Plot if we are ok to here.
C
                    IF  ( ier .eq. 0 )  THEN
                      CALL calc_vals(numlev, data, results)
C
C*                    Grouping the station model as group type 10.
C
                      igroup = 10
                      CALL GSGRP (igroup, iret )
                      IF (stabmap .eq. "map1") THEN
                        outd(1)=-9999.
                        IF (results(8)>0) THEN
                          outd(2)=INT(results(8)/100.0+.5);
                        ENDIF
                        outd(3)=-9999.
                        outd(4)=results(5)
                        outd(5)=-9999.
                        IF (results(7)>0) THEN
                          outd(6)=INT(results(7)/100.0+.5);
                        ENDIF
                        outd(7)=results(10)
                        IF (results(9)>0) THEN
                          outd(8)=INT(results(9)/100.0+.5);
                        ENDIF
                        outd(9)=results(11)
                      ELSE IF (stabmap .eq. "map2") THEN
                        outd(1)=-9999.
                        outd(2)=results(1)
                        outd(3)=-9999.
                        outd(4)=results(2)
                        outd(5)=-9999.
                        outd(6)=results(3)
                        outd(7)=results(4)*100
                        outd(8)=results(5)
                        outd(9)=results(6)
                      ELSE IF (stabmap .eq. "map3") THEN
                        outd(1)=-9999.
                        outd(2)=results(1)
                        outd(3)=-9999.
                        outd(4)=results(2)
                        outd(5)=-9999.
                        outd(6)=results(3)
                        outd(7)=results(10)
                        outd(8)=results(5)
                        outd(9)=results(6)
                      ELSE IF (stabmap .eq. "map4") THEN
                        outd(1)=-9999.
                        outd(2)=results(5)
                        outd(3)=-9999.
                        outd(4)=results(12)
                        outd(5)=-9999.
                        outd(6)=results(10)
                        outd(7)=results(13)
                        outd(8)=results(4)*100
                        outd(9)=results(14)
                      ENDIF
                      CALL SNMPLT ( icolor, parms, sx, sy, slat,
     +                              slon, chrflg, ncprm, outd,
     +                              chd, ier )
                      CALL GEGRP ( iret )
                    END IF
                  END IF
                END DO
C
C*                    Create and draw the title.
C
                ipbar = INDEX ( title, '|' )
                IF  ( ipbar .ne. 0 )  THEN
                  shrtin = title ( ipbar+1: )
                  IF  ( ipbar .eq. 1 )  THEN
                  ttlinp = ' '
                  ELSE
                  ttlinp = title ( :ipbar-1 )
                  END IF
                ELSE
                  shrtin = ' '
                  ttlinp = title
                END IF
C
C*                    Create and draw the title.
C
                CALL IN_TITL ( title, -3, ititl, linttl,
     +                               ttlstr, ier )
                DO ii = 1, ncprm
                  iscale (ii) = 0
                END DO
                ilvl = NINT ( vlevel )
                IF  ( ititl .gt. 0 )  THEN
                  CALL GR_MTTL  ( ttlstr, '^ @ _', .false.,
     +                                  times (itime), ' ', .true.,
     +                                  ilvl, -1, lvert, ncprm,
     +                                  prcons, iscale, ' ', ttt, ier )
                  CALL GSCOLR  ( ititl, ier )
                  CALL GG_WSTR ( ttt, linttl, ier )
                END IF
C
C*                    Create the short title string.
C
                IF  ( clear )  THEN
                  CALL GR_MTTL  ( ttlstr, 'UPPER_AIR ^ @ #',
     +                       .true., times (itime), ' ',
     +                       .true., ilvl, -1, lvert,
     +                       ncprm, prcons, iscale, area,
     +                       shrttl, ier )
                  CALL GMESG ( shrttl, ier )
                END IF
              END DO
            END IF
            ilevl = ilevl + 1
            IF  ( ilevl .gt. nlev )  morlev = .false.
          END DO
          itime = itime + 1
          IF  ( itime .gt. ntime )  more = .false.
        END DO
      END IF
C
C*                  Flush the graphics buffer.
C
      CALL GEPLOT  ( iret )
      RETURN
      END
