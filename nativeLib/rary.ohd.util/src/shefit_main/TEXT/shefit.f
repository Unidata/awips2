C  =====================================================================
C  pgm: SHEFIT .. Mainline for UNIX shefpars filter program
C
C  use:     shef-msg | SHEFIT > stdout    or    SHEFIT in [out] [err]
C
C   in: (stdin) ....... shef messages as text input lines - CHAR*128
C   in: (file) ........ SHEFPARM file in the current dir; if not found
C   in:                 get it from the apps-defaults dir defined by
C   in:                 token "rfs_sys_dir"
C   in: (unit number) . stderr unit in apps-defaults "fortran_stderr",
C   in:                 else unit 7 is used (HPUX)
C  out: (stdout) ...... shef messages decoded into a text str - CHAR*80
C  out: (stderr) ...... error messages (apps-defaults "fortran_stderr")
C   in: in ............ alternately, input shef msg file path - CHAR*128
C   in: out ........... (optional) if the input file is given, then
C   in:                 the output file pathname may be given - CHAR*128
C   in: out ........... (optional) if both input and output paths are
C   in:                 given, the error file may be given -- CHAR*128
C
C  rqd: SHDRIV,SHOPIT,SHCLIT
C
C  cmt: One of the following options can be given as the first argument:
C  cmt:     -1 ... Text output as one long line, full quotes (default)
C  cmt:     -2 ... Text output as two lines, quotes limited to 66 chars
C  cmt:     -b ... Binary output if output is to a file
C  cmt:     -h ... Output a help message on how to run shefit (stdout)
C
C  cmt: Routines that can be adjusted for desired output:
C  cmt:   shefit
C  cmt:   shout
C
C  cmt: Routines that are utilities:
C  cmt:   shclit    shfmot    shopit
C  cmt:   shclwr    shhelp    shclbl
C  cmt:   shopfl
C  =====================================================================
      SUBROUTINE SHEFIT_MAIN

      EXTERNAL       SHDRIV,SHOPIT,SHCLIT

      INTEGER        LUIN,LUOT,LUSH,LUER,LUCP

C                   Open files
C                   Call parsing program, output display file
C                   Close files

        CALL SHOPIT(LUIN,LUOT,LUSH,LUER,LUCP)
        CALL SHDRIV(LUIN,LUOT,LUSH,LUER,LUCP)
        CALL SHCLIT(LUIN,LUOT,LUSH,LUER,LUCP)

      END
