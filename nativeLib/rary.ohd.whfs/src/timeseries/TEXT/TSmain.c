
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/RepType.h>
#include <Xm/DialogS.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>

#include "DbmsAccess.h"
#include "GeneralUtil.h"
#include "TSControl_show.h"
#include "Xtools.h"

#include "tsgen_info.h"

XtAppContext app;

int timeseries_main (int argc, const char ** argv)
{
	extern	char *optarg;

	char	bin_dir[128],
		systr[200];

	int	gad_token_len=0, gad_value_len=0;

	Arg 	al[10];
	Widget  toplevel, label;

	TSGEN_INFO  tsgen;

	int     c,
		ac,
		dbstatus,
		ok = 0;

	while ((c = getopt(argc, argv, "d:l:t:h:n:i:s:")) != -1)
	{
		 switch (c)
		 {
			case 'd':
			ok = 1;
			if ((dbstatus = OpenDbms(optarg)) != 0)
			{
				gad_token_len = strlen("whfs_bin_dir");
				get_apps_defaults("whfs_bin_dir", &gad_token_len, bin_dir, &gad_value_len);
				if ( strlen(bin_dir) <= 0)
				{
					fprintf(stderr, "whfs_bin_dir undefined.  Edit not performed.\n");
					exit (  0 );
				}
				sprintf(systr, "%s/x_showerr.LX -t\"TimeSeries Abort\" -l\"%s\"",
				bin_dir, "Error in OpenDbms()....");
				system(systr);
				exit (1);
			}
		}

	}

	if ( !ok )
	{

			printf("Error arguments not found .....\n");
			exit (-1);
	}


	XtSetLanguageProc ( (XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL );

	toplevel = XtVaAppInitialize(&app,"timeseries_RSRC",
				 NULL,0,&argc,argv,NULL,
				 XmNtitle,  "TimeSeries",
				 NULL);

	ac = 0;
	label = XmCreateLabel(toplevel, "TimeSeries Starts", al, ac );
	XtManageChild(label);

	XtRealizeWidget(toplevel);
	SetCursor(toplevel, XC_watch);;
	XtVaSetValues(toplevel, XmNx, 5,
		                XmNy, 100,
			        NULL);

	sprintf(tsgen.lid,"%s"," ");
	tsgen.pedtse_defined = 0;
	tsgen.group_check    = 0;
	tsgen.standalone     = STANDALONE;
	tsgen.nitems         = 0;

	show_TSControlDS ( toplevel, tsgen);

	UnsetCursor(toplevel);

	XtAppMainLoop (app);
	CloseDbms();

        return 0 ;

}
