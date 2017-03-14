/* Routine to display a diagnostic message window.

*/

#include <string.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/ScrollBar.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>

void msg_window (Widget widget, char *msg)
{
static  Widget  msgBox;
Arg             arg[4];
int             iarg;


   if ( ! msgBox ) {
      iarg = 0;
      XtSetArg(arg[iarg], XmNtitle, " "); iarg++;
      XtSetArg(arg[iarg], XmNdialogType, XmDIALOG_ERROR); iarg++;
      XtSetArg(arg[iarg], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); iarg++;
      msgBox = XmCreateMessageDialog(widget, "msgBox", arg, iarg);
      XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
      XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));
      }

   iarg = 0;
   XtSetArg(arg[iarg], XmNmessageString,
            XmStringCreateLtoR(msg,
            (XmStringCharSet)XmFONTLIST_DEFAULT_TAG)); iarg++;
   XtSetValues(msgBox, arg, iarg);

   XtManageChild(msgBox);
   XtPopup(XtParent(msgBox), XtGrabNone);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/msg_window.c,v $";
 static char rcs_id2[] = "$Id: msg_window.c,v 1.1 2000/02/04 16:36:12 dws Exp $";}
/*  ===================================================  */

}
