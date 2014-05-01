/* ----------------------------------------------------------------------------
** GetDef- get definition/default value for an Data variable
** ----------------------------------------------------------------------------
** history:
**
** 1.0 (11-92)		JTOstrowski - HRL	Original code as
**						"get_apps_defaults" program.
** 2.0 (5-20-93)	SAM, RTi		Rewrote code to work better as
**						a stand-alone program and
**						revised search list to work
**						with Data configuration files.
**						Also added the passed in
**						"afile" argument to add to the
**						search list to allow other
**						programs to specify file to
**						search.
** 01 Aug 1996		SAM, RTi		Simplify code to just be a
**						wrapper around
**						get_apps_defaults so that we
**						can get ESPADP utility routines
**						linked in in NWS environment.
**-----------------------------------------------------------------------------
**
**			It does so for checking for the variable name as an
**			environment variable and in defaults files:
**
**			1.	If an environment variable matching the name of
**				"request is found, "reply" is set to the value
**				of the environment variable.
**
**			2.	Next, "request" is searched for in a file that
**				establishes HMData resource defaults.  Two files
**				are scanned:  the $HOME/.hmdatarc file and
**				the $HMDATA/static/hmdefaults file.  The user's
**				defaults file has precedence.  The HMData file
**				will only be scanned if the variable HMDATA can
**				be resolved.  If "request" can be resolved,
**				"reply" is set to the value taken from the file.
**
**		(2)	If "request" cannot be resolved, "reply" is set to an
**			empty string and 1 is returned.  If "request" can be
**			resolved, 0 is returned.
**
**		(3)	Each file is scanned until the first match between
**			"request" and a defined token is found.  The file syntax
**			is:
**
**				<token> <delimiter> <resource>
**
**			where <token> is defined as a string delimited by white
**			space or <delimiter>, <delimiter> is the : (colon), and
**			<resource> is any string.  The following file
**			conventions are assumed:
**
**			-	a resource line must be contained on a one line
**			-	white space may or may not surround <delimiter>
**			-	comments are bracketed by # and a newline
**			-	comments cannot be inserted between <token> and
**				<resource> (only full lines or end of line)
**			-	<resource> can contain white space if it is
**				bounded by the ' or " characters
**			-	blank lines are allowed in the file
**			-	references to previously defined values are
**				indicated by $(...)
**			-	multiple references are allowed, but nested
**				references are not (no $($(...)))
**
**			Sample entries are:
**
**			#------------------------------------------------------
**			#  This is a comment line; so was previous line.
**			ofs_level       : testcase
**			ofs_inpt_grp    : "test case"
**			ofs_file_grp    : /home/$(ofs_level)/files
**			ofs_xxx:          xxx
**			ofs_yyy:yyy
**-----------------------------------------------------------------------------
** Variable	I/O	Description
**
** len_reply	L	Length of reply string.
** len_request	L	Length of request string.
** reply	O	Value if variable is found.
** request	I	Requested variable.
**-----------------------------------------------------------------------------
*/

#include "ResJ.h"
int GetDef ( char *request, char *reply )
{	int	len_request, len_reply;

	len_request = strlen ( request );
	get_apps_defaults ( request, &len_request, reply, &len_reply );
	if( !*reply )
		return STATUS_FAILURE;
	else	return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_utils/RCS/GetDef.c,v $";
 static char rcs_id2[] = "$Id: GetDef.c,v 1.1 1999/02/18 15:16:43 dws Exp $";}
/*  ===================================================  */

}
