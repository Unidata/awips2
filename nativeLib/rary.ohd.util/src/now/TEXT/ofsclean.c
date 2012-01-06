#include <stdio.h>
#include <stdlib.h>

extern int get_apps_defaults(char *request, int *request_len, char *reply, int *reply_len);

void ofsclean(int *stat)
{
    char procid[8];
    char hostid[12];
    char suffix[24];
    int i;
    
    /* APPS-DEFAULTS variables. */
    int  len1, len2;
    char token[256];
    char result[256];
    
    /* STORES DIRECTORIES */
    char homedir[256];
    char fs5filesdir[256];
    
    /* STORES THE SYSTEM CALL */
    char systemcall[2048];
    
    *stat = 0;
    
/*printf("CLEANING UP.........................\n");*/
    
    /* 
    * Acquire the process id and host id.
    */
    sprintf(procid, "%5d", getpid());
    sprintf(hostid, "%10d", gethostid());
    
/*printf("  pid = %s; hostid = %s.\n", procid, hostid);*/

    /*
    * Examine the first couple characters in both strings.  If they are ' ',
    * then make them '0'.
    */
    for (i = 0; i < strlen(procid); i ++)
        if (procid[i] == ' ')
            procid[i] = '0';
    for (i = 0; i < strlen(hostid); i ++)
        if (hostid[i] == ' ')
            hostid[i] = '0';
    
    /*
    * Build the suffix.
    */
    sprintf(suffix, "%s.%s", procid, hostid);
    
/*printf("  suffix = %s.\n", suffix);*/
    
    /*
    * Load the user's home dir.
    */
    strcpy(token, "HOME"); 
    len1 = strlen(token);
    if ( get_apps_defaults(token, &len1, result, &len2) == 1 )
    {
        *stat = 1;
        return;
    }
    else
    {
        result[len2] = '\0';
        strcpy(homedir, result);
    }
    
    
    /*
    * Load the ofs_fs5files dir.
    */
    strcpy(token, "ofs_fs5files"); 
    len1 = strlen(token);
    if ( get_apps_defaults(token, &len1, result, &len2) == 1 )
    {
        *stat = 1;
        return;
    }
    else
    {
        result[len2] = '\0';
        strcpy(fs5filesdir, result);
    }
    
/*printf("  homedir = %s.\n", homedir);
printf("  fs5files = %s.\n", fs5filesdir);*/

    /* 
    * Build the system call.
    */
/*    sprintf(systemcall, "rm -f /tmp/TEMP.*.%s; rm -f %s/TEMP.*.%s; rm -f %s/.resj.*.%s",
        suffix, homedir, suffix, homedir, suffix);*/
    sprintf(systemcall, "rm -f /tmp/TEMP.*.%s; rm -f /tmp/resj_tmp_file.*.%s",
        suffix, suffix);
        
/*printf("  systemc all = '%s'.\n", systemcall);*/
        
    /*
    * Do the system call, and discard the results of the call.
    */
    system(systemcall);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/ofsclean.c,v $";
 static char rcs_id2[] = "$Id: ofsclean.c,v 1.3 2004/05/26 13:24:55 hank Exp $";}
/*  ===================================================  */

}





