/*********************************************************/
/* search for $ string in the buffer string              */
/* This is used for searching mods that have $ at the    */
/* begining of the line ( commneted out by user) for e.g */
/* $.RRICHNG                                             */
/* $ .30 .56                                             */
/* buf - length of the input buffer (length of the line) */
/* *******************************************************/
void chkdollarsign(int *iRet, char buf[100]) 
{

    char  tbuf[9];
    int   i, ntokens=0;

    memset(tbuf,'\0',9);
    strncpy(tbuf,buf,8);

    for(i=0;i<8;i++)
        if(strncmp(&tbuf[i],"$",1)==0)ntokens++; 
    *iRet = ntokens;
    return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/chkdollarsign.c,v $";
 static char rcs_id2[] = "$Id: chkdollarsign.c,v 1.1 2004/09/08 16:18:17 aivo Exp $";}
/*  ===================================================  */

}
