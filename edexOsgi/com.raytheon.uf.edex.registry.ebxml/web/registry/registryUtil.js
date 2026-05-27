/*
This software was developed and / or modified by Raytheon Company,
pursuant to Contract DG133W-05-CQ-1067 with the US Government.

U.S. EXPORT CONTROLLED TECHNICAL DATA
This software product contains export-restricted data whose
export/transfer/disclosure is restricted by U.S. law. Dissemination
to non-U.S. persons whether in the United States or abroad requires
an export license or other authorization.

Contractor Name:        Raytheon Company
Contractor Address:     6825 Pine Street, Suite 340
                        Mail Stop B8  
                        Omaha, NE 68106
                        402.291.0100

See the AWIPS II Master Rights File ("Master Rights File.pdf") for 
further licensing information.

Navigation pane containing links

SOFTWARE HISTORY

Date         Ticket#     Engineer    Description
------------ ----------  ----------- --------------------------
10/15/2013   1682        bphillip    Initial implementation
5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
</pre>

@author bphillip
@version 1
*/
    
    function callRestService(url,arg){
        var url = "https://"+window.location.host+"/"+url
        if(arg != null){
            url+="/"+arg;
        }
        var client = new XMLHttpRequest();
        client.open("GET", url, false);
        client.setRequestHeader("Content-Type", "text/plain");
        client.send();
        return client.responseText
    }

    function callDataAccessServiceWithArg(func,arg){
        return callRestService("rest/dataAccess/"+func+"/"+arg);
    }

    function callDataAccessService(func){
        return callRestService("rest/dataAccess/"+func);
    }