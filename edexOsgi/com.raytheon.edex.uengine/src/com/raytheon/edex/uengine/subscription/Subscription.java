/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.edex.uengine.subscription;

import java.io.Serializable;
import com.raytheon.edex.uengine.tasks.query.TermQuery;

/**
 * Basic subscription that contains the script ID, the data URI, and whether
 * or not the subscription is active.  The data URI is typically a regular expression
 * of a data URI so it can be matched to multiple products.
 * 
 * JavaScript scripts need to call the setup method on their subscription for the
 * subscription to work correctly.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 5, 2007                      njensen             Initial Creation
 *
 * </PRE>
 *
 */
public class Subscription implements Serializable
{
    private static final long serialVersionUID = 1L;
    
    private boolean subscriptionActive = false;
    
    private String dataUri = "";
    
    private String scriptId = null;
    
    /**
     * Prepares the subscription based on the query.
     * @param aQuery
     */
    public void setup(TermQuery aQuery)
    {
        setup(null, aQuery);
    }
    
    /**
     * Prepares the subscription based on the values passed in.
     * @param aScriptId
     * @param aQuery
     */
    public void setup(String aScriptId, TermQuery aQuery)
    {
        dataUri = aQuery.getMatchURI();
        scriptId = aScriptId;
        subscriptionActive = true;
    }

    public String getDataUri()
    {
        return dataUri;
    }

    public void setDataUri(String aDataUri)
    {
        dataUri = aDataUri;
    }

    public String getScriptId()
    {
        return scriptId;
    }

    public void setScriptId(String aScriptId)
    {
        scriptId = aScriptId;
    }

    public boolean isSubscriptionActive()
    {
        return subscriptionActive;
    }

    public void setSubscriptionActive(boolean aSubscriptionActive)
    {
        subscriptionActive = aSubscriptionActive;
    }             
    

}
