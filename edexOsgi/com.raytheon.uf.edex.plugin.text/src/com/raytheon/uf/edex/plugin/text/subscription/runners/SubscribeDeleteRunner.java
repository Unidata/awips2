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
package com.raytheon.uf.edex.plugin.text.subscription.runners;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.SubscriptionRecord;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.edex.plugin.text.dao.SubscriptionDAO;
import com.raytheon.uf.edex.plugin.text.subscription.util.Tools;

/**
 * Implements a subscription request runner that performs a delete operation in
 * the subscription database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * May 22, 2014 2536       bclement    moved from autobldsrv to edex.plugin.text
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class SubscribeDeleteRunner extends ASubscribeRunner {

    /**
     * 
     */
    public SubscribeDeleteRunner() {
        super();
    }

    /**
     * @param message
     */
    public SubscribeDeleteRunner(Message message) {
        super(message);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.text.subscription.runners.ASubscribeRunner
     * #execute()
     */
    @Override
    public boolean execute() {
        // get a list of matching subscriptions
        Property[] props = this.message.getHeader().getProperties();
        List<Property> attribs = Tools.adjustMessageProperties(props);
        SubscriptionDAO dao = new SubscriptionDAO();
        List<SubscriptionRecord> retVal = null;
        if (attribs.size() > 0) {
            retVal = dao.getSubscriptions(attribs);
        } else {
            retVal = dao.getSubscriptions();
        }
        if (retVal.size() == 0) {
            this.results = prepareNullResponse();
        } else {
            int count = 0;
            for (SubscriptionRecord rec : retVal) {
                dao.delete(rec);
                count++;
            }
            this.results = prepareDeleteResponse(count);
        }
        return true;
    }
    /**
     * Creates a results list for a "no matching records" response.
     * 
     * @return the response list
     */
    private List<Property> prepareNullResponse() {
        String msg = "There are no subscriptions matching the criteria";
        Property resp = new Property(RESPONSE_NORMAL,msg);
        List<Property> retVal = new ArrayList<Property>();
        retVal.add(resp);
        return retVal;
    }
    /**
     * Creates a response list providing the number of records deleted.
     * @param count the number of records that were deleted
     * @return the response list
     */
    private List<Property> prepareDeleteResponse(int count) {
        String msg = String.format("Successfully deleted %d subscriptions matching the criteria",count);
        Property resp = new Property(RESPONSE_NORMAL,msg);
        List<Property> retVal = new ArrayList<Property>();
        retVal.add(resp);
        return retVal;        
    }
}
