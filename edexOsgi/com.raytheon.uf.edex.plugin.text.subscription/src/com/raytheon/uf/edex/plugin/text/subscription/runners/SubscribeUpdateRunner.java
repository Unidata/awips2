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

import com.raytheon.uf.common.dataplugin.text.subscription.db.SubscriptionRecord;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.edex.plugin.text.subscription.dao.SubscriptionDAO;
import com.raytheon.uf.edex.plugin.text.subscription.util.Tools;

/**
 * Implements a subscription request runner that performs an update of the data
 * in the subscription table.
 * <p>
 * Typical Usage:
 * 
 * <PRE>
 * <CODE>
 *     List<Property> results = null;
 *     SubscribeAction action = SubscribeAction.ACTION_UPDATE;
 *     ISubscribeRunner runner = RunnerFactory.getInstance().getWorker(action);
 *     if (runner != null) {
 *        runner.initialize(message);
 *        runner.execute();
 *        results = runner.getResults();
 *     }
 * </CODE>
 * </PRE>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05Dec2008    1709       MW Fegan    Initial creation.
 * May 22, 2014 2536       bclement    moved from autobldsrv to edex.plugin.text
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.subscription
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class SubscribeUpdateRunner extends ASubscribeRunner {

    /**
     * 
     */
    public SubscribeUpdateRunner() {
        super();
    }

    /**
     * @param message
     */
    public SubscribeUpdateRunner(Message message) {
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
        String msg = "";
        Header header = this.message.getHeader();
        Property[] props = header.getProperties();
        List<Property> attribs = Tools.adjustMessageProperties(props);

        List<Property> index = new ArrayList<Property>();
        String indx = "";
        for (Property prop : attribs) {
            if ("id".equalsIgnoreCase(prop.getName())) {
                indx = prop.getValue();
                index.add(prop);
                attribs.remove(prop);
                break;
            }
        }
        List<SubscriptionRecord> records = null;
        SubscriptionDAO dao = new SubscriptionDAO();
        if (index.size() == 0) {
            msg = "Unable to perform update - no record ID specified";
            logger.warn(msg);
            this.results.add(new Property(RESPONSE_ERROR, msg));
            return false;
        } else {
            records = dao.getSubscriptions(index);
        }
        // get the appropriate record
        if (records.size() == 0) {
            // this indicates the record wasn't located - need to do something.
            msg = String
                    .format("Unable to perform update - no subscriptions with ID [%s] available",
                            indx);
            logger.warn(msg);
            this.results.add(new Property(RESPONSE_NORMAL, msg));
            return true;
        }
        // update the record
        boolean upd = false;
        for (SubscriptionRecord record : records) {
            for (Property prop : attribs) {
                if ("update".equalsIgnoreCase(prop.getName())) {
                    String[] parts = prop.getValue().split(":", 2);
                    record.setAttribute(parts[0], parts[1]);
                    upd = true;
                }
            }
        }
        // update the database
        if (upd) {
            for (SubscriptionRecord record : records) {
                dao.update(record);
            }
        }
        msg = String.format("Updated %d subscription records.", records.size());
        logger.info(msg);
        this.results.add(new Property(RESPONSE_NORMAL, msg));
        return true;
    }

}
