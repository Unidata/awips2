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

import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.SubscriptionRecord;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.edex.plugin.text.dao.SubscriptionDAO;
import com.raytheon.uf.edex.plugin.text.subscription.util.Tools;

/**
 * Implements a subscription request runner that performs an insert into the
 * subscription database tables. It expects an Message object containing the
 * subscription insert request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * 14Sep2010    3944       cjeanbap    Trim property values.
 * 25May2011    8686       cjeanbap    Updated if-statement to check for filepath
 * 26May2011    8686       cjeanbap    fixed a punctuation bug    
 * May 22, 2014 2536       bclement    moved from autobldsrv to edex.plugin.text
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class SubscribeAddRunner extends ASubscribeRunner {

    private static final String TRIGGER = "trigger";
    
    private static final String FILEPATH = "filepath";
    
    /**
     * Constructor.
     */
    public SubscribeAddRunner() {
        super();
    }

    /**
     * Constructor. The resulting object has the subscription message set.
     * 
     * @param message
     *            the subscription message to process
     */
    public SubscribeAddRunner(Message message) {
        super(message);
    }

    @Override
    public boolean execute() {
        SubscriptionRecord rec = new SubscriptionRecord();
        Property[] properties = this.message.getHeader().getProperties();
        List<Property> props = Tools.adjustMessageProperties(properties);

        for (Property prop : props) {
            if (TRIGGER.equalsIgnoreCase(prop.getName()) || FILEPATH.equalsIgnoreCase(prop.getName())) {
                if (prop.getValue().startsWith("[") && prop.getValue().endsWith("]")) {
                    int len = prop.getValue().length();
                    prop.setValue(prop.getValue().substring(1, len-1));
                }
                prop.setValue(prop.getValue().replace(",", " "));
                prop.setValue(prop.getValue().replace("'", ""));
            }
            rec.setAttribute(prop.getName(), prop.getValue().trim());
        }
        rec.setActive(true);
        SubscriptionDAO dao = new SubscriptionDAO();
        boolean success = false;
        try {
            success = dao.write(rec);
        } catch (Exception e) {
            String msg = "Unable to perform database insert";
            logger.error(msg, e);
            msg += " due to " + e.toString();
            this.results.add(new Property(RESPONSE_ERROR, msg));
            return false;
        }
        if (success) {
            this.results.add(new Property(RESPONSE_NORMAL,
                    "Database insert was successful."));
        } else {
            this.results
                    .add(new Property(RESPONSE_NORMAL,
                            "Insert not performed: Subscription record already exists."));
        }
        return success;
    }

}
