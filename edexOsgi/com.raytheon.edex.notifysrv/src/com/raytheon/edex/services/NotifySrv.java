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

package com.raytheon.edex.services;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.subscription.ISubscriptionManager;
import com.raytheon.edex.subscription.Script;
import com.raytheon.edex.subscription.Subscription;
import com.raytheon.edex.subscription.SubscriptionManager;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Processes incoming data events to determine if the data matches an existing
 * product subscription. Receives the metadata representing a single data
 * record, satellite image, GRIB record, etc. Checks the subscription list to
 * determine if the data fits an existing product sbscription. If so, retrieves
 * the script(s) and forwards it to the Auto Product Builder.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 14Aug2006    #18         MW Fegan    Initial Creation.
 * 06Feb2007    TO5         MW Fegan    Removed JMX registration.
 * 27Apr2007    208         MW Fegan    Pass only dataURI in message.
 * 26Nov2007    443         bphillip    Modified to receive lists of PluginDataObjects
 * Jul 10, 2014 2914        garmendariz Remove EnvProperties
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

public class NotifySrv {

    private ISubscriptionManager subscriptionManager;

    private Log logger = LogFactory.getLog(getClass());

    /**
     * Constructor. Constructs the Notification Server object and registers with
     * the JMX server.
     */
    public NotifySrv() {
        subscriptionManager = new SubscriptionManager();
        /*
         * The following commented code is being retained in the event we want
         * to restore class based JMX management at a later time.
         */
        // String domain = this.getClass().getPackage().getName();
        // String name = this.getClass().getSimpleName();
        // register(rightShortenName(domain, 2), name);
        // logger.debug("NotifySrv Constructor Ran");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.util.AbstractMessageSrv#process()
     */
    protected Object process(List<PluginDataObject> list) throws EdexException {
        ArrayList<String> messages = null;

        // check for records to process before proceeding
        if (list.size() > 0) {
            messages = new ArrayList<String>();

            for (PluginDataObject record : list) {
                logger.debug("Notification for: [" + record + "]");
                /*
                 * Processing of the meta data...
                 */
                String dataURI = record.getIdentifier();

                if (subscriptionManager.isSubscribed(dataURI)) {
                    String subscriptionKey = subscriptionManager
                            .getSubscriptionKey(dataURI);
                    // messages = new ArrayList<String>();
                    logger.info("-subscription exists for "
                            + StringUtil.printString(dataURI));
                    Subscription subscription = subscriptionManager
                            .getSubscription(subscriptionKey);
                    for (Script script : subscription.getScripts()) {
                        if (script != null) {
                            String key = script.getScriptid();
                            if (key != null) {
                                messages.add(key);
                            }
                        }
                    }
                    logger.info("Created [" + messages.size()
                            + "] Notification Messages");
                }
            }

            // execTime += System.currentTimeMillis() - start;
            // logger.debug("Notification message processed in "
            // + (execTime / 1000f) + "s");
            // execCount++;

            if (messages.size() == 0) {
                messages = null;
            }
        }
        return messages;

    }

    /**
     * @return the subscriptionManager
     */
    public ISubscriptionManager getSubscriptionManager() {
        return subscriptionManager;
    }

    /**
     * @param subscriptionManager
     *            the subscriptionManager to set
     */
    public void setSubscriptionManager(ISubscriptionManager subscriptionManager) {
        this.subscriptionManager = subscriptionManager;
    }

}
