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
package com.raytheon.edex.subscription.runners;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.subscription.dao.SubscriptionDAO;
import com.raytheon.edex.subscription.data.SubscriptionRecord;
import com.raytheon.edex.subscription.util.Tools;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Implements a subscription request runner that performs a query to determine
 * if there are currently any subscriptions for a specific trigger value. If there
 * are subscriptions, they are placed into the response list.
 * <P>
 * This runner expects a message similar to 
 * <PRE><CODE>
 *     <message>
 *        <header>
 *           <properties name="operation" value="query" />
 *           <properties name="type" value=" ... " />
 *           <properties name="trigger" value=" ... " />
 *        </header>
 *     </message>
 * </CODE></PRE>
 * The response following execution is a list of Property objects that can be used
 * to form a Message similar to
 * <PRE><CODE>
 *     <message>
 *        <header>
 *           <properties name="count" value=" ... " />
 *           <properties name="subscription" value=" ... " />
 *           <properties name="subscription" value=" ... " />
 *        </header>
 *     </message>
 * </CODE></PRE>
 * In the response, the value of each subscription property is the XML serialization
 * of the subscription from the database. 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class SubscribeQueryRunner extends ASubscribeRunner {
    private String trigger = null;
    /**
     * Constructor.
     */
    public SubscribeQueryRunner() {
        super();
    }

    /**
     * Constructor.
     * @param message the message to process
     */
    public SubscribeQueryRunner(Message message) {
        super(message);
    }

    /* (non-Javadoc)
     * @see com.raytheon.edex.subscription.runners.ASubscribeRunner#execute()
     */
    @Override
    public boolean execute() {
        Property[] properties = this.message.getHeader().getProperties();
        List<Property> attribs = Tools.adjustMessageProperties(properties);
        attribs = modifyTriggerProperty(attribs);
        SubscriptionDAO dao = new SubscriptionDAO();
        List<SubscriptionRecord> retVal = null;
        if (attribs.size() > 0) {
            retVal = dao.getSubscriptions(attribs);
        } else {
            retVal = dao.getSubscriptions();
        }
        retVal = filterRecords(retVal);
        this.results = packageSubscriptions(retVal);
        return false;
    }
    /**
     * Filters the list of records received, limiting to those that match
     * the trigger condition.
     * 
     * @param records the list of records to filter
     * 
     * @return the filtered records
     */
    private List<SubscriptionRecord> filterRecords(List<SubscriptionRecord> records) {
        List<SubscriptionRecord> retVal = new ArrayList<SubscriptionRecord>();
        for (SubscriptionRecord record : records) {
            if (record.matchesTrigger(this.trigger)) {
                retVal.add(record);
            }
        }
        return retVal;
    }
    /**
     * Packages the subscriptions for return to the client. Each record
     * is bundled within the value of a {@link Property} object with a
     * name of <em>subscription</em>.
     * 
     * @param records the list of records to bundle
     * 
     * @return the list of bundled records
     */
    private List<Property> packageSubscriptions(List<SubscriptionRecord> records) {
        List<Property> retVal = new ArrayList<Property>();
        if (records.size() == 0) {
            retVal.add(new Property("count","0"));
        } else {
            retVal.add(new Property("count",String.valueOf(records.size())));
            for (SubscriptionRecord record : records) {
                try {
                    String xml = SerializationUtil.marshalToXml(record);
                    retVal.add(new Property("subscription",xml));
                } catch (Exception e) {
                    logger.warn("Unable to serialize subscription record " + record.toString() + " - skipping",e);
                }
            }
        }
        return retVal;
    }
    /**
     * Modifies the list of {@link Property} objects by removing the <em>trigger</em>
     * property and adding a <em>active</em> property with a value of <em>TRUE</em>.
     * 
     * @param properties the list of properties to modify
     * 
     * @return the modified property list
     */
    private List<Property> modifyTriggerProperty(List<Property> properties) {
        List<Property> retVal = new ArrayList<Property>();
        for (Property prop : properties) {
            if ("trigger".equalsIgnoreCase(prop.getName())) {
                this.trigger = prop.getValue();
            } else {
                retVal.add(prop);
            }
        }
        retVal.add(new Property("active","TRUE"));
        return retVal;
    }
}
