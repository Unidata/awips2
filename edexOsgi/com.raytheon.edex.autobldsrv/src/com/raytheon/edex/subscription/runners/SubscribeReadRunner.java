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
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;

/**
 * Implements a subscription request runner that performs a read operation in
 * the subscription database tables. Following the read, data is formatted for
 * return based on the type of request; subscription or LDAD. (The LDAD request
 * supports the legacy textdb interface.) Following execution, query results are
 * obtained via {@link #getResults()}; the results are a <code>List</code> of
 * {@link Property} objects.
 * <p>
 * Typical Usage:
 * 
 * <PRE>
 * <CODE>
 *     List<Property> results = null;
 *     SubscribeAction action = SubscribeAction.ACTION_READ;
 *     ISubscribeRunner runner = RunnerFactory.getInstance().getWorker(action);
 *     if (runner != null) {
 *        runner.initialize(message);
 *        runner.execute();
 *        results = runner.getResults();
 *     }
 * </CODE>
 * </PRE>
 * 
 * Typical results:
 * 
 * <PRE>
 * LDAD:
 *     PRODUCTID SCRIPT
 *     --------- ------
 *     OMASVROAX /awips/edex/opt/scripts/dissemprod.pl
 *     
 * Subscription:
 *     ID     ACTIVE TYPE  RUNNER TRIGGER   SCRIPT
 *     ------ ------ ----- ------ --------- ------
 *     1      True   timer system 0 0/2 * * * ? /bin/ps aux
 *     2      True   timer system 0 0/5 * * * ? /bin/pwd
 * </PRE>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class SubscribeReadRunner extends ASubscribeRunner {
    /**
     * Constructor. Creates an empty Runner object. This is the constructor
     * called by the {@link RunnerFactory}'s
     * {@link RunnerFactory#getWorker(com.raytheon.edex.subscription.util.SubscribeAction)
     * getWorker(...)} method. The {@link #initialize(Message)} method must be
     * called prior to using the worker.
     */
    public SubscribeReadRunner() {
        super();
    }

    /**
     * Constructor. Creates a runner object for the specified message.
     * 
     * @param message
     *            the message to execute
     */
    public SubscribeReadRunner(Message message) {
        super(message);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.subscription.runners.ASubscribeRunner#execute()
     */
    @Override
    public boolean execute() {
        Header header = this.message.getHeader();
        Property[] props = header.getProperties();
        String type = header.getProperty("type");
        boolean ldad = ("ldad".equalsIgnoreCase(type));

        List<Property> attribs = Tools.adjustMessageProperties(props);

        SubscriptionDAO dao = new SubscriptionDAO();
        List<SubscriptionRecord> retVal = null;
        if (attribs.size() > 0) {
            retVal = dao.getSubscriptions(attribs);
        } else {
            retVal = dao.getSubscriptions();
        }
        this.results = generateResponse(ldad, retVal);
        return true;
    }

    /**
     * Converts the data returned by the database query into a list of Property
     * objects.
     * 
     * @param ldad
     *            flag to indicate if this is a textdb (LDAD) request
     * @param responses
     *            list of responses from the database query
     * 
     * @return list of property objects containing the query results
     */
    private List<Property> generateResponse(boolean ldad,
            List<SubscriptionRecord> responses) {
        List<Property> props = new ArrayList<Property>();
        if (ldad) {
            props.add(new Property(RESPONSE_NORMAL, "PRODUCTID SCRIPT"));
            props.add(new Property(RESPONSE_NORMAL, "--------- ------"));
            for (SubscriptionRecord spr : responses) {
                String trigger = spr.getTrigger();
                String script = spr.getFilepath();
                String line = String.format("%s %s", trigger, script);
                props.add(new Property(RESPONSE_NORMAL, line));
            }
        } else {
            props.add(new Property(RESPONSE_NORMAL,
                    "ID     ACTIVE TYPE  RUNNER TRIGGER   SCRIPT"));
            props.add(new Property(RESPONSE_NORMAL,
                    "------ ------ ----- ------ --------- ------"));
            for (SubscriptionRecord spr : responses) {
                String script;
                if ("system".equalsIgnoreCase(spr.getRunner())) {
                    script = spr.getFilepath();
                    if (spr.getArguments() != null
                            && !spr.getArguments().equals("")) {
                        script += " " + spr.getArguments();
                    }
                } else {
                    script = spr.getScript();
                }
                String line = String.format("%6d %-6s %5s %6s %8s %s",
                        spr.getId(), spr.isActive() ? "True" : "False",
                        spr.getType(), spr.getRunner(), spr.getTrigger(),
                        script);
                props.add(new Property(RESPONSE_NORMAL, line));
            }
        }
        return props;
    }
}
