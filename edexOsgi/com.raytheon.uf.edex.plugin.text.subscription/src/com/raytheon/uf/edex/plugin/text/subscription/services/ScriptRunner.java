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
package com.raytheon.uf.edex.plugin.text.subscription.services;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.dataplugin.text.subscription.db.ReplacementRecord;
import com.raytheon.uf.common.dataplugin.text.subscription.db.SubscriptionRecord;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.util.ReflectionUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.plugin.text.subscription.runners.SubscribeQueryRunner;

/**
 * Main class of the EDEX Script Runner.
 * <P>
 * The intent is to have an end-point that will interact with the subscription
 * service to determine if a script should be executed. Generally, an instance
 * will receive a message containing a trigger condition. It will then create a
 * message to send to the subscription manager and will receive a list of
 * scripts in reply. If the list of scripts is empty, it will loop through the
 * scripts and execute them.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 15Dec2008    1709       MW Fegan    Initial creation. Replaces ScriptRunnerSrv.
 * 14Apr2009    2045       MW Fegan    Added support for AFOS PIL (LDAD) triggers.
 * 30Sep2009    3076       MW Fegan    Allow LDAD triggers to be simple strings.
 * 06Jan2010    4166       MW Fegan    Log error when script execution fails.
 * 24May2011    5163       cjeanbap    Log error when script execution fails to 
 *                                     separate log file.
 * 30Aug2011    10581      rferrel     executeScript now sending proper trigger 
 *                                     argument to the engine.
 * Feb 15, 2013 1638       mschenke    Moved DataURINotificationMessage to uf.common.dataplugin
 * May 22, 2014 2536       bclement    moved from autobldsrv to edex.plugin.text
 * Sep 05, 2014 2926       bclement    get query results directly, removed decodeResponse()
 * Dec 09, 2015 5166       kbisanz     Update logging to use SLF4J.
 * Jan 04, 2016 5203       tjensen     Removed dependency on uEngine. Replaced with calls to 
 *                                     TextTriggerHandler.
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.subscription
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class ScriptRunner {
    private transient Logger logger = LoggerFactory
            .getLogger("FailedTriggerLog");

    private String type = null;

    private static final String SUBSCRIBE_OPERATION = "query";

    private static final String TRIGGER_KEY = "%TRIGGER%";

    /**
     * Constructor.
     */
    public ScriptRunner() {
        super();
    }

    /**
     * This is the main method of the ScriptRunner class. It accepts a Trigger
     * event Object from the framework, decodes the event, obtains any
     * subscriptions matching the triggering event and executes the script for
     * each subscription.
     * 
     * @param event
     *            the Trigger event
     * 
     * @throws TriggerException
     *             if an error occurs
     */
    public void runScripts(Object event) throws TriggerException {
        List<String> triggers = decodeTrigger(event);
        for (String trigger : triggers) {
            Message query = prepareQueryMessage(trigger);
            List<SubscriptionRecord> subscriptions = querySubscriptions(query);
            for (SubscriptionRecord record : subscriptions) {
                if (record.getTrigger().indexOf(" ") > 0) {
                    trigger = record.getTrigger();
                }
                executeScript(record, trigger);
            }
        }
    }

    /**
     * Decodes the Trigger event.
     * 
     * @param event
     *            the Trigger event.
     * 
     * @return the decoded trigger event
     * 
     * @throws TriggerException
     *             if an error occurs
     */
    private List<String> decodeTrigger(Object event) throws TriggerException {
        List<String> trigger = new ArrayList<String>();
        switch (ScriptRunnerType.translate(this.type)) {
        case TIMER:
            Date now = new Date();
            trigger.add(String.valueOf(now.getTime()));
            break;
        case DATA:
            if (event != null) {
                DataURINotificationMessage msg = null;
                if (event instanceof DataURINotificationMessage) {
                    msg = (DataURINotificationMessage) event;
                    String[] uris = msg.getDataURIs();
                    if (uris != null) {
                        for (String uri : uris) {
                            trigger.add(uri);
                        }
                    } else {
                        logger.warn("notification contained null uri array");
                    }
                } else if (event instanceof String[]) {
                    String[] uris = (String[]) event;
                    if (uris != null) {
                        for (String uri : uris) {
                            trigger.add(uri);
                        }
                    }
                } else if (event instanceof String) {
                    trigger.add((String) event);
                } else {
                    logger.warn("notification contained invalid message, type was "
                            + event.getClass().getSimpleName());
                }
            }
            break;
        case LDAD:
            if (event != null) {
                if (event instanceof String) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("Processing trigger: " + event);
                    }
                    trigger.add(event.toString());
                } else if (event instanceof String[]) {
                    String[] prodIds = (String[]) event;
                    for (String prodId : prodIds) {
                        trigger.add(prodId);
                    }
                } else if (event instanceof PluginDataObject[]) {
                    PluginDataObject[] pdos = (PluginDataObject[]) event;
                    for (PluginDataObject pdo : pdos) {
                        try {
                            String prodID = ReflectionUtil.getter(String.class,
                                    pdo, "productId");
                            if (logger.isDebugEnabled()) {
                                logger.debug("Processing trigger: " + prodID
                                        + ", class = "
                                        + pdo.getClass().getSimpleName());
                            }
                            trigger.add(prodID);
                        } catch (Exception e) {
                            logger.warn("Unable to extract product information from ["
                                    + pdo.toString() + "] skipping...");
                        }
                    }
                } else if (event instanceof PluginDataObject) {
                    PluginDataObject pdo = (PluginDataObject) event;
                    try {
                        String prodID = ReflectionUtil.getter(String.class,
                                pdo, "productId");
                        if (logger.isDebugEnabled()) {
                            logger.debug("Processing trigger: " + prodID
                                    + ", class = "
                                    + pdo.getClass().getSimpleName());
                        }
                        trigger.add(prodID);
                    } catch (Exception e) {
                        logger.warn("Unable to extract product information from ["
                                + pdo.toString() + "] skipping...");
                    }
                } else if (event instanceof byte[]) {
                    String value = new String((byte[]) event);
                    logger.warn("notification contained invalid message, type was "
                            + event.getClass().getSimpleName()
                            + " value: "
                            + value);
                } else {
                    logger.warn("notification contained invalid message, type was "
                            + event.getClass().getSimpleName());
                }
            }
            break;
        default:
            throw new TriggerException("Invalid scriptrunner type ["
                    + this.type + "] configured");
        }
        if (logger.isDebugEnabled()) {
            logger.debug("script runner fired: type= " + this.type
                    + ", trigger= " + trigger);
        }
        return trigger;
    }

    /**
     * Executes the subscription script.
     * 
     * @param record
     *            the record containing the script to execute
     * @param triggerObj
     *            the trigger
     */
    private void executeScript(SubscriptionRecord record, String trigger) {
        String runner = record.getRunner();
        String script = record.getScript();
        // check for empty string and set to null so checks will work
        if (script.equals("")) {
            script = null;
        }
        if (script != null) {
            for (ReplacementRecord replacement : record.getReplacements()) {
                String token = replacement.getKey();
                String value = replacement.getValue();
                script = script.replaceAll("%" + token + "%", value);
            }
            script = script.replaceAll(TRIGGER_KEY, trigger);
        } else {
            String path = record.getFilepath();
            String args = record.getArguments();
            if (StringUtil.isEmptyString(path)
                    && StringUtil.isEmptyString(args)) {
                logger.warn("Unable to execute script for " + record.toString());
                return;
            }
            if (!StringUtil.isEmptyString(args)) {
                args = args.replaceAll(TRIGGER_KEY, trigger);
            }
            script = (StringUtil.isEmptyString(path) ? "" : path) + " "
                    + (StringUtil.isEmptyString(args) ? "" : args);
        }

        if (runner.toLowerCase().equals("ldad")
                || runner.toLowerCase().equals("pil")) {
            TextTriggerHandler exHandler = new TextTriggerHandler();
            try {
                exHandler.execute(script, trigger);
                logger.info("Executed script: runner= " + runner + ", script= "
                        + script.replaceAll("\\n", "<ret>"));
            } catch (Exception e) {
                logger.error(generateErrorMsg(runner, script), e);
            }
        } else {
            logger.error("Unable to execute script '" + script
                    + "' due to unexpected script runner type: " + runner);
        }
    }

    /**
     * Creates the Subscription message used to query for subscriptions.
     * 
     * @param type
     *            type of subscription (timer, data, etc)
     * @param triggerObj
     *            the trigger event value
     * 
     * @return message to perform the query
     */
    private Message prepareQueryMessage(String trigger) {
        Message retVal = new Message();
        Header header = new Header();
        List<Property> properties = new ArrayList<Property>();
        properties.add(new Property("operation", SUBSCRIBE_OPERATION));
        properties.add(new Property("type", this.type));
        properties.add(new Property("trigger", trigger));
        header.setProperties(properties.toArray(new Property[] {}));
        retVal.setHeader(header);
        return retVal;
    }

    /**
     * Performs a query of the database to determine if there are any
     * subscriptions that match the trigger criteria.
     * 
     * @param message
     *            message containing the desired query information
     * 
     * @return subscription record list containing the results of the query
     * 
     * @throws TriggerException
     *             if an error occurs
     */
    private List<SubscriptionRecord> querySubscriptions(Message message)
            throws TriggerException {
        SubscribeQueryRunner runner = new SubscribeQueryRunner(message);
        runner.execute();
        return runner.getDirectResults();
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Enumeration providing a of script runner types.
     * 
     * @author mfegan
     * @version 1.0
     */
    private enum ScriptRunnerType {
        TIMER, DATA, LDAD;
        private static final Map<String, ScriptRunnerType> types = new HashMap<String, ScriptRunnerType>() {
            private static final long serialVersionUID = 1L;
            {
                put("timer", TIMER);
                put("data", DATA);
                put("pil", LDAD);
                put("ldad", LDAD);
            }
        };

        public static final ScriptRunnerType translate(String type) {
            return types.get(type);
        }
    }

    private String generateErrorMsg(String runner, String script) {
        StringBuilder sb = new StringBuilder();
        try {
            File file = new File(script);
            if (!file.exists()) {
                sb.append("Directory/File does not exist!");
            } else if (!file.canExecute()) {
                sb.append("File is not an executable!");
            } else {
                sb.append("Unknown error occured!");
            }
        } catch (Exception e) {
            // logger.error("Unexpected error occured!", e);
            sb.append("Unexpected error occured! ");
            sb.append(e.getCause());
        }
        sb.append("\n");
        sb.append("Encountered errors executing script: runner= ");
        sb.append(runner).append(", script= ");
        sb.append(script.replaceAll("\\n", "<ret>"));
        sb.append(" ");
        return sb.toString();
    }
}
