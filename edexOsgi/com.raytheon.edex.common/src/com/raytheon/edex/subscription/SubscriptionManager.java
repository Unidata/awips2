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

/**
 * 
 */
package com.raytheon.edex.subscription;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;

import com.raytheon.edex.db.dao.SubscribeDAO;
import com.raytheon.edex.exception.SubscriptionException;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.XMLUtils;
import com.raytheon.uf.common.util.StringUtil;

/**
 * This class manages the product subscription list. It is designed as a POJO
 * class using sychronized methods to manage the subscription list. The
 * subscription list is implemented using Java Object persistence within the
 * data access layer. Each
 * {@link com.raytheon.edex.subscription.Subscription Subscription} may contain
 * multiple &mu;Engine scripts that will operate on the same data. Methods are
 * implemented to {@link #subscribe(String, String, Object) subscribe},
 * {@link #unsubscribe(String, String) unsubscribe},
 * {@link #isSubscribed(String) check a subscription},
 * {@link #updateSubscription(String, String, Object) update an existing subscription},
 * {@link #getModifiedScript(String, String, String) retrieve a script}, and
 * {@link #getSubscription(String) retrieve a subscription}.
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 15Aug2006    #18         MW Fegan    Initial creation.
 * 27Feb2007    #208        MW Fegan    Modified to use EDEX DAL.
 * 01May2007    208         MW Fegan    Changed name and added interface.
 * 03Jul2007    #338        MW Fegan    Corrected matching of subscriptions.
 * 14Sep2010    3944        cjeanbap    Fixed a NullPointerException.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */
public final class SubscriptionManager implements ISubscriptionManager {
    private static final Log logger = LogFactory
            .getLog(SubscriptionManager.class);

    /**
     * The data access layer.
     */
    // private static ISubscriber dataLayer = null;
    private HashMap<String, String> subscriptions = new HashMap<String, String>();

    /*
     * define constants that refer to XML based action script parts that are
     * modified for subscriptions.
     */
    private static final String ATTR_DATA_URI = "datauri";

    private static final String TERM_QUERY = "termQuery";

    private static final String QUERY_TAG = "query";

    private static final String MAKE_RESPONSE = "makeResponse";

    private static final String RESPONSE_METHOD = "returnMethod";

    private static final String RESPONSE_URI = "uri";

    private static final String RESPONSE_INLINE = "inline";

    private static final String QUERY_COUNT = "count";

    private static final String QUERY_COUNT_ONE = "1";

    private SubscribeDAO subscriptionDao;

    /**
     * Constructor.
     * 
     */
    public SubscriptionManager() {
        this.subscriptionDao = new SubscribeDAO();
        initSubscriptions();
    }

    /**
     * Initializes the internal subscription list by retrieving it from the EDEX
     * DAL.
     */
     private void initSubscriptions() {
         try {
             List<Subscription> list = subscriptionDao.getSubscriptions();
             
             for (Subscription obj : list) {
                 this.subscriptions.put((String) obj.getIdentifier(), "");
             }
         } catch (Exception e) {
             logger.warn("Unable to obtain subscription list.", e);
         }
     }
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.subscription.ISubscriptionManager#subscribe(java.lang.String,
     *      java.lang.String, java.lang.Object)
     */
    public synchronized void subscribe(String dataURI, String scriptID,
            Object script) throws SubscriptionException {
        // logger.debug("subscribe(): received subscription request, uri="
        // + Util.printString(dataURI) + ", script ID="
        // + Util.printString(scriptID) + ". script is " + Util.EOL
        // + script.toString());
        Subscription subscription;

        try {
            if (isSubscribed(dataURI)) {
                /*
                 * if subscription for the data exists, we need to update the
                 * subscription with the script
                 */
                updateSubscription(getSubscriptionKey(dataURI), scriptID,
                        modifyScript(script));
            } else {
                /*
                 * no existing subscription based on this data, need to create a
                 * new element.
                 */
                subscription = new Subscription(dataURI, scriptID,
                        modifyScript(script));
                subscriptionDao = new SubscribeDAO();
                subscriptionDao.persist(subscription);
                // dataLayer.manageSubscriptions(subscription,
                // ISubscriber.SUBSCRIBE_MODE_SAVE);
                // dataLayer.saveMetadata(subscription);
                subscriptions.put(dataURI, "");
            }
        } catch (Exception e) {
            String msg = "Unable fulfill subscription request, dataURI="
                    + StringUtil.printString(dataURI) + ", scriptID="
                    + StringUtil.printString(scriptID);
            e.printStackTrace();
            throw new SubscriptionException(msg, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.subscription.ISubscriptionManager#getModifiedScript(java.lang.String,
     *      java.lang.String, java.lang.String)
     */
    public synchronized Serializable getModifiedScript(String id,
            String scriptID, String dataURI) {
        Serializable retVal = null;
        try {
            Subscription subscription = getSubscription(id);
            Script script = subscription.getScript(scriptID);
            Object obj = script.getScript();
            if (obj instanceof String) {
                String xml = (String) obj;
                Document action = XMLUtils.scanXMLtoDOM(xml);
                XMLUtils.setValueIntoXML(action, XMLUtils.QUERY,
                        XMLUtils.VALUE, dataURI);
                retVal = XMLUtils.transformXMLDocument(action);
            } else {
                retVal = (Serializable) obj;
            }
        } catch (Exception e) {
            String msg = "Unable to obtain modified script.";
            logger.error(msg, e);
        }
        return retVal;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.subscription.ISubscriptionManager#getSubscriptionKey(java.lang.String)
     */
    public synchronized String getSubscriptionKey(String dataURI) {
        String retVal = "";
        for (String key : subscriptions.keySet()) {
            if (fixMatchURI(dataURI).matches(key)
                    || fixMatchURI(key).matches(dataURI)) {
                retVal = key;
                break;
            }
        }
        return retVal;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.subscription.ISubscriptionManager#isSubscribed(java.lang.String)
     */
    public synchronized boolean isSubscribed(String dataURI) {
        logger.debug("isSubscribed() received subscription inquery, URI is "
                + StringUtil.printString(dataURI));
        /* check to see if the uri exactly matches a subscription */
        if (subscriptions.containsKey(dataURI)) {
            return true;
        }
        // if (dataURI.indexOf(".+") == -1) {
        // return subscriptions.containsKey(dataURI);
        // }
        /* check for a regular expression match */
        boolean retVal = false;
        for (String key : subscriptions.keySet()) {
            // String temp1 = fixMatchURI(dataURI);
            // String temp2 = fixMatchURI(key);
            if (fixMatchURI(dataURI).matches(key)
                    || fixMatchURI(key).matches(dataURI)) {
                retVal = true;
                break;
            }
        }
        return retVal;
    }

    private String fixMatchURI(String matchURI) {
        return matchURI.replaceAll("\\\\\\(", "(").replaceAll("\\\\\\)", ")");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.subscription.ISubscriptionManager#updateSubscription(java.lang.String,
     *      java.lang.String, java.lang.Object)
     */
    public synchronized void updateSubscription(String id, String scriptID,
            Object script) throws SubscriptionException {
        Subscription subscription = getSubscription(id);
        boolean result = subscription.addScript(scriptID, script);
        if (!result) {
            String msg = "Encountered error updating subscription - "
                    + "script ID = " + scriptID + " for ID = " + id
                    + " already exists";
            logger.error(msg);
            throw new SubscriptionException(msg);
        }
        try {
            subscriptionDao = new SubscribeDAO();
            subscriptionDao.update(subscription);
            // dataLayer.manageSubscriptions(subscription,
            // ISubscriber.SUBSCRIBE_MODE_UPDATE);
        } catch (Exception e) {
            String msg = "Encountered error updating subscription - " + "ID = "
                    + id + ", script ID = " + scriptID;
            logger.error(msg, e);
            throw new SubscriptionException(msg, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.subscription.ISubscriptionManager#getSubscription(java.lang.String)
     */
    public synchronized Subscription getSubscription(String dataURI) {
        Subscription subscription = null;
        /* quick return if not subscribed */
        if (!isSubscribed(dataURI)) {
            // logger.info("No subscription exists for " +
            // Util.printString(dataURI));
            return subscription;
        }

        try {
            subscriptionDao = new SubscribeDAO();
            Subscription obj = (Subscription) subscriptionDao
                    .queryById(dataURI);
            // Object obj = dataLayer.getSubscription(dataURI);
            if (obj == null && obj instanceof Subscription) {
                logger.error("No Subscription object available for "
                        + StringUtil.printString(dataURI));
            }

        } catch (Exception e) {
            logger.error("Unable to get subscription information for "
                    + StringUtil.printString(dataURI), e);
        }
        return subscription;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.subscription.ISubscriptionManager#unsubscribe(java.lang.String,
     *      java.lang.String)
     */
    public synchronized void unsubscribe(String dataURI, String scriptID)
            throws SubscriptionException {
        boolean retVal;
        // logger.debug("unsubscribe() recieved unsubscribe request, dataURI="
        // + Util.printString(dataURI) + ", scriptID="
        // + Util.printString(scriptID));
        try {
            if (isSubscribed(dataURI)) {
                // data subscription exists
                Subscription subscription = getSubscription(dataURI);
                retVal = subscription.removeScript(scriptID);
                if (retVal) {
                    subscriptionDao = new SubscribeDAO();
                    // the script was removed
                    if (subscription.getCount() > 0) {
                        // still has more scripts
                        subscriptionDao.update(subscription);
                        // dataLayer.manageSubscriptions(subscription,
                        // ISubscriber.SUBSCRIBE_MODE_UPDATE);
                    } else {
                        subscriptionDao.delete(subscription);
                        // no additional scripts
                        // dataLayer.manageSubscriptions(subscription,
                        // ISubscriber.SUBSCRIBE_MODE_DELETE);
                        subscriptions.remove(dataURI);
                    }
                } else {
                    // no matching script
                    String msg = "no matching script, dataURI="
                            + StringUtil.printString(dataURI) + ", scriptID= "
                            + StringUtil.printString(scriptID);
                    logger.warn("unsubscribe() " + msg);
                    throw new SubscriptionException(msg);
                }
            } else {
                // no subscription for the data
                String msg = "No subscription exists, dataURI="
                        + StringUtil.printString(dataURI);
                logger.warn("unsubscribe() " + msg);
                throw new SubscriptionException(msg);
            }
        } catch (Exception e) {
            // error performing the unsubscribe
            if (!(e instanceof SubscriptionException)) {
                String msg = "Unable to unsubscribe. dataURI="
                        + StringUtil.printString(dataURI) + ", scriptID="
                        + StringUtil.printString(scriptID);
                throw new SubscriptionException(msg, e);
            } else {
                throw (SubscriptionException) e;
            }

        }
    }

    /**
     * This will modify the script so that it can be be used with the Auto Build
     * Server.
     * 
     * @param object
     *            the &mu;Engine script to modify
     * 
     * @return the modified script
     * 
     * @throws SubscriptionException
     *             if any error oours
     */
    private Object modifyScript(Object object) throws SubscriptionException {
        try {
            if (!(object instanceof String)) {
                return object;
            }
            String script = (String) object;
            Document document = XMLUtils.scanXMLtoDOM(script);
            /* this will remove all the query tags from the termQuery tag */
            XMLUtils.removeSubDocumentAll(document, QUERY_TAG);
            /* create the query tag for the datauri query */
            HashMap<String, String> attributes = new HashMap<String, String>();
            attributes.put(XMLUtils.NAME, ATTR_DATA_URI);
            attributes.put(XMLUtils.VALUE, "");
            XMLUtils.addChildToTag(document, TERM_QUERY, QUERY_TAG, attributes);
            /* set the query count to 1 */
            XMLUtils.setValueIntoXML(document, TERM_QUERY, QUERY_COUNT,
                    QUERY_COUNT_ONE);
            /* Modify the return type, if needed. */
            String method = XMLUtils.getAttributeValueFromTag(document,
                    MAKE_RESPONSE, RESPONSE_METHOD);
            if (method.equalsIgnoreCase(RESPONSE_INLINE)) {
                XMLUtils.setValueIntoXML(document, MAKE_RESPONSE,
                        RESPONSE_METHOD, RESPONSE_URI);
            }
            return XMLUtils.transformXMLDocument(document).replaceAll(Util.EOL,
                    "");
        } catch (Exception e) {
            throw new SubscriptionException("Unable to modify action script", e);
        }
    }
}
