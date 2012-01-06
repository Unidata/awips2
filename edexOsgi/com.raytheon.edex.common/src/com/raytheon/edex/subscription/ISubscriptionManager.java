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
package com.raytheon.edex.subscription;

import java.io.Serializable;

import com.raytheon.edex.exception.SubscriptionException;

/**
 * Provides an interface for utilizing the subscription manager.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01May2007    208         MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public interface ISubscriptionManager {
    public static final String CONFIGURATION_NAME = "SUBSCRIPTION_SERVICE";
    public static final String KEY_FIELD = "KEY_FIELD";
    public static final String SUBSCRIPTION_QUERY_VAR = "SUBSCRIPTION_QUERY_VAR";
    public static final String SUBSCRIPTION_QUERY_FIELD = "SUBSCRIPTION_QUERY_FIELD";
    public static final String HIB_CLASS = "HIB_CLASS";
    /**
     * Returns the script from the data store that matches the specified subscription and 
     * script IDs. For XML based scripts, the script is updated to query for the specified
     * data URI.
     * 
     * @param id the subscription ID
     * @param scriptID the script ID
     * @param dataURI the dataURI - may be {@code null} for JS based uEngine scripts
     * 
     * @return the (possibly) modified script - will return {@code null} on a error
     */
    public Serializable getModifiedScript(String id, String scriptID,String dataURI);
    /**
     * Gets the {@code Subscription}. Returns the 
     * {@link com.raytheon.edex.subscription.Subscription Subscription} object
     * associated with the {@code dataURI}.
     *  
     * @param dataURI the URI for the subscribed data
     * 
     * @return the subscription - will be {@code null} if no matching subscription
     *         exists.  
     */
    public Subscription getSubscription(String dataURI);
    /**
     * Returns the subscription key (URI) that matches the data URI.
     *  
     * @param dataURI the data URI to match
     * 
     * @return the subscription key
     */
    public String getSubscriptionKey(String dataURI);
    /**
     * Check to see if the specified {@code dataURI} has a subscription.
     *  
     * @param dataURI the data URI to check
     * 
     * @return {@code true} if a subscription exists, {@code false} otherwise. 
     */
    public boolean isSubscribed(String dataURI);
    /**
     * Perform the product subscription. If the data used to create the product
     * is already under subscription, adds the script to the existing subscription. 
     * 
     * @param dataURI the data identifier(may be a regular expression)
     * @param script the script used to modify the data
     * 
     * @throws SubscriptionException if any error occurs
     */
    public void subscribe(String dataURI, String scriptID, Object script) throws SubscriptionException;
    /**
     * Performs the unsubscribe action. If there is no subscription for
     * the specified {@code dataURI}, no action is taken. If the subscription
     * exists and has a single &mu;Engine action script matching the {@code scriptID},
     * the subscription is removed from the subscription list. Otherwise, the script 
     * matching the {@code scriptID} is removed from the subscription.
     * 
     * @param dataURI the URI of the subscription
     * @param scriptID the client provided script ID
     * 
     * @throws SubscriptionException if any error occurs
     */
    public void unsubscribe(String dataURI, String scriptID) throws SubscriptionException;
    /**
     * Updates an existing subscription by adding a new script.
     *  
     * @param id the subscription ID
     * @param scriptID the script ID for the new script
     * @param script the script to add

     * @throws SubscriptionException when a error occurs
     */
    public void updateSubscription(String id, String scriptID, Object script) throws SubscriptionException;
}
