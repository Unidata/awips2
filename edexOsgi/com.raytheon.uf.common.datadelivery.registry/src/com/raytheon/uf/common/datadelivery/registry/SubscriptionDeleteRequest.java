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
package com.raytheon.uf.common.datadelivery.registry;

import java.util.List;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.datadelivery.registry.handlers.IBaseSubscriptionHandler;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A request for the server to delete a subscription.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1187       djohnson     Initial creation
 * Nov 05, 2012 1306       djohnson     Remove dynamic serialize field level adapters.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionDeleteRequest extends AbstractPrivilegedRequest {

    @DynamicSerializeElement
    private List<String> subscriptionIds;

    @DynamicSerializeElement
    private String handlerClass;

    @DynamicSerializeElement
    private String username;

    /**
     * Constructor.
     */
    public SubscriptionDeleteRequest() {

    }

    /**
     * Constructor.
     * 
     * @param subscriptionId
     * @param subscriptionClass
     * @param user
     */
    public SubscriptionDeleteRequest(
            final List<String> subscriptionIds,
            final Class<? extends IBaseSubscriptionHandler<? extends Subscription>> handlerClass,
            String username) {
        this.subscriptionIds = subscriptionIds;
        this.handlerClass = handlerClass.getName();
        this.username = username;
    }

    /**
     * @return the subscriptionId
     */
    public List<String> getSubscriptionIds() {
        return subscriptionIds;
    }

    /**
     * @return the username
     */
    public String getUsername() {
        return username;
    }

    /**
     * @param subscriptionIds
     *            the subscriptionIds to set
     */
    public void setSubscriptionIds(List<String> subscriptionIds) {
        this.subscriptionIds = subscriptionIds;
    }

    /**
     * @param username
     *            the username to set
     */
    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * @return the handlerClass
     */
    public String getHandlerClass() {
        return handlerClass;
    }

    /**
     * @param handlerClass
     *            the handlerClass to set
     */
    public void setHandlerClass(String handlerClass) {
        this.handlerClass = handlerClass;
    }
}
