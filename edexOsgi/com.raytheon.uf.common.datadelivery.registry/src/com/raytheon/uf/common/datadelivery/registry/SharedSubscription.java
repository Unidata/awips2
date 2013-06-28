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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Subscription that is shared among sites.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2013 1841       djohnson    Initial creation
 * May 15, 2013 1040       mpduff      Added addOfficeId.
 * May 29, 2013 1650       djohnson    Add setOwner() so reflection works.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@RegistryObject(value = { Subscription.PROVIDER_NAME_SLOT,
        Subscription.NAME_SLOT, Subscription.DATA_SET_SLOT,
        Subscription.OWNER_SLOT })
@DynamicSerialize
public class SharedSubscription extends RecurringSubscription {

    private static final long serialVersionUID = -7221500266253493273L;

    private static final String SHARED_SUBSCRIPTION_OWNER = "shared";

    /**
     * Constructor.
     */
    public SharedSubscription() {

    }

    /**
     * @param sharedSubscription
     */
    public SharedSubscription(Subscription sub) {
        super(sub);
    }

    /**
     * @param sharedSubscription
     */
    public SharedSubscription(SharedSubscription sub, String newName) {
        super(sub, newName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getOwner() {
        return SHARED_SUBSCRIPTION_OWNER;
    }

    /**
     * This method does nothing. It is only required due to reflective
     * associations when creating associations between
     * {@link SharedSubscription} and {@link PendingSharedSubscription}
     * instances.
     * 
     * @param owner
     *            owner
     */
    public void setOwner(String owner) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SharedSubscription copy() {
        return new SharedSubscription(this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SharedSubscription copy(String newName) {
        return new SharedSubscription(this, newName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InitialPendingSubscription initialPending(String currentUser) {
        return new InitialPendingSharedSubscription(this, currentUser);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PendingSubscription pending(String currentUser) {
        return new PendingSharedSubscription(this, currentUser);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addOfficeID(String officeId) {
        if (!officeIDs.contains(officeId)) {
            this.officeIDs.add(officeId);
        }
    }
}