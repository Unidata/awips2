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
import com.raytheon.uf.common.registry.annotations.RegistryObjectVersion;
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
 * Sept 30, 2013 1797      dhladky     Generics
 * Oct 23, 2013   2484     dhladky     Unique ID for subscriptions updated.
 * Nov 14, 2013   2548     mpduff       Add a subscription type slot.
 * jan 23, 2013   2584     dhladky     Versions.
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
        Subscription.OWNER_SLOT, Subscription.ORIGINATING_SITE_SLOT,
        Subscription.SUBSCRIPTION_TYPE_SLOT })
@RegistryObjectVersion(value = 1.0f)
@DynamicSerialize
public class SharedSubscription<T extends Time, C extends Coverage> extends
        RecurringSubscription<T, C> {

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
    public SharedSubscription(Subscription<T, C> sub) {
        super(sub);
    }

    /**
     * @param sharedSubscription
     */
    public SharedSubscription(SharedSubscription<T, C> sub, String newName) {
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
    public SharedSubscription<T, C> copy() {
        return new SharedSubscription<T, C>(this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SharedSubscription<T, C> copy(String newName) {
        return new SharedSubscription<T, C>(this, newName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InitialPendingSubscription<T, C> initialPending(String currentUser) {
        return new InitialPendingSharedSubscription<T, C>(this, currentUser);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PendingSubscription<T, C> pending(String currentUser) {
        return new PendingSharedSubscription<T, C>(this, currentUser);
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