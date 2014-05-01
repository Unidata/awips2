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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.RegistryObjectVersion;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Initial Pending User Subscription Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep  24, 2012           mpduff      Initial creation
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * Dec 12, 2012 1433       bgonzale    Use new Subscription copy ctor.
 * Mar 29, 2013 1841       djohnson    Subscription is now UserSubscription.
 * Apr 02, 2013 1841       djohnson    InitialPendingSubscription is now InitialPendingUserSubscription.
 * May 21, 2013 2020       mpduff      Rename UserSubscription to SiteSubscription.
 * Sept 30, 2013 1797      dhladky     Some Generics
 * Nov 14, 2013   2548     mpduff      Add a subscription type slot.
 * Feb 18, 2013  2786      dhladky     Forgot this one in version changes.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject(value = { Subscription.PROVIDER_NAME_SLOT,
        Subscription.NAME_SLOT, Subscription.DATA_SET_SLOT,
        Subscription.OWNER_SLOT,
        InitialPendingSubscription.CHANGE_REQUEST_ID_SLOT,
        Subscription.ORIGINATING_SITE_SLOT, Subscription.SUBSCRIPTION_TYPE_SLOT })
@RegistryObjectVersion(value = 1.0f)
public class InitialPendingSiteSubscription<T extends Time, C extends Coverage>
        extends SiteSubscription<T, C> implements
        InitialPendingSubscription<T, C> {
    private static final long serialVersionUID = 2779084460608459754L;

    /** ID of the user requesting the change */
    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute(InitialPendingSubscription.CHANGE_REQUEST_ID_SLOT)
    private String changeReqId;

    /** Reason for requesting the change */
    @XmlAttribute
    @DynamicSerializeElement
    private String changeReason;

    /**
     * Constructor
     */
    public InitialPendingSiteSubscription() {
        // empty
    }

    /**
     * Constructor
     * 
     * @param subscription
     *            subscription object
     * @param user
     *            user
     */
    public InitialPendingSiteSubscription(SiteSubscription<T, C> subscription,
            String user) {
        super(subscription);

        this.setChangeReqId(user);
        this.setDeleted(subscription.isDeleted());
        this.setId(RegistryUtil.getRegistryObjectKey(this));
    }

    /**
     * @param changeReqId
     *            the changeReqId to set
     */
    @Override
    public void setChangeReqId(String changeReqId) {
        this.changeReqId = changeReqId;
    }

    /**
     * @return the changeReqId
     */
    @Override
    public String getChangeReqId() {
        return changeReqId;
    }

    /**
     * @return the changeReason
     */
    @Override
    public String getChangeReason() {
        return changeReason;
    }

    /**
     * @param changeReason
     *            the changeReason to set
     */
    @Override
    public void setChangeReason(String changeReason) {
        this.changeReason = changeReason;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription<T, C> subscription() {
        return new SiteSubscription<T, C>(this);
    }

}
