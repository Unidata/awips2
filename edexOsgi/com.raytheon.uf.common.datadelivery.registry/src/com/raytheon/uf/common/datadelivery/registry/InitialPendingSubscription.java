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
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Initial Pending Subscription Object.
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
        InitialPendingSubscription.CHANGE_REQUEST_ID_SLOT })
public class InitialPendingSubscription extends Subscription {
    public static final String CHANGE_REQUEST_ID_SLOT = "changeReqId";

    /** ID of the user requesting the change */
    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute(CHANGE_REQUEST_ID_SLOT)
    private String changeReqId;

    /** Reason for requesting the change */
    @XmlAttribute
    @DynamicSerializeElement
    private String changeReason;

    /**
     * Constructor
     */
    public InitialPendingSubscription() {
        //empty
    }

    /**
     * Constructor
     *
     * @param subscription
     *           subscription object
     * @param user
     *           user
     */
    public InitialPendingSubscription(Subscription subscription, String user) {
        super(subscription);

        this.setChangeReqId(user);
        this.setDeleted(subscription.isDeleted());
    }



    /**
     * @param changeReqId the changeReqId to set
     */
    public void setChangeReqId(String changeReqId) {
        this.changeReqId = changeReqId;
    }

    /**
     * @return the changeReqId
     */
    public String getChangeReqId() {
        return changeReqId;
    }

    /**
     * @return the changeReason
     */
    public String getChangeReason() {
        return changeReason;
    }

    /**
     * @param changeReason the changeReason to set
     */
    public void setChangeReason(String changeReason) {
        this.changeReason = changeReason;
    }
}
