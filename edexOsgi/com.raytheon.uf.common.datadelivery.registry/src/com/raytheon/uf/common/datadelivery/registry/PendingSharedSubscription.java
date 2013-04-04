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

import com.raytheon.uf.common.registry.annotations.AssociationMapping;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Pending Subscription for shared subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 04, 2013 1841       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject(objectType = InitialPendingSharedSubscription.class,
        value = { Subscription.PROVIDER_NAME_SLOT,
        Subscription.NAME_SLOT, Subscription.DATA_SET_SLOT,
        Subscription.OWNER_SLOT,
        InitialPendingSubscription.CHANGE_REQUEST_ID_SLOT }, associationMappings = { @AssociationMapping(associationType = RegistryUtil.ASSOCIATION_RELATED_TO, keyFields = {
        Subscription.PROVIDER_NAME_SLOT, Subscription.NAME_SLOT,
        Subscription.DATA_SET_SLOT, Subscription.OWNER_SLOT }, required = false, targetObject = SharedSubscription.class) })
public class PendingSharedSubscription extends InitialPendingSharedSubscription
        implements PendingSubscription, ISerializableObject {

    private static final long serialVersionUID = 7607153845750089310L;

    public PendingSharedSubscription() {

    }

    public PendingSharedSubscription(SharedSubscription subscription,
            String currentUser) {
        super(subscription, currentUser);
    }

}
