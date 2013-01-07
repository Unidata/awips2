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
 * Pending Subscription Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2012            mpduff     Initial creation
 * Aug 27, 2012 0743      djohnson   Fix copy constructor, add association annotations.
 * Sep 14, 2012 1169      djohnson   Include owner in the registry id and association.
 * Sep 24, 2012 1157      mpduff     Changed to extend IniitialPendingSubscription.
 * Nov 19, 2012 1166      djohnson   Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject(objectType = InitialPendingSubscription.class,
        value = { Subscription.PROVIDER_NAME_SLOT,
        Subscription.NAME_SLOT, Subscription.DATA_SET_SLOT,
        Subscription.OWNER_SLOT,
        InitialPendingSubscription.CHANGE_REQUEST_ID_SLOT }, associationMappings = { @AssociationMapping(associationType = RegistryUtil.ASSOCIATION_RELATED_TO, keyFields = {
        Subscription.PROVIDER_NAME_SLOT, Subscription.NAME_SLOT,
        Subscription.DATA_SET_SLOT, Subscription.OWNER_SLOT }, required = false, targetObject = Subscription.class) })
public class PendingSubscription extends InitialPendingSubscription
        implements ISerializableObject {

    public PendingSubscription() {

    }

    public PendingSubscription(Subscription subscription,
            String currentUser) {
        super(subscription, currentUser);
    }


}
