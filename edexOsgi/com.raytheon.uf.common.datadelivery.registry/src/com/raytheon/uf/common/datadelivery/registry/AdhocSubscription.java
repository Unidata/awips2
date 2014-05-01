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
 * AdHoc query object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2012            mpduff       Initial creation
 * Aug 20, 2012 0743       djohnson     Set group name on construction.
 * Sep 14, 2012 1169       djohnson     Use registry object configuration from {@link Subscription}.
 * Sep 21, 2012 1169       djohnson     Add @RegistryObject again, shouldn't show up from subscription queries.
 * Oct 10, 2012 1261       djohnson     Copy constructor.
 * Dec 11, 2012 1403       djohnson     No longer a registry object.
 * Dec 12, 2012 1433       bgonzale     Use new Subscription copy ctor.
 * Mar 29, 2013 1841       djohnson     Subscription is now UserSubscription.
 * May 21, 2013 2020       mpduff       Rename UserSubscription to SiteSubscription.
 * Oct 2,  2013 1797       dhladky      Generics start
 * Oct 11, 2013 2460       dhladky      Restored Adhoc's to registryObject store, WFO only
 * Oct 23, 2013   2484     dhladky      Unique ID for subscriptions updated.
 * Nov 14, 2013   2548     mpduff       Add a subscription type slot.
 * jan 23, 2013   2584     dhladky     Versions.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "AdhocSubscription")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject({ Subscription.PROVIDER_NAME_SLOT, Subscription.NAME_SLOT,
        Subscription.DATA_SET_SLOT, Subscription.OWNER_SLOT,
        Subscription.ORIGINATING_SITE_SLOT, Subscription.SUBSCRIPTION_TYPE_SLOT })
@RegistryObjectVersion(value = 1.0f)
public class AdhocSubscription<T extends Time, C extends Coverage> extends
        SiteSubscription<T, C> {

    private static final long serialVersionUID = -2200080380095632486L;

    public AdhocSubscription() {
        setGroupName("Adhoc");
    }

    public AdhocSubscription(SiteSubscription<T, C> subscription) {
        super(subscription);
        setGroupName("Adhoc");
    }

}
