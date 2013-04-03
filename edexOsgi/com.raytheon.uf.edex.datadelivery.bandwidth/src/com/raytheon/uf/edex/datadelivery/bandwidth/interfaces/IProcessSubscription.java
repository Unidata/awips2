package com.raytheon.uf.edex.datadelivery.bandwidth.interfaces;

import java.util.ArrayList;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;

/**
 * Processor Interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2012            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public interface IProcessSubscription {

    public ArrayList<SubscriptionBundle> process(ArrayList<Subscription> subs);

}
