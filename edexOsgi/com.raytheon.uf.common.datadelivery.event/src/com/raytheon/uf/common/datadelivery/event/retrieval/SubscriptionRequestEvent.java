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
package com.raytheon.uf.common.datadelivery.event.retrieval;

import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.stats.StatisticsEvent;

/**
 * 
 * Event that occurs when a subscription request is made.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 08, 2013 1654       bgonzale    Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionRequestEvent extends StatisticsEvent {

    private static final long serialVersionUID = -5302262650866371587L;

    @DynamicSerializeElement
    protected String owner;

    @DynamicSerializeElement
    protected String network;

    @DynamicSerializeElement
    protected String provider;

    @DynamicSerializeElement
    protected int numRecords = 1;

    public String getOwner() {
        return owner;
    }

    public String getNetwork() {
        return network;
    }

    public String getProvider() {
        return provider;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public void setNetwork(String network) {
        this.network = network;
    }

    public void setProvider(String provider) {
        this.provider = provider;
    }

    public int getNumRecords() {
        return numRecords;
    }

    public void setNumRecords(int numRecords) {
        this.numRecords = numRecords;
    }
    
    public void incrementNumRecords() {
        ++this.numRecords;
    }
    
    @Override
    public String toString() {
        return super.toString() + " provider: " + provider + " owner: " + owner
                + " network: " + network + " numRecords: " + numRecords;
    }

    @Override
    protected Map<String, String> getFieldUnitMap() {
        // Not Implemented.
        return null;
    }

    @Override
    public void finalizeEvent() {
        // Not Implemented.
    }

}
