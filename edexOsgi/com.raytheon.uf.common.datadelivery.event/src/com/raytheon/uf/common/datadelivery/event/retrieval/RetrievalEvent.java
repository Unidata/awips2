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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.stats.StatisticsEvent;

/**
 * 
 * The event that occurs when a successful or failed retrieval is made.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            jsanchez     Initial creation
 * Aug 21, 2012           jsanchez     Made object serializable.
 * Dec 07, 2012 1104      djohnson     Simplify event type hierarchy.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@DynamicSerialize
public abstract class RetrievalEvent extends StatisticsEvent {

    private static final long serialVersionUID = 7910168230909582105L;

    @DynamicSerializeElement
    protected String plugin;

    @DynamicSerializeElement
    protected String owner;

    @DynamicSerializeElement
    protected String network;

    @DynamicSerializeElement
    protected String provider;

    public String getOwner() {
        return owner;
    }

    public String getPlugin() {
        return plugin;
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

    public void setPlugin(String plugin) {
        this.plugin = plugin;
    }

    public void setNetwork(String network) {
        this.network = network;
    }

    public void setProvider(String provider) {
        this.provider = provider;
    }

    @Override
    public String toString() {
        return super.toString() + " plugin: " + plugin + " owner: " + owner
                + "network: " + network;
    }
}
