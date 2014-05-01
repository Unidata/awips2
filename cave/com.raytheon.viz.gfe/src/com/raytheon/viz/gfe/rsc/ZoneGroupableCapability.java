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
/**
 * 
 */
package com.raytheon.viz.gfe.rsc;

import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * @author wdougherty
 * 
 */
public class ZoneGroupableCapability extends AbstractCapability {

    protected transient boolean zoneGrouping = false;

    /**
     * @return the zoneGrouping
     */
    public boolean isZoneGrouping() {
        return zoneGrouping;
    }

    /**
     * @param zoneGrouping
     *            the zoneGrouping to set
     */
    public void setZoneGrouping(boolean zoneGrouping) {
        this.zoneGrouping = zoneGrouping;
    }

    @Override
    public AbstractCapability clone() {
        ZoneGroupableCapability zgc = new ZoneGroupableCapability();
        zgc.zoneGrouping = zoneGrouping;
        return zgc;
    }
}
