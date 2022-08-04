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
package com.raytheon.viz.awipstools.capabilities;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * Provides the capability to set the Distance Scale Unit to 'Statute Miles' (SM),
 * 'Nautical Miles' (NM), or 'Kilometers' (KM).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2018 56957      edebebe     Initial creation
 *
 * </pre>
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DistanceScaleUnitsCapability extends AbstractCapability {

    /**
     * Initialize the Distance Scale Unit to:
     *   SM - Statute Miles
     */
    @XmlAttribute
    private String distanceScaleUnit = "SM";

    /**
     * @return the distanceScaleUnit
     */
    public String getDistanceScaleUnit() {
        return distanceScaleUnit;
    }

    /**
     * @param distanceScaleUnit
     *            the distanceScaleUnit to set
     */
    public void setDistanceScaleUnit(String distanceScaleUnit) {
        if ((this.distanceScaleUnit == null) && (distanceScaleUnit == null)) {
            return;
        }
        if ((this.distanceScaleUnit == null) || !this.distanceScaleUnit.equals(distanceScaleUnit)) {
            this.distanceScaleUnit = distanceScaleUnit;
            this.capabilityChanged();
        }
    }

    @Override
    public AbstractCapability clone() {
        DistanceScaleUnitsCapability ds = new DistanceScaleUnitsCapability();
        ds.distanceScaleUnit= distanceScaleUnit;
        return ds;
    }

}