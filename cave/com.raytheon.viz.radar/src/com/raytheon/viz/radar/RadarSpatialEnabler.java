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
package com.raytheon.viz.radar;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.core.rsc.AbstractSpatialEnabler;
import com.raytheon.viz.radar.rsc.RadarResourceData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 2, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class RadarSpatialEnabler extends AbstractSpatialEnabler {

    private final String elevAngle = "primaryElevationAngle";

    public RadarSpatialEnabler() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.AbstractSpatialEnabler#enable(com.raytheon.
     * uf.common.dataplugin.PluginDataObject,
     * com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData)
     */
    @Override
    public void enable(PluginDataObject d, AbstractRequestableResourceData arrd) {
        if (arrd instanceof RadarResourceData && d instanceof RadarRecord) {
            RadarRecord rr = (RadarRecord) d;
            RadarResourceData rrd = (RadarResourceData) arrd;
            rr.setAddSpatial(!rrd.isLatest());
        }
    }
}
