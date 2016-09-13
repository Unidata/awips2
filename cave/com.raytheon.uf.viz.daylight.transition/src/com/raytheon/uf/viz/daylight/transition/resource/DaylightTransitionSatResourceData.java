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
package com.raytheon.uf.viz.daylight.transition.resource;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.satellite.rsc.SatResourceData;

/**
 * 
 * An extension of the {@link SatResourceData} that constructs a
 * {@link DaylightTransitionSatResource} in order to block out data when there
 * is no sunlight.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 28, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DaylightTransitionSatResourceData extends SatResourceData {

    private int sunDelta = 75;

    public DaylightTransitionSatResourceData() {

    }

    public DaylightTransitionSatResourceData(SatResourceData resourceData) {
        setMetadataMap(resourceData.getMetadataMap());
    }

    public SatResourceData toSatResourceData() {
        SatResourceData resourceData = new SatResourceData();
        resourceData.setMetadataMap(getMetadataMap());
        return resourceData;
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        records = new SatelliteRecord[objects.length];
        for (int i = 0; i < objects.length; i++) {
            records[i] = (SatelliteRecord) objects[i];
        }
        return new DaylightTransitionSatResource(this, loadProperties);
    }

    public int getSunDelta() {
        return sunDelta;
    }

    public void setSunDelta(int sunDelta) {
        this.sunDelta = sunDelta;
    }

}
