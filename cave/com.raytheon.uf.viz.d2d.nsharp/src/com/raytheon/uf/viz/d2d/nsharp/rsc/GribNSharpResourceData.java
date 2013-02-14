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
package com.raytheon.uf.viz.d2d.nsharp.rsc;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.topo.ITopoQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.topo.TopoQuery;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GribNSharpResourceData extends D2DNSharpResourceData {

    private float surfaceElevation = NcSoundingProfile.MISSING;

    public GribNSharpResourceData() {
        super();
    }

    public GribNSharpResourceData(String soundingType) {
        super(soundingType);
    }

    @Override
    protected void preparePointInfo() throws VizException {
        // everything should already be set
        return;
    }

    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        float[][] latLon = { { (float) coordinate.y, (float) coordinate.x } };
        String refTimeStr = formatTimestamp(stnInfo.getReftime());
        String validTimeStr = formatTimestamp(stnInfo.getRangestarttime());
        NcSoundingCube cube = NcSoundingQuery.mdlSoundingQueryByLatLon(
                refTimeStr, validTimeStr, latLon, "grid", getSoundingType(),
                false, "-1");
        if (cube != null && !cube.getSoundingProfileList().isEmpty()) {
            NcSoundingProfile profileList = cube.getSoundingProfileList()
                    .get(0);
            // If stationElevation is not set, set it to the topo value.
            if (profileList.getStationElevation() == NcSoundingProfile.MISSING) {
                if (surfaceElevation == NcSoundingProfile.MISSING) {
                    ITopoQuery topoQuery = TopoQuery.getInstance();
                    if (topoQuery != null) {
                        surfaceElevation = (float) topoQuery
                                .getHeight(coordinate);
                    }
                }
                profileList.setStationElevation(surfaceElevation);
                NcSoundingLayer sfcLayer = new NcSoundingLayer();
                sfcLayer.setPressure(profileList.getSfcPress());
                sfcLayer.setGeoHeight(profileList.getStationElevation());
                List<NcSoundingLayer> layers = profileList.getSoundingLyLst();
                for (int i = 0; i < layers.size(); i++) {
                    if (layers.get(i).getGeoHeight() > sfcLayer.getGeoHeight()) {
                        if (i > 0) {
                            // The profile list surface pressure is currently
                            // being set to the sea level pressure, which is not
                            // surface pressure, so attempt to interpolate a
                            // surface pressure.
                            float p1 = layers.get(i - 1).getPressure();
                            float p2 = layers.get(i).getPressure();
                            float h1 = layers.get(i - 1).getGeoHeight();
                            float h2 = layers.get(i).getGeoHeight();
                            float h = sfcLayer.getGeoHeight();
                            float p = p1 + (h - h1) * (p1 - p2) / (h1 - h2);
                            sfcLayer.setPressure(p);
                        }
                        layers.add(i, sfcLayer);
                        break;
                    }
                }
            }
        }
        return cube;
    }
}
