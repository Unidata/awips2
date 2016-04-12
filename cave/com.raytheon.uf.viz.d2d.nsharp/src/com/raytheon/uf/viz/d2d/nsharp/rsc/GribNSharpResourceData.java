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
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.viz.soundingrequest.NcSoundingQuery;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.topo.TopoException;
import com.raytheon.uf.common.topo.TopoQuery;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * {@link D2DNSharpResourceData} for loading gridded data into nsharp.
 * 
 * TODO rename to GridNSharpResourceData, grib only remains for backward
 * compatibility in xml.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jul 26, 2011           bsteffen  Initial creation
 * Feb 15, 2013  1638     mschenke  Got rid of viz/edex topo classes and moved
 *                                  into common
 * 04/27/2015   RM#6674&7787 Chin Chen   support model sounding query data interpolation and nearest point option                       
 * Aug 03, 2015  3861     bsteffen  Automatically determine sounding type from
 *                                  request constraints.
 * Mar 01, 2016 RM14647 mgamazayhikov  Added constructor.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GribNSharpResourceData extends D2DNSharpResourceData {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribNSharpResourceData.class);

    private float surfaceElevation = NcSoundingProfile.MISSING;

    public GribNSharpResourceData() {
        super();
    }

    public GribNSharpResourceData(String soundingType) {
        super(soundingType);
    }

    public GribNSharpResourceData(String soundingType, String soundingTitle) {
        super(soundingType,soundingTitle);
    }

    @Override
    protected void preparePointInfo() throws VizException {
        /* Always keep the type in sync with what is in the metadata map. */
        RequestConstraint typeConstraint = metadataMap.get("info.datasetId");
        if (typeConstraint != null) {
            this.setSoundingType(typeConstraint.getConstraintValue());
        }
    }

    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        float[][] latLon = { { (float) coordinate.y, (float) coordinate.x } };
        String refTimeStr = formatTimestamp(stnInfo.getReftime());
        String validTimeStr = formatTimestamp(stnInfo.getRangestarttime());
      //RM#6674                         
        NsharpConfigManager mgr =NsharpConfigManager.getInstance();
        NsharpConfigStore configStore = mgr.retrieveNsharpConfigStoreFromFs();
        boolean gridInterpolation;
		if(configStore != null){
			gridInterpolation = configStore.getGraphProperty().isGridInterpolation();
		}
		else
			gridInterpolation = true; //by default
        NcSoundingCube cube = NcSoundingQuery.mdlSoundingQueryByLatLon(
                refTimeStr, validTimeStr, latLon, "grid", getSoundingType(),
                false, "-1", gridInterpolation);
        if ((cube != null) && !cube.getSoundingProfileList().isEmpty()) {
            NcSoundingProfile profileList = cube.getSoundingProfileList()
                    .get(0);
            // If stationElevation is not set, set it to the topo value.
            if (profileList.getStationElevation() == NcSoundingProfile.MISSING) {
                if (surfaceElevation == NcSoundingProfile.MISSING) {
                    try {
                        TopoQuery topoQuery = TopoQuery.getInstance();
                        surfaceElevation = (float) topoQuery
                                .getHeight(coordinate);
                    } catch (TopoException e) {
                        statusHandler.error("Unable to retrieve topo", e);
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
                            float p = p1 + (((h - h1) * (p1 - p2)) / (h1 - h2));
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
