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
package com.raytheon.uf.viz.acarssounding;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingConstants;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingLayer;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.d2d.nsharp.SoundingLayerBuilder;
import com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import si.uom.NonSI;
import si.uom.SI;
import tec.uom.se.AbstractUnit;

/**
 * Provides sounding data to nsharp from aircraft reports.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2013            bsteffen     Initial creation
 * Jul 23, 2014 3410       bclement     preparePointInfo() calls unpackResultLocation()
 * Dec 17, 2015 5215       dgilling     Set point name to stationId.
 * Mar 17, 2016 5459       tgurney      Compute specific humidity from mixing ratio
 * Jan 15, 2019 7697       bsteffen     Add aircraft info to location dislay info.
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AcarsSndNSharpResourceData extends D2DNSharpResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AcarsSndNSharpResourceData.class);

    private static final String LONGITUDE = "location.longitude";

    private static final String LATITUDE = "location.latitude";

    private static final String STATION_ID = "location.stationId";

    public AcarsSndNSharpResourceData() {
        super("MDCRS");
    }

    @Override
    protected void preparePointInfo() throws VizException {
        if ((coordinate == null) || (pointName == null)) {
            DbQueryRequest request = new DbQueryRequest();
            request.setConstraints(getMetadataMap());
            request.addFields(new String[] { STATION_ID, LONGITUDE, LATITUDE });
            request.setDistinct(true);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);

            for (Map<String, Object> result : response.getResults()) {
                if (pointName == null) {
                    pointName = (String) result.get(STATION_ID);
                }

                if (coordinate == null) {
                    coordinate = unpackResultLocation(result, LONGITUDE,
                            LATITUDE);
                }
            }
        }
    }

    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(ACARSSoundingRecord.class);
        request.setLimit(1);
        request.setConstraints(new HashMap<>(
                getMetadataMap()));
        request.addConstraint("dataTime", new RequestConstraint(new DataTime(
                stnInfo.getReftime()).toString()));
        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            ACARSSoundingRecord[] records = response
                    .getEntityObjects(ACARSSoundingRecord.class);
            if (records.length > 0) {
                ACARSSoundingRecord record = records[0];
                String phase = record.getPhase();
                String loc = record.getTailNumber();
                if(ACARSSoundingConstants.ASCENDING_PHASE.equals(phase)){
                    loc = loc + " Asc.";
                }else if(ACARSSoundingConstants.DESCENDING_PHASE.equals(phase)){
                    loc = loc + " Desc.";
                }else if(phase != null){
                    loc = loc + " " + phase;
                }
                stnInfo.setLocationDetails(loc);
                List<NcSoundingLayer> layers = new ArrayList<>(
                        record.getLevels().size());
                for (ACARSSoundingLayer layer : record.getLevels()) {
                    SoundingLayerBuilder builder = new SoundingLayerBuilder();
                    if (layer.getDwpt() != null) {
                        builder.addDewpoint(layer.getDwpt(), SI.KELVIN);
                    }
                    if (layer.getTemp() != null) {
                        builder.addTemperature(layer.getTemp(), SI.KELVIN);
                    }
                    if (layer.getWindDirection() != null) {
                        builder.addWindDirection(layer.getWindDirection(),
                                NonSI.DEGREE_ANGLE);
                    }
                    if (layer.getWindSpeed() != null) {
                        builder.addWindSpeed(layer.getWindSpeed(),
                                SI.METRE_PER_SECOND);
                    }
                    if (layer.getPressure() != null) {
                        builder.addPressure(layer.getPressure(), SI.PASCAL);
                    }
                    if (layer.getFlightLevel() != null) {
                        builder.addHeight(layer.getFlightLevel(), SI.METRE);
                    }
                    if (layer.getMixingRatio() != null) {
                        double mixingRatio = layer.getMixingRatio();
                        if (mixingRatio != 0) {
                            double specHum = mixingRatio / (mixingRatio + 1.0);
                            builder.addSpecificHumidity(specHum, AbstractUnit.ONE);
                        }
                    }
                    layers.add(builder.toNcSoundingLayer());
                }
                Collections.sort(layers, NsharpDataHandling
                        .reversePressureHeightWindComparator());

                NcSoundingProfile profile = new NcSoundingProfile();
                profile.setSoundingLyLst(layers);
                NcSoundingCube cube = new NcSoundingCube(Arrays.asList(profile));
                cube.setRtnStatus(QueryStatus.OK);
                return cube;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return null;
    }
}
