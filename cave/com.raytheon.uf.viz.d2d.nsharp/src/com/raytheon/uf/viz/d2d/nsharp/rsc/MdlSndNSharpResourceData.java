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
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;

import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

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
 * Jul 23, 2014 3410       bclement     preparePointInfo() calls unpackResultLocation()
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MdlSndNSharpResourceData extends D2DNSharpResourceData {

    private static final String STATION_NAME = "location.stationId";

    private static final String LONGITUDE = "location.longitude";

    private static final String LATITUDE = "location.latitude";

    public MdlSndNSharpResourceData() {
        super();
    }

    public MdlSndNSharpResourceData(String soundingType) {
        super(soundingType);
    }

    @Override
    protected void preparePointInfo() throws VizException {
        if (coordinate == null || pointName == null) {
            DbQueryRequest request = new DbQueryRequest();
            request.setConstraints(getMetadataMap());
            request.addFields(new String[] { STATION_NAME, LONGITUDE, LATITUDE });
            request.setDistinct(true);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);

            for (Map<String, Object> result : response.getResults()) {
                if (pointName == null) {
                    pointName = (String) result.get(STATION_NAME);
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
        double[][] latLon = { { stnInfo.getLatitude(), stnInfo.getLongitude() } };
        return NcSoundingQuery.pfcSoundingQueryByLatLon(stnInfo.getReftime()
                .getTime(), stnInfo.getRangestarttime().getTime(), latLon,
                stnInfo.getSndType(), NcSoundingLayer.DataType.ALLDATA, false,
                "-1");
    }

}
