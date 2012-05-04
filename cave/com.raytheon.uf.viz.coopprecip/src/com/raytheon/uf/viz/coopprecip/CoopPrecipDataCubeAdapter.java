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
package com.raytheon.uf.viz.coopprecip;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.pointdata.StaticPlotInfoPV;
import com.raytheon.viz.pointdata.StaticPlotInfoPV.SPIEntry;
import com.vividsolutions.jts.geom.Coordinate;

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
 * Dec 3, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class CoopPrecipDataCubeAdapter implements IDataCubeAdapter {

    private static final String FFG = "FFG";

    private static final String RTP = "RTP";

    private static final List<String> supportedNNNid = Arrays.asList(FFG, RTP);

    private static final PointDataDescription ffgDescription = new PointDataDescription();

    private static final PointDataDescription rtpDescription = new PointDataDescription();

    static {
        ffgDescription.parameters = new ParameterDescription[9];
        ffgDescription.parameters[0] = new ParameterDescription("time",
                Type.LONG);
        ffgDescription.parameters[1] = new ParameterDescription("latitude",
                Type.FLOAT);
        ffgDescription.parameters[2] = new ParameterDescription("longitude",
                Type.FLOAT);
        ffgDescription.parameters[3] = new ParameterDescription("1hr",
                Type.FLOAT);
        ffgDescription.parameters[4] = new ParameterDescription("3hr",
                Type.FLOAT);
        ffgDescription.parameters[5] = new ParameterDescription("6hr",
                Type.FLOAT);
        ffgDescription.parameters[6] = new ParameterDescription("stationId",
                Type.STRING);
        ffgDescription.parameters[7] = new ParameterDescription("id", Type.INT);
        ffgDescription.parameters[8] = new ParameterDescription("dataURI", Type.STRING);

        rtpDescription.parameters = new ParameterDescription[7];
        rtpDescription.parameters[0] = new ParameterDescription("time",
                Type.LONG);
        rtpDescription.parameters[1] = new ParameterDescription("latitude",
                Type.FLOAT);
        rtpDescription.parameters[2] = new ParameterDescription("longitude",
                Type.FLOAT);
        rtpDescription.parameters[3] = new ParameterDescription("precip",
                Type.FLOAT);
        rtpDescription.parameters[4] = new ParameterDescription("stationId",
                Type.STRING);
        rtpDescription.parameters[5] = new ParameterDescription("id", Type.INT);
        rtpDescription.parameters[6] = new ParameterDescription("dataURI", Type.STRING);

    }

    @Override
    public String[] getSupportedPlugins() {
        return new String[] { "textPoints" };
    }

    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws VizException {
        List<List<DataTime>> results = new ArrayList<List<DataTime>>(
                requests.size());
        for (TimeQueryRequest request : requests) {
            DataTime[] result = timeQuery(request.getQueryTerms(),
                    request.isMaxQuery(), request.getBinOffset());
            if (result != null) {
                results.add(Arrays.asList(result));
            } else {
                results.add(Collections.<DataTime> emptyList());
            }
        }
        return results;
    }

    public DataTime[] timeQuery(Map<String, RequestConstraint> queryParams,
            boolean latestOnly, BinOffset binOffset) throws VizException {

        String nnnid = getNNNid(queryParams);

        Set<DataTime> times = new HashSet<DataTime>();
        PointDataContainer pdc = getData(nnnid);
        for (int uriCounter = 0; uriCounter < pdc.getCurrentSz(); uriCounter++) {
            PointDataView pdv = pdc.readRandom(uriCounter);
            DataTime time = new DataTime(new Date(pdv.getLong("time")));
            if (binOffset != null) {
                time = binOffset.getNormalizedTime(time);
            }
            times.add(time);
        }
        return times.toArray(new DataTime[0]);
    }

    private String getNNNid(Map<String, RequestConstraint> queryParams) {
        // we are very specific about what we will accept at this point so make
        // sure the request is proper.
        RequestConstraint nnnidRC = queryParams.get("nnnid");
        if (nnnidRC == null
                || nnnidRC.getConstraintType() != ConstraintType.EQUALS) {
            throw new UnsupportedOperationException(
                    "Request for text Point Data must include an equals constraint on nnnid");
        }
        String nnnid = nnnidRC.getConstraintValue();
        if (!supportedNNNid.contains(nnnid)) {
            throw new UnsupportedOperationException("Unsupported nnnid: "
                    + nnnid);
        }
        return nnnid;
    }

    private PointDataContainer getData(String nnnid) throws VizException {
        List<Object[]> queryResult = DirectDbQuery.executeQuery(
                "select createtime, product from stdtextproducts where nnnid = '"
                        + nnnid + "'", "fxa", DirectDbQuery.QueryLanguage.SQL);
        List<Long> times = new ArrayList<Long>(queryResult.size());
        List<String> products = new ArrayList<String>(queryResult.size());
        for (Object[] objArr : queryResult) {
            times.add((Long) objArr[0]);
            products.add((String) objArr[1]);
        }
        if (FFG.equals(nnnid)) {
            return getFfgData(times, products);
        } else if (RTP.equals(nnnid)) {
            return getRtpData(times, products);
        } else {
            return null;
        }
    }

    private PointDataContainer getFfgData(List<Long> times,
            List<String> products) throws VizException {
        Map<String, Coordinate> stationCoordMap = getFfgCoords();

        PointDataContainer pdc = PointDataContainer.build(ffgDescription);
        for (int i = 0; i < times.size(); i++) {
            long time = times.get(i);
            String product = products.get(i);
            int index = 0;
            for (String line : product.split("\n")) {
                if (line.startsWith(":")) {
                    continue;
                } else if (index == 0 && line.startsWith(".B")) {
                    index = 1;
                } else if (line.startsWith(".END")) {
                    index = 0;
                } else if (index > 0 && line.contains(":")
                        && line.contains("/")) {
                    String[] parts = line.split(":");
                    String station = null;
                    switch (parts.length) {
                    case 2:
                        if (parts[1].contains("/")) {
                            station = parts[0].trim();
                            parts = parts[1].split("/");
                            break;
                        }
                    case 1:
                        parts = parts[0].split(" ", 2);
                        station = parts[0].trim();
                        parts = parts[1].split("/");
                        break;
                    case 3:
                        station = parts[0].trim();
                        parts = parts[2].split("/");
                        break;
                    default:
                        continue;
                    }
                    Coordinate coord = stationCoordMap.get(station);
                    if (coord != null) {
                        PointDataView pdv = pdc.append();
                        pdv.setLong("time", time);
                        pdv.setFloat("longitude", (float) coord.x);
                        pdv.setFloat("latitude", (float) coord.y);
                        pdv.setFloat("1hr", Float.valueOf(parts[0]));
                        pdv.setFloat("3hr", Float.valueOf(parts[1]));
                        pdv.setFloat("6hr", Float.valueOf(parts[2]));
                        pdv.setString("stationId", station);
                        pdv.setString("dataURI", "/textPoints/" + station + "/" + time);
                        // TODO this id is not really guaranteed to be unique
                        pdv.setInt("id", ((int) time) + station.hashCode());
                    }
                }
            }
        }
        return pdc;
    }

    private PointDataContainer getRtpData(List<Long> times,
            List<String> products) throws VizException {
        Map<String, SPIEntry> stationCoordMap = getRtpSpi();

        PointDataContainer pdc = PointDataContainer.build(rtpDescription);
        for (int i = 0; i < times.size(); i++) {
            long time = times.get(i);
            String product = products.get(i);
            // Adapted from shef-read.p in the awips1 baseline.
            int index = 0;
            for (String line : product.split("\n")) {
                if (line.startsWith(":")) {
                    continue;
                } else if (index == 0 && line.startsWith(".B")) {
                    String[] parts = line.split(" ");
                    int c = 0;
                    for (String parameter : parts[parts.length - 1].split("/")) {
                        if (parameter.startsWith("PP")) {
                            index = c;
                            break;
                        } else if (!parameter.startsWith("D")) {
                            c += 1;
                        }
                    }
                } else if (line.startsWith(".END")) {
                    index = 0;
                } else if (index > 0 && line.contains(":")
                        && line.contains("/")) {
                    String[] parts = line.split(":");
                    String station = null;
                    switch (parts.length) {
                    case 2:
                        if (parts[1].contains("/")) {
                            station = parts[0].trim();
                            parts = parts[1].split("/");
                            break;
                        }
                    case 1:
                        parts = parts[0].split(" ", 2);
                        station = parts[0].trim();
                        parts = parts[1].split("/");
                        break;
                    case 3:
                        station = parts[0].trim();
                        parts = parts[2].split("/");
                        break;
                    default:
                        continue;
                    }
                    float precip = -9999f;
                    String value = "";
                    if (parts.length > index) {
                        parts[index].trim();
                    }
                    if (value.isEmpty() || value.equals("M")
                            || value.equals("T")) {
                        precip = -9999f;
                    } else {
                        precip = Float.parseFloat(value);
                    }
                    SPIEntry coord = stationCoordMap.get(station);
                    if (coord != null) {
                        PointDataView pdv = pdc.append();
                        pdv.setLong("time", time);
                        pdv.setFloat("longitude", (float) coord.latlon.x);
                        pdv.setFloat("latitude", (float) coord.latlon.y);
                        pdv.setFloat("precip", precip);
                        pdv.setString("stationId", station);
                        pdv.setString("dataURI", "/textPoints/" + station + "/" + time);
                        // TODO this id is not really guaranteed to be unique
                        pdv.setInt("id", ((int) time) + station.hashCode());
                    }
                }
            }
        }
        return pdc;
    }

    private Map<String, Coordinate> getFfgCoords() throws VizException {
        Map<String, Coordinate> result = new HashMap<String, Coordinate>();
        String cwa = LocalizationManager.getInstance().getCurrentSite();
        List<Object[]> queryResult = DirectDbQuery.executeQuery(
                "select lat, lon, fips, state from mapdata.county where cwa = '"
                        + cwa + "'", "maps", DirectDbQuery.QueryLanguage.SQL);
        for (Object[] arr : queryResult) {
            Number lat = (Number) arr[0];
            Number lon = (Number) arr[1];
            String fips = (String) arr[2];
            String state = (String) arr[3];
            String stationId = state + "C" + fips.substring(2);
            result.put(stationId,
                    new Coordinate(lon.doubleValue(), lat.doubleValue()));
        }
        queryResult = DirectDbQuery.executeQuery(
                "select lat, lon, zone, state from mapdata.zone where cwa = '"
                        + cwa + "'", "maps", DirectDbQuery.QueryLanguage.SQL);
        for (Object[] arr : queryResult) {
            Number lat = (Number) arr[0];
            Number lon = (Number) arr[1];
            String zone = (String) arr[2];
            String state = (String) arr[3];
            String stationId = state + "Z" + zone;
            result.put(stationId,
                    new Coordinate(lon.doubleValue(), lat.doubleValue()));
        }
        return result;
    }

    private Map<String, SPIEntry> getRtpSpi() {
        return StaticPlotInfoPV.readStaticPlotInfoPV("basemaps/coopPrecip.spi")
                .getSpiList();
    }

    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            Map<String, RequestConstraint> queryParams) throws VizException {
        return getPoints(plugin, parameters, null, queryParams);

    }

    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams)
            throws VizException {
        String nnnid = getNNNid(queryParams);
        return getData(nnnid);
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj)
            throws VizDataCubeException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws VizDataCubeException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws VizDataCubeException {
        // TODO Auto-generated method stub

    }

    @Override
    public List<Object> getData(LayerProperty property, int timeOut)
            throws VizException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void initInventory() {
        // TODO Auto-generated method stub

    }

    @Override
    public Object getInventory() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        List<Map<String, RequestConstraint>> result = new ArrayList<Map<String, RequestConstraint>>(
                1);
        result.add(constraints);
        return result;
    }

}
