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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datacube.IDataCubeAdapter;
import com.raytheon.viz.pointdata.StaticPlotInfoPV;
import com.raytheon.viz.pointdata.StaticPlotInfoPV.SPIEntry;
import org.locationtech.jts.geom.Coordinate;

/**
 * 
 * {@link IDataCubeAdapter} for coop precip data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 03, 2010            bsteffen    Initial creation
 * Aug 14, 2012  #1055     dgilling    Fix getData regression from
 *                                     fxatext schema changes.
 * Sep 09, 2013  #2277     mschenke    Got rid of ScriptCreator references
 * Aug 18, 2015   4763     rjpeter     Use Number in blind cast.
 * Feb 05, 2018   6811     njensen     Fixed some parsing errors for RTP data
 * Apr 11, 2018   6655     njensen     Sort times after querying times so menu
 *                                     time matches latest data
 * Jul 26, 2018   6811     njensen     Fixed parsing errors for TRACE, NA, and m
 * Sep 12, 2018   6811     njensen     Catch NumberFormatException for RTP and continue
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class CoopPrecipDataCubeAdapter implements IDataCubeAdapter {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(CoopPrecipDataCubeAdapter.class);

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
        ffgDescription.parameters[8] = new ParameterDescription("dataURI",
                Type.STRING);

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
        rtpDescription.parameters[6] = new ParameterDescription("dataURI",
                Type.STRING);
    }

    @Override
    public String[] getSupportedPlugins() {
        return new String[] { "textPoints" };
    }

    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws DataCubeException {
        List<List<DataTime>> results = new ArrayList<>(requests.size());
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
            boolean latestOnly, BinOffset binOffset) throws DataCubeException {

        String nnnid = getNNNid(queryParams);

        Set<DataTime> times = new TreeSet<>();
        /*
         * latestOnly is somewhat meaningless as the latest may be outside of
         * your area. We have to retrieve the data cause there's not enough
         * metadata about the products.
         */
        PointDataContainer pdc = getData(nnnid);
        for (int uriCounter = 0; uriCounter < pdc
                .getCurrentSz(); uriCounter++) {
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
            throw new UnsupportedOperationException(
                    "Unsupported nnnid: " + nnnid);
        }
        return nnnid;
    }

    private PointDataContainer getData(String nnnid) throws DataCubeException {
        List<Object[]> queryResult;
        try {
            queryResult = DirectDbQuery.executeQuery(
                    "select refTime, product from stdtextproducts where nnnid = '"
                            + nnnid + "'",
                    "fxa", DirectDbQuery.QueryLanguage.SQL);
        } catch (VizException e) {
            throw new DataCubeException(e);
        }

        return processQueryResults(nnnid, queryResult);
    }

    private PointDataContainer processQueryResults(String nnnid,
            List<Object[]> queryResult) throws DataCubeException {
        List<Long> times = new ArrayList<>(queryResult.size());
        List<String> products = new ArrayList<>(queryResult.size());
        for (Object[] objArr : queryResult) {
            times.add(((Number) objArr[0]).longValue());
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

    private PointDataContainer getDataAtTime(String nnnid,
            Map<String, RequestConstraint> queryParams)
            throws DataCubeException {
        List<Object[]> queryResult;
        try {
            RequestConstraint rqc = queryParams.get("dataTime");
            if (rqc == null) {
                rqc = queryParams.get("dataTime.refTime");
            }
            if (rqc == null) {
                throw new UnsupportedOperationException(
                        "Requires dataTime or dataTime.refTime when requesting coop precip data at a specific time: "
                                + queryParams);
            }
            if (rqc.getConstraintType() == RequestConstraint.ConstraintType.EQUALS) {
                queryResult = DirectDbQuery.executeQuery(
                        "select refTime, product from stdtextproducts where nnnid = '"
                                + nnnid + "' and refTime = "
                                + new DataTime(rqc.getConstraintValue())
                                        .getRefTime().getTime(),
                        "fxa", DirectDbQuery.QueryLanguage.SQL);
            } else if (rqc
                    .getConstraintType() == RequestConstraint.ConstraintType.BETWEEN) {
                String[] stringTimes = rqc.getConstraintValue().split("--");
                Date firstTime = new DataTime(stringTimes[0]).getRefTime();
                Date lastTime = new DataTime(stringTimes[1]).getRefTime();
                queryResult = DirectDbQuery.executeQuery(
                        "select refTime, product from stdtextproducts where nnnid = '"
                                + nnnid + "' and refTime between "
                                + firstTime.getTime() + " and "
                                + lastTime.getTime(),
                        "fxa", DirectDbQuery.QueryLanguage.SQL);
            } else {
                throw new UnsupportedOperationException(
                        "Only EQUALS and BETWEEN supported for retrieving coop precip data at a specific time: "
                                + queryParams);
            }
        } catch (VizException e) {
            throw new DataCubeException(e);
        }

        return processQueryResults(nnnid, queryResult);
    }

    private PointDataContainer getFfgData(List<Long> times,
            List<String> products) throws DataCubeException {
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
                        pdv.setString("dataURI",
                                "/textPoints/" + station + "/" + time);
                        // TODO this id is not really guaranteed to be unique
                        pdv.setInt("id", ((int) time) + station.hashCode());
                    }
                }
            }
        }
        return pdc;
    }

    private PointDataContainer getRtpData(List<Long> times,
            List<String> products) {
        Map<String, SPIEntry> stationCoordMap = getRtpSpi();

        PointDataContainer pdc = PointDataContainer.build(rtpDescription);
        for (int i = 0; i < times.size(); i++) {
            long time = times.get(i);
            String product = products.get(i);
            // Adapted from shef-read.p in the awips1 baseline.
            int index = 0;
            for (String line : product.split("\n")) {
                if (line.startsWith(":") || line.contains("www.")) {
                    continue;
                } else if (index == 0 && line.startsWith(".B")) {
                    String[] parts = line.split(" ");
                    int c = 0;
                    for (String parameter : parts[parts.length - 1]
                            .split("/")) {
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
                        value = parts[index].trim();
                    }
                    /*
                     * Check to see if a report forgot a / to separate precip
                     * and snow, such "0.04     M". If so, get rid of the spaces
                     * and take the first value for precip.
                     */
                    if (value.contains(" ")) {
                        value = value.split(" ")[0];
                    }

                    /*
                     * Variants of missing seen so far are M, MM, MMMM, 0.00M,
                     * m, and ---. M stands for Missing Data. T stands for Trace
                     * Amounts. N can be the first half of N/A where the split
                     * on / above separated out the A. NA is also possible, as
                     * is TRACE.
                     */
                    if (value.isEmpty() || value.contains("M")
                            || "m".equals(value) || ("---").equals(value)
                            || value.contains("T") || value.contains("N")) {
                        precip = -9999f;
                    } else {
                        try {
                            precip = Float.parseFloat(value);
                        } catch (NumberFormatException e) {
                            logger.debug(
                                    "Skipping piece of data due to failing to parse line "
                                            + line,
                                    e);
                            continue;
                        }
                    }
                    /*
                     * If the station name isn't in your SPI file, we have to
                     * throw it out because we have no idea what the lat/lon is
                     * to place the station.
                     */
                    SPIEntry coord = stationCoordMap.get(station);
                    if (coord != null) {
                        PointDataView pdv = pdc.append();
                        pdv.setLong("time", time);
                        pdv.setFloat("longitude", (float) coord.latlon.x);
                        pdv.setFloat("latitude", (float) coord.latlon.y);
                        pdv.setFloat("precip", precip);
                        pdv.setString("stationId", station);
                        pdv.setString("dataURI",
                                "/textPoints/" + station + "/" + time);
                        // TODO this id is not really guaranteed to be unique
                        pdv.setInt("id", ((int) time) + station.hashCode());
                    }
                }
            }
        }
        return pdc;
    }

    private Map<String, Coordinate> getFfgCoords() throws DataCubeException {
        Map<String, Coordinate> result = new HashMap<>();
        String cwa = LocalizationManager.getInstance().getCurrentSite();
        List<Object[]> queryResult;
        try {
            queryResult = DirectDbQuery.executeQuery(
                    "select lat, lon, fips, state from mapdata.county where cwa = '"
                            + cwa + "'",
                    "maps", DirectDbQuery.QueryLanguage.SQL);
        } catch (VizException e) {
            throw new DataCubeException(e);
        }
        for (Object[] arr : queryResult) {
            Number lat = (Number) arr[0];
            Number lon = (Number) arr[1];
            String fips = (String) arr[2];
            String state = (String) arr[3];
            String stationId = state + "C" + fips.substring(2);
            result.put(stationId,
                    new Coordinate(lon.doubleValue(), lat.doubleValue()));
        }
        try {
            queryResult = DirectDbQuery.executeQuery(
                    "select lat, lon, zone, state from mapdata.zone where cwa = '"
                            + cwa + "'",
                    "maps", DirectDbQuery.QueryLanguage.SQL);
        } catch (VizException e) {
            throw new DataCubeException(e);
        }
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
            Map<String, RequestConstraint> queryParams)
            throws DataCubeException {
        return getPoints(plugin, parameters, null, queryParams);

    }

    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams)
            throws DataCubeException {
        String nnnid = getNNNid(queryParams);
        return getDataAtTime(nnnid, queryParams);
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj)
            throws DataCubeException {
        return null;
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws DataCubeException {
        return null;
    }

    @Override
    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws DataCubeException {
        // just here to match the interface
    }

    @Override
    public PluginDataObject[] getData(
            Map<String, RequestConstraint> constraints,
            DataTime[] selectedTimes) throws DataCubeException {
        return null;
    }

    @Override
    public void initInventory() {
        // just here to match the interface
    }

    @Override
    public Object getInventory() {
        return null;
    }

    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        List<Map<String, RequestConstraint>> result = new ArrayList<>(1);
        result.add(constraints);
        return result;
    }

}
