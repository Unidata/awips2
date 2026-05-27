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
package com.raytheon.uf.viz.grid.radar;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord.ScanType;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.AbstractBaseDataNode;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 * A node which fulfills radar time and data requests by going to EDEX.
 *
 * This can be passed standard radar parameter abbreviations (RR) or their
 * virtual volume versions (RRvirt). For the virtual volume versions, this
 * filters to only {@link ScanType#NORMAL} scan data and uses a virtual volume
 * for the latest volume scan (blends higher tilts from the previous volume scan
 * into the current volume scan).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 13, 2010  4473     rjpeter   Initial creation
 * Aug 15, 2017  6332     bsteffen  Move to viz.grid.radar plugin
 * Jul 07, 2021  8576     randerso  Changed RadarAdapter to support multiple
 *                                  local radars as defined in radarsInUse.txt
 * Jul 28, 2021  8611     randerso  Use RadarRequestableDataFactory
 * May 22, 2024  2037092  mapeters  Add virtual volume support, build constraint
 *                                  map internally
 * Sep 16, 2024  2037941  mapeters  Don't cache virtual availability
 *
 * </pre>
 */
public class RadarRequestableLevelNode extends AbstractBaseDataNode {

    protected final String icao;

    protected final int productCode;

    protected final double tilt;

    protected final String paramAbbrev;

    protected final String standardParamAbbrev;

    protected final String paramName;

    protected final Map<String, RequestConstraint> rcMap;

    /**
     * Copy constructor
     *
     * @param that
     */
    public RadarRequestableLevelNode(RadarRequestableLevelNode that) {
        super(that);
        this.icao = that.icao;
        this.productCode = that.productCode;
        this.tilt = that.tilt;
        this.paramAbbrev = that.paramAbbrev;
        this.standardParamAbbrev = that.standardParamAbbrev;
        this.paramName = that.paramName;
        this.rcMap = that.rcMap;
    }

    /**
     * Create a new requestable Level Node that is LevelNode clone of that and
     * creates a request constraint map from the parameters to use for all
     * requests.
     *
     * @param that
     * @param icao
     * @param productCode
     * @param paramAbbrev
     * @param paramName
     */
    public RadarRequestableLevelNode(LevelNode that, String icao,
            int productCode, String paramAbbrev, String paramName) {
        super(that);
        this.icao = icao;
        this.productCode = productCode;
        this.tilt = that.getLevel().getLevelonevalue();
        this.paramAbbrev = paramAbbrev;
        this.standardParamAbbrev = RadarAsGridUtil
                .getStandardParamAbbrev(paramAbbrev);
        this.paramName = paramName;

        this.rcMap = buildRcMap(icao, productCode, tilt);
    }

    protected Map<String, RequestConstraint> buildRcMap(String icao,
            int productCode, double tilt) {
        Map<String, RequestConstraint> rcMap = new HashMap<>();
        rcMap.put(RadarAdapter.PLUGIN_NAME_QUERY,
                new RequestConstraint(RadarAdapter.RADAR_SOURCE));
        rcMap.put(RadarAdapter.ICAO_QUERY, new RequestConstraint(icao));
        rcMap.put(RadarAdapter.PRODUCT_CODE_QUERY,
                new RequestConstraint(Integer.toString(productCode)));
        rcMap.put(RadarAdapter.TILT_QUERY,
                new RequestConstraint(Double.toString(tilt)));
        if (isVirtualVolume()) {
            rcMap.put(RadarAdapter.SCAN_TYPE_QUERY,
                    new RequestConstraint(ScanType.NORMAL.name()));
        }
        return Collections.unmodifiableMap(rcMap);
    }

    @Override
    public DbQueryRequest getAvailabilityRequest(
            Map<String, RequestConstraint> originalConstraints) {
        return null;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<String, RequestConstraint> originalConstraints, Object response)
            throws DataCubeException {
        Set<TimeAndSpace> resultsSet = RadarUpdater.getInstance()
                .getTimes(this);
        if (resultsSet == null) {
            DataTime[] times;
            try {
                times = CatalogQuery.performTimeQuery(rcMap, false, null);
            } catch (VizException e) {
                throw new DataCubeException(e);
            }
            resultsSet = new HashSet<>(times.length);
            for (DataTime time : times) {
                resultsSet.add(new TimeAndSpace(time,
                        RadarAdapter.getInstance().getCoverage(icao)));
            }
            RadarUpdater.getInstance().setTimes(this, resultsSet);
        }

        /*
         * Virtual availability isn't cached because it would be messy to handle
         * the case where the first tilt comes in for a new volume scan, which
         * may make higher tilts virtually available for that scan as well.
         */
        RadarVirtualTimeAndSpace virtualAvailability = getVirtualAvailability();
        if (virtualAvailability != null) {
            /*
             * Ensure tilt is actually available for the previous scan in order
             * to be blended into the current one
             */
            if (resultsSet.stream().anyMatch(tas -> tas.getTime()
                    .equals(virtualAvailability.getPrevScanTime()))) {
                resultsSet = new HashSet<>(resultsSet);
                resultsSet.add(virtualAvailability);
            }
        }
        return resultsSet;
    }

    @Override
    public DbQueryRequest getDataRequest(
            Map<String, RequestConstraint> originalConstraints,
            Set<TimeAndSpace> requestedAvailability) {
        Map<String, RequestConstraint> newQuery = new HashMap<>(rcMap);
        DbQueryRequest dbRequest = new DbQueryRequest();

        RadarVirtualTimeAndSpace virtualAvailability = getVirtualAvailability();
        Set<DataTime> times = new HashSet<>();
        for (TimeAndSpace ast : requestedAvailability) {
            if (virtualAvailability != null
                    && virtualAvailability.getTime().equals(ast.getTime())) {
                /*
                 * We want to request data for the current volume scan, which
                 * isn't actually available. Request the previous volume scan
                 * data instead, so that we can create a virtual record from
                 * that data in getData().
                 */
                times.add(virtualAvailability.getPrevScanTime());
            } else {
                times.add(ast.getTime());
            }
        }

        String[] timeStrs = times.stream().map(DataTime::toString)
                .toArray(String[]::new);
        newQuery.put(PluginDataObject.DATATIME_ID,
                new RequestConstraint(timeStrs));

        dbRequest.setConstraints(newQuery);
        return dbRequest;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Map<String, RequestConstraint> originalConstraints,
            Set<TimeAndSpace> requestedAvailability, Object response)
            throws DataCubeException {
        Set<AbstractRequestableData> rval = new HashSet<>();

        RadarVirtualTimeAndSpace virtualAvailability = getVirtualAvailability();
        for (RadarRecord record : ((DbQueryResponse) response)
                .getEntityObjects(RadarRecord.class)) {
            try {
                RadarRequestableData rrd = RadarRequestableDataFactory
                        .getInstance()
                        .getRadarRequestableData(record, standardParamAbbrev);
                if (isVirtualVolume()) {
                    /*
                     * Wrap without any virtual time, just so that the data uses
                     * the virtual volume parameter version
                     */
                    rval.add(new RadarVirtualVolumeRequestableData(null, rrd));
                } else {
                    rval.add(rrd);
                }

                if (virtualAvailability != null
                        && virtualAvailability.getPrevScanTime()
                                .equals(record.getDataTime(), true)
                        && requestedAvailability.stream()
                                .anyMatch(a -> a.getTime().equals(
                                        virtualAvailability.getTime()))) {
                    /*
                     * This record is for the previous scan and the current scan
                     * was requested, create a virtual data record so that the
                     * previous scan data gets blended in the current scan.
                     */
                    rval.add(new RadarVirtualVolumeRequestableData(
                            virtualAvailability.getTime(), rrd));
                }
            } catch (VizException e) {
                throw new DataCubeException(e);
            }
        }
        return rval;
    }

    /**
     * Return a virtual time and space if this tilt's data from the previous
     * volume scan should be included in the latest virtual volume. This does
     * not check if this tilt is actually available for the previous scan.
     *
     * @return virtual availability if we should attempt to blend this tilt into
     *         the virtual volume, null otherwise
     */
    protected RadarVirtualTimeAndSpace getVirtualAvailability() {
        if (!isVirtualVolume() || !RadarDisplayManager.getInstance()
                .getCurrentSettings().isVirtualVolumeEnabled()) {
            return null;
        }

        VirtualVolumeInfo virtualVolumeInfo = RadarVolumeScanTracker
                .getInstance(icao, standardParamAbbrev).getVirtualVolumeInfo();
        if (virtualVolumeInfo != null) {
            DataTime latestScan = virtualVolumeInfo.getLatestVolumeScan();
            DataTime prevScan = virtualVolumeInfo.getPrevVolumeScan();
            /*
             * Only tilts in the previous scan that are higher than the current
             * scan's latest tilt are included in the virtual volume. This is
             * primarily because it would be difficult to indicate in the
             * product display/legend that individual tilts are being included.
             */
            if (tilt > virtualVolumeInfo.getLatestTilt()) {
                return new RadarVirtualTimeAndSpace(latestScan,
                        RadarAdapter.getInstance().getCoverage(icao), prevScan);
            }
        }
        return null;
    }

    public String getIcao() {
        return icao;
    }

    public int getProductCode() {
        return productCode;
    }

    public double getTilt() {
        return tilt;
    }

    public String getParamAbbrev() {
        return paramAbbrev;
    }

    public boolean isVirtualVolume() {
        return RadarAsGridUtil.isVirtualVolume(paramAbbrev);
    }

    @Override
    public RadarRequestableLevelNode clone() {
        return new RadarRequestableLevelNode(this);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Objects.hash(icao, paramAbbrev, paramName,
                productCode, rcMap, standardParamAbbrev, tilt);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        RadarRequestableLevelNode other = (RadarRequestableLevelNode) obj;
        return Objects.equals(icao, other.icao)
                && Objects.equals(paramAbbrev, other.paramAbbrev)
                && Objects.equals(paramName, other.paramName)
                && productCode == other.productCode
                && Objects.equals(rcMap, other.rcMap)
                && Objects.equals(standardParamAbbrev,
                        other.standardParamAbbrev)
                && Double.doubleToLongBits(tilt) == Double
                        .doubleToLongBits(other.tilt);
    }
}
