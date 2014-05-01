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
package com.raytheon.viz.volumebrowser.datacatalog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.pointdata.StaticPlotInfoPV;
import com.raytheon.viz.pointdata.StaticPlotInfoPV.SPIEntry;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.raytheon.viz.volumebrowser.xml.VbSource;
import com.raytheon.viz.volumebrowser.xml.VbSourceList;

/**
 * Catalog for model sounding data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2011            mschenke     Initial creation
 * May 02, 2013 1949       bsteffen    Force ModelSounding in Vb to play nicely
 *                                     with others.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ModelSoundingCatalog extends PointDataCatalog {

    private static Map<String, String> typeMap = null;

    private static final String pluginName = "modelsounding";

    private static final String typeKey = "reportType";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.PointDataCatalog#isValidSelection
     * (com.raytheon.viz.volumebrowser.vbui.SelectedData)
     */
    @Override
    protected boolean isValidSelection(SelectedData selData) {
        return super.isValidSelection(new SelectedData(
                selData.getSourcesText(), pluginName, selData.getFieldsText(),
                selData.getFieldsKey(), selData.getPlanesText(), selData
                        .getPlanesKey(), selData.getUniqueKey()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.volumebrowser.datacatalog.PointDataCatalog#
     * getSupportedSources()
     */
    @Override
    public List<String> getSupportedSources() {
        return new ArrayList<String>(getTypeMap().keySet());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.PointDataCatalog#getPlugins
     * (com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu)
     */
    @Override
    protected String[] getPlugins(ViewMenu setting) {
        return new String[] { pluginName };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.PointDataCatalog#getType(java
     * .lang.String)
     */
    @Override
    protected String getType(String sourceKey) {
        return getTypeMap().get(sourceKey);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.PointDataCatalog#getTypeKey
     * (java.lang.String)
     */
    @Override
    protected String getTypeKey(String sourceKey) {
        return typeKey;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.PointDataCatalog#getAvailableData
     * (com.raytheon.viz.volumebrowser.datacatalog.AvailableDataRequest)
     */
    @Override
    public void getAvailableData(AvailableDataRequest request) {
        super.getAvailableData(new ModelSoundingAvailableDataRequest(request));
    }

    public class ModelSoundingAvailableDataRequest extends
            DelegateAvailableRequest {
        public ModelSoundingAvailableDataRequest(AvailableDataRequest request) {
            super(modifySelectedSources(request));
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.volumebrowser.datacatalog.DelegateAvailableRequest
         * #addAvailableSource(java.lang.String)
         */
        @Override
        public void addAvailableSource(String source) {
            for (String modSource : getTypeMap().keySet()) {
                super.addAvailableField(modSource);
            }
        }

    }

    private static AvailableDataRequest modifySelectedSources(
            AvailableDataRequest request) {
        String[] selectedSources = request.getSelectedSources();
        if (selectedSources != null) {
            for (int i = 0; i < selectedSources.length; i++) {
                if (selectedSources[i].startsWith(pluginName)) {
                    selectedSources[i] = pluginName;
                }
            }
        }
        return request;
    }

    private static synchronized Map<String, String> getTypeMap() {
        if (typeMap == null) {
            long t0 = System.currentTimeMillis();
            typeMap = new HashMap<String, String>();
            try {
                // The code in this try block is an optimization for the current
                // version of postgres. Currently there are only two types, GFS
                // and ETA, if you query for all distinct types it can take as
                // long as 12 seconds. If you query for the types one at a time
                // then it takes less than 100ms for each of the two queries.
                DbQueryRequest request = new DbQueryRequest();
                request.setDistinct(true);
                request.setLimit(1);
                request.addConstraint("pluginName", new RequestConstraint(
                        pluginName));
                request.addRequestField(typeKey);
                for (VbSource source : VbSourceList.getInstance().getEntries()) {
                    if (source.getKey().startsWith(pluginName)) {
                        String type = source.getKey().replace(pluginName, "");
                        request.addConstraint(typeKey, new RequestConstraint(
                                type));
                        DbQueryResponse response = (DbQueryResponse) ThriftClient
                                .sendRequest(request);
                        if (!response.getResults().isEmpty()) {
                            typeMap.put(source.getKey(), type);
                        }
                    }
                }
            } catch (VizException e) {
                // If something went wrong try to just query for all distinct
                // types.
                DbQueryRequest request = new DbQueryRequest();
                request.setDistinct(true);
                request.addConstraint("pluginName", new RequestConstraint(
                        pluginName));
                request.addRequestField(typeKey);
                try {
                    DbQueryResponse response = (DbQueryResponse) ThriftClient
                            .sendRequest(request);
                    for (Map<String, Object> result : response.getResults()) {
                        String type = String.valueOf(result.get(typeKey));
                        typeMap.put(pluginName + type, type);
                    }
                } catch (VizException e1) {
                    statusHandler.handle(Priority.PROBLEM,
                            e1.getLocalizedMessage(), e1);
                }
            }
            System.out.println("Time to populate typeMap = "
                    + (System.currentTimeMillis() - t0) + "ms");
        }
        return typeMap;
    }

    @Override
    protected SurfaceObsLocation[] getStationLocations(String sourceKey) {
        if (availableStations.containsKey(sourceKey)) {
            return availableStations.get(sourceKey);
        }
        if (!Arrays.asList(getPlugins(null)).contains(getPlugin(sourceKey))) {
            availableStations.put(sourceKey, null);
            return null;
        }
        StaticPlotInfoPV spipv = StaticPlotInfoPV
                .readStaticPlotInfoPV("basemaps/modelBufr.spi");
        if (spipv == null) {
            return super.getStationLocations(sourceKey);
        }
        List<SurfaceObsLocation> locs = new ArrayList<SurfaceObsLocation>();
        for (Entry<String, SPIEntry> entry : spipv.getSpiList().entrySet()) {
            SurfaceObsLocation loc = new SurfaceObsLocation();
            loc.setStationId(entry.getKey());
            loc.setLatitude(entry.getValue().latlon.y);
            loc.setLongitude(entry.getValue().latlon.x);
            locs.add(loc);
        }
        Collections.sort(locs, locComparator);
        SurfaceObsLocation[] result = locs.toArray(new SurfaceObsLocation[0]);
        availableStations.put(sourceKey, result);
        return result;

    }

}
