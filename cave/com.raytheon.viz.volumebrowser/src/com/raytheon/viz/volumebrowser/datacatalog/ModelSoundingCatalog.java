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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

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
                selectedSources[i] = pluginName;
            }
        }
        return request;
    }

    private static synchronized Map<String, String> getTypeMap() {
        if (typeMap == null) {
            long t0 = System.currentTimeMillis();
            typeMap = new HashMap<String, String>();
            DbQueryRequest request = new DbQueryRequest();
            request.setDistinct(true);
            Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
            constraints.put("pluginName", new RequestConstraint(pluginName));
            request.addRequestField(typeKey);
            request.setConstraints(constraints);
            try {
                DbQueryResponse response = (DbQueryResponse) ThriftClient
                        .sendRequest(request);
                for (Map<String, Object> result : response.getResults()) {
                    String type = String.valueOf(result.get(typeKey));
                    typeMap.put(pluginName + type, type);
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            System.out.println("Time to populate typeMap = "
                    + (System.currentTimeMillis() - t0) + "ms");
        }
        return typeMap;
    }
}
