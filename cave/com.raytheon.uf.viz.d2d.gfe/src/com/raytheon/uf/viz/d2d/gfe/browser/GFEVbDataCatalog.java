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
package com.raytheon.uf.viz.d2d.gfe.browser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleManager.StyleType;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.d2d.gfe.rsc.GFEGridResource;
import com.raytheon.viz.volumebrowser.datacatalog.AbstractDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.AvailableDataRequest;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogEntry;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;

import tec.uom.se.format.SimpleUnitFormat;

/**
 *
 * Data Catalog for using gfe data in the volume browser. This works by using
 * selected model, field, and plane to create ParmId LIKE constraints that can
 * be used to narrow down the selection.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Mar 09, 2011           bsteffen  Initial creation
 * May 02, 2013  1949     bsteffen  Update GFE data access in Product Browser,
 *                                  Volume Browser, and Data Access Framework
 * Aug 03, 2015  3861     bsteffen  Move resource creation to product creator
 * Apr 02, 2018  6783     dgilling  Set site ID when requesting data.
 * May 25, 2018  6609     njensen   Use constant PluginDataObject.PLUGIN_NAME_ID
 *
 *
 * </pre>
 *
 * @author bsteffen
 */
public class GFEVbDataCatalog extends AbstractDataCatalog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEVbDataCatalog.class);

    @Override
    public IDataCatalogEntry getCatalogEntry(SelectedData selectedData) {
        Map<String, RequestConstraint> queryList = new HashMap<>();
        queryList.put(PluginDataObject.PLUGIN_NAME_ID,
                new RequestConstraint(
                GFERecord.PLUGIN_NAME));
        queryList.putAll(getParmIdConstraint(selectedData));
        try {
            DbQueryRequest request = new DbQueryRequest(queryList);
            request.addRequestField(GFEDataAccessUtil.PARM_ID);
            request.setLimit(1);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            ParmID[] results = response.getFieldObjects(
                    GFEDataAccessUtil.PARM_ID, ParmID.class);
            if (results.length > 0) {
                return new GFECatalogEntry(selectedData, results[0]);
            } else {
                return null;
            }
        } catch (VizException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void getAvailableData(AvailableDataRequest request) {
        String[] selectedSources = request.getSelectedSources();
        String[] selectedFields = request.getSelectedFields();
        String[] selectedPlanes = request.getSelectedPlanes();

        List<String> gfeSources = null;
        if (selectedSources != null && selectedSources.length != 0) {
            gfeSources = new ArrayList<>(selectedSources.length);
            for (String selectedSource : selectedSources) {
                String gfeSource = VbGFEMapping.getGfeSource(selectedSource);
                if (gfeSource != null) {
                    gfeSources.add(gfeSource);
                }
            }
            if (gfeSources.isEmpty()) {
                return;
            }
        }

        List<String> gfeFields = null;
        if (selectedFields != null && selectedFields.length != 0) {
            gfeFields = new ArrayList<>(selectedFields.length);
            for (String selectedField : selectedFields) {
                String gfeParam = VbGFEMapping.getGfeParam(selectedField);
                if (gfeParam != null) {
                    gfeFields.add(gfeParam);
                }
            }
            if (gfeFields.isEmpty()) {
                return;
            }
        }

        List<String> gfePlanes = null;
        if (selectedPlanes != null && selectedPlanes.length != 0) {
            gfePlanes = new ArrayList<>(selectedPlanes.length);
            for (String selectedPlane : selectedPlanes) {
                String gfePlane = VbGFEMapping.getGfeLevel(selectedPlane);
                if (gfePlane != null) {
                    gfePlanes.add(gfePlane);
                }
            }
            if (gfePlanes.isEmpty()) {
                return;
            }
        }

        DbQueryRequest dbRequest = new DbQueryRequest();
        dbRequest.setEntityClass(GFERecord.class);
        dbRequest.addRequestField(GFEDataAccessUtil.MODEL_NAME);
        dbRequest.addRequestField(GFEDataAccessUtil.PARM_NAME);
        dbRequest.addRequestField(GFEDataAccessUtil.PARM_LEVEL);
        dbRequest.addConstraint(GFEDataAccessUtil.SITE_ID,
                new RequestConstraint(
                        LocalizationManager.getInstance().getCurrentSite()));
        DbQueryResponse dbResponse = null;
        try {
            dbResponse = (DbQueryResponse) ThriftClient.sendRequest(dbRequest);
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Unable to load availability for GFE in the Volume Browser",
                            e);
            return;
        }

        for (Map<String, Object> row : dbResponse.getResults()) {
            String modelName = row.get(GFEDataAccessUtil.MODEL_NAME).toString();
            String parmName = row.get(GFEDataAccessUtil.PARM_NAME).toString();
            String parmLevel = row.get(GFEDataAccessUtil.PARM_LEVEL).toString();
            boolean source = true;
            boolean field = true;
            boolean plane = true;

            if (gfeSources != null && !gfeSources.contains(modelName)) {
                field = false;
                plane = false;
            }
            if (gfeFields != null && !gfeFields.contains(parmName)) {
                source = false;
                plane = false;
            }
            if (gfePlanes != null && !gfePlanes.contains(parmLevel)) {
                source = false;
                field = false;
            }
            if (source) {
                String vbSource = VbGFEMapping.getVbSource(modelName);
                if (vbSource != null) {
                    request.addAvailableSource(vbSource);
                }
            }
            if (field) {
                String vbParam = VbGFEMapping.getVbParam(parmName);
                if (vbParam != null) {
                    request.addAvailableField(vbParam);
                }
            }
            if (plane) {
                String vbLevel = VbGFEMapping.getVbLevel(parmLevel);
                if (vbLevel != null) {
                    request.addAvailablePlane(vbLevel);
                }
            }
        }

    }

    @Override
    public List<String> getSupportedSources() {
        List<String> results = new ArrayList<>();

        DbQueryRequest dbRequest = new DbQueryRequest();
        dbRequest.setEntityClass(GFERecord.class);
        dbRequest.setDistinct(true);
        dbRequest.addRequestField(GFEDataAccessUtil.MODEL_NAME);
        dbRequest.addConstraint(GFEDataAccessUtil.SITE_ID,
                new RequestConstraint(
                        LocalizationManager.getInstance().getCurrentSite()));
        try {
            DbQueryResponse dbResponse = (DbQueryResponse) ThriftClient
                    .sendRequest(dbRequest);
            for (String modelName : dbResponse.getFieldObjects(
                    GFEDataAccessUtil.MODEL_NAME, String.class)) {
                String vbSource = VbGFEMapping.getVbSource(modelName);
                if (vbSource != null) {
                    results.add(vbSource);
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load any GFE sources in the Volume Browser", e);
        }
        return results;
    }

    @Override
    protected String[] getPlugins() {
        return new String[] { "gfe" };
    }

    @Override
    protected void addProductParameters(IDataCatalogEntry catalogEntry,
            HashMap<String, RequestConstraint> productParameters) {
        productParameters.putAll(getParmIdConstraint(catalogEntry
                .getSelectedData()));
    }

    @Override
    protected String getDisplayUnit(IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        ParmID sampleId = ((GFECatalogEntry) catalogEntry).getSampleId();
        ParamLevelMatchCriteria criteria = GFEGridResource
                .getMatchCriteria(sampleId);
        StyleRule sr = null;
        try {

            StyleType styleType = StyleManager.StyleType.CONTOUR;

            if (displayType.equals(DisplayType.IMAGE)) {
                styleType = StyleManager.StyleType.IMAGERY;
            }

            if (displayType.equals(DisplayType.BARB)) {
                styleType = StyleManager.StyleType.ARROW;
            }

            sr = StyleManager.getInstance().getStyleRule(styleType, criteria);
        } catch (StyleException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to obtain a style rule for"
                            + catalogEntry.getSelectedData().getUniqueKey(), e);
        }
        if (sr != null) {
            return sr.getPreferences().getDisplayUnitLabel();
        } else {
            try {
                return SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(
                        GFEDataAccessUtil.getGridParmInfo(sampleId)
                                .getUnitObject());
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Unable to obtain a unit information for"
                                        + catalogEntry.getSelectedData()
                                                .getUniqueKey(), e);
                return "";
            }
        }
    }

    private Map<String, RequestConstraint> getParmIdConstraint(
            SelectedData selectedData) {
        String parmName = VbGFEMapping.getGfeParam(selectedData.getFieldsKey());
        String parmLevel = VbGFEMapping
                .getGfeLevel(selectedData.getPlanesKey());
        String modelName = VbGFEMapping.getGfeSource(selectedData
                .getSourcesKey());

        Map<String, RequestConstraint> result = new HashMap<>();
        result.put(GFEDataAccessUtil.PARM_NAME, new RequestConstraint(parmName));
        result.put(GFEDataAccessUtil.PARM_LEVEL, new RequestConstraint(
                parmLevel));
        result.put(GFEDataAccessUtil.MODEL_NAME, new RequestConstraint(
                modelName));
        result.put(GFEDataAccessUtil.SITE_ID, new RequestConstraint(
                LocalizationManager.getInstance().getCurrentSite()));
        return result;
    }

    private static class GFECatalogEntry extends DataCatalogEntry {

        private final ParmID sampleId;

        public GFECatalogEntry(SelectedData selectedData, ParmID sampleId) {
            super(selectedData);
            this.sampleId = sampleId;
        }

        public ParmID getSampleId() {
            return sampleId;
        }

    }
}
