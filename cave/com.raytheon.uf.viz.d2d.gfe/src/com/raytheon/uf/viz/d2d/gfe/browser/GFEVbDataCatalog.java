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
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.uf.viz.d2d.gfe.rsc.GFEGridResource;
import com.raytheon.uf.viz.d2d.gfe.rsc.GFEGridResourceData;
import com.raytheon.viz.volumebrowser.datacatalog.AbstractDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.AvailableDataRequest;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogEntry;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GFEVbDataCatalog extends AbstractDataCatalog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEVbDataCatalog.class);

    @Override
    public IDataCatalogEntry getCatalogEntry(SelectedData selectedData) {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put(GFEDataAccessUtil.PLUGIN_NAME, new RequestConstraint("gfe"));
        queryList.put(GFEDataAccessUtil.PARM_ID, getParmIdConstraint(selectedData));
        try {
            String[] result = CatalogQuery.performQuery(GFEDataAccessUtil.PARM_ID,
                    queryList);
            if (result != null && result.length > 0) {
                ParmID sampleId = new ParmID(result[0]);
                return new GFECatalogEntry(selectedData, sampleId);
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
            gfeSources = new ArrayList<String>();
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
            gfeFields = new ArrayList<String>();
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
            gfePlanes = new ArrayList<String>();
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

        for (String parmIdStr : getParmIds()) {
            ParmID parmId = new ParmID(parmIdStr);
            boolean source = true;
            boolean field = true;
            boolean plane = true;

            if (gfeSources != null
                    && !gfeSources.contains(parmId.getDbId().getModelName())) {
                field = false;
                plane = false;
            }
            if (gfeFields != null && !gfeFields.contains(parmId.getParmName())) {
                source = false;
                plane = false;
            }
            if (gfePlanes != null && !gfePlanes.contains(parmId.getParmLevel())) {
                source = false;
                field = false;
            }
            if (source) {
                String vbSource = VbGFEMapping.getVbSource(parmId.getDbId()
                        .getModelName());
                if (vbSource != null) {
                    request.addAvailableSource(vbSource);
                }
            }
            if (field) {
                String vbParam = VbGFEMapping.getVbParam(parmId.getParmName());
                if (vbParam != null) {
                    request.addAvailableField(vbParam);
                }
            }
            if (plane) {
                String vbLevel = VbGFEMapping.getVbLevel(parmId.getParmLevel());
                if (vbLevel != null) {
                    request.addAvailablePlane(vbLevel);
                }
            }
        }

    }

    @Override
    public List<String> getSupportedSources() {
        List<String> results = new ArrayList<String>();
        for (String parmIdStr : getParmIds()) {
            ParmID parmId = new ParmID(parmIdStr);
            String vbSource = VbGFEMapping.getVbSource(parmId.getDbId()
                    .getModelName());
            if (vbSource != null) {
                results.add(vbSource);
            }
        }
        return results;
    }

    @Override
    protected String[] getPlugins(ViewMenu setting) {
        return new String[] { "gfe" };
    }

    @Override
    protected void addProductParameters(IDataCatalogEntry catalogEntry,
            HashMap<String, RequestConstraint> productParameters) {
        productParameters.put(GFEDataAccessUtil.PARM_ID,
                getParmIdConstraint(catalogEntry.getSelectedData()));
    }

    private String[] getParmIds() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put(GFEDataAccessUtil.PLUGIN_NAME, new RequestConstraint("gfe"));
        try {
            return CatalogQuery.performQuery(GFEDataAccessUtil.PARM_ID, queryList);
        } catch (VizException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Collection<ResourcePair> getResourcesToLoad(
            IDataCatalogEntry catalogEntry, ResourceType resourceType,
            DisplayType displayType) {
        Collection<ResourcePair> rsc = super.getResourcesToLoad(catalogEntry,
                resourceType, displayType);
        for (ResourcePair pair : rsc) {
            AbstractResourceData rd = pair.getResourceData();
            if (rd instanceof GFEGridResourceData) {
                ((GFEGridResourceData) rd).setLegendString(getName(
                        catalogEntry, displayType));
            }
        }
        return rsc;
    }

    @Override
    protected AbstractRequestableResourceData getResourceData(
            IDataCatalogEntry catalogEntry, ResourceType resourceType) {
        if (resourceType == ResourceType.PLAN_VIEW) {
            return new GFEGridResourceData();
        }
        return super.getResourceData(catalogEntry, resourceType);
    }

    @Override
    protected String getDisplayUnit(IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        if (catalogEntry instanceof GFECatalogEntry) {
            ParmID sampleId = ((GFECatalogEntry) catalogEntry).getSampleId();
            ParamLevelMatchCriteria criteria = GFEGridResource
                    .getMatchCriteria(sampleId);
            StyleRule sr = null;
            try {

                StyleManager.StyleType styleType = StyleManager.StyleType.CONTOUR;

                if (displayType.equals(DisplayType.IMAGE)) {
                    styleType = StyleManager.StyleType.IMAGERY;
                }

                if (displayType.equals(DisplayType.BARB)) {
                    styleType = StyleManager.StyleType.ARROW;
                }

                sr = StyleManager.getInstance().getStyleRule(styleType,
                        criteria);
            } catch (VizStyleException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Unable to obtain a style rule for"
                                        + catalogEntry.getSelectedData()
                                                .getUniqueKey(), e);
            }
            if (sr != null) {
                return sr.getPreferences().getDisplayUnitLabel();
            } else {
                try {
                    return UnitFormat.getUCUMInstance().format(
                            GFEDataAccessUtil.getGridParmInfo(sampleId).getUnitObject());
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to obtain a unit information for"
                                    + catalogEntry.getSelectedData()
                                            .getUniqueKey(), e);
                    return "";
                }
            }
        } else {
            return super.getDisplayUnit(catalogEntry, displayType);
        }
    }

    private RequestConstraint getParmIdConstraint(SelectedData selectedData) {
        String parmName = VbGFEMapping.getGfeParam(selectedData.getFieldsKey());
        String parmLevel = VbGFEMapping
                .getGfeLevel(selectedData.getPlanesKey());
        String modelName = VbGFEMapping.getGfeSource(selectedData
                .getSourcesKey());

        Map<String, String> parmIdComponents = new HashMap<String, String>();
        parmIdComponents.put(GFEDataAccessUtil.PARM_NAME, parmName);
        parmIdComponents.put(GFEDataAccessUtil.PARM_LEVEL, parmLevel);
        parmIdComponents.put(GFEDataAccessUtil.MODEL_NAME, modelName);
        return GFEDataAccessUtil.createParmIdConstraint(parmIdComponents);
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
