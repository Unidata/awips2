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
package com.raytheon.uf.viz.d2d.gfe.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfeRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetDiscreteDefinitionRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetWXDefinitionRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxDefinition;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.level.Level;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;
import com.raytheon.uf.viz.core.grid.rsc.data.ScalarGridData;
import com.raytheon.uf.viz.core.grid.rsc.data.VectorGridData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.d2d.gfe.display.D2DDataManagerUIFactory;
import com.raytheon.uf.viz.d2d.gfe.display.DiscreteRenderable;
import com.raytheon.uf.viz.d2d.gfe.display.WeatherRenderable;
import com.raytheon.uf.viz.d2d.gfe.rsc.data.DiscreteGridData;
import com.raytheon.uf.viz.d2d.gfe.rsc.data.WeatherGridData;
import com.raytheon.viz.gfe.colortable.DiscreteColorTable;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerFactory;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.rsc.DiscreteDisplayUtil;
import com.raytheon.viz.ui.EditorUtil;

/**
 *
 * Resource to render GFE grid data in D2D.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 09, 2011           bsteffen  Initial creation
 * Aug 29, 2019  67962    tjensen   Update for GeneralGridData
 * Dec 02, 2019  71896    tjensen   Add functionality for discrete products
 * Dec 13, 2019  72475    tjensen   Add support for Weather products and
 *                                  refactor discreteColorbar
 * Feb 18, 2020  74905    tjensen   Updated to support loading data from
 *                                  different sites
 *
 * </pre>
 *
 * @author bsteffen
 */
public class GFEGridResource extends AbstractGridResource<GFEGridResourceData> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEGridResource.class);

    private ParmID parmId;

    private final String siteId;

    private boolean discreteColorbarUser;

    protected GFEGridResource(GFEGridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        setDiscreteColorbarUser(false);
        siteId = resourceData.getMetadataMap().get("parmId.dbId.siteId")
                .getConstraintValue();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (WeatherSubKey.wxDef(siteId) == null) {
            // get and set the WxDefinition
            ServerResponse<WxDefinition> sr = getWxDefinition();
            WxDefinition wxDef = sr.getPayload();
            if (!sr.isOkay()) {
                statusHandler.error(sr.message());
            }
            WeatherSubKey.setWxDefinition(siteId, wxDef);
        }
        if (DiscreteKey.discreteDefinition(siteId) == null) {
            // get and set the DiscreteDefinition
            ServerResponse<DiscreteDefinition> sr2 = getDiscreteDefinition();
            DiscreteDefinition dDef = sr2.getPayload();
            if (!sr2.isOkay()) {
                statusHandler.error(sr2.message());
            }
            DiscreteKey.setDiscreteDefinition(siteId, dDef);
        }
        super.initInternal(target);
    }

    @Override
    protected void addDataObject(PluginDataObject pdo) {
        if (pdo instanceof GFERecord) {
            GFERecord gfeRecord = (GFERecord) pdo;
            if (parmId == null) {
                parmId = gfeRecord.getParmId();
            }
            super.addDataObject(pdo);
        }
    }

    private void populateGridParmInfo(GFERecord gfeRecord) {
        if (gfeRecord.getGridInfo() != null) {
            return;
        }
        try {
            gfeRecord.setGridInfo(
                    GFEDataAccessUtil.getGridParmInfo(gfeRecord.getParmId()));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        return getMatchCriteria(parmId);
    }

    @Override
    public List<GeneralGridData> getData(DataTime time,
            List<PluginDataObject> pdos) throws VizException {
        if (pdos == null) {
            return null;
        }
        GFERecord gfeRecord = (GFERecord) pdos.get(0);
        IGridSlice slice = null;
        try {
            slice = GFEDataAccessUtil.getSlice(gfeRecord);
        } catch (Exception e) {
            throw new VizException(e);
        }
        populateGridParmInfo(gfeRecord);
        GridGeometry2D gridGeometry = MapUtil
                .getGridGeometry(gfeRecord.getGridInfo().getGridLoc());
        if (slice instanceof VectorGridSlice) {
            VectorGridSlice vSlice = (VectorGridSlice) slice;
            return Arrays.asList(VectorGridData.createVectorData(gridGeometry,
                    vSlice.getMagGrid().getBuffer(),
                    vSlice.getDirGrid().getBuffer(),
                    slice.getGridInfo().getUnitObject()));
        } else if (slice instanceof ScalarGridSlice) {
            ScalarGridSlice sSlice = (ScalarGridSlice) slice;
            return Arrays.asList(ScalarGridData.createScalarData(gridGeometry,
                    sSlice.getScalarGrid().getBuffer(),
                    slice.getGridInfo().getUnitObject()));
        } else if (slice instanceof DiscreteGridSlice) {
            DiscreteGridSlice dSlice = (DiscreteGridSlice) slice;
            return Arrays.asList(DiscreteGridData.createDiscreteData(
                    gridGeometry, dSlice.getDiscreteGrid().getBuffer(),
                    dSlice.getKeys(), slice.getGridInfo().getUnitObject()));
        } else if (slice instanceof WeatherGridSlice) {
            WeatherGridSlice wSlice = (WeatherGridSlice) slice;
            return Arrays.asList(WeatherGridData.createWeatherData(gridGeometry,
                    wSlice.getWeatherGrid().getBuffer(), wSlice.getKeys(),
                    slice.getGridInfo().getUnitObject()));
        } else if (slice == null) {
            throw new VizException("Unable to load GFE Slice Data");
        } else {
            throw new VizException("Unable GFE Slice of type "
                    + slice.getClass().getSimpleName());
        }

    }

    @Override
    public String getName() {
        if (resourceData.getLegendString() != null) {
            return resourceData.getLegendString();
        }
        String displayTypeString = DisplayType
                .getAbbreviation(getDisplayType());
        String siteId = "";
        String modelName = "";
        String parmName = "";
        String unitLabel = "?";
        if (parmId != null) {
            siteId = parmId.getDbId().getSiteId();
            modelName = parmId.getDbId().getModelName();
            parmName = parmId.getParmName();
        }
        if (stylePreferences != null) {
            unitLabel = stylePreferences.getDisplayUnitLabel();
        }

        return String.format("GFE(%s %s) %s %s (%s)  ", siteId, modelName,
                parmName, displayTypeString, unitLabel);
    }

    public static ParamLevelMatchCriteria getMatchCriteria(ParmID parmId) {
        ParamLevelMatchCriteria criteria = new ParamLevelMatchCriteria();
        criteria.setParameterName(new ArrayList<String>());
        criteria.setLevels(new ArrayList<Level>());
        criteria.setCreatingEntityNames(new ArrayList<String>());
        String parameter = "GFE:" + parmId.getParmName();
        SingleLevel level = new SingleLevel(Level.LevelType.SURFACE);
        String model = "GFE:" + parmId.getDbId().getModelName();
        if (!criteria.getParameterNames().contains(parameter)) {
            criteria.getParameterNames().add(parameter);
        }
        if (!criteria.getLevels().contains(level)) {
            criteria.getLevels().add(level);
        }
        if (!criteria.getCreatingEntityNames().contains(model)) {
            criteria.getCreatingEntityNames().add(model);
        }
        return criteria;
    }

    @Override
    public IRenderable createRenderable(IGraphicsTarget target,
            GeneralGridData data) throws VizException {
        IRenderable renderable = null;

        if (data instanceof DiscreteGridData) {
            DiscreteGridData discreteData = (DiscreteGridData) data;
            Parm parm = getParm();

            ImagingCapability imagingCap = getCapability(
                    ImagingCapability.class);
            createDiscreteColorbar();

            ColorMapParameters params = DiscreteDisplayUtil
                    .buildColorMapParameters(parm);
            IColorMap colorMap = params.getColorMap();
            DiscreteColorTable colorTable = new DiscreteColorTable(parm,
                    colorMap);

            renderable = new DiscreteRenderable(discreteData, parm, descriptor,
                    imagingCap, colorTable);

        } else if (data instanceof WeatherGridData) {
            WeatherGridData weatherData = (WeatherGridData) data;
            Parm parm = getParm();

            ImagingCapability imagingCap = getCapability(
                    ImagingCapability.class);
            createDiscreteColorbar();

            renderable = new WeatherRenderable(weatherData, parm, descriptor,
                    imagingCap);
        } else {
            // Make sure this is set to false
            setDiscreteColorbarUser(false);
            renderable = super.createRenderable(target, data);
        }

        return renderable;
    }

    private void createDiscreteColorbar() {
        setDiscreteColorbarUser(true);

        // If the descriptor doesn't have a discreteColorbar, add one.
        D2DDiscreteColorbarResource discreteColorbar = null;
        for (ResourcePair rp : descriptor.getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc instanceof D2DDiscreteColorbarResource) {
                discreteColorbar = (D2DDiscreteColorbarResource) rsc;
                break;
            }
        }
        if (discreteColorbar == null) {
            discreteColorbar = new D2DDiscreteColorbarResource(
                    getResourceData());
            discreteColorbar.setDescriptor(descriptor);
            descriptor.getResourceList().add(discreteColorbar);
            descriptor.getResourceList().getProperties(discreteColorbar)
                    .setSystemResource(true);
        }
    }

    public Parm getParm() {
        DataManager dataManager = DataManagerFactory.getInstance(
                D2DDataManagerUIFactory.getInstance(),
                EditorUtil.getActiveEditor(), siteId);
        IParmManager parmManager = dataManager.getParmManager();
        ParmID[] parmIds = { parmId };
        parmManager.setDisplayedParms(parmIds);
        return parmManager.getParm(parmId);
    }

    @Override
    protected void disposeRenderable(final IRenderable renderable) {
        if (renderable instanceof DiscreteRenderable
                || renderable instanceof WeatherRenderable) {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (renderable instanceof DiscreteRenderable) {
                        ((DiscreteRenderable) renderable).dispose();
                    } else if (renderable instanceof WeatherRenderable) {
                        ((WeatherRenderable) renderable).dispose();
                    } else {
                        statusHandler.warn("Undisposed renderable of type: "
                                + renderable.getClass().getSimpleName());
                    }
                }
            });
        } else {
            super.disposeRenderable(renderable);
        }
    }

    public ServerResponse<DiscreteDefinition> getDiscreteDefinition() {
        GetDiscreteDefinitionRequest request = new GetDiscreteDefinitionRequest();

        return (ServerResponse<DiscreteDefinition>) makeRequest(request);
    }

    public ServerResponse<WxDefinition> getWxDefinition() {
        GetWXDefinitionRequest request = new GetWXDefinitionRequest();

        return (ServerResponse<WxDefinition>) makeRequest(request);
    }

    public ServerResponse<?> makeRequest(AbstractGfeRequest request) {
        request.setSiteID(siteId);

        ServerResponse<Object> ssr = new ServerResponse<>();
        try {
            ServerResponse<?> sr = (ServerResponse<?>) RequestRouter
                    .route(request);
            ssr.addMessages(sr);
            ssr.setPayload(sr.getPayload());
        } catch (Exception e) {
            statusHandler
                    .error(String.format("Server error processing %s request.",
                            request.getClass().getName()), e);
            ssr.addMessage(String.format(
                    "Server error processing %s request: %s",
                    request.getClass().getName(), e.getLocalizedMessage()));
        }

        return ssr;
    }

    public boolean isDiscreteColorbarUser() {
        return discreteColorbarUser;
    }

    public void setDiscreteColorbarUser(boolean discreteColorbarUser) {
        this.discreteColorbarUser = discreteColorbarUser;
    }

}
