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
package com.raytheon.viz.hydrocommon;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.hydrocommon.colorscalemgr.HydroColorManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.NamedColorSetGroup;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;
import com.raytheon.viz.hydrocommon.data.ArealData;
import com.raytheon.viz.hydrocommon.data.DamMaster;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.events.StationSelectionChangeEvent;
import com.raytheon.viz.hydrocommon.listeners.StationSelectionChangeListener;
import com.raytheon.viz.hydrocommon.resource.ArealFfgResourceData;
import com.raytheon.viz.hydrocommon.resource.FFGGridResourceData;
import com.raytheon.viz.hydrocommon.resource.GriddedArealFFGResourceData;
import com.raytheon.viz.hydrocommon.resource.MPELegendResource;
import com.raytheon.viz.hydrocommon.resource.MeanArealPrecipResource;
import com.raytheon.viz.hydrocommon.resource.RFCGriddedBasinFFGResourceData;
import com.raytheon.viz.hydrocommon.resource.TimeLapseResource;
import com.raytheon.viz.hydrocommon.resource.XmrgResource;
import com.raytheon.viz.hydrocommon.resource.XmrgResourceData;
import com.raytheon.viz.hydrocommon.util.HydroDialogStatus;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * This class manages the interaction between the Hydro Perspective and the
 * Hydro dialogs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2008 1644       mpduff      Initial creation
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HydroDisplayManager {
    /** Singleton instance of this class */
    private static HydroDisplayManager displayManager = null;

    /** MPE application name */
    public static final String MPE_APPLICATION_NAME = "hmapmpe";

    /** Hydro application name */
    public static final String HYDRO_APPLICATION_NAME = "hydroview";

    /** The currently selected lid */
    private GageData currentdata = null;

    /** Draw Station Flag */
    private boolean drawStation = false;

    /** Font size */
    private int fontSize = 10;

    /** List of dam data */
    private List<DamMaster> damList = null;

    /** The currently displayed resource */
    private AbstractVizResource<?, ?> displayedResource;

    /**
     * The previously displayed resource.
     */
    private AbstractVizResource<?, ?> previousResource = null;

    /** List of NamedColorUseSet objects */
    private static final List<NamedColorUseSet> pColorSetGroup = MPEColors
            .build_mpe_colors();

    /**
     * Display the Dam Id flag
     */
    private boolean displayDamId = false;

    /**
     * Display the Dam Name flag
     */
    private boolean displayDamName = false;

    /**
     * Display the Dam Icon flag
     */
    private boolean displayDamIcon = false;

    private String cvUse = null;

    /**
     * The Accumulation interval for the xmrg data, Also the number of frames
     * for the time lapse mode
     */
    private int accumInterval;

    /**
     * Date of the xmrg data
     */
    private Date dataDate = null;

    /**
     * The display type
     */
    private String displayType = null;

    /** The xmrg resource */
    private XmrgResource bestEstRsc = null;

    /** The xmrg resource for WFO FFG grids */
    private final XmrgResource ffgRsc = null;

    /** The Time Lapse resource */
    private TimeLapseResource timeLapseRsc = null;

    /** Accumulate mode flag */
    private boolean accumulate = true;

    /** Time lapse mode flag */
    private boolean timeLapseMode = false;

    /** Display pane */
    private IDisplayPane pane = null;

    /** The mean areal precipitation resource */
    private MeanArealPrecipResource map = null;

    /** Display id flag */
    private boolean ids = false;

    /** Display label flag */
    private boolean labels = false;

    private MPELegendResource mpeLegend = null;

    /** Flag for data type changed */
    private boolean dataChanged = true;

    /** Flag for Colors changed when changed by the Color Scale Manager */
    private boolean colorChanged = true;

    /** Current Selection Listener list */
    private final ArrayList<StationSelectionChangeListener> currentSelectionListenerList = new ArrayList<StationSelectionChangeListener>();

    /**
     * User Id.
     */
    private String userId = null;

    /**
     * The color use name.
     */
    private String useName = null;

    /**
     * The duration for the color set.
     */
    private int duration;

    /**
     * The gage color list.
     */
    private List<Colorvalue> gageColorSet = null;

    private List<Colorvalue> ffgColorSet = null;

    private ResolutionLevel ffgResolution;

    /* Private Constructor */
    private HydroDisplayManager() {
    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static synchronized HydroDisplayManager getInstance() {
        if (displayManager == null) {
            displayManager = new HydroDisplayManager();
        }
        return displayManager;
    }

    /**
     * Get the currently selected lid. If no lid selected display error message
     * pop up.
     * 
     * @return the current lid or null if nothing selected
     */
    public boolean isCurrentLidSelected(Shell shell) {

        if (getCurrentLid() == null) {
            MessageDialog.openError(shell, "Invalid Selection",
                    "You must select a site first");
            return false;
        }

        return true;
    }

    /**
     * Get the currently selected location id.
     * 
     * @return String The LID
     */
    public String getCurrentLid() {
        if (currentdata != null) {
            return currentdata.getLid();
        } else {
            return null;
        }
    }

    public void displayGriddedFFG(Date date, int dur, String paramAbr,
            String rfc, ResolutionLevel resolution) {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();
            IMapDescriptor md = (IMapDescriptor) pane.getDescriptor();

            this.ffgResolution = resolution;
            this.duration = dur;

            ResourceList rl = md.getResourceList();
            Iterator<ResourcePair> iter = rl.iterator();
            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                if ((pair.getResource() != null)
                        && (pair.getResource().getName() == "FFG")) {
                    pair.getResource().dispose();
                    rl.removeRsc(pair.getResource());
                    break;
                }
            }

            previousResource = getDisplayedResource();
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

            // Get the Grib data
            Map<String, RequestConstraint> reqMap = new HashMap<String, RequestConstraint>();
            reqMap.put(GridConstants.PLUGIN_NAME, new RequestConstraint(
                    GridConstants.GRID));
            reqMap.put(GridConstants.PARAMETER_ABBREVIATION,
                    new RequestConstraint(paramAbr));
            reqMap.put("dataTime.refTime",
                    new RequestConstraint(sdf.format(date)));
            reqMap.put(GridConstants.DATASET_ID, new RequestConstraint("FFG-"
                    + rfc));

            try {
                LayerProperty lp = new LayerProperty();
                lp.setNumberOfImages(9999);
                lp.setEntryQueryParameters(reqMap, false);

                List<Object> dataList = DataCubeContainer.getData(lp, 30);
                GridRecord gr = null;
                int i = 0;
                for (Object o : dataList) {
                    gr = (GridRecord) o;
                    IDataRecord[] recArr = DataCubeContainer
                            .getDataRecord((PluginDataObject) o);
                    gr.setMessageData(((FloatDataRecord) recArr[0])
                            .getFloatData());
                    ++i;
                }

                FFGGridResourceData resourceData = new FFGGridResourceData(
                        duration, gr, resolution, date);
                ResourcePair rp = new ResourcePair();
                rp.setResourceData(resourceData);
                rl.add(rp);
                rl.instantiateResources(pane.getDescriptor(), true);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void displayGriddedFFG(File xmrgFile, int dur,
            ResolutionLevel resolution) {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();
            IMapDescriptor md = (IMapDescriptor) pane.getDescriptor();

            this.duration = dur;
            this.ffgResolution = resolution;
            ResourceList rl = md.getResourceList();
            Iterator<ResourcePair> iter = rl.iterator();
            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                if ((pair.getResource() != null)
                        && (pair.getResource().getName() == "FFG")) {
                    pair.getResource().dispose();
                    rl.removeRsc(pair.getResource());
                    break;
                }
            }

            previousResource = getDisplayedResource();

            XmrgFile xmrg = new XmrgFile(xmrgFile);
            XmrgResourceData resourceData = new XmrgResourceData(xmrg,
                    duration, ffgResolution);
            ResourcePair rp = new ResourcePair();
            rp.setResourceData(resourceData);
            rl.add(rp);
            rl.instantiateResources(pane.getDescriptor(), true);
        }
    }

    // public void displayGriddedFFGBasin(Date date, int dur, String paramAbr,
    // String rfc, ResolutionLevel resolution) {
    // IEditorPart part = VizApp.getCurrentEditor();
    // if (part instanceof AbstractEditor) {
    // AbstractEditor ae = ((AbstractEditor) part);
    // pane = ae.getActiveDisplayPane();
    // IMapDescriptor md = (IMapDescriptor) pane.getDescriptor();
    //
    // this.duration = dur;
    // this.ffgResolution = resolution;
    // ResourceList rl = md.getResourceList();
    // Iterator<ResourcePair> iter = rl.iterator();
    // while (iter.hasNext()) {
    // ResourcePair pair = iter.next();
    // if ((pair.getResource() != null)
    // && (pair.getResource().getName() == "FFG")) {
    // pair.getResource().dispose();
    // rl.removeRsc(pair.getResource());
    // break;
    // }
    // }
    //
    // previousResource = getDisplayedResource();
    // SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    // sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    //
    // // Get the Grib data
    // Map<String, RequestConstraint> reqMap = new HashMap<String,
    // RequestConstraint>();
    // reqMap.put("pluginName", new RequestConstraint("grib"));
    // reqMap.put("modelInfo.parameterAbbreviation",
    // new RequestConstraint(paramAbr));
    // reqMap.put("dataTime.refTime",
    // new RequestConstraint(sdf.format(date)));
    // reqMap.put("modelInfo.modelName", new RequestConstraint("FFG-"
    // + rfc));
    //
    // try {
    // LayerProperty lp = new LayerProperty();
    // lp.setNumberOfImages(9999);
    // lp.setEntryQueryParameters(reqMap, false);
    //
    // List<Object> dataList = DataCubeContainer.getData(lp, 30);
    // GribRecord gr = null;
    // int i = 0;
    // for (Object o : dataList) {
    // gr = (GribRecord) o;
    // IDataRecord[] recArr = DataCubeContainer
    // .getDataRecord((PluginDataObject) o);
    // gr.setMessageData(((FloatDataRecord) recArr[0])
    // .getFloatData());
    // ++i;
    // }
    // GriddedArealFFGResourceData resourceData = new
    // GriddedArealFFGResourceData(duration, gr, resolution, dataDate);
    // // FFGGridResourceData resourceData = new FFGGridResourceData(
    // // duration, gr, resolution, date);
    // ResourcePair rp = new ResourcePair();
    // rp.setResourceData(resourceData);
    // rl.add(rp);
    // rl.instantiateResources(pane.getDescriptor(), true);
    // } catch (Exception e) {
    // e.printStackTrace();
    // }
    // }
    // }

    public void displayRfcGriddedFFGBasin(Date date, int dur, String paramAbr,
            String rfc, ResolutionLevel resolution) {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();
            IMapDescriptor md = (IMapDescriptor) pane.getDescriptor();

            this.duration = dur;
            this.ffgResolution = resolution;
            ResourceList rl = md.getResourceList();
            Iterator<ResourcePair> iter = rl.iterator();
            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                if ((pair.getResource() != null)
                        && (pair.getResource().getName() == "FFG")) {
                    pair.getResource().dispose();
                    rl.removeRsc(pair.getResource());
                    break;
                }
            }

            previousResource = getDisplayedResource();

            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

            // Get the Grib data
            Map<String, RequestConstraint> reqMap = new HashMap<String, RequestConstraint>();
            reqMap.put(GridConstants.PLUGIN_NAME, new RequestConstraint(
                    GridConstants.GRID));
            reqMap.put(GridConstants.PARAMETER_ABBREVIATION,
                    new RequestConstraint(paramAbr));
            reqMap.put("dataTime.refTime",
                    new RequestConstraint(sdf.format(date)));
            reqMap.put(GridConstants.DATASET_ID, new RequestConstraint("FFG-"
                    + rfc));

            try {
                LayerProperty lp = new LayerProperty();
                lp.setNumberOfImages(9999);
                lp.setEntryQueryParameters(reqMap, false);

                List<Object> dataList = DataCubeContainer.getData(lp, 30);
                GridRecord gr = null;
                int i = 0;
                for (Object o : dataList) {
                    gr = (GridRecord) o;
                    IDataRecord[] recArr = DataCubeContainer
                            .getDataRecord((PluginDataObject) o);
                    gr.setMessageData(((FloatDataRecord) recArr[0])
                            .getFloatData());
                    ++i;
                }

                RFCGriddedBasinFFGResourceData resourceData = new RFCGriddedBasinFFGResourceData(
                        duration, gr, resolution, dataDate);
                ResourcePair rp = new ResourcePair();
                rp.setResourceData(resourceData);
                rl.add(rp);
                rl.instantiateResources(pane.getDescriptor(), true);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void displayGriddedFFGBasin(File xmrgFile, int dur,
            ResolutionLevel resolution, Date dataDate) {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();
            IMapDescriptor md = (IMapDescriptor) pane.getDescriptor();

            this.duration = dur;
            this.ffgResolution = resolution;
            ResourceList rl = md.getResourceList();
            Iterator<ResourcePair> iter = rl.iterator();
            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                if ((pair.getResource() != null)
                        && (pair.getResource().getName() == "FFG")) {
                    pair.getResource().dispose();
                    rl.removeRsc(pair.getResource());
                    break;
                }
            }

            previousResource = getDisplayedResource();

            XmrgFile xmrg = new XmrgFile(xmrgFile);
            GriddedArealFFGResourceData resourceData = new GriddedArealFFGResourceData(
                    duration, xmrg, resolution, dataDate);
            ResourcePair rp = new ResourcePair();
            rp.setResourceData(resourceData);
            rl.add(rp);
            rl.instantiateResources(pane.getDescriptor(), true);

        }
    }

    public void displayArealFfg(List<ArealData> arealDataList, int duration,
            String site, Date dataDate, boolean displayValues,
            boolean displayIds) {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();
            IMapDescriptor md = (IMapDescriptor) pane.getDescriptor();

            ResourceList rl = md.getResourceList();
            Iterator<ResourcePair> iter = rl.iterator();
            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                if ((pair.getResource() != null)
                        && (pair.getResource().getName() == "FFG")) {
                    pair.getResource().dispose();
                    rl.removeRsc(pair.getResource());
                    break;
                }
            }

            previousResource = getDisplayedResource();

            ArealFfgResourceData resourceData = new ArealFfgResourceData(
                    arealDataList, duration, site, dataDate, displayValues,
                    displayIds);
            ResourcePair rp = new ResourcePair();
            rp.setResourceData(resourceData);
            rl.add(rp);
            rl.instantiateResources(pane.getDescriptor(), true);
            rp.getResource();
        }
    }

    public void updateArealFfgDisplay(boolean values, boolean ids) {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();
            IMapDescriptor md = (IMapDescriptor) pane.getDescriptor();

            ResourceList rl = md.getResourceList();
            Iterator<ResourcePair> iter = rl.iterator();
            while (iter.hasNext()) {
                ResourcePair pair = iter.next();
                if ((pair.getResource() != null)
                        && (pair.getResource().getName().startsWith("FFG"))) {
                    if (pair.getResourceData() instanceof ArealFfgResourceData) {
                        ArealFfgResourceData rscData = (ArealFfgResourceData) pair
                                .getResourceData();
                        rscData.setDisplayValues(values);
                        rscData.setDisplayIds(ids);
                        pair.getResource().issueRefresh();
                    }
                    break;
                }
            }
        }
    }

    /**
     * Display the Best Estimate QPE.
     * 
     * @throws VizException
     */
    public void displayQPE() throws VizException {
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {

            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();

            IDescriptor md = pane.getDescriptor();

            if (isAccumulate()) {

                if (md.getResourceList().containsRsc(bestEstRsc)) {
                    md.getResourceList().removeRsc(bestEstRsc);
                    bestEstRsc.dispose();
                }

                if ((timeLapseRsc != null)
                        && md.getResourceList().containsRsc(timeLapseRsc)) {
                    md.getResourceList().removeRsc(timeLapseRsc);
                    timeLapseRsc.dispose();
                }

                if ((map != null) && md.getResourceList().containsRsc(map)) {
                    md.getResourceList().removeRsc(map);
                    map.dispose();
                }

                previousResource = getDisplayedResource();

                if (getDisplayType().equalsIgnoreCase("Grid")) {
                    if (md.getResourceList().containsRsc(bestEstRsc)) {
                        md.getResourceList().removeRsc(bestEstRsc);
                        bestEstRsc.dispose();
                    }

                    // Get the xmrg file
                    String dirname = null;

                    if (getCvUse().equalsIgnoreCase("xmrg")) {
                        dirname = AppsDefaults.getInstance().getToken(
                                HydroConstants.XMRG_DIR_TOKEN);
                    } else {
                        dirname = AppsDefaults.getInstance().getToken(
                                HydroConstants.RFCMOSAIC_DIR_TOKEN);
                    }
                    String fname = FileUtil.join(
                            dirname,
                            cvUse
                                    + HydroConstants.QPE_DATE_FORMAT
                                            .format(getDataDate()) + "z");
                    XmrgFile xmrg = new XmrgFile(fname);
                    XmrgResourceData xmrgResourceData = new XmrgResourceData(
                            xmrg, duration, null);
                    xmrgResourceData.setAccumInterval(accumInterval);
                    xmrgResourceData.setCv_use(cvUse);
                    xmrgResourceData.setColorList(getGridColorMap());

                    // bestEstRsc = (XmrgResource)
                    // xmrgResourceData.construct(new LoadProperties(), md);
                    // bestEstRsc = new XmrgResource(xmrgResourceData,
                    // cvUse, accumInterval, xmrg, getGridColorMap());
                    ResourcePair resourcePair = new ResourcePair();
                    // resourcePair.setProperties(new ResourceProperties());
                    // resourcePair.setResource(bestEstRsc);
                    resourcePair.setResourceData(xmrgResourceData);
                    md.getResourceList().add(resourcePair);
                    md.getResourceList().instantiateResources(md, true);
                    bestEstRsc = (XmrgResource) resourcePair.getResource();
                    // bestEstRsc.init(pane.getTarget());
                    bestEstRsc.updateXmrg(false);

                    if (md.getResourceList().contains(previousResource)) {
                        md.getResourceList().remove(previousResource);
                    }
                    HydroDisplayManager.getInstance().setDisplayedResource(
                            bestEstRsc);
                } else {
                    if (md.getResourceList().containsRsc(map)) {
                        md.getResourceList().removeRsc(map);
                        map.dispose();
                    }

                    map = new MeanArealPrecipResource(null,
                            new LoadProperties(), getDisplayType()
                                    .toUpperCase(), getGridColorMap());
                    md.getResourceList().add(map);
                    displayManager.setDisplayedResource(map);
                }
            } else {
                if (md.getResourceList().containsRsc(timeLapseRsc)) {
                    md.getResourceList().removeRsc(timeLapseRsc);
                    timeLapseRsc.dispose();
                }

                if ((bestEstRsc != null)
                        && md.getResourceList().containsRsc(bestEstRsc)) {
                    md.getResourceList().removeRsc(bestEstRsc);
                    bestEstRsc.dispose();
                }

                // Get the xmrg file
                String dirname = AppsDefaults.getInstance().getToken(
                        HydroConstants.XMRG_DIR_TOKEN);
                String fname = FileUtil.join(dirname, cvUse
                        + HydroConstants.QPE_DATE_FORMAT.format(getDataDate())
                        + "z");

                XmrgFile xmrg = new XmrgFile(fname);

                previousResource = getDisplayedResource();
                timeLapseRsc = new TimeLapseResource(cvUse, accumInterval,
                        xmrg, getGridColorMap(), getDataDate());
                md.getResourceList().add(timeLapseRsc);
                if (md.getResourceList().contains(previousResource)) {
                    md.getResourceList().remove(previousResource);
                }

                LoopProperties loopProps = ((AbstractEditor) EditorUtil
                        .getActiveEditor()).getLoopProperties();
                int frameRate = Integer.parseInt(AppsDefaults.getInstance()
                        .getToken("hydroview_mpe_timelapse", "1000"));
                loopProps.setFwdFrameTime(frameRate);
                loopProps.setLooping(true);

                try {
                    md.redoTimeMatching();
                } catch (VizException e) {
                    e.printStackTrace();
                }
            }

            md.getResourceList()
                    .add(ResourcePair
                            .constructSystemResourcePair(new GenericResourceData(
                                    MPELegendResource.class)));
            md.getResourceList().instantiateResources(md, true);
            pane.refresh();
        }
    }

    /**
     * Gets the xmrg data from the file.
     * 
     * @param dataType
     *            DisplayFieldData type
     * @return short[][] of data
     * @throws IOException
     */
    public static short[] getXmrgData(String cvUse) throws IOException {
        String cv_use = cvUse;
        String dirname = AppsDefaults.getInstance().getToken(
                HydroConstants.XMRG_DIR_TOKEN);
        String fname = FileUtil.join(
                dirname,
                cv_use
                        + HydroConstants.QPE_DATE_FORMAT
                                .format(HydroDisplayManager.getInstance()
                                        .getDataDate()) + "z");

        XmrgFile file = new XmrgFile(fname);
        file.load();
        short[] data = file.getData();

        return data;
    }

    /**
     * @return colormap for gages
     */
    private List<Colorvalue> getGridColorMap() {
        String user_id = System.getProperty("user.name");
        String app_name = MPE_APPLICATION_NAME;

        String cvu = null;
        if (getCvUse().equalsIgnoreCase("MULTIHOUR")
                || getCvUse().equalsIgnoreCase("xmrg")) {
            cvu = "XMRG";
        } else {
            cvu = getCvUse();
        }

        List<Colorvalue> gColorSet = GetColorValues.get_colorvalues(user_id,
                app_name, cvu, 3600, "E", pColorSetGroup);

        return gColorSet;
    }

    // find a way to store if a data set is new or not, if not just return the
    // old set
    public List<Colorvalue> getGageColorMap(String userId, String useName,
            int duration) {

        if ((gageColorSet == null) || !this.userId.equals(userId)
                || !this.useName.equals(useName) || (this.duration != duration)
                || HydroDisplayManager.getInstance().isColorChanged()) {
            this.userId = userId;
            this.useName = useName;
            this.duration = duration;
            List<NamedColorUseSet> pColorSetGroup = getNamedColorUseSetList();
            gageColorSet = GetColorValues.get_colorvalues(userId,
                    HYDRO_APPLICATION_NAME, useName, duration, "E",
                    pColorSetGroup);
            HydroDisplayManager.getInstance().setColorChanged(false);
        }

        return gageColorSet;
    }

    public List<Colorvalue> getFFGColorMap(String userId, String useName,
            int duration) {
        if ((ffgColorSet == null) || !this.userId.equals(userId)
                || !this.useName.equals(useName) || (this.duration != duration)
                || HydroDisplayManager.getInstance().isColorChanged()) {
            this.userId = userId;
            this.useName = useName;
            this.duration = duration;
            List<NamedColorUseSet> pColorSetGroup = getNamedColorUseSetList();
            this.ffgColorSet = GetColorValues.get_colorvalues(userId,
                    HYDRO_APPLICATION_NAME, useName, duration, "E",
                    pColorSetGroup);
            HydroDisplayManager.getInstance().setColorChanged(false);
        }

        return ffgColorSet;
    }

    /**
     * Get the currently selected location id.
     * 
     * @return String The LID
     */
    public GageData getCurrentData() {
        return currentdata;
    }

    /**
     * Set the currently selected data.
     * 
     * @param gageData
     *            or null if none selected
     */
    public void setCurrentData(GageData currentdata) {
        this.currentdata = currentdata;
        HydroDisplayManager.getInstance().fireStationSelectionChangeEvent(
                new StationSelectionChangeEvent(currentdata));
    }

    /**
     * Sets the currently selected data.
     * 
     * @param lid
     *            The current station.
     */
    public void setCurrentData(String lid) {
        GageData newStation = new GageData();
        newStation.setLid(lid);

        setCurrentData(newStation);
    }

    /**
     * Clear the display of QPE data.
     */
    public void clearQPEData() {
        ResourceList rl = pane.getDescriptor().getResourceList();

        if (rl.containsRsc(bestEstRsc)) {
            rl.removeRsc(bestEstRsc);
            bestEstRsc.dispose();
        }

        if (rl.containsRsc(timeLapseRsc)) {
            rl.removeRsc(timeLapseRsc);
            timeLapseRsc.dispose();
        }

        if (rl.containsRsc(map)) {
            rl.removeRsc(map);
            map.dispose();
        }

        this.accumInterval = 0;
    }

    // Clears the Hydro display
    public void clearDisplay() {
        if (pane == null) {
            IEditorPart part = EditorUtil.getActiveEditor();
            AbstractEditor ae = ((AbstractEditor) part);
            pane = ae.getActiveDisplayPane();
        }
        ResourceList rl = pane.getDescriptor().getResourceList();
        Iterator<ResourcePair> iter = rl.iterator();
        ArrayList<AbstractVizResource> removeList = new ArrayList<AbstractVizResource>();
        while (iter.hasNext()) {
            ResourcePair rp = iter.next();
            if (!rp.getProperties().isMapLayer()) {
                // mark for removal
                removeList.add(rp.getResource());
            }
        }

        // remove
        for (AbstractVizResource rsc : removeList) {
            rl.removeRsc(rsc);
            rsc.dispose();
        }

        clearQPEData();
    }

    public void addStationSelectionListener(StationSelectionChangeListener sscl) {
        currentSelectionListenerList.add(sscl);
    }

    public void removeStationSelectionListener(
            StationSelectionChangeListener sscl) {
        if (currentSelectionListenerList.contains(sscl)) {
            currentSelectionListenerList.remove(sscl);
        }
    }

    /**
     * Fire a StationSelectionChangeEvent.
     * 
     * @param event
     *            The event
     */
    public void fireStationSelectionChangeEvent(
            StationSelectionChangeEvent event) {
        Iterator<StationSelectionChangeListener> iter = currentSelectionListenerList
                .iterator();

        while (iter.hasNext()) {
            StationSelectionChangeListener listener = iter.next();
            if (listener.getClass().toString().contains("StationListDlg")) {
                if (HydroDialogStatus.stationListDlgOpen == true) {
                    listener.notifyUpdate(event);
                }
            } else {
                listener.notifyUpdate(event);
            }
        }
    }

    /**
     * Set the RiverID.
     * 
     * @param gageData
     *            or null if none selected
     */
    public void setRiverID(String riverID) {
        currentdata.setRiverID(riverID);
    }

    /**
     * @return the drawStationFlag
     */
    public boolean isDrawStation() {
        return drawStation;
    }

    /**
     * @param drawStationFlag
     *            the drawStationFlag to set
     */
    public void setDrawStation(boolean drawStationFlag) {
        drawStation = drawStationFlag;
    }

    /**
     * @return the fontSize
     */
    public int getFontSize() {
        return fontSize;
    }

    /**
     * @param fontSize
     *            the fontSize to set
     */
    public void setFontSize(int fontSize) {
        this.fontSize = fontSize;
    }

    /**
     * Set the dam data list.
     * 
     * @param damList
     */
    public void setDamList(List<DamMaster> damList) {
        this.damList = damList;
    }

    public List<DamMaster> getDamList() {
        return damList;
    }

    /**
     * @return the displayDamId
     */
    public boolean isDisplayDamId() {
        return displayDamId;
    }

    /**
     * @param displayDamId
     *            the displayDamId to set
     */
    public void setDisplayDamId(boolean displayDamId) {
        this.displayDamId = displayDamId;
    }

    /**
     * @return the displayDamName
     */
    public boolean isDisplayDamName() {
        return displayDamName;
    }

    /**
     * @param displayDamName
     *            the displayDamName to set
     */
    public void setDisplayDamName(boolean displayDamName) {
        this.displayDamName = displayDamName;
    }

    /**
     * @return the displayDamIcon
     */
    public boolean isDisplayDamIcon() {
        return displayDamIcon;
    }

    /**
     * @param displayDamIcon
     *            the displayDamIcon to set
     */
    public void setDisplayDamIcon(boolean displayDamIcon) {
        this.displayDamIcon = displayDamIcon;
    }

    /**
     * @return the cvUse
     */
    public String getCvUse() {
        return cvUse;
    }

    /**
     * @param cvUse
     *            the cvUse to set
     */
    public void setCvUse(String cvUse) {
        this.cvUse = cvUse;
    }

    /**
     * @return the accumInterval
     */
    public int getAccumInterval() {
        return accumInterval;
    }

    /**
     * @param accumInterval
     *            the accumInterval to set
     */
    public void setAccumInterval(int accumInterval) {
        this.accumInterval = accumInterval;
    }

    /**
     * @return the dataDate
     */
    public Date getDataDate() {
        return dataDate;
    }

    /**
     * @param dataDate
     *            the dataDate to set
     */
    public void setDataDate(Date dataDate) {
        this.dataDate = dataDate;
    }

    /**
     * @return the displayType
     */
    public String getDisplayType() {
        return displayType;
    }

    /**
     * @param cvUse
     *            the cvUse to set
     */
    public void setDisplayType(String displayType) {
        this.displayType = displayType;
    }

    /**
     * @return the displayedResource
     */
    public AbstractVizResource<?, ?> getDisplayedResource() {
        return displayedResource;
    }

    /**
     * @param displayedResource
     *            the displayedResource to set
     */
    public void setDisplayedResource(AbstractVizResource<?, ?> displayedResource) {
        this.displayedResource = displayedResource;
    }

    /**
     * @return the previousResource
     */
    public AbstractVizResource<?, ?> getPreviousResource() {
        return previousResource;
    }

    /**
     * @param previousResource
     *            the previousResource to set
     */
    public void setPreviousResource(AbstractVizResource<?, ?> previousResource) {
        this.previousResource = previousResource;
    }

    /**
     * @return the accumulate
     */
    public boolean isAccumulate() {
        return accumulate;
    }

    /**
     * @param accumulate
     *            the accumulate to set
     */
    public void setAccumulate(boolean accumulate) {
        this.accumulate = accumulate;
    }

    /**
     * @return the pane
     */
    public IDisplayPane getPane() {
        return pane;
    }

    /**
     * @return the ids
     */
    public boolean isIds() {
        return ids;
    }

    /**
     * @param ids
     *            the ids to set
     */
    public void setIds(boolean ids) {
        this.ids = ids;
    }

    /**
     * @return the labels
     */
    public boolean isLabels() {
        return labels;
    }

    /**
     * @param labels
     *            the labels to set
     */
    public void setLabels(boolean labels) {
        this.labels = labels;
    }

    /**
     * @return the mpeLegend
     */
    public MPELegendResource getMpeLegend() {
        return mpeLegend;
    }

    /**
     * @param mpeLegend
     *            the mpeLegend to set
     */
    public void setMpeLegend(MPELegendResource mpeLegend) {
        this.mpeLegend = mpeLegend;
    }

    /**
     * @return the timeLapseMode
     */
    public boolean isTimeLapseMode() {
        return timeLapseMode;
    }

    /**
     * @param timeLapseMode
     *            the timeLapseMode to set
     */
    public void setTimeLapseMode(boolean timeLapseMode) {
        this.timeLapseMode = timeLapseMode;
    }

    /**
     * @return the NamedColorUseSet list
     */
    public List<NamedColorUseSet> getNamedColorUseSetList() {
        HydroColorManager colorManager = HydroColorManager.getInstance();
        NamedColorSetGroup ncsg = colorManager.readColorValuesFromDatabase();

        return ncsg.getColorGroupArray();
    }

    /**
     * Get the default named color use set list.
     * 
     * @return List<NamedColorUseSet>
     */
    public List<NamedColorUseSet> getDefaultNamedColorUseSetList() {
        HydroColorManager colorManager = HydroColorManager.getInstance();
        NamedColorSetGroup ncsg = colorManager.getDefaultColorSetGroup();

        return ncsg.getColorGroupArray();
    }

    /**
     * @return the dataChanged
     */
    public boolean isDataChanged() {
        return dataChanged;
    }

    /**
     * @param dataChanged
     *            the dataChanged to set
     */
    public void setDataChanged(boolean dataChanged) {
        this.dataChanged = dataChanged;
    }

    /**
     * @return the colorChanged
     */
    public boolean isColorChanged() {
        return colorChanged;
    }

    /**
     * @param colorChanged
     *            the colorChanged to set
     */
    public void setColorChanged(boolean colorChanged) {
        this.colorChanged = colorChanged;
    }
}
