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
package com.raytheon.viz.gfe.rsc;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.legend.AbstractLegendResource;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.INewModelAvailableListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.ShowQuickViewDataMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 *
 * Port of SELegendVisual from AWIPS I GFE
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 17, 2008           chammack  Initial Creation.
 * Aug 19, 2009  2547     rjpeter   Implement Test/Prac database display.
 * Jul 10, 2012  15186    ryu       Clean up initInternal per Ron
 * Nov 30, 2012  1328     mschenke  Made GFE use descriptor for time matching
 *                                  and time storage and manipulation
 * Jan 22, 2013  1518     randerso  Removed use of Map with Parms as keys,
 *                                  really just needed a list anyway.
 * Nov 20, 2013  2331     randerso  Corrected legend for Topography
 * Mar 10, 2016  5479     randerso  Use improved GFEFonts API
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author chammack
 */
public class GFELegendResource
        extends AbstractLegendResource<GFELegendResourceData>
        implements IMessageClient, INewModelAvailableListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFELegendResource.class);

    /** GFE Legend Modes */
    public static enum LegendMode {
        /** Show only the spatial editor time */
        SETIME("Hide"),

        /** Show all weather elements */
        GRIDS("Show All Weather Elements"),

        /** Show only mutable weather elements */
        MUTABLE("Show Fcst Weather Elements"),

        /** Show only the active weather element */
        ACTIVE("Show Active Weather Element"),

        /** Show map legends */
        MAPS("Show Map");

        private String str;

        LegendMode(String str) {
            this.str = str;
        }

        @Override
        public String toString() {
            return str;
        }

    };

    private class GFELegendInputHandler extends InputAdapter {

        private ResourcePair mouseDownRsc = null;

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            if ((mouseButton == 1) || (mouseButton == 2)) {
                mouseDownRsc = checkLabelSpace(descriptor,
                        getResourceContainer().getActiveDisplayPane()
                                .getTarget(),
                        x, y);
                if (mouseDownRsc != null) {
                    return true;
                }
            }
            return false;
        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            if (mouseDownRsc != null) {
                return true;
            } else {
                return super.handleMouseDownMove(x, y, mouseButton);
            }
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            ResourcePair rsc = checkLabelSpace(descriptor,
                    getResourceContainer().getActiveDisplayPane().getTarget(),
                    x, y);
            if ((rsc != null) && (rsc == mouseDownRsc)) {
                mouseDownRsc = null;
                if (mouseButton == 1) {
                    ResourceProperties props = rsc.getProperties();
                    if (props != null) {

                        if (rsc.getResource() instanceof GFEResource) {
                            GFEResource gfeRsc = (GFEResource) rsc
                                    .getResource();
                            GFELegendResource.this.dataManager
                                    .getSpatialDisplayManager()
                                    .makeVisible(gfeRsc.getParm(),
                                            !props.isVisible(), false);

                        } else {
                            props.setVisible(!props.isVisible());
                        }
                        issueRefresh();
                    }
                    return true;
                } else if (mouseButton == 2) {
                    if (rsc.getResource() instanceof GFEResource) {
                        GFEResource gfeRsc = (GFEResource) rsc.getResource();
                        ISpatialDisplayManager sdm = GFELegendResource.this.dataManager
                                .getSpatialDisplayManager();
                        Parm parm = gfeRsc.getParm();

                        try {
                            Parm activeParm = sdm.getActivatedParm();
                            if (gfeRsc.getParm().equals(activeParm)) {
                                sdm.activateParm(null);
                                return true;
                            } else {
                                IGridData grid = parm.overlappingGrid(
                                        sdm.getSpatialEditorTime());
                                if ((grid != null) && grid.isOkToEdit()) {
                                    sdm.activateParm(parm);
                                    return true;
                                } else if (grid == null) {
                                    statusHandler.handle(Priority.SIGNIFICANT,
                                            "No Grid to make editable");
                                } else if ((grid != null)
                                        && !grid.isOkToEdit()) {
                                    statusHandler.handle(Priority.SIGNIFICANT,
                                            "Grid cannot be edited");
                                }
                            }
                        } catch (GFEOperationFailedException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Unable to set parm editable", e);
                        }
                    }
                }
            }
            mouseDownRsc = null;
            return false;
        }
    }

    protected boolean localTime = true;

    protected SimpleDateFormat resourceDateFormat;

    protected DataManager dataManager;

    protected LegendMode mode;

    protected IFont font;

    protected IGraphicsTarget lastTarget;

    private GridID qvGrid;

    private IInputHandler handler = new GFELegendInputHandler();

    protected RGB imageLegendColor;

    /**
     * Constructor
     *
     * @param dataManager
     * @param resourceData
     * @param loadProps
     */
    public GFELegendResource(DataManager dataManager,
            GFELegendResourceData resourceData, LoadProperties loadProps) {
        super(resourceData, loadProps);
        resourceDateFormat = new SimpleDateFormat("E HH'Z' dd-MMM-yy");
        resourceDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        this.dataManager = dataManager;

        String color = GFEPreference.getString("ImageLegend_color", "White");
        imageLegendColor = RGBColors.getRGBColor(color);

        String s = GFEPreference.getString("LegendMode", "GRIDS");
        try {
            mode = LegendMode.valueOf(s);
        } catch (Exception e) {
            statusHandler.error(String.format(
                    "GFE config file '%' contains an invalid value for preference: LegendMode",
                    GFEPreference.getConfigName()), e);
            mode = LegendMode.GRIDS;
        }
    }

    protected void addSpaces(StringBuilder sb, int numSpace) {
        for (int i = 0; i < numSpace; i++) {
            sb.append(" ");
        }
    }

    @Override
    public LegendEntry[] getLegendData(IDescriptor descriptor) {
        LegendData[] data = null;
        switch (mode) {
        case GRIDS:
        case MUTABLE:
        case ACTIVE:
            data = getLegendDataGrids(descriptor);
            break;
        case SETIME:
            data = getLegendDataSETime(descriptor);
            break;
        case MAPS:
            data = getLegendDataMaps(descriptor);
            break;
        }

        LegendEntry[] entries = new LegendEntry[data.length];
        for (int i = 0; i < entries.length; ++i) {
            entries[i] = new LegendEntry();
            entries[i].font = font;
            entries[i].legendParts = new LegendData[] { data[i] };
        }
        return entries;

    }

    /**
     * Gets an ordered list of Parm/ResourcePair pairs to display for the
     * legend.
     *
     * @param descriptor
     * @return the parm/resource pairs
     */
    protected List<Pair<Parm, ResourcePair>> getLegendOrderedParms(
            IDescriptor descriptor) {
        List<Pair<Parm, ResourcePair>> parms = new ArrayList<>();
        for (ResourcePair rp : descriptor.getResourceList()) {
            if (rp.getResource() instanceof GFEResource) {
                Parm parm = ((GFEResource) rp.getResource()).getParm();
                if ((qvGrid == null)
                        || ((qvGrid != null) && (qvGrid.getParm() == parm))) {
                    parms.add(new Pair<>(parm, rp));
                    if (qvGrid != null) {
                        break;
                    }
                }
            }
        }

        Collections.sort(parms, new Comparator<Pair<Parm, ResourcePair>>() {

            @Override
            public int compare(Pair<Parm, ResourcePair> o1,
                    Pair<Parm, ResourcePair> o2) {
                return o2.getFirst().compareTo(o1.getFirst());
            }
        });
        return parms;
    }

    private LegendData[] getLegendDataGrids(IDescriptor descriptor) {
        int lengthOfTime = "144h Tue 19Z 12-Sep-06".length();

        List<LegendData> legendDataList = new ArrayList<>();

        FramesInfo currInfo = descriptor.getFramesInfo();

        Parm activeParm = dataManager.getSpatialDisplayManager()
                .getActivatedParm();
        StringBuilder labelBuilder = new StringBuilder();

        List<Pair<Parm, ResourcePair>> parms = getLegendOrderedParms(
                descriptor);
        Parm qvParm = null;
        if (qvGrid != null) {
            qvParm = qvGrid.getParm();
        }

        int[] lengths = getLongestFields(parms);

        ParmID topoID = dataManager.getTopoManager().getCompositeParmID();

        // Topmost resources: GFE Parms
        for (Pair<Parm, ResourcePair> pair : parms) {
            Parm parm = pair.getFirst();
            ParmID parmId = parm.getParmID();
            DatabaseID dbId = parmId.getDbId();
            StringBuilder sb = new StringBuilder();
            ResourcePair rp = pair.getSecond();
            GFEResource rsc = (GFEResource) rp.getResource();
            LegendData ld = new LegendData();
            ResourceProperties props = rp.getProperties();

            // Skip parms that aren't supposed to be displayed in this mode
            if (!isGridDisplayed(parm)) {
                continue;
            }

            if (!props.isVisible()) {
                ld.color = ColorUtil.GREY;
            } else if ((parm == qvParm) || (VisMode.IMAGE == parm
                    .getDisplayAttributes().getVisMode())) {
                ld.color = imageLegendColor;
            } else {
                String legColor = GFEPreference.getString(
                        parm.getParmID().compositeNameUI() + "_Legend_color");
                if (!legColor.isEmpty()) {
                    ld.color = RGBColors.getRGBColor(legColor);
                } else {
                    ld.color = rsc.getCapability(ColorableCapability.class)
                            .getColor();
                }
            }

            // Check to see if parm is active

            if (parm == activeParm) {
                sb.append("(edit) ");
            } else {
                sb.append("       ");
            }

            // get the parm name
            String parmText = parmId.getParmName();

            if (parmId.equals(topoID)) {
                parmText = "Topography";
                sb.append(parmText);
                addSpaces(sb,
                        lengths[0] + lengths[1] + lengths[2] + lengths[3] + 15);
            } else {
                sb.append(parmText);

                int diff = lengths[0] - parmText.length();
                addSpaces(sb, diff + 1);

                String levelText = parmId.getParmLevel();
                sb.append(levelText);
                diff = lengths[1] - levelText.length();
                addSpaces(sb, diff + 1);

                // get the model name
                labelBuilder.setLength(0);
                labelBuilder.append(dbId.getShortModelId());

                // FIXME this is from A1 and is not consistent with the code in
                // getLongestFields

                // if (_showISCMode && _quickViewGrid == GridID()
                // && _grids[i].gridID().parm()->parmID().databaseID() ==
                // _dbss->dataManager()->parmMgr()->mutableDatabase())
                // {
                // unsigned int mpos = 0;
                // if (modelText.found(' ', mpos))
                // {
                // ParmID iscPID =
                // _dbss->dataManager()->parmMgr()->getISCParmID(
                // _grids[i].gridID().parm()->parmID());
                // TextString iscStr = "+" + iscPID.databaseID().type() +
                // iscPID.databaseID().model();
                // modelText.insertBefore(mpos, iscStr);
                // }
                // }

                sb.append(labelBuilder.toString());
                diff = lengths[3] - labelBuilder.length();
                addSpaces(sb, diff + 1);

                String unitText = "(" + parm.getGridInfo().getUnitString()
                        + ")";
                sb.append(unitText);
                diff = lengths[2] - unitText.length();
                addSpaces(sb, diff + 3);

                labelBuilder.setLength(0);
                if (parm.getGridInfo().isTimeIndependentParm()) {
                    labelBuilder.append(" Persistent");
                } else {
                    DataTime currRscTime = currInfo.getTimeForResource(rsc);
                    if (currRscTime != null) {
                        TimeRange tr = currRscTime.getValidPeriod();
                        labelBuilder.append(String.format("%3d",
                                tr.getDuration() / TimeUtil.MILLIS_PER_HOUR));
                        labelBuilder.append("h ");
                        labelBuilder.append(
                                resourceDateFormat.format(tr.getStart()));
                    } else {
                        labelBuilder.append(" <No Grid>");
                    }
                }

                diff = lengthOfTime - labelBuilder.length();

                sb.append(labelBuilder.toString());
                addSpaces(sb, diff);
            }

            ld.label = sb.toString();
            ld.resource = rp;
            legendDataList.add(ld);

        }

        return legendDataList.toArray(new LegendData[legendDataList.size()]);
    }

    private LegendData[] getLegendDataMaps(IDescriptor descriptor) {
        Iterator<ResourcePair> rl = descriptor.getResourceList().iterator();
        List<LegendData> legendDataList = new ArrayList<>();

        while (rl.hasNext()) {
            ResourcePair rp = rl.next();
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rp.getProperties().isMapLayer()) {
                LegendData ld = new LegendData();
                if (!rp.getProperties().isVisible()) {
                    ld.color = ColorUtil.GREY;
                } else {
                    ld.color = rsc.getCapability(ColorableCapability.class)
                            .getColor();
                }
                ld.label = rsc.getName();
                ld.resource = rp;
                legendDataList.add(ld);
            }

        }

        return legendDataList.toArray(new LegendData[legendDataList.size()]);
    }

    private LegendData[] getLegendDataSETime(IDescriptor descriptor) {
        Date seTime = dataManager.getSpatialDisplayManager()
                .getSpatialEditorTime();
        if (seTime == null) {
            return new LegendData[0];
        }

        LegendData ld = new LegendData();
        ld.color = ColorUtil.WHITE;
        ld.label = resourceDateFormat.format(seTime);

        return new LegendData[] { ld };
    }

    /**
     * Get the legend mode
     *
     * @return the legend mode
     */
    public LegendMode getLegendMode() {
        return mode;
    }

    /**
     * Works in a single pass to perform the operations performed in AWIPS I
     * getLargestLevelName, etc.
     *
     * The fields in order:
     * <LI>FieldName
     * <LI>LevelName
     * <LI>Units
     * <LI>ModelName
     *
     *
     * @param descriptor
     * @return
     */
    private int[] getLongestFields(List<Pair<Parm, ResourcePair>> parms) {
        // Iterator<ResourcePair> rl = descriptor.getResourceList().iterator();
        int[] sz = new int[4];
        StringBuilder labelBuilder = new StringBuilder();
        // synchronized (rl) {
        // while (rl.hasNext()) {
        for (Pair<Parm, ResourcePair> pair : parms) {
            Parm parm = pair.getFirst();
            ParmID parmId = parm.getParmID();
            sz[0] = Math.max(sz[0], parmId.getParmName().length());
            sz[1] = Math.max(sz[1], parmId.getParmLevel().length());
            sz[2] = Math.max(sz[2],
                    parm.getGridInfo().getUnitString().length());

            DatabaseID dbId = parmId.getDbId();
            labelBuilder.setLength(0);
            labelBuilder.append(dbId.getShortModelId());

            // FIXME this is A1 code and is not consistent with the code in
            // getLegendDataGrids

            // if (showIscMode
            // && ids[i].gridID().parm()->parmID().databaseID() ==
            // _dbss->dataManager()->parmMgr()->mutableDatabase())
            // label += "+VISC";

            sz[3] = Math.max(sz[3], labelBuilder.length());
        }

        return sz;
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();

        this.dataManager.getParmManager().removeNewModelAvailableListener(this);
        Message.unregisterInterest(this, ShowQuickViewDataMsg.class);

        if (font != null) {
            font.dispose();
            font = null;
        }
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(handler);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);

        Message.registerInterest(this, ShowQuickViewDataMsg.class);
        this.dataManager.getParmManager().addNewModelAvailableListener(this);

        font = GFEFonts.makeGFEIFont(target, "SELegend_font", 3);

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(handler, InputPriority.PERSPECTIVE);
        }
    }

    private boolean isGridDisplayed(Parm parm) {
        if ((mode == LegendMode.ACTIVE) && (dataManager
                .getSpatialDisplayManager().getActivatedParm() != parm)) {
            return false;
        }

        if ((mode == LegendMode.MUTABLE) && !dataManager.getParmManager()
                .getMutableDatabase().equals(parm.getParmID().getDbId())) {
            return false;
        }

        return true;

    }

    /**
     * Set the legend mode
     *
     * @param mode
     *            the legend mode
     */
    public void setLegendMode(LegendMode mode) {
        this.mode = mode;
        this.getProperties().setVisible(true);
    }

    @Override
    public void receiveMessage(Message message) {
        if (message instanceof ShowQuickViewDataMsg) {
            qvGrid = ((ShowQuickViewDataMsg) message).getGridId();
        }
    }

    @Override
    public void newModelAvailable(DatabaseID additions) {
        issueRefresh();
    }

}
