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

import static com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode.IMAGE;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
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
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.PreferenceInitializer;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.ShowQuickViewDataMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * 
 * Port of SELegendVisual from AWIPS I GFE
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/17/2008              chammack    Initial Creation.
 * 08/19/2009   2547       rjpeter     Implement Test/Prac database display.
 * 07/10/2012   15186      ryu         Clean up initInternal per Ron
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class GFELegendResource extends
        AbstractLegendResource<GFELegendResourceData> implements IMessageClient {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFELegendResource.class);

    public static enum LegendMode {
        SETIME("Hide"), GRIDS("Show All Weather Elements"), MUTABLE(
                "Show Fcst Weather Elements"), ACTIVE(
                "Show Active Weather Element"), MAPS("Show Map");

        private String str;

        LegendMode(String str) {
            this.str = str;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return str;
        }

    };

    private class GFELegendInputHandler extends InputAdapter {

        private boolean inDrag;

        ResourcePair mouseDownRsc = null;

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            if (mouseButton == 1 || mouseButton == 2) {
                mouseDownRsc = checkLabelSpace(descriptor,
                        getResourceContainer().getActiveDisplayPane()
                                .getTarget(), x, y);
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
            if (rsc != null && rsc == mouseDownRsc) {
                mouseDownRsc = null;
                if (mouseButton == 1) {
                    ResourceProperties props = rsc.getProperties();
                    if (props != null) {

                        if (rsc.getResource() instanceof GFEResource) {
                            GFEResource gfeRsc = (GFEResource) rsc
                                    .getResource();
                            DataManager
                                    .getCurrentInstance()
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
                        ISpatialDisplayManager sdm = DataManager
                                .getCurrentInstance()
                                .getSpatialDisplayManager();
                        Parm parm = gfeRsc.getParm();

                        try {
                            Parm activeParm = sdm.getActivatedParm();
                            if (gfeRsc.getParm().equals(activeParm)) {
                                sdm.activateParm(null);
                                return true;
                            } else {
                                IGridData grid = parm.overlappingGrid(sdm
                                        .getSpatialEditorTime());
                                if (grid != null && grid.isOkToEdit()) {
                                    sdm.activateParm(parm);
                                    return true;
                                } else if (grid == null) {
                                    statusHandler.handle(Priority.SIGNIFICANT,
                                            "No Grid to make editable");
                                } else if (grid != null && !grid.isOkToEdit()) {
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

    protected static RGB imageLegendColor = new RGB(255, 255, 255);

    static {
        new PreferenceInitializer() {
            @Override
            public void init() {
                IPreferenceStore prefs = Activator.getDefault()
                        .getPreferenceStore();

                if (prefs.contains("ImageLegend_color")) {
                    String color = prefs.getString("ImageLegend_color");
                    imageLegendColor = RGBColors.getRGBColor(color);
                }
            }
        }.run();
    }

    @SuppressWarnings("unchecked")
    public GFELegendResource(DataManager dataManager,
            GFELegendResourceData resourceData, LoadProperties loadProps) {
        super(resourceData, loadProps);
        resourceDateFormat = new SimpleDateFormat("E HH'Z' dd-MMM-yy");
        resourceDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        this.dataManager = dataManager;

        String s = Activator.getDefault().getPreferenceStore()
                .getString("LegendMode");
        try {
            mode = LegendMode.valueOf(s);
        } catch (Exception e) {
            mode = LegendMode.GRIDS;
        }

        Message.registerInterest(this, ShowQuickViewDataMsg.class);
    }

    protected void addSpaces(StringBuilder sb, int numSpace) {
        for (int i = 0; i < numSpace; i++) {
            sb.append(" ");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#finalize()
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void finalize() throws Throwable {
        // FIXME: this needs to be a dispose method.
        Message.unregisterInterest(this, ShowQuickViewDataMsg.class);

        if (font != null) {
            font.dispose();
            font = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.legend.ILegendDecorator#getLegendData(com.raytheon
     * .viz.core.drawables.IDescriptor)
     */
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
     * Gets an ordered collection of Parms to display for the legend.
     * 
     * @param descriptor
     * @param parmRscMap
     *            optional map to create Parm->ResourcePair mapping for parms
     *            returned
     * @return
     */
    protected Collection<Parm> getLegendOrderedParms(IDescriptor descriptor,
            Map<Parm, ResourcePair> parmRscMap) {
        List<Parm> parms = new ArrayList<Parm>();
        for (ResourcePair rp : descriptor.getResourceList()) {
            if (rp.getResource() instanceof GFEResource) {
                Parm parm = ((GFEResource) rp.getResource()).getParm();
                if (qvGrid == null
                        || (qvGrid != null && qvGrid.getParm() == parm)) {
                    parms.add(parm);
                    if (parmRscMap != null) {
                        parmRscMap.put(parm, rp);
                    }
                    if (qvGrid != null) {
                        break;
                    }
                }
            }
        }

        Collections.sort(parms);
        Collections.reverse(parms);
        return parms;
    }

    private LegendData[] getLegendDataGrids(IDescriptor descriptor) {
        int lengthOfTime = "144h Tue 19Z 12-Sep-06".length();

        List<LegendData> legendDataList = new ArrayList<LegendData>();

        FramesInfo currInfo = descriptor.getFramesInfo();

        Parm activeParm = dataManager.getSpatialDisplayManager()
                .getActivatedParm();
        StringBuilder labelBuilder = new StringBuilder();

        Map<Parm, ResourcePair> parmRscMap = new HashMap<Parm, ResourcePair>();
        Collection<Parm> parms = getLegendOrderedParms(descriptor, parmRscMap);
        Parm qvParm = null;
        if (qvGrid != null) {
            qvParm = qvGrid.getParm();
        }

        int[] lengths = getLongestFields(parms);

        ParmID topoID = dataManager.getTopoManager().getCompositeParmID();

        // Topmost resources: GFE Parms
        for (Parm parm : parms) {
            ParmID parmId = parm.getParmID();
            DatabaseID dbId = parmId.getDbId();
            StringBuilder sb = new StringBuilder();
            ResourcePair rp = parmRscMap.get(parm);
            GFEResource rsc = (GFEResource) rp.getResource();
            LegendData ld = new LegendData();
            ResourceProperties props = rp.getProperties();

            // Skip parms that aren't supposed to be displayed in this mode
            if (!isGridDisplayed(parm)) {
                continue;
            }

            if (!props.isVisible()) {
                ld.color = ColorUtil.GREY;
            } else if ((parm == qvParm)
                    || (IMAGE == parm.getDisplayAttributes().getVisMode())) {
                ld.color = imageLegendColor;
            } else {
                String legColor = Activator
                        .getDefault()
                        .getPreferenceStore()
                        .getString(
                                parm.getParmID().compositeNameUI()
                                        + "_Legend_color");
                if (!legColor.equals("")) {
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
            sb.append(parmText);

            if (parmId.equals(topoID)) {
                addSpaces(sb, lengths[0] + lengths[1] + lengths[2] + lengths[3]
                        + 15);
            } else {

                int diff = lengths[0] - parmText.length();
                addSpaces(sb, diff + 1);

                String levelText = parmId.getParmLevel();
                sb.append(levelText);
                diff = lengths[1] - levelText.length();
                addSpaces(sb, diff + 1);

                // get the model name
                labelBuilder.setLength(0);
                labelBuilder.append(dbId.getModelName());

                boolean iscTyped = false;
                if (dataManager.getParmManager().iscMode()
                        && dbId.equals(dataManager.getParmManager()
                                .getMutableDatabase())) {

                    ParmID iscPID = dataManager.getParmManager().getISCParmID(
                            parmId);
                    if (iscPID.isValid()) {
                        // vparms (i.e. temp hazards) can't get here
                        String iscStr = "+" + iscPID.getDbId().getDbType()
                                + iscPID.getDbId().getModelName();
                        labelBuilder.append(iscStr);
                        iscTyped = true;
                    }
                }

                if (!iscTyped) {
                    String type = dbId.getDbType();
                    if ((type != null) && (type.length() > 0)) {
                        labelBuilder.append("_");
                        labelBuilder.append(type);
                    }
                }

                labelBuilder.append(" (" + dbId.getSiteId() + ")");
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
                                tr.getDuration() / 3600000));
                        labelBuilder.append("h ");
                        labelBuilder.append(resourceDateFormat.format(tr
                                .getStart()));
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
        List<LegendData> legendDataList = new ArrayList<LegendData>();

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
     * The fields in order: <LI>FieldName <LI>LevelName <LI>Units <LI>ModelName
     * 
     * 
     * @param descriptor
     * @return
     */
    private int[] getLongestFields(Collection<Parm> parms) {
        // Iterator<ResourcePair> rl = descriptor.getResourceList().iterator();
        int[] sz = new int[4];
        StringBuilder labelBuilder = new StringBuilder();
        // synchronized (rl) {
        // while (rl.hasNext()) {
        for (Parm parm : parms) {
            // AbstractVizResource<?, ?> resource = rl.next().getResource();
            // if (resource instanceof GFEResource) {
            // Parm parm = ((GFEResource) resource).getParm();
            ParmID parmId = parm.getParmID();
            sz[0] = Math.max(sz[0], parmId.getParmName().length());
            sz[1] = Math.max(sz[1], parmId.getParmLevel().length());
            sz[2] = Math
                    .max(sz[2], parm.getGridInfo().getUnitString().length());

            DatabaseID dbId = parmId.getDbId();
            labelBuilder.setLength(0);
            labelBuilder.append(dbId.getModelName());

            boolean iscTyped = false;
            if (dataManager.getParmManager().iscMode()
                    && dbId.equals(dataManager.getParmManager()
                            .getMutableDatabase())) {

                ParmID iscPID = dataManager.getParmManager().getISCParmID(
                        parmId);
                if (iscPID.isValid()) {
                    // vparms (i.e. temp hazards) can't get here
                    String iscStr = "+" + iscPID.getDbId().getDbType()
                            + iscPID.getDbId().getModelName();
                    labelBuilder.append(iscStr);
                    iscTyped = true;
                }
            }

            if (!iscTyped) {
                String type = dbId.getDbType();
                if ((type != null) && (type.length() > 0)) {
                    labelBuilder.append("_");
                    labelBuilder.append(type);
                }
            }

            labelBuilder.append(" (" + dbId.getSiteId() + ")");
            // TODO: FIXME
            // if (showIscMode
            // && ids[i].gridID().parm()->parmID().databaseID() ==
            // _dbss->dataManager()->parmMgr()->mutableDatabase())
            // label += "+VISC";
            sz[3] = Math.max(sz[3], labelBuilder.length());

            // }
        }
        // }

        return sz;
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        if (font != null) {
            font.dispose();
        }
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(handler);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        int fontNum = 3;
        if (GFEPreference.contains("SELegend_font")) {
            fontNum = GFEPreference.getIntPreference("SELegend_font");
        }
        font = GFEFonts.getFont(target, fontNum);

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(handler, InputPriority.PERSPECTIVE);
        }
    }

    private boolean isGridDisplayed(Parm parm) {
        if ((mode == LegendMode.ACTIVE)
                && (dataManager.getSpatialDisplayManager().getActivatedParm() != parm)) {
            return false;
        }

        if ((mode == LegendMode.MUTABLE)
                && !dataManager.getParmManager().getMutableDatabase()
                        .equals(parm.getParmID().getDbId())) {
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.Message.IMessageClient#receiveMessage(
     * com.raytheon.viz.gfe.core.msgs.Message)
     */
    @Override
    public void receiveMessage(Message message) {
        if (message instanceof ShowQuickViewDataMsg) {
            qvGrid = ((ShowQuickViewDataMsg) message).getGridId();
        }
    }

}
