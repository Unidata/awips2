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
package com.raytheon.viz.gfe.dialogs;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.UIFormat;
import com.raytheon.viz.gfe.core.UIFormat.FilterType;
import com.raytheon.viz.gfe.core.griddata.DiscreteDataObject;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherDataObject;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import org.locationtech.jts.geom.Coordinate;

/**
 * Dialog to display information about the given grid
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 25, 2008           ebabin    Initial creation
 * Jun 20, 2008  875      bphillip  Implemented Dialog functionality
 * Sep 20, 2012  1190     dgilling  Use new WsId.getHostName() method.
 * Nov 12, 2012  1298     rferrel   Code cleanup for non-blocking dialog.
 * Jan 10, 2013  15572    jzeng     add getMaxWidth(String str) and
 *                                  adjustDlg(String str), change gridInfoText
 *                                  from Label to Text to make sure the info get
 *                                  displayed inside the screen and can be
 *                                  scrolled.
 * Feb 15, 2013  1638     mschenke  Moved Util.getUnixTime into TimeUtil
 * Dec 03, 2013  2597     randerso  Fixed spacing when displaying multiple grid
 *                                  histories. Fixed weather element state and
 *                                  data distribution for Weather and Discrete
 *                                  elements.
 * Jan 03, 2018  7178     randerso  Change to use IDataObject. Code cleanup.
 * Feb 08, 2018  6788     randerso  Made resizable, constrained to monitor
 *                                  bounds
 *
 * </pre>
 *
 * @author ebabin
 */

public class GridInfoDialog extends CaveJFACEDialog {

    private Parm parm;

    private DataManager dataMgr;

    private IGridData gridData;

    private UIFormat uiFormat;

    private Composite top;

    private static enum GridInfoElements {
        GRID_INFO("Grid Info"),
        GRID_HISTORY("Grid History"),
        ISC_HISTORY("ISC History"),
        WEATHER_ELEMENT_INFO("Weather Element Info"),
        WEATHER_ELEMENT_STATE("Weather Element State"),
        LOCKS("Locks"),
        DATA_DISTRIBUTION("Data Distribution");

        private String text;

        GridInfoElements(String text) {
            this.text = text;
        }

        @Override
        public String toString() {
            return text;
        }
    };

    // set gridInfoText to be Text
    private Text gridInfoText;

    private SimpleDateFormat gmtFormatter;

    /**
     * Constructor
     *
     * @param parent
     * @param parm
     * @param clickTime
     */
    public GridInfoDialog(Shell parent, Parm parm, Date clickTime) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS);
        this.parm = parm;
        this.dataMgr = parm.getDataManager();
        gridData = parm.overlappingGrid(clickTime);

        this.uiFormat = new UIFormat(dataMgr.getParmManager(),
                FilterType.DISPLAYED, FilterType.DISPLAYED);

        gmtFormatter = new SimpleDateFormat("MMM dd yy HH:mm:ss zzz");
        gmtFormatter.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) top.getLayout();
        layout.numColumns = 2;
        layout.makeColumnsEqualWidth = false;

        initializeComponents();

        return top;
    }

    private void initializeComponents() {
        Group group = new Group(top, SWT.BORDER);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        group.setLayoutData(layoutData);
        group.setLayout(new GridLayout(1, true));

        SelectionAdapter selectionAdapter = new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                buttonClicked(e);
            }
        };
        for (GridInfoElements element : GridInfoElements.values()) {
            Button b = new Button(group, SWT.RADIO);
            b.setData(element);
            b.setText(element.toString());
            b.addSelectionListener(selectionAdapter);
        }

        gridInfoText = new Text(top, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridInfoText.setBackground(group.getBackground());
        gridInfoText.setLayoutData(layoutData);
    }

    @Override
    protected void initializeBounds() {
        super.initializeBounds();
        Shell shell = getShell();
        shell.setMinimumSize(shell.getSize());
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, Window.CANCEL, "Cancel", false);

    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Grid Information");
    }

    private void buttonClicked(SelectionEvent e) {
        Button b = (Button) e.getSource();
        if (!b.getSelection()) {
            return;
        }

        GridInfoElements choice = (GridInfoElements) b.getData();

        String infoText;
        switch (choice) {
        case GRID_INFO:
            infoText = getGridInfo();
            break;

        case GRID_HISTORY:
            infoText = getGridHistory();
            break;

        case ISC_HISTORY:
            infoText = getISCHistory();
            break;

        case WEATHER_ELEMENT_INFO:
            infoText = getWEInfo();
            break;

        case WEATHER_ELEMENT_STATE:
            infoText = getWEState();
            break;

        case LOCKS:
            infoText = getLockInfo();
            break;

        case DATA_DISTRIBUTION:
            infoText = getDataDistribution();
            break;

        default:
            infoText = "";
            break;
        }
        gridInfoText.setText(infoText);

        Shell shell = this.getShell();
        /*
         * need to get monitor here as it can change after layout and pack
         * causing the dialog to jump to another monitor
         */
        Monitor monitor = shell.getMonitor();

        shell.layout();
        shell.pack();

        // constrain the shell bounds to the monitor bounds
        constrainShellToMonitor(shell, monitor);
    }

    private void constrainShellToMonitor(Shell shell, Monitor monitor) {
        Rectangle preferredSize = shell.getBounds();

        Rectangle constrained = new Rectangle(preferredSize.x, preferredSize.y,
                preferredSize.width, preferredSize.height);

        Rectangle bounds = monitor.getClientArea();

        if (constrained.height > bounds.height) {
            constrained.height = bounds.height;
        }

        if (constrained.width > bounds.width) {
            constrained.width = bounds.width;
        }

        constrained.x = Math.max(bounds.x, Math.min(constrained.x,
                bounds.x + bounds.width - constrained.width));
        constrained.y = Math.max(bounds.y, Math.min(constrained.y,
                bounds.y + bounds.height - constrained.height));

        if (!preferredSize.equals(constrained)) {
            shell.setBounds(constrained);
        }
    }

    private String getLockInfo() {
        StringBuilder info = new StringBuilder();
        info.append(getBoxTitle(null, false, "Current Data Locks"));
        List<Lock> locks = parm.getLockTable().getLocks();

        if (locks.isEmpty()) {
            info.append("\nWeather element is not locked");
        } else {
            info.append("\n");
            for (Lock lock : locks) {
                info.append("(")
                        .append(gmtFormatter
                                .format(lock.getTimeRange().getStart()))
                        .append(", ");
                info.append(gmtFormatter.format(lock.getTimeRange().getEnd()))
                        .append("):");
                info.append("Locked by ");
                if (lock.getWsId().equals(dataMgr.getWsId())) {
                    info.append("me\n");
                } else {
                    info.append(lock.getWsId()).append("\n");
                }
            }
        }

        return info.toString();
    }

    private String getISCHistory() {
        StringBuilder info = new StringBuilder();
        info.append(getBoxTitle(null, false, "ISC History"));

        GridID iscGridID = new GridID(parm,
                dataMgr.getSpatialDisplayManager().getSpatialEditorTime());
        if (!parm.isIscParm()) {
            iscGridID = dataMgr.getIscDataAccess().getISCGridID(iscGridID,
                    true);
        }

        IGridData grid = null;
        if (iscGridID != null) {
            grid = iscGridID.grid();
        }
        if (grid == null) {
            info.append("\nNo ISC Grid");
        } else {
            info.append("\nISC Grid Valid Time: ").append(grid.getGridTime())
                    .append("\n\n");

            GridDataHistory[] history = grid.getHistory();
            info.append("Site  ---Last Updated---\n");
            for (GridDataHistory h : history) {
                String site = h.getOriginParm().getDbId().getSiteId();
                String upTime = gmtFormatter.format(h.getUpdateTime());
                String ago = this.formatAgo(h.getUpdateTime());
                String pubTime = "";
                if (TimeUtil.getUnixTime(h.getPublishTime()) != 0) {
                    pubTime = " PUBLISHED";
                }
                info.append(site).append(" ").append(upTime).append(" ")
                        .append(ago).append(" ").append(pubTime).append("\n");

            }

            info.append("\n");
            info.append("Site  -----History Info-----\n");
            for (GridDataHistory h : history) {
                String site = h.getOriginParm().getDbId().getSiteId();
                String org = h.getOrigin().toString();
                String orgParm = uiFormat.uiParmIDcollapsed(h.getOriginParm());
                String orgTR = gmtFormatter
                        .format(h.getOriginTimeRange().getStart()) + " -> "
                        + gmtFormatter.format(h.getOriginTimeRange().getEnd());
                String timeMod = "Not Modified";
                String ago = "";
                String user = "";
                if (TimeUtil.getUnixTime(h.getTimeModified()) != 0) {
                    timeMod = gmtFormatter.format(h.getTimeModified());
                    ago = this.formatAgo(h.getTimeModified());
                    user = this.whoLabel(h.getModified());
                }

                info.append(site).append(" ").append(org).append(" ")
                        .append(orgParm).append(" ").append(orgTR).append(" ")
                        .append(timeMod).append(" ").append(ago).append(" ")
                        .append(user).append("\n");

            }
        }
        return info.toString();
    }

    private String getWEState() {
        StringBuilder info = new StringBuilder();

        info.append(getBoxTitle(null, false, "Weather Element State"));

        if (parm.getGridInfo().getGridType().equals(GridType.SCALAR)
                || parm.getGridInfo().getGridType().equals(GridType.VECTOR)) {

            info.append("\nFuzz Value: ")
                    .append(parm.getParmState().getFuzzValue()).append("\n");
        }

        if (parm.isMutable()) {

            if (parm.getGridInfo().getGridType().equals(GridType.SCALAR) || parm
                    .getGridInfo().getGridType().equals(GridType.VECTOR)) {
                info.append("Delta value: ")
                        .append(parm.getParmState().getDeltaValue())
                        .append("\n");
            }
            info.append("Assign Value: ")
                    .append(parm.getParmState().getPickUpValue()).append("\n");

            if (parm.getGridInfo().getGridType().equals(GridType.VECTOR)) {
                info.append("Vector Edit Mode: ")
                        .append(parm.getParmState().getVectorMode())
                        .append("\n");
            }
            if (parm.getGridInfo().getGridType().equals(GridType.DISCRETE)
                    || parm.getGridInfo().getGridType()
                            .equals(GridType.WEATHER)) {
                info.append("Weather/Discrete Combine Mode: ")
                        .append(parm.getParmState().getCombineMode())
                        .append("\n");
            }
        }

        info.append("Selected: ")
                .append((parm.getParmState().isSelected() ? "Yes" : "No"))
                .append("\n");

        if (parm.getParmState().isSelected()) {
            info.append("Selected Time Range: ")
                    .append(this.timeRangeToGMT(
                            parm.getParmState().getSelectedTimeRange()))
                    .append("\n");
        }
        info.append("Graphic Color: ")
                .append(formatRGB(parm.getDisplayAttributes().getBaseColor()))
                .append("\n");

        return info.toString();
    }

    private String formatRGB(RGB rgb) {
        return String.format("#%02x%02x%02x", rgb.red, rgb.green, rgb.blue);
    }

    private float getMaxMin(Grid2DFloat grid, boolean getMax) {
        float value = getMax ? -Float.MAX_VALUE : Float.MAX_VALUE;

        for (int y = 0; y < grid.getYdim(); y++) {
            for (int x = 0; x < grid.getXdim(); x++) {
                if (getMax) {
                    value = Math.max(value, grid.get(x, y));
                } else {
                    value = Math.min(value, grid.get(x, y));
                }
            }
        }
        return value;
    }

    private int[] calcCounts(Grid2DByte grid, int numKeys) {
        int[] counts = new int[numKeys];

        byte[] data = grid.getBytes();
        for (int i = 0; i < data.length; i++) {
            int index = data[i] & 0xFF;
            counts[index]++;
        }
        return counts;
    }

    private String getDataDistribution() {

        StringBuilder info = new StringBuilder();
        info.append(getBoxTitle(gridData, true, "Data Distribution"));

        if (gridData == null) {
            info.append("\nNo Grid");
        } else {
            info.append("\n");
            if (parm.getGridInfo().getGridType().equals(GridType.SCALAR)) {
                Grid2DFloat grid = ((ScalarGridData) gridData).getDataObject()
                        .getScalarGrid();
                info.append("Maximum Data Value: ")
                        .append(getMaxMin(grid, true))
                        .append("\nMinimum Data Value: ")
                        .append(getMaxMin(grid, false)).append("\n");
            } else if (parm.getGridInfo().getGridType()
                    .equals(GridType.VECTOR)) {
                Grid2DFloat grid = ((VectorGridData) gridData).getDataObject()
                        .getMagGrid();
                info.append("Maximum Data Value: ")
                        .append(getMaxMin(grid, true))
                        .append("\nMinimum Data Value: ")
                        .append(getMaxMin(grid, false)).append("\n");
            } else if (parm.getGridInfo().getGridType()
                    .equals(GridType.WEATHER)) {
                WeatherDataObject dataObject = ((WeatherGridData) gridData)
                        .getDataObject();
                Grid2DByte grid = dataObject.getWeatherGrid();
                WeatherKey[] keys = dataObject.getKeys();

                int[] counts = calcCounts(grid, keys.length);
                for (int i = 0; i < keys.length; i++) {
                    info.append(counts[i]).append(" ----> ");
                    info.append(keys[i].toPrettyString());
                    info.append("\n");
                }
            } else if (parm.getGridInfo().getGridType()
                    .equals(GridType.DISCRETE)) {
                DiscreteDataObject dataObject = ((DiscreteGridData) gridData)
                        .getDataObject();
                Grid2DByte grid = dataObject.getDiscreteGrid();
                DiscreteKey[] keys = dataObject.getKeys();
                int[] counts = calcCounts(grid, keys.length);
                for (int i = 0; i < keys.length; i++) {
                    info.append(counts[i]).append(" ----> ");
                    info.append(keys[i].toString());
                    info.append("\n");
                }
            }
        }

        return info.toString();

    }

    private String getGridInfo() {
        StringBuilder info = new StringBuilder();
        info.append(getBoxTitle(gridData, true, "Grid Information"));

        if (gridData == null) {
            info.append("\nNo Grid");
        } else {
            info.append("\nGrid okay to edit: ")
                    .append((gridData.isOkToEdit() ? "Yes" : "No"))
                    .append("\n");

            info.append("Grid Lock State: ");
            List<Lock> locks = parm.getLockTable().getLocks();
            boolean found = false;
            for (Lock lock : locks) {
                if (lock.getTimeRange().overlaps(gridData.getGridTime())) {
                    found = true;
                    if (lock.getWsId().equals(dataMgr.getWsId())) {
                        info.append("Locked by me\n");
                    } else {
                        info.append("Locked by ").append(lock.getWsId())
                                .append("\n");
                    }
                    break;
                }
            }
            if (!found) {
                info.append("Unlocked\n");
            }
        }

        return info.toString();
    }

    private String getWEInfo() {
        StringBuilder info = new StringBuilder();
        info.append(getBoxTitle(null, false, "Weather Element Information"));

        info.append("\nWeather Element Name: ")
                .append(parm.getParmID().getParmName()).append("\n");

        info.append("Weather Element Level: ")
                .append(parm.getParmID().getParmLevel()).append("\n");

        info.append("Weather Element Model: ")
                .append(uiFormat.uiDatabaseID(parm.getParmID().getDbId()))
                .append("\n");

        info.append("Modified, but not yet saved: ")
                .append((parm.isModified() ? "Yes" : "No")).append("\n");

        info.append("Mutable Weather Element: ")
                .append((parm.isMutable() ? "Yes" : "No")).append("\n");

        info.append("Weather Element Type: ")
                .append(parm.getGridInfo().getGridType()).append("\n");

        info.append("Units: ").append(parm.getGridInfo().getUnitString())
                .append("\n");

        java.awt.Point gridSize = parm.getGridInfo().getGridLoc().gridSize();
        info.append("Grid Size is: (").append(gridSize.x).append(",")
                .append(gridSize.y).append(")\n");

        Coordinate origin = parm.getGridInfo().getGridLoc().getOrigin();
        Coordinate extent = parm.getGridInfo().getGridLoc().getExtent();
        DecimalFormat df = new DecimalFormat("0.#");
        info.append("Grid Location is: [o=(").append(df.format(origin.x))
                .append(",").append(df.format(origin.y)).append("),e=(")
                .append(df.format(extent.x)).append(",")
                .append(df.format(extent.y)).append(")]\n");
        info.append("Grid Projection is: ").append(parm.getGridInfo()
                .getGridLoc().getProjection().getProjectionID()).append("\n");

        if (parm.getGridInfo().getGridType().equals(GridType.SCALAR)
                || parm.getGridInfo().getGridType().equals(GridType.VECTOR)) {
            info.append("Minimum Allowed Value: ")
                    .append(parm.getGridInfo().getMinValue())
                    .append("  Maximum Allowed Value: ")
                    .append(parm.getGridInfo().getMaxValue()).append("\n");

        }

        info.append("Descriptive Name: ")
                .append(parm.getGridInfo().getDescriptiveName()).append("\n");

        TimeConstraints sb = parm.getGridInfo().getTimeConstraints();

        info.append("Time Constraints:  Duration=");

        info.append(sb.getDuration() / 3600).append("hr. Repeats every ");
        info.append(sb.getRepeatInterval() / 3600).append("hr. Starts at ");
        info.append(sb.getStartTime() / 3600).append("Z\n");

        info.append("Rate Dependent WE: ")
                .append((parm.getGridInfo().isRateParm() ? "Yes" : "No"))
                .append("\n");

        return info.toString();

    }

    private String getGridHistory() {
        StringBuilder info = new StringBuilder();
        info.append(getBoxTitle(gridData, true, "Grid History"));

        if (gridData == null) {
            info.append("No Grid");
        } else {
            for (GridDataHistory h : gridData.getHistory()) {

                info.append("\nGrid origin: ").append(h.getOrigin())
                        .append("\n");
                info.append("Original Source: ")
                        .append(uiFormat.uiParmIDcollapsed(h.getOriginParm()))
                        .append("\n");

                info.append("Original ValidTime: ")
                        .append(this.timeRangeToGMT(h.getOriginTimeRange()))
                        .append("\n");

                if (!gridData.getGridTime().equals(h.getOriginTimeRange())) {
                    info.append("Grid has been time-shifted\n");
                }
                if (h.getTimeModified() == null) {
                    info.append("Grid not modified\n");
                } else {
                    info.append("Grid last modified at: ")
                            .append(gmtFormatter
                                    .format(h.getTimeModified().getTime()))
                            .append(" ").append(formatAgo(h.getTimeModified()))
                            .append("\n");
                    if (h.getWhoModified().equals(dataMgr.getWsId())) {
                        info.append("Modified by me\n");
                    } else {
                        info.append("Modified by user: ")
                                .append(whoLabel(h.getWhoModified()))
                                .append("\n");
                    }
                }
                if (h.getPublishTime() != null) {
                    info.append("Last Published: ")
                            .append(gmtFormatter
                                    .format(h.getPublishTime().getTime()))
                            .append(" ").append(formatAgo(h.getPublishTime()))
                            .append("\n");
                }
                if (h.getUpdateTime() != null) {
                    info.append("Last Stored: ")
                            .append(gmtFormatter
                                    .format(h.getUpdateTime().getTime()))
                            .append(" ").append(formatAgo(h.getUpdateTime()))
                            .append("\n");
                }
                if (h.getLastSentTime() != null) {
                    info.append("Last Sent: ")
                            .append(gmtFormatter
                                    .format(h.getLastSentTime().getTime()))
                            .append(" ").append(formatAgo(h.getLastSentTime()))
                            .append("\n");
                }
            }
        }

        return info.toString();
    }

    private String formatAgo(Date modTime) {

        long tdiff = (SimulatedTime.getSystemTime().getTime().getTime()
                - modTime.getTime());

        if (tdiff < TimeUtil.MILLIS_PER_MINUTE) {
            return "(< 1 minute ago)";
        } else if (tdiff < TimeUtil.MILLIS_PER_HOUR) {
            return "(" + (tdiff / TimeUtil.MILLIS_PER_MINUTE) + " minutes ago)";
        } else if (tdiff < TimeUtil.MILLIS_PER_DAY) {
            long hours = (tdiff / TimeUtil.MILLIS_PER_HOUR);
            long minutes = (tdiff % TimeUtil.MILLIS_PER_HOUR)
                    / TimeUtil.MILLIS_PER_MINUTE;

            return "(" + hours + " hours " + minutes + " minutes ago)";
        } else {
            return "(more than 1 day ago)";
        }
    }

    private String getBoxTitle(IGridData gridData, boolean includeGridID,
            String title) {

        StringBuilder info = new StringBuilder();
        info.append("Weather Element: ")
                .append(uiFormat.uiParmIDcollapsed(parm.getParmID()))
                .append("\n");

        if (includeGridID) {
            info.append("Grid Valid Time: ");
            if (gridData != null) {
                info.append(this.timeRangeToGMT(gridData.getGridTime()));
            } else {
                info.append("No Grid");
            }
            info.append("\n");
        }

        info.append("\n").append(title).append(":\n");
        return info.toString();
    }

    private String timeRangeToGMT(TimeRange range) {
        if (range.isValid()) {
            return "(" + gmtFormatter.format(range.getStart()) + ", "
                    + gmtFormatter.format(range.getEnd()) + ")";
        } else {
            return range.toString();
        }
    }

    private String whoLabel(WsId wsId) {
        String progName = wsId.getProgName();
        String hostname = wsId.getHostName();

        String label = wsId.getUserName() + " (" + progName + ")" + "  on: "
                + hostname;
        return label;
    }
}
