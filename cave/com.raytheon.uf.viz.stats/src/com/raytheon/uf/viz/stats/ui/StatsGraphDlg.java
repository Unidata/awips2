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
package com.raytheon.uf.viz.stats.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.stats.GraphDataRequest;
import com.raytheon.uf.common.stats.GraphDataResponse;
import com.raytheon.uf.common.stats.data.GraphData;
import com.raytheon.uf.common.stats.util.DataView;
import com.raytheon.uf.common.stats.util.UnitUtils;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.duallist.ButtonImages;
import com.raytheon.viz.ui.widgets.duallist.ButtonImages.ButtonImage;

/**
 * The Graph Data Structure.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2012     728    mpduff      Initial creation.
 * Jan 17, 2013    1357    mpduff      Add ability to change units.
 * Jan 18, 2013    1386    mpduff      Change menu text.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StatsGraphDlg extends CaveSWTDialog implements IStatsDisplay,
        IStatsGroup {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StatsGraphDlg.class);

    /** Data Request parameters */
    public enum REQUEST_PARAMETER {
        FullLeft, HalfLeft, HalfRight, FullRight
    }

    /** Callback */
    private final IStatsControl callback;

    /** The graph canvas */
    private StatsDisplayCanvas displayCanvas;

    /** The Graph data object */
    private GraphData graphData;

    /** The graph title */
    private String graphTitle;

    /** Menu bar */
    private Menu menuBar;

    /** Save Menu Item */
    private MenuItem saveMI;

    /** Exit Menu item */
    private MenuItem exitMI;

    /** Show control dialog menu item */
    private MenuItem controlDisplayMI;

    /** Show grid lines menu item */
    private MenuItem gridLinesMI;

    /** Draw grid lines flag */
    private boolean drawGridLines = true;

    /** Draw data lines flag */
    private boolean drawDataLines = true;

    /** Draw data items menu item */
    private MenuItem dataLinesMI;

    /** Grouping composite */
    private GroupingComp groupComp;

    /** Group Settings Map */
    private Map<String, RGB> groupSettingsMap;

    /** List of groups */
    private List<String> groupList;

    /** Category of stats data */
    private String category;

    /** Event type id of data */
    private String typeID;

    /** Data type (field) of the data */
    private String dataTypeID;

    /** Data view combo */
    private Combo viewCombo;

    /** Menu item map */
    private final Map<String, MenuItem> menuItemMap = new HashMap<String, MenuItem>();

    /** The currently displayed unit */
    private String displayUnit;

    private UnitUtils unitUtils;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell
     * @param callback
     *            Callback
     */
    public StatsGraphDlg(Shell parent, IStatsControl callback) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        this.callback = callback;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.verticalSpacing = 0;
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createMenus();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        GridLayout gl = new GridLayout(2, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, true);
        gl = new GridLayout(1, false);
        Composite leftComp = new Composite(mainComp, SWT.NONE);
        leftComp.setLayout(gl);
        leftComp.setLayoutData(gd);

        createCanvas(leftComp);

        gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        gl = new GridLayout(1, false);
        groupComp = new GroupingComp(mainComp, SWT.BORDER, graphData, this);
        groupComp.setLayout(gl);
        groupComp.setLayoutData(gd);

        displayCanvas.setCallback(groupComp);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, true);
        gl = new GridLayout(2, false);
        Composite ctrlComp = new Composite(leftComp, SWT.NONE);
        ctrlComp.setLayout(gl);
        ctrlComp.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, false, true);
        gl = new GridLayout(2, false);
        Composite dataComp = new Composite(ctrlComp, SWT.NONE);
        dataComp.setLayout(gl);
        dataComp.setLayoutData(gd);

        Label graphLabel = new Label(dataComp, SWT.NONE);
        graphLabel.setText("Graph: ");

        List<String> viewList = new ArrayList<String>();
        viewList.add(DataView.AVG.getView());
        viewList.add(DataView.MIN.getView());
        viewList.add(DataView.MAX.getView());
        viewList.add(DataView.SUM.getView());
        viewList.add(DataView.COUNT.getView());

        gd = new GridData(SWT.LEFT, SWT.CENTER, true, false);
        viewCombo = new Combo(dataComp, SWT.READ_ONLY);
        viewCombo.setLayoutData(gd);
        viewCombo.setItems(viewList.toArray(new String[viewList.size()]));
        viewCombo.select(0);
        viewCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDataViewChange();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        gl = new GridLayout(4, false);
        Composite btnComp = new Composite(ctrlComp, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        ButtonImages btnImg = new ButtonImages(leftComp);

        final int buttonWidth = 45;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button fullLeftBtn = new Button(btnComp, SWT.PUSH);
        fullLeftBtn.setImage(btnImg.getImage(ButtonImage.RemoveAll));
        fullLeftBtn.setLayoutData(btnData);
        fullLeftBtn
                .setToolTipText("Moves the display a full time period to the left");
        fullLeftBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                requestRedraw(REQUEST_PARAMETER.FullLeft);
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button halfLeftBtn = new Button(btnComp, SWT.PUSH);
        halfLeftBtn.setImage(btnImg.getImage(ButtonImage.Remove));
        halfLeftBtn.setLayoutData(btnData);
        halfLeftBtn
                .setToolTipText("Moves the display half a time period to the left");
        halfLeftBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                requestRedraw(REQUEST_PARAMETER.HalfLeft);
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button halfRightBtn = new Button(btnComp, SWT.PUSH);
        halfRightBtn.setImage(btnImg.getImage(ButtonImage.Add));
        halfRightBtn.setLayoutData(btnData);
        halfRightBtn
                .setToolTipText("Moves the display half a time period to the right");
        halfRightBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                requestRedraw(REQUEST_PARAMETER.HalfRight);
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button fullRightBtn = new Button(btnComp, SWT.PUSH);
        fullRightBtn.setImage(btnImg.getImage(ButtonImage.AddAll));
        fullRightBtn.setLayoutData(btnData);
        fullRightBtn
                .setToolTipText("Moves the display a full time period to the right");
        fullRightBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                requestRedraw(REQUEST_PARAMETER.FullRight);
            }
        });

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the menus
     */
    private void createMenus() {
        menuBar = new Menu(shell, SWT.BAR);

        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        saveMI = new MenuItem(fileMenu, SWT.NONE);
        saveMI.setText("&Save Graph Image\tCtrl+S");
        saveMI.setAccelerator(SWT.CTRL + 'S');
        saveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveGraph();
            }
        });

        exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("&Quit\tCtrl+Q");
        exitMI.setAccelerator(SWT.CTRL + 'Q');
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        MenuItem graphMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        graphMenuItem.setText("Graph");

        Menu graphMenu = new Menu(menuBar);
        graphMenuItem.setMenu(graphMenu);

        controlDisplayMI = new MenuItem(graphMenu, SWT.NONE);
        controlDisplayMI.setText("Show &Display Control\tCtrl+D");
        controlDisplayMI.setAccelerator(SWT.CTRL + 'D');
        controlDisplayMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callback.showControl();
            }
        });

        gridLinesMI = new MenuItem(graphMenu, SWT.CHECK);
        gridLinesMI.setText("Display Grid Lines");
        gridLinesMI.setSelection(true);
        gridLinesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                drawGridLines = gridLinesMI.getSelection();
                displayCanvas.redraw();
            }
        });

        dataLinesMI = new MenuItem(graphMenu, SWT.CHECK);
        dataLinesMI.setText("Display Data Lines");
        dataLinesMI.setSelection(true);
        dataLinesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                drawDataLines = dataLinesMI.getSelection();
                displayCanvas.redraw();
            }
        });

        // Set up the display units menu choices
        MenuItem unitsMI = new MenuItem(graphMenu, SWT.CASCADE);
        unitsMI.setText("Display Units");

        unitUtils = new UnitUtils(graphData.getEventType(),
                graphData.getDataType());
        String displayUnit = graphData.getDisplayUnit();
        unitUtils.setUnitType(displayUnit);
        unitUtils.setDisplayUnit(displayUnit);
        Set<String> units = unitUtils.getUnitOptions();

        Menu unitsMenu = new Menu(graphMenu);
        unitsMI.setMenu(unitsMenu);

        for (String unit : units) {
            MenuItem mi = new MenuItem(unitsMenu, SWT.CHECK);
            mi.setText(unit);
            if (unit.equals(graphData.getDisplayUnit())) {
                mi.setSelection(true);
            }
            mi.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    handleUnitsSelection(e);
                }
            });

            menuItemMap.put(unit, mi);
        }
    }

    /**
     * Handle the unit selection event.
     * 
     * @param e
     *            The selection event
     */
    private void handleUnitsSelection(SelectionEvent e) {
        MenuItem m = (MenuItem) e.getSource();
        for (Entry<String, MenuItem> es : menuItemMap.entrySet()) {
            MenuItem mi = es.getValue();
            if (es.getKey().equals(m.getText())) {
                m.setSelection(true);
                displayUnit = m.getText();
                unitUtils.setDisplayUnit(displayUnit);
                graphData.setDisplayUnit(displayUnit);
                displayCanvas.redraw();
            } else {
                mi.setSelection(false);
            }
        }
    }

    /**
     * Create the canvas.
     * 
     * @param comp
     *            Composite holding the canvas
     */
    private void createCanvas(Composite comp) {
        Composite canvasComp = new Composite(comp, SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 0;
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        canvasComp.setLayoutData(gd);
        canvasComp.setLayout(gl);

        displayCanvas = new StatsDisplayCanvas(canvasComp, this, graphTitle);
    }

    /**
     * Set the GraphData object.
     * 
     * @param graphData
     */
    public void setGraphData(GraphData graphData) {
        this.graphData = graphData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GraphData getGraphData() {
        return this.graphData;
    }

    /**
     * Set the title.
     * 
     * @param title
     */
    public void setTitle(String title) {
        setText(title);
    }

    /**
     * Set the graph title.
     * 
     * @param graphTitle
     */
    public void setGraphTitle(String graphTitle) {
        this.graphTitle = graphTitle;
    }

    /**
     * Open a file dialog for saving the canvas.
     */
    private void saveGraph() {
        FileDialog dialog = new FileDialog(shell, SWT.SAVE);
        String filename = dialog.open();
        if (filename == null) {
            return;
        }
        saveCanvas(filename);
    }

    /**
     * Captures the canvas and saves the result into a file in a format
     * determined by the filename extension .
     * 
     * @param control
     *            The control to save
     * @param fileName
     *            The name of the image to be saved
     */
    public void saveCanvas(String filename) {
        StringBuilder sb = new StringBuilder();
        Display display = displayCanvas.getDisplay();
        Image image = new Image(display, displayCanvas.getBounds().width,
                displayCanvas.getBounds().height);
        GC gc = new GC(image);

        displayCanvas.drawCanvas(gc);

        /* Default to PNG */
        int style = SWT.IMAGE_PNG;

        if ((filename.endsWith(".jpg") == true) || filename.endsWith("jpeg")) {
            style = SWT.IMAGE_JPEG;
        } else if (filename.endsWith(".bmp") == true) {
            style = SWT.IMAGE_BMP;
        } else {
            filename += ".png";
        }

        ImageLoader loader = new ImageLoader();
        loader.data = new ImageData[] { image.getImageData() };
        loader.save(filename, style);

        sb.setLength(0);
        image.dispose();
        gc.dispose();
    }

    /**
     * Request the graph be redrawn with a new time range.
     * 
     * @param parameter
     *            The amount of time to move
     */
    private void requestRedraw(REQUEST_PARAMETER parameter) {
        TimeRange tr = this.graphData.getTimeRange();
        long dur = tr.getDuration();
        long newStart;
        long newEnd;
        long start;
        long end;

        switch (parameter) {
        case FullLeft:
            start = tr.getStart().getTime();
            newStart = start - dur;
            newEnd = newStart + dur;
            break;
        case HalfLeft:
            start = tr.getStart().getTime();
            newStart = start - dur / 2;
            newEnd = newStart + dur;
            break;
        case HalfRight:
            start = tr.getStart().getTime();
            newStart = start + dur / 2;
            newEnd = newStart + dur;
            break;
        case FullRight:
            end = tr.getEnd().getTime();
            newStart = end;
            newEnd = newStart + dur;
            break;
        default:
            return;
        }
        GraphDataRequest request = new GraphDataRequest();
        request.setGrouping(this.groupList);
        request.setCategory(this.category);
        request.setEventType(typeID);
        request.setDataType(dataTypeID);
        request.setField(dataTypeID);
        TimeRange newTimeRange = new TimeRange(newStart, newEnd);
        request.setTimeRange(newTimeRange);
        request.setTimeStep((int) graphData.getTimeStep());
        try {
            GraphDataResponse response = (GraphDataResponse) ThriftClient
                    .sendRequest(request);
            GraphData localGraphData = response.getGraphData();
            if (localGraphData == null
                    || localGraphData.getStatsDataMap() == null
                    || localGraphData.getStatsDataMap().size() == 0) {
                MessageBox messageDialog = new MessageBox(getShell(),
                        SWT.ICON_INFORMATION);
                messageDialog.setText("No Data Available");
                messageDialog
                        .setMessage("No data available for the time range selected.");
                messageDialog.open();

                return;
            }

            this.graphData = localGraphData;
            this.displayCanvas.redraw();
        } catch (VizException e) {
            this.statusHandler.handle(Priority.ERROR, "Error Requesting Data",
                    e);
        }
    }

    /**
     * Handler for data view combo box change.
     */
    private void handleDataViewChange() {
        String viewStr = viewCombo.getText();
        DataView view = DataView.fromString(viewStr);
        this.displayCanvas.setView(view);
        this.displayCanvas.redraw();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean drawGridLines() {
        return this.drawGridLines;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean drawDataLines() {
        return this.drawDataLines;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setGroupData(Map<String, RGB> groupSettingsMap) {
        this.groupSettingsMap = groupSettingsMap;
        displayCanvas.redraw();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, RGB> getGroupSettings() {
        return this.groupSettingsMap;
    }

    /**
     * Set the groups.
     * 
     * @param groupList
     *            List of groups
     */
    public void setGrouping(List<String> groupList) {
        this.groupList = groupList;
    }

    /**
     * Set the category.
     * 
     * @param category
     */
    public void setCategory(String category) {
        this.category = category;
    }

    /**
     * Set the event type.
     * 
     * @param typeID
     */
    public void setEventType(String typeID) {
        this.typeID = typeID;
    }

    /**
     * Set the data type id.
     * 
     * @param dataTypeID
     */
    public void setDataType(String dataTypeID) {
        this.dataTypeID = dataTypeID;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public UnitUtils getUnitUtils() {
        return this.unitUtils;
    }
}
