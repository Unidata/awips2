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

package com.raytheon.viz.texteditor.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseWheelListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.AfosBrowserModel;
import com.raytheon.viz.texteditor.TextDBQuery;
import com.raytheon.viz.texteditor.TextDisplayModel;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.msgs.IAfosBrowserCallback;
import com.raytheon.viz.texteditor.msgs.ITextWorkstationCallback;
import com.raytheon.viz.texteditor.util.AFOS_CLASS;
import com.raytheon.viz.texteditor.util.AFOS_ORIGIN;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The AFOS browser dialog allows the user to look through and choose text
 * products by AFOS ID to be displayed in the text window.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/13/07      368         lvenable    Initial creation.
 * 10/11/2007   482         grichard    Reformatted file.
 * 11/8/2007    520         grichard    Implemented build 11 features.
 * 12/7/2007    582         grichard    Implemented build 12 features.
 * 12/17/2007   639         grichard    Added &quot;fxa&quot; parm to scripts.
 * 07/23/2009   2191        rjpeter     Finished implementation.
 * 06/01/2010   2187        cjeanbap    Added StdTextProductFactory 
 *                                       functionality.
 * 06/28/2010   3283        cjeanbap    Implement window resize.
 * 25Sep2012    1196        lvenable    Dialog refactor to prevent blocking.
 * 29Jan2013    1496        rferrel     Changes to designator hours query
 *                                       off the UI thread.
 *                                      Changes to have multiple query jobs.
 * 15Apr2014    #3031       lvenable    Added dispose check in the runAsync calls.
 * 19May2014    2536        bclement    removed TimeTools usage
 * 09Sep2014    3580        mapeters    Removed IQueryTransport usage 
 *                                      (no longer exists).
 * 16Feb2106    5391        randerso    Fixed button layouts so text is not cut off with larger fonts/higher DPI
 * 12Feb2016    4716        rferrel     Change to indicate AFOS browser instead of AWIPS browser.
 * </pre>
 * 
 * @author lvenable
 */
public class AfosBrowserDlg extends CaveSWTDialog implements
        ITextWorkstationCallback {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AfosBrowserDlg.class);

    private static final String TIME_FORMAT = "HH:mm MMM dd";

    private final AfosBrowserModel browserData = AfosBrowserModel.getInstance();

    private enum ListType {
        NODE, CATEGORY, DESIGNATOR, DESIGNATOR_TIME, DISPLAY
    };

    private final int NUM_ITEMS = 10;

    /**
     * Designator time display error for getting data for designator.
     */
    private final String DATA_ERROR = "ERROR";

    /**
     * Designator time display when no data retrieved for designator.
     */
    private final String DATA_NONE = "NO DATA";

    /**
     * Designator time display while data retrieval is pending.
     */
    private final String DATA_LOADING = "??:?? ??? ??";

    /**
     * Flag to indicate loading of designator times is active.
     */
    private boolean loadingProduct = false;

    /**
     * Flag to indicate loading of product(s) is active in the parent's dialog.
     */
    private boolean loadingTimes = false;

    /**
     * Newest time used to populate "000" designator's time.
     */
    private long newestTime = -1L;

    /**
     * custom mouse handling so we can do the required vertical bar
     * non-scrolling.
     */
    private boolean leftMouse = false;

    private boolean rightMouse = false;

    /**
     * Origin combo box.
     */
    private Combo originCombo;

    /**
     * Class combo box.
     */
    private Combo categoryClassCombo;

    /**
     * List control displaying nodes.
     */
    private List nodeList;

    /**
     * List control displaying categories.
     */
    private List categoryList;

    /**
     * List control displaying designator (site or area for which the product
     * applies).
     */
    private List designatorList;

    /**
     * List control displaying designator times.
     */
    private List designatorTimeList;

    /**
     * List control showing the prefixes used to select special groupings of
     * text products.
     */
    private List displayList;

    /**
     * The node that is selected.
     */
    private String selectedNode;

    /**
     * The category that is selected.
     */
    private String selectedCategory;

    /**
     * The designator that is selected.
     */
    private String selectedDesignator;

    /**
     * Load the text product and keep the browser dialog open.
     */
    private Button loadContinueBtn;

    /**
     * Load the text product and close browser dialog.
     */
    private Button loadCloseBtn;

    /**
     * Close the dialog.
     */
    private Button closeBtn;

    /**
     * Interface variable for Afos Browser callback
     */
    private IAfosBrowserCallback callbackClient = null;

    /**
     * Flag that indicates whether the Browser is active or not
     */
    private boolean isActive = true;

    private String localSite = LocalizationManager.getInstance()
            .getCurrentSite();

    private String currentAfosCommand = null;

    private MenuManager menuMgr = null;

    private QueryRequests queryRequests = new QueryRequests();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param browserHdr
     *            Browser name.
     * @param cbClient
     *            Interface to callback for Afos Browser.
     */
    public AfosBrowserDlg(Shell parent, String browserHdr,
            IAfosBrowserCallback cbClient, String token) {
        super(parent, SWT.DIALOG_TRIM | SWT.MODELESS | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);

        setText(browserHdr + " AFOS Browser");

        callbackClient = cbClient;

        TextDisplayModel.getInstance().setITextWorkstationCallback(token, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        initializeComponents();

        // will load all subsequent lists if data is selected
        loadNodeList();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                // Block the disposal of this dialog.
                AfosBrowserDlg.this.hide();
                isActive = false;
                event.doit = false;
            }
        });
    }

    /**
     * Initialize the components of the dialog.
     */
    private void initializeComponents() {
        createOriginClassControls();
        createListControls();
        createBottomButtons();
    }

    /**
     * Create the Origin and Class controls.
     */
    private void createOriginClassControls() {
        final AfosBrowserModel browserData = AfosBrowserModel.getInstance();

        // Create the composite that will hold the controls.
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(5, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        controlComp.setLayout(gridLayout);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        // Add the origin label.
        Label originLbl = new Label(controlComp, SWT.NONE);
        originLbl.setText("Origin: ");
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        originLbl.setLayoutData(gd);

        // Add the origin combo box.
        originCombo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        originCombo.setLayoutData(gd);

        for (AFOS_ORIGIN origin : AFOS_ORIGIN.values()) {
            originCombo.add(origin.name());
        }
        AFOS_ORIGIN origin = browserData.getDefaultOrigin(localSite);
        originCombo.select(origin.ordinal());
        selectedNode = localSite;

        // Use this listener to update the node list based on the origin.
        originCombo.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent event) {
                loadNodeList();
            }
        });

        Label fill = new Label(controlComp, SWT.NONE);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        fill.setLayoutData(gd);

        // Add the class label.
        Label classLbl = new Label(controlComp, SWT.NONE);
        classLbl.setText("Class: ");
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        classLbl.setLayoutData(gd);

        // Populate the category class dialog
        // Add the class combo box.
        categoryClassCombo = new Combo(controlComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        categoryClassCombo.setLayoutData(gd);

        for (AFOS_CLASS c : AFOS_CLASS.values()) {
            categoryClassCombo.add(c.value());
        }
        categoryClassCombo.select(0);

        // Use this listener to update the category list based on the origin
        // plus the class.
        categoryClassCombo.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent event) {
                if (nodeList.getSelectionCount() > 0) {
                    loadCategoryList();
                }
            }
        });
    }

    /**
     * Create the list help buttons and list box controls.
     * 
     * NOTE: The help buttons may change. The original dialog has embedded menus
     * that SWT does not support. We may go with a button/pop-up menu
     * combination or a coolbar with a drop-down. This will be decided later.
     */
    private void createListControls() {
        // Create the composite that will hold the controls.
        Composite listComp = new Composite(shell, SWT.NONE);

        GridLayout gridLayout = new GridLayout(4, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        listComp.setLayout(gridLayout);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        listComp.setLayoutData(gd);

        GC gc = new GC(listComp);
        FontMetrics fm = gc.getFontMetrics();
        int charWidth = fm.getAverageCharWidth();
        gc.dispose();

        // ------------------------------------
        // Create the Node component
        // ------------------------------------
        Group nodeGroup = new Group(listComp, SWT.NONE);
        gridLayout = new GridLayout(1, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        gridLayout.verticalSpacing = 0;
        nodeGroup.setLayout(gridLayout);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        nodeGroup.setLayoutData(gd);
        nodeGroup.setText("Node");

        ToolBar toolBar = new ToolBar(nodeGroup, SWT.FLAT | SWT.RIGHT);
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        toolBar.setLayoutData(gd);

        ToolItem toolItem = new ToolItem(toolBar, SWT.DROP_DOWN);
        toolItem.setText("Help");
        toolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.NODE, nodeList);
            }
        });

        nodeList = new List(nodeGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Rectangle trim = nodeList.computeTrim(0, 0, charWidth * 3,
                nodeList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        nodeList.setLayoutData(gd);

        nodeList.addMouseListener(new ListMouseHandler(ListType.NODE));
        nodeList.addSelectionListener(new ListSelectionHandler(ListType.NODE));
        nodeList.setData(new boolean[0]);

        // ------------------------------------
        // Create the Category component
        // ------------------------------------
        Group categoryGroup = new Group(listComp, SWT.NONE);
        gridLayout = new GridLayout(1, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        gridLayout.verticalSpacing = 0;
        categoryGroup.setLayout(gridLayout);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        categoryGroup.setLayoutData(gd);
        categoryGroup.setText("Category");

        toolBar = new ToolBar(categoryGroup, SWT.FLAT | SWT.RIGHT);
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        toolBar.setLayoutData(gd);

        toolItem = new ToolItem(toolBar, SWT.DROP_DOWN);
        toolItem.setText("Help");
        toolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.CATEGORY, categoryList);
            }
        });

        categoryList = new List(categoryGroup, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        trim = categoryList.computeTrim(0, 0, charWidth * 3,
                categoryList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        categoryList.setLayoutData(gd);

        categoryList.addMouseListener(new ListMouseHandler(ListType.CATEGORY));
        categoryList.addSelectionListener(new ListSelectionHandler(
                ListType.CATEGORY));
        categoryList.setData(new boolean[0]);

        // ------------------------------------
        // Create the Designator component
        // ------------------------------------
        Group designatorGroup = new Group(listComp, SWT.NONE);
        gridLayout = new GridLayout(2, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        gridLayout.verticalSpacing = 0;
        designatorGroup.setLayout(gridLayout);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        designatorGroup.setLayoutData(gd);
        designatorGroup.setText("Designator");

        toolBar = new ToolBar(designatorGroup, SWT.FLAT | SWT.RIGHT);
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        toolBar.setLayoutData(gd);

        toolItem = new ToolItem(toolBar, SWT.DROP_DOWN);
        toolItem.setText("Help");
        toolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.DESIGNATOR, designatorList);
            }
        });

        // dummy label to fill the spot in the grid
        new Label(designatorGroup, SWT.NONE);

        designatorList = new List(designatorGroup, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        trim = designatorList.computeTrim(0, 0, charWidth * 3,
                designatorList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        designatorList.setLayoutData(gd);

        designatorList.addMouseListener(new ListMouseHandler(
                ListType.DESIGNATOR));
        designatorList.addSelectionListener(new ListSelectionHandler(
                ListType.DESIGNATOR));
        designatorList.addMouseWheelListener(new DesignatorMouseWheelHandler());
        designatorList.setData(new boolean[0]);

        designatorTimeList = new List(designatorGroup, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        trim = designatorTimeList.computeTrim(0, 0, charWidth * 12,
                designatorTimeList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        designatorTimeList.setLayoutData(gd);

        designatorTimeList.addMouseListener(new ListMouseHandler(
                ListType.DESIGNATOR_TIME));
        designatorTimeList.addSelectionListener(new ListSelectionHandler(
                ListType.DESIGNATOR_TIME));
        designatorTimeList.setData(new boolean[0]);

        designatorList.getVerticalBar().setVisible(false);
        scrollListsTogether(designatorList, designatorTimeList);

        // ------------------------------------
        // Create the Display component
        // ------------------------------------
        Group displayGroup = new Group(listComp, SWT.NONE);
        gridLayout = new GridLayout(1, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        gridLayout.verticalSpacing = 0;
        displayGroup.setLayout(gridLayout);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        displayGroup.setLayoutData(gd);
        displayGroup.setText("Display");

        toolBar = new ToolBar(displayGroup, SWT.FLAT | SWT.RIGHT);
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        toolBar.setLayoutData(gd);

        toolItem = new ToolItem(toolBar, SWT.DROP_DOWN);
        toolItem.setText("Help");
        toolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.DISPLAY, displayList);
            }
        });

        displayList = new List(displayGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        trim = displayList.computeTrim(0, 0, charWidth * 4,
                displayList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        displayList.setLayoutData(gd);

        displayList.addMouseListener(new ListMouseHandler(ListType.DISPLAY));
        displayList.addSelectionListener(new ListSelectionHandler(
                ListType.DISPLAY));
        displayList.setData(new boolean[0]);
    }

    /**
     * Create the bottom control buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(3, true);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        buttonComp.setLayout(gridLayout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        loadContinueBtn = new Button(buttonComp, SWT.PUSH);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        loadContinueBtn.setLayoutData(gd);
        loadContinueBtn.setText("Load and Continue");
        loadContinueBtn.setEnabled(false);
        loadContinueBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callbackClient.executeCommand(CommandFactory
                        .getAfosCommand(currentAfosCommand));
                loadContinueBtn.setFocus();
            }
        });

        loadCloseBtn = new Button(buttonComp, SWT.PUSH);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        loadCloseBtn.setLayoutData(gd);
        loadCloseBtn.setText("Load and Close");
        loadCloseBtn.setEnabled(false);
        loadCloseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callbackClient.executeCommand(CommandFactory
                        .getAfosCommand(currentAfosCommand));

                setReturnValue(false);
                hide();
                isActive = false;
            }
        });

        closeBtn = new Button(buttonComp, SWT.PUSH);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        closeBtn.setLayoutData(gd);
        closeBtn.setText("Close");
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                hide();
                isActive = false;
            }
        });
    }

    /**
     * Allow parent dialog to display the hidden dialog or just force it to the
     * top of the display.
     */
    public void showDialog() {
        bringToTop();
        isActive = true;
    }

    /**
     * Scroll two lists at the same time using the vertical scroll bar from each
     * list.
     * 
     * @param list1
     *            List control 1.
     * @param list2
     *            List control 2.
     */
    private void scrollListsTogether(final List list1, final List list2) {
        final ScrollBar vBar1 = list1.getVerticalBar();
        final ScrollBar vBar2 = list2.getVerticalBar();
        SelectionListener listener1 = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int y = vBar1.getSelection()
                        * (vBar2.getMaximum() - vBar2.getThumb())
                        / Math.max(1, vBar1.getMaximum() - vBar1.getThumb());
                vBar2.setSelection(y);
            }
        };
        SelectionListener listener2 = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int y = vBar2.getSelection()
                        * (vBar1.getMaximum() - vBar1.getThumb())
                        / Math.max(1, vBar2.getMaximum() - vBar2.getThumb());
                vBar1.setSelection(y);
            }
        };
        vBar1.addSelectionListener(listener1);
        vBar2.addSelectionListener(listener2);
    }

    /**
     * Create a query command and a list of product ids to obtain the hours for
     * and pass the request off to the query request job.
     */
    private void queryTableUsingNodeAndCategory() {
        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        TextDBQuery dbQuery = new TextDBQuery();
        dbQuery.setQueryViewName("text");
        dbQuery.setQueryOpName("GET");
        dbQuery.setQuerySubObName("LATEST");
        dbQuery.setQueryTimeFormatName("RAW");
        Boolean result = new Boolean(
                CAVEMode.OPERATIONAL.equals(CAVEMode.getMode())
                        || CAVEMode.OPERATIONAL.equals(CAVEMode.getMode()) ? true
                        : false);
        dbQuery.setQueryOperationalMode(result.toString().toUpperCase());

        // Build up the query list.
        java.util.List<String> productIds = new ArrayList<String>();
        for (int i = 1; i < designatorList.getItemCount(); i++) {
            String prodId = selectedNode + selectedCategory
                    + designatorList.getItem(i);
            productIds.add(prodId);
        }

        queryRequests.addRequest(dbQuery, productIds);
    }

    /**
     * Set up designator Time list to be the same size as designator list
     * populated with loading template; and set up variables needed for queries.
     */
    private void startDesignatorTimeList() {
        loadingTimes = true;
        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        newestTime = -1L;
        designatorTimeList.removeAll();
        int cnt = designatorList.getItemCount();
        for (int i = 1; i < cnt; ++i) {
            designatorTimeList.add(DATA_LOADING);
        }
        // Keep this list the same size as disignatorList.
        designatorTimeList.add("");
        updateSelectionList(designatorTimeList);
    }

    /**
     * Finish setup of designator time list after all times have been retrieved.
     */
    private void finishDesignatorTimeList() {
        SimpleDateFormat sdf = new SimpleDateFormat(TIME_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        String value = null;
        if (newestTime > 0L) {
            value = sdf.format(new Date(newestTime));
        } else if (newestTime == -1) {
            value = DATA_ERROR;
        } else {
            value = DATA_NONE;
        }
        designatorTimeList.setItem(0, value);

        checkLoadBtn();

        loadingTimes = false;
        if (!loadingProduct) {
            getShell().setCursor(null);
        }
    }

    /**
     * Update the the chuck of times retrieved. The values should be inserted
     * starting at startIndex.
     * 
     * @param queryResponse
     * @param startIndex
     */
    private void updateAfterQuery(Message queryResponse, int startIndex) {

        Property[] properties = queryResponse.getHeader().getProperties();
        ArrayList<Long> times = new ArrayList<Long>();

        // We use this time to populate the "000" designator entry.
        if (properties != null) {
            for (Property p : properties) {
                if ("STDOUT".equals(p.getName())) {
                    Long currTime = new Long(p.getValue());
                    newestTime = Math.max(currTime, newestTime);
                    times.add(currTime);
                } else {
                    times.add(-1L); // indicate an error for this location
                }
            }
        }

        SimpleDateFormat sdf = new SimpleDateFormat(TIME_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String value = null;
        try {
            int index = startIndex;
            int selectIndex = designatorList.getSelectionIndex();
            for (Long time : times) {
                if (time > 0) {
                    value = sdf.format(new Date(time));
                } else if (time == -1) {
                    value = DATA_ERROR;
                } else {
                    value = DATA_NONE;
                }
                designatorTimeList.setItem(index, value);
                ++index;
            }

            // Selected designator time updated check status of load buttons.
            if (selectIndex > startIndex && selectIndex <= index) {
                checkLoadBtn();
            }
        } catch (IllegalArgumentException ex) {
            // ignore caused by cancel being performed.
        }
    }

    @Override
    public boolean isBrowserActive() {
        return isActive;
    }

    /**
     * Handles setting up status of this dialog when parent is loading
     * product(s).
     * 
     * @param status
     *            - true when active loading product(s) otherwise false.
     */
    public void setLoading(boolean status) {
        if (loadingProduct != status) {
            loadingProduct = status;
            if (!loadingTimes) {
                if (loadingProduct) {
                    setLoadBtnEnabled(false);
                    shell.setCursor(getDisplay().getSystemCursor(
                            SWT.CURSOR_WAIT));
                } else {
                    setLoadBtnEnabled(designatorTimeList.getSelectionIndex() != -1);
                    shell.setCursor(null);
                }
            }
        }
    }

    /**
     * Convince method for keeping the handling of load buttons the same.
     * 
     * @param flag
     */
    private void setLoadBtnEnabled(boolean flag) {
        loadContinueBtn.setEnabled(flag);
        loadCloseBtn.setEnabled(flag);
    }

    /**
     * Loads the node list. Assumes the query for the list will not hang up the
     * UI thread.
     */
    private void loadNodeList() {
        setLoadBtnEnabled(false);
        nodeList.removeAll();
        categoryList.removeAll();
        designatorList.removeAll();
        designatorTimeList.removeAll();
        displayList.removeAll();

        // Add the list of nodes for the selected origin
        for (String node : browserData.getOriginNodeList(originCombo
                .getItem(originCombo.getSelectionIndex()))) {
            nodeList.add(node);

            if (node.equals(selectedNode)) {
                selectNode(nodeList.getItemCount() - 1);
            }
        }
        updateSelectionList(nodeList);
    }

    /**
     * Populate the category list. Assumes query for the list will not hang up
     * the UI thread.
     */
    private void loadCategoryList() {
        setLoadBtnEnabled(false);
        categoryList.removeAll();
        designatorList.removeAll();
        designatorTimeList.removeAll();
        displayList.removeAll();

        String categoryClass = categoryClassCombo.getItem(categoryClassCombo
                .getSelectionIndex());

        if ("WSR".equals(selectedNode)) {
            for (String category : browserData
                    .getUnfilteredCategoryList(selectedNode)) {
                categoryList.add(category);

                if (category.equals(selectedCategory)) {
                    selectCategory(categoryList.getItemCount() - 1);
                }
            }
        } else {
            for (String category : browserData.getFilteredCategoryList(
                    selectedNode, categoryClass)) {
                categoryList.add(category);

                if (category.equals(selectedCategory)) {
                    selectCategory(categoryList.getItemCount() - 1);
                }
            }
        }
        updateSelectionList(categoryList);
    }

    /**
     * Load the designator list Assumes query for the list will not hang up the
     * UI thread.
     */
    private void loadDesignatorList() {
        setLoadBtnEnabled(false);
        designatorList.removeAll();
        designatorTimeList.removeAll();
        displayList.removeAll();

        SortedSet<String> xxx = browserData.getDesignatorList(selectedNode,
                selectedCategory);

        if (xxx != null) {
            designatorList.add("000");

            // Get the designator list - ensure that the entries are three
            // characters in length.
            for (String s : xxx) {
                if (s.length() == 1) {
                    s = s + "  ";
                } else if (s.length() == 2) {
                    s = s + " ";
                }
                designatorList.add(s);
                // can't use same method as other lists to select as the times
                // list needs to be loaded before selecting a designator
            }

            queryTableUsingNodeAndCategory();

            // times loaded, now select a designator if applicable
            String items[] = designatorList.getItems();
            for (int i = 0; i < items.length; i++) {
                if (items[i].equals(selectedDesignator)) {
                    selectDesignator(i);
                }
            }

            // add a blank entry to allow deselection
            designatorList.add("");
            designatorTimeList.add("");
        }
        updateSelectionList(designatorList);

        loadDisplayList();
    }

    /**
     * Create a boolean array initialized to false that is the same size as the
     * list and place it in the list's data object.
     * 
     * @param list
     */
    private void updateSelectionList(List list) {
        boolean[] selectedIndexes = new boolean[list.getItemCount()];
        Arrays.fill(selectedIndexes, false);
        list.setData(selectedIndexes);
    }

    /**
     * Update the Display list based on the selected category and designator.
     */
    private void loadDisplayList() {
        displayList.removeAll();

        if (categoryList.getSelectionIndex() >= 0) {
            int designatorIndex = designatorList.getSelectionIndex();

            if (designatorIndex >= 0) {
                String xxx = designatorList.getItem(designatorList
                        .getSelectionIndex());

                if (xxx.length() == 0) {
                    displayList.add("A:");
                } else if (!xxx.equals("000")) {
                    displayList.add("ALL:");
                }
            } else {
                displayList.add("A:");
            }
        }

        if (displayList.getItemCount() > 0) {
            displayList.add("");
        }
        updateSelectionList(displayList);
    }

    /**
     * Set selected node and update AFOS command and the category list.
     * 
     * @param index
     */
    private void selectNode(int index) {
        selectedNode = nodeList.getItem(index);
        nodeList.setSelection(index);
        nodeList.showSelection();
        setAFOSCommand(selectedNode);
        loadCategoryList();
    }

    /**
     * Update the category list selection, the AFOS command and start load the
     * designator lists.
     * 
     * @param index
     */
    private void selectCategory(int index) {
        selectedCategory = categoryList.getItem(index);
        categoryList.setSelection(index);
        categoryList.showSelection();
        setAFOSCommand(selectedNode + selectedCategory);
        loadDesignatorList();
    }

    /**
     * Update the selected designator, the AFOS command and it the designator's
     * hour is valid enable the load buttons.
     * 
     * @param index
     */
    private void selectDesignator(int index) {
        String tmp = designatorList.getItem(index);
        if (tmp.length() > 0) {
            try {
                selectedDesignator = designatorList.getItem(index);
                designatorList.setSelection(index);
                designatorTimeList.setSelection(index);
                designatorList.showSelection();
                designatorTimeList.showSelection();
                setAFOSCommand(selectedNode + selectedCategory
                        + selectedDesignator);
            } catch (IllegalArgumentException ex) {
                designatorList.deselectAll();
                designatorTimeList.deselectAll();
                setAFOSCommand(selectedNode + selectedCategory);
            }
        } else {
            designatorList.deselectAll();
            designatorTimeList.deselectAll();
            setAFOSCommand(selectedNode + selectedCategory);
        }

        loadDisplayList();
        checkLoadBtn();
    }

    /**
     * Change the designators selected entry and perform updates to the GUI.
     * 
     * @param index
     */
    private void selectDisplay(int index) {
        String designator = null;
        String display = null;

        if (designatorList.getSelectionIndex() >= 0) {
            designator = selectedDesignator;
        } else {
            designator = "";
        }

        if (index < displayList.getItemCount()) {
            displayList.select(index);
            display = displayList.getItem(index);
            if (display.length() == 0) {
                displayList.deselectAll();
            }
        } else {
            display = "";
        }

        setAFOSCommand(display + selectedNode + selectedCategory + designator);
        checkLoadBtn();
    }

    /**
     * Check the selected designator and determine if the load buttons should be
     * enabled.
     */
    private void checkLoadBtn() {
        boolean enabled = false;
        int designatorIndex = designatorTimeList.getSelectionIndex();
        int displayIndex = displayList.getSelectionIndex();
        if (designatorIndex >= 0) {
            String time = designatorTimeList.getItem(designatorIndex);
            if (time.length() > 0) {
                enabled = !(time.equals(DATA_ERROR) || time.equals(DATA_NONE) || time
                        .equals(DATA_LOADING));
            } else if (displayIndex >= 0) {
                enabled = displayList.getItem(displayIndex).length() > 0;
            }
        } else if (displayIndex >= 0) {
            enabled = displayList.getItem(displayIndex).length() > 0;
        }

        setLoadBtnEnabled(enabled);
    }

    /**
     * Reset the a list's selected entries based on its data boolean array.
     * 
     * @param type
     */
    private void resetSelection(ListType type) {
        List list = null;

        switch (type) {
        case NODE: {
            list = nodeList;
            break;
        }
        case CATEGORY: {
            list = categoryList;
            break;
        }
        case DESIGNATOR: {
            list = designatorList;
            break;
        }
        case DESIGNATOR_TIME: {
            list = designatorTimeList;
            break;
        }

        case DISPLAY: {
            list = displayList;
            break;
        }
        }

        if (list != null) {
            boolean[] indexArray = (boolean[]) list.getData();
            list.deselectAll();
            int indexes[] = new int[list.getItemCount()];
            Arrays.fill(indexes, -99);
            for (int i = 0; i < indexArray.length; ++i) {
                if (indexArray[i] == true) {
                    indexes[i] = i;
                }
            }
            list.select(indexes);
        }
    }

    /**
     * Display a help dialog for the visible entries of the desired list.
     * 
     * @param type
     * @param list
     */
    private void displayHelpList(ListType type, List list) {
        int height = list.getClientArea().height;
        String[] items = list.getItems();
        ArrayList<IAction> actions = new ArrayList<IAction>();
        int startIndex = list.getTopIndex();

        // list.getItemHeight() can be off by a few pixels depending on
        // height/style, in terms of the few list entries the difference should
        // be miniscule
        int endIndex = startIndex + height / list.getItemHeight();

        // sanity check
        if (endIndex >= items.length) {
            endIndex = items.length - 1;
        }

        // dispose of current menu
        if (menuMgr != null) {
            menuMgr.dispose();
            menuMgr = null;
        }

        switch (type) {
        case NODE: {
            for (int i = startIndex; i <= endIndex; i++) {
                final int index = i;
                String helpText = browserData.getNodeHelp(items[index]);
                actions.add(new Action(items[index] + ": " + helpText) {
                    @Override
                    public void run() {
                        selectNode(index);
                    }
                });
            }
            break;
        }
        case CATEGORY: {
            for (int i = startIndex; i <= endIndex; i++) {
                final int index = i;
                String helpText = browserData.getCategoryHelp(items[index]);
                actions.add(new Action(items[index] + ": " + helpText) {
                    @Override
                    public void run() {
                        selectCategory(index);
                    }
                });
            }
            break;
        }
        case DESIGNATOR: {
            // Don't display help for empty item at end of list
            for (int i = startIndex; i < endIndex; i++) {
                final int index = i;
                String helpText = browserData.getDesignatorHelp(items[index]);
                actions.add(new Action(items[index] + ": " + helpText) {
                    @Override
                    public void run() {
                        selectDesignator(index);
                    }
                });
            }
            break;
        }
        case DISPLAY: {
            for (int i = startIndex; i < endIndex; i++) {
                final int index = i;
                String helpText = browserData.getDisplayHelp(items[index]);
                actions.add(new Action(items[index] + ": " + helpText) {
                    @Override
                    public void run() {
                        selectDisplay(index);
                    }
                });
            }
            break;
        }
        }

        // if there are actions, display pop up menu
        if (actions.size() > 0) {
            menuMgr = new MenuManager("#PopupMenu");

            for (IAction action : actions) {
                menuMgr.add(action);
            }

            Menu menu = menuMgr.createContextMenu(list);
            menu.setLocation(list.toDisplay(0, 0));
            menu.setVisible(true);
            list.setMenu(menu);
        }
    }

    /**
     * Popup a help menu for the desire item in the list based on the type of
     * the list.
     * 
     * @param type
     * @param list
     * @param index
     */
    private void displayHelpText(ListType type, List list, final int index) {
        IAction action = null;

        if (index < list.getItemCount()) {
            String field = list.getItem(index);
            String helpText = null;
            if (field != null) {
                switch (type) {
                case NODE: {
                    helpText = browserData.getNodeHelp(field);
                    if (helpText != null && helpText.length() > 0) {
                        action = new Action(helpText) {
                            @Override
                            public void run() {
                                selectListItem(AfosBrowserDlg.ListType.NODE,
                                        index);
                                selectNode(index);
                            }
                        };
                    }
                    break;
                }
                case CATEGORY: {
                    helpText = browserData.getCategoryHelp(field);
                    if (helpText != null && helpText.length() > 0) {
                        action = new Action(helpText) {
                            @Override
                            public void run() {
                                selectListItem(
                                        AfosBrowserDlg.ListType.CATEGORY, index);
                                selectCategory(index);
                            }
                        };
                    }
                    break;
                }
                case DESIGNATOR: {
                    helpText = browserData.getDesignatorHelp(field);
                    if (helpText != null && helpText.length() > 0) {
                        action = new Action(helpText) {
                            @Override
                            public void run() {
                                selectListItem(
                                        AfosBrowserDlg.ListType.DESIGNATOR,
                                        index);
                                selectDesignator(index);
                            }
                        };
                    }
                    break;
                }
                case DISPLAY: {
                    helpText = browserData.getDisplayHelp(field);
                    if (helpText != null && helpText.length() > 0) {
                        action = new Action(helpText) {
                            @Override
                            public void run() {
                                selectListItem(AfosBrowserDlg.ListType.DISPLAY,
                                        index);
                                selectDisplay(index);
                            }
                        };
                    }
                    break;
                }
                default:
                }

                if (action != null) {
                    menuMgr = new MenuManager("#PopupMenu");
                    menuMgr.add(action);
                    Menu menu = menuMgr.createContextMenu(list);
                    menu.setVisible(true);
                    list.setMenu(menu);
                }
            }
        }
    }

    /**
     * Based on the moust click either select the desired item in the list
     * populating lists impacted by the selection or display help for the item.
     * 
     * @param type
     * @param index
     */
    private void selectListItem(ListType type, int index) {
        List list = null;
        switch (type) {
        case NODE: {
            list = nodeList;
            break;
        }
        case CATEGORY: {
            list = categoryList;
            break;
        }
        case DESIGNATOR: {
            list = designatorList;
            break;
        }
        case DESIGNATOR_TIME: {
            list = designatorTimeList;
            break;
        }
        case DISPLAY: {
            list = displayList;
            break;
        }
        }
        if (leftMouse) {
            if (list != null) {
                boolean[] indexArray = (boolean[]) list.getData();
                Arrays.fill(indexArray, false);
                indexArray[list.getSelectionIndex()] = true;
                int indexes[] = new int[list.getItemCount()];
                Arrays.fill(indexes, -99);
                for (int i = 0; i < indexArray.length; i++) {
                    if (indexArray[i] == true) {
                        indexes[i] = i;
                    }
                }
                list.select(indexes);
                list.setData(indexArray);
            }

            switch (type) {
            case NODE: {
                selectNode(index);
                break;
            }
            case CATEGORY: {
                selectCategory(index);
                break;
            }
            case DESIGNATOR: // fall through
            case DESIGNATOR_TIME: {
                selectDesignator(index);
                break;
            }
            case DISPLAY: {
                selectDisplay(index);
                break;
            }
            }
        } else if (rightMouse) {
            displayHelpText(type, list, index);
        }
    }

    /**
     * Set the AFOS Cmd in the parent dialog.
     * 
     * @param command
     */
    private void setAFOSCommand(String command) {
        // save off command so whenever load and continue is used, the current
        // selected command is executed
        currentAfosCommand = command;
        callbackClient.setAfosCmdField(currentAfosCommand);
    }

    /*
     * Selection handler for all lists.
     */
    private class ListSelectionHandler extends SelectionAdapter {
        private final ListType type;

        public ListSelectionHandler(ListType type) {
            this.type = type;
        }

        @Override
        public void widgetSelected(SelectionEvent e) {
            List list = (List) e.getSource();
            int index = list.getSelectionIndex();
            selectListItem(type, index);
            if (rightMouse) {
                rightMouse = false;
                resetSelection(type);
            }
        }
    }

    /*
     * Mouse adaptor for all lists.
     */
    private class ListMouseHandler extends MouseAdapter {
        final private ListType type;

        public ListMouseHandler(ListType type) {
            this.type = type;
        }

        @Override
        public void mouseDown(MouseEvent e) {
            if (menuMgr != null) {
                menuMgr.dispose();
                menuMgr = null;
            }

            leftMouse = false;
            rightMouse = false;

            // ignore button 2
            if (e.button == 2) {
                resetSelection(type);
            } else {
                if (e.button == 1) {
                    leftMouse = true;
                } else if (e.button == 3) {
                    rightMouse = true;
                }
            }
        }
    }

    /*
     * This handles keeping the two designator lists and the their one scroll
     * bar in sync.
     */
    private class DesignatorMouseWheelHandler implements MouseWheelListener {

        @Override
        public void mouseScrolled(MouseEvent e) {
            ScrollBar designatorBar = designatorList.getVerticalBar();
            int selection = designatorBar.getSelection();

            if (e.count > 0) {
                selection -= designatorBar.getIncrement();
            } else {
                selection += designatorBar.getIncrement();
            }

            // verify bound
            if (selection < 0) {
                selection = 0;
            } else if (selection > designatorBar.getMaximum()
                    - designatorBar.getThumb()) {
                selection = designatorBar.getMaximum()
                        - designatorBar.getThumb();
            }

            designatorBar.setSelection(selection);
            designatorTimeList.getVerticalBar().setSelection(selection);
        }

    }

    /**
     * Job to query for designator times.
     */
    private class QueryRequests extends Job {
        /*
         * This contains the query command for getting a some of the Designator
         * times for the query and where in the designator time list the times
         * belong.
         */
        private class TimesRequest {
            /**
             * Clone copy of the query passed to the constructor.
             */
            protected TextDBQuery query;

            /**
             * The index in the Designator Time list where to start placing the
             * results.
             */
            protected int startIndex;

            /**
             * Query results.
             */
            protected Message queryResults;

            public TimesRequest(TextDBQuery query, int startIndex) {
                this.query = query.clone();
                this.startIndex = startIndex;
            }
        }

        /**
         * List of pending Times Requests needed to populate the designator
         * times.
         */
        final LinkedBlockingQueue<TimesRequest> timesRequestQueue = new LinkedBlockingQueue<AfosBrowserDlg.QueryRequests.TimesRequest>();

        /**
         * List of completed Times Requests that need to be processed to update
         * the designator's hours.
         */
        final LinkedBlockingQueue<TimesRequest> timesResultQueue = new LinkedBlockingQueue<AfosBrowserDlg.QueryRequests.TimesRequest>();

        /**
         * A job for taking a pending TimesRequest, perform the query and then
         * place it on the result list.
         */
        private class QueryJob extends Job {
            /**
             * Flag to indicate active request has been canceled.
             */
            private boolean canceled = false;

            /**
             * Constructor.
             * 
             * @param name
             */
            public QueryJob(String name) {
                super("QueryJob");
                setSystem(true);
            }

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime
             * .IProgressMonitor)
             */
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    while (true) {
                        TimesRequest timesRequest = timesRequestQueue.poll();
                        if (timesRequest == null || canceled) {
                            break;
                        }
                        timesRequest.queryResults = timesRequest.query
                                .executeQuery();
                        timesRequest.query = null;

                        if (!canceled) {
                            timesResultQueue.add(timesRequest);
                        }
                    }
                } finally {
                    queryJobList.remove(this);
                }
                return Status.OK_STATUS;
            }
        }

        /**
         * Maximum number of product ids to place in a Times Request.
         */
        private final int MAX_PRODUCTS_PER_QUERY = 25;

        /**
         * Maximum number of jobs to service the Times Request.
         */
        private final int MAX_QUERIES = 5;

        /**
         * Active Query Jobs. The list must be synchronized since it is modified
         * in multiple threads outside of any synchronized block.
         */
        private final java.util.List<QueryJob> queryJobList = Collections
                .synchronizedList(new ArrayList<AfosBrowserDlg.QueryRequests.QueryJob>(
                        MAX_QUERIES));

        /**
         * The pending request for getting designator hours.
         */
        private TextDBQuery pendingRequest = null;

        /**
         * Flag to indicate the current request is canceled.
         */
        private boolean canceled = false;

        /**
         * The complete list of productIds to run in the request query.
         */
        private java.util.List<String> productIds;

        /**
         * Constructor.
         */
        public QueryRequests() {
            super("QueryRequests");
            setSystem(true);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            java.util.List<String> prodIds = null;
            TextDBQuery request = null;

            synchronized (this) {
                if (monitor.isCanceled()) {
                    return Status.OK_STATUS;
                } else {
                    prodIds = productIds;
                    productIds = null;
                    request = pendingRequest;
                    pendingRequest = null;
                }
            }

            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (isDisposed()) {
                        return;
                    }
                    startDesignatorTimeList();
                }
            });

            int cnt = 0;
            int startIndex = 1;

            // Queue up requests for getting Designator hours and start jobs
            // to process them.
            Iterator<String> iterator = prodIds.iterator();
            while (iterator.hasNext()) {
                String productId = iterator.next();

                if (canceled) {
                    break;
                }

                request.addProductId(productId);
                ++cnt;

                if (cnt >= MAX_PRODUCTS_PER_QUERY || !iterator.hasNext()) {
                    TimesRequest timesRequest = new TimesRequest(request,
                            startIndex);
                    timesRequestQueue.add(timesRequest);
                    if (queryJobList.size() < MAX_QUERIES) {
                        QueryJob queryJob = new QueryJob("");
                        queryJobList.add(queryJob);
                        queryJob.schedule();
                    }
                    request.clearProductIds();
                    startIndex += cnt;
                    cnt = 0;
                }
            }

            // Shorter wait for better response when few product IDs.
            long waitTime = 40L;
            long waitDelta = 10L;
            long waitMax = 200L;

            // Wait for all query jobs to finish and update results.
            boolean finished = false;
            while (!(finished || canceled)) {
                if (!timesResultQueue.isEmpty()) {
                    final java.util.List<TimesRequest> resultList = new ArrayList<AfosBrowserDlg.QueryRequests.TimesRequest>();
                    timesResultQueue.drainTo(resultList);

                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }
                            for (TimesRequest result : resultList) {
                                if (canceled) {
                                    break;
                                }
                                updateAfterQuery(result.queryResults,
                                        result.startIndex);
                            }
                        }
                    });
                }

                if (queryJobList.size() > 0) {
                    synchronized (this) {
                        try {
                            wait(waitTime);
                        } catch (InterruptedException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                        if (waitTime < waitMax) {
                            waitTime += waitDelta;
                        }
                    }
                } else {
                    finished = timesResultQueue.isEmpty();
                }
            }

            if (!canceled) {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        if (isDisposed()) {
                            return;
                        }
                        finishDesignatorTimeList();
                    }
                });
            }

            // Make sure everything is in proper state for the next time job is
            // schedule.
            timesRequestQueue.clear();
            timesResultQueue.clear();
            queryJobList.clear();
            canceled = false;
            return Status.OK_STATUS;
        }

        /**
         * Prepare to process the request for the list of product IDs. If needed
         * cancel any active query before scheduling this request.
         * 
         * @param request
         *            - Query to run.
         * @param productIds
         *            - List of products for the query.
         */
        public synchronized void addRequest(TextDBQuery request,
                java.util.List<String> productIds) {
            if (getState() != Job.NONE) {
                cancel();
            }

            this.pendingRequest = request;
            this.productIds = productIds;
            schedule();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#canceling()
         */
        @Override
        protected void canceling() {
            canceled = true;
            for (QueryJob query : queryJobList) {
                query.canceled = true;
            }
        }
    }
}