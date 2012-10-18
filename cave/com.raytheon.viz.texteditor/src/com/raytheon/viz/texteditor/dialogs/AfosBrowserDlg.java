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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.SortedSet;

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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.textdb.dbapi.impl.AFOS_CLASS;
import com.raytheon.edex.textdb.dbapi.impl.AFOS_ORIGIN;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.services.textdbsrv.IQueryTransport;
import com.raytheon.uf.edex.services.textdbsrv.TextDBQuery;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.AfosBrowserModel;
import com.raytheon.viz.texteditor.TextDisplayModel;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.msgs.IAfosBrowserCallback;
import com.raytheon.viz.texteditor.msgs.ITextWorkstationCallback;
import com.raytheon.viz.texteditor.util.TextEditorUtil;
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
 * </pre>
 * 
 * @author lvenable
 */
public class AfosBrowserDlg extends CaveSWTDialog implements
        ITextWorkstationCallback {

    private static final String TIME_FORMAT = "%1$tH:%1$tM %1$tb %1$td";

    private final AfosBrowserModel browserData = AfosBrowserModel.getInstance();

    private enum ListType {
        NODE, CATEGORY, DESIGNATOR, DESIGNATOR_TIME, DISPLAY
    };

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
     * The node index that is selected.
     */
    private int selectedNodeIndex = -1;

    /**
     * The category that is selected.
     */
    private String selectedCategory;

    /**
     * The category index that is selected.
     */
    private int selectedCategoryIndex = -1;

    /**
     * The designator that is selected.
     */
    private String selectedDesignator;

    /**
     * The designator index that is selected.
     */
    private int selectedDesignatorIndex = -1;

    /**
     * The designator time that is selected.
     */
    private String selectedDesignatorTime;

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
     * Flag that indicates whether the AFOS Browser is active or not
     */
    private boolean isAfosActive = true;

    private IQueryTransport queryTransport = null;

    private String localSite = LocalizationManager.getInstance()
            .getCurrentSite();;

    private String currentAfosCommand = null;

    private MenuManager menuMgr = null;

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

        setText(browserHdr + " Browser");

        callbackClient = cbClient;

        TextDisplayModel.getInstance().setITextWorkstationCallback(token, this);

        queryTransport = TextEditorUtil.getTextDbsrvTransport();
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

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
                isAfosActive = false;
                event.doit = false;
            }
        });
    }

    /**
     * Initialize the components of the dialog.
     */
    private void initializeComponents() {
        createOriginClassControls();
        createListHelpMenus();
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
        GridLayout gridLayout = new GridLayout(4, false);
        // gridLayout.marginLeft = 5;
        controlComp.setLayout(gridLayout);

        // Add the origin label.
        Label originLbl = new Label(controlComp, SWT.NONE);
        originLbl.setText("Origin: ");

        // Add the origin combo box.
        originCombo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (AFOS_ORIGIN origin : AFOS_ORIGIN.values()) {
            originCombo.add(origin.name());
        }
        AFOS_ORIGIN origin = browserData.getDefaultOrigin(localSite);
        originCombo.select(origin.ordinal());
        selectedNode = localSite;

        // Use this listener to update the node list based on the origin.
        originCombo.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                loadNodeList();
            }
        });

        // Add the class label.
        Label classLbl = new Label(controlComp, SWT.NONE);
        classLbl.setText("Class: ");

        // Populate the category class dialog
        // Add the class combo box.
        categoryClassCombo = new Combo(controlComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        for (AFOS_CLASS c : AFOS_CLASS.values()) {
            categoryClassCombo.add(c.value());
        }
        categoryClassCombo.select(0);

        // Use this listener to update the category list based on the origin
        // plus the class.
        categoryClassCombo.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (nodeList.getSelectionCount() > 0) {
                    loadCategoryList();
                }
            }
        });
    }

    /**
     * Create the List help buttons.
     * 
     * NOTE: The help buttons may change. The original dialog has embedded menus
     * that SWT does not support. We may go with a button/pop-up menu
     * combination or a coolbar with a drop-down. This will be decided later.
     */
    private void createListHelpMenus() {
        // Create the composite that will hold the controls.
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Composite helpComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(4, false);
        // gridLayout.marginLeft = 3;
        helpComp.setLayout(gridLayout);
        helpComp.setLayoutData(gd);

        // ------------------------------------
        // Create the Node component
        // ------------------------------------
        gd = new GridData(100, SWT.DEFAULT);
        Composite nodeComp = new Composite(helpComp, SWT.NONE);
        gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 3;
        nodeComp.setLayout(gridLayout);
        nodeComp.setLayoutData(gd);

        Label nodeLbl = new Label(nodeComp, SWT.NONE);
        nodeLbl.setText("Node");

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        Button nodeHelpButton = new Button(nodeComp, SWT.PUSH);
        nodeHelpButton.setText("Help");
        nodeHelpButton.setLayoutData(gd);

        nodeHelpButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.NODE, nodeList);
            }
        });

        // ------------------------------------
        // Create the Category component
        // ------------------------------------
        gd = new GridData(100, SWT.DEFAULT);
        Composite categoryComp = new Composite(helpComp, SWT.NONE);
        gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 3;
        categoryComp.setLayout(gridLayout);
        categoryComp.setLayoutData(gd);

        Label categoryLbl = new Label(categoryComp, SWT.NONE);
        categoryLbl.setText("Ctgry");

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        Button ctgryHelpButton = new Button(categoryComp, SWT.PUSH);
        ctgryHelpButton.setText("Help");
        ctgryHelpButton.setLayoutData(gd);

        ctgryHelpButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.CATEGORY, categoryList);
            }
        });

        // ------------------------------------
        // Create the Designator component
        // ------------------------------------
        gd = new GridData(225, SWT.DEFAULT);
        Composite designatorComp = new Composite(helpComp, SWT.NONE);
        gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 3;
        designatorComp.setLayout(gridLayout);
        designatorComp.setLayoutData(gd);

        Label designatorLbl = new Label(designatorComp, SWT.NONE);
        designatorLbl.setText("Designator");

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        Button designatorHelpButton = new Button(designatorComp, SWT.PUSH);
        designatorHelpButton.setText("Help");
        designatorHelpButton.setLayoutData(gd);

        designatorHelpButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.DESIGNATOR, designatorList);
            }
        });

        // ------------------------------------
        // Create the Display component
        // ------------------------------------
        gd = new GridData(140, SWT.DEFAULT);
        Composite displayComp = new Composite(helpComp, SWT.NONE);
        gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 3;
        displayComp.setLayout(gridLayout);
        displayComp.setLayoutData(gd);

        Label displayLbl = new Label(displayComp, SWT.NONE);
        displayLbl.setText("Display");

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        Button displayHelpButton = new Button(displayComp, SWT.PUSH);
        displayHelpButton.setText("Help");
        displayHelpButton.setLayoutData(gd);

        displayHelpButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // nothing to do, no help available for display
            }
        });
    }

    /**
     * Create the List box controls.
     */
    private void createListControls() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        // Create the composite that will hold the controls.
        Composite listComp = new Composite(shell, SWT.NONE);

        GridLayout gridLayout = new GridLayout(5, false);
        listComp.setLayout(gridLayout);
        listComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 80;
        gd.heightHint = 200;
        nodeList = new List(listComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        nodeList.setLayoutData(gd);
        nodeList.addMouseListener(new ListMouseHandler(ListType.NODE));
        nodeList.addSelectionListener(new ListSelectionHandler(ListType.NODE));
        nodeList.setData(new boolean[0]);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 80;
        gd.heightHint = 200;
        categoryList = new List(listComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        categoryList.setLayoutData(gd);
        categoryList.addMouseListener(new ListMouseHandler(ListType.CATEGORY));
        categoryList.addSelectionListener(new ListSelectionHandler(
                ListType.CATEGORY));
        categoryList.setData(new boolean[0]);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 80;
        gd.heightHint = 200;
        designatorList = new List(listComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        designatorList.setLayoutData(gd);
        designatorList.addMouseListener(new ListMouseHandler(
                ListType.DESIGNATOR));
        designatorList.addSelectionListener(new ListSelectionHandler(
                ListType.DESIGNATOR));
        designatorList.addMouseWheelListener(new DesignatorMouseWheelHandler());
        designatorList.setData(new boolean[0]);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 100;
        gd.heightHint = 200;
        designatorTimeList = new List(listComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        designatorTimeList.setLayoutData(gd);
        designatorTimeList.addMouseListener(new ListMouseHandler(
                ListType.DESIGNATOR_TIME));
        designatorTimeList.addSelectionListener(new ListSelectionHandler(
                ListType.DESIGNATOR_TIME));
        designatorTimeList.setData(new boolean[0]);

        designatorList.getVerticalBar().setVisible(false);
        scrollListsTogether(designatorList, designatorTimeList);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 100;
        gd.heightHint = 200;
        displayList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        displayList.setLayoutData(gd);
        displayList.addMouseListener(new ListMouseHandler(ListType.DISPLAY));
        displayList.addSelectionListener(new ListSelectionHandler(
                ListType.DISPLAY));
    }

    /**
     * Create the bottom control buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(3, true);
        buttonComp.setLayout(gridLayout);
        buttonComp.setLayoutData(gd);

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.heightHint = 27;
        gd.grabExcessHorizontalSpace = true;
        loadContinueBtn = new Button(buttonComp, SWT.PUSH);
        loadContinueBtn.setText("Load and Continue");
        loadContinueBtn.setEnabled(false);
        loadContinueBtn.setLayoutData(gd);
        loadContinueBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callbackClient.executeCommand(CommandFactory
                        .getAfosCommand(currentAfosCommand));
                loadContinueBtn.setFocus();
            }
        });

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.heightHint = 27;
        loadCloseBtn = new Button(buttonComp, SWT.PUSH);
        loadCloseBtn.setText("Load and Close");
        loadCloseBtn.setEnabled(false);
        loadCloseBtn.setLayoutData(gd);
        loadCloseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callbackClient.executeCommand(CommandFactory
                        .getAfosCommand(currentAfosCommand));

                setReturnValue(false);
                hide();
                isAfosActive = false;
            }
        });

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.heightHint = 27;
        closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                hide();
                isAfosActive = false;
            }
        });
    }

    public void showDialog() {
        bringToTop();
        isAfosActive = true;
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

    private void queryTableUsingNodeAndCategory() {
        TextDBQuery dbQuery = new TextDBQuery(queryTransport);
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
        for (int i = 1; i < designatorList.getItemCount(); i++) {
            String prodId = selectedNode + selectedCategory
                    + designatorList.getItem(i);
            dbQuery.addProductId(prodId);
        }

        Message queryResponse = dbQuery.executeQuery();
        Property[] properties = queryResponse.getHeader().getProperties();

        ArrayList<Long> times = new ArrayList<Long>();
        // We use this time to populate the "000" designator entry.
        long newestTime = -1L;
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

        designatorTimeList.removeAll();
        if (newestTime > 0) {
            Calendar c = TimeTools.newCalendar(newestTime);
            designatorTimeList.add(String.format(TIME_FORMAT, c));
        } else if (newestTime == -1) {
            designatorTimeList.add("ERROR");
        } else {
            designatorTimeList.add("NO DATA");
        }

        for (Long time : times) {
            if (time > 0) {
                Calendar c = TimeTools.newCalendar(time);
                designatorTimeList.add(String.format(TIME_FORMAT, c));
            } else if (time == -1) {
                designatorTimeList.add("ERROR");
            } else {
                designatorTimeList.add("NO DATA");
            }
        }
    }

    @Override
    public boolean isAfosBrowserActive() {
        return isAfosActive;
    }

    private void setLoadBtnEnabled(boolean flag) {
        loadContinueBtn.setEnabled(flag);
        loadCloseBtn.setEnabled(flag);
    }

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
        updateSelectionList(designatorTimeList);

        loadDisplayList();
    }

    private void updateSelectionList(List list) {
        boolean[] selectedIndexes = new boolean[list.getItemCount()];
        Arrays.fill(selectedIndexes, false);
        list.setData(selectedIndexes);
    }

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
    }

    private void selectNode(int index) {
        selectedNodeIndex = index;
        selectedNode = nodeList.getItem(index);
        nodeList.setSelection(index);
        nodeList.showSelection();
        setAFOSCommand(selectedNode);
        loadCategoryList();
    }

    private void selectCategory(int index) {
        selectedCategoryIndex = index;
        selectedCategory = categoryList.getItem(index);
        categoryList.setSelection(index);
        categoryList.showSelection();
        setAFOSCommand(selectedNode + selectedCategory);
        loadDesignatorList();
    }

    private void selectDesignator(int index) {
        String tmp = designatorList.getItem(index);

        if (tmp.length() > 0) {
            selectedDesignatorIndex = index;
            selectedDesignator = designatorList.getItem(index);
            selectedDesignatorTime = designatorTimeList.getItem(index);
            designatorList.setSelection(index);
            designatorTimeList.setSelection(index);
            designatorList.showSelection();
            designatorTimeList.showSelection();
            setAFOSCommand(selectedNode + selectedCategory + selectedDesignator);
        } else {
            designatorList.deselectAll();
            designatorTimeList.deselectAll();
            setAFOSCommand(selectedNode + selectedCategory);
        }

        loadDisplayList();
        checkLoadBtn();
    }

    private void selectDisplay(int index) {
        String designator = null;
        String display = null;

        if (designatorList.getSelectionIndex() >= 0) {
            designator = selectedDesignator;
        } else {
            designator = "";
        }

        if (index < displayList.getItemCount()) {
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

    private void checkLoadBtn() {
        boolean enabled = false;
        int designatorIndex = designatorTimeList.getSelectionIndex();
        int displayIndex = displayList.getSelectionIndex();
        if (designatorIndex >= 0) {
            String time = designatorTimeList.getItem(designatorIndex);
            if (time.length() > 0) {
                enabled = !time.equals("ERROR") && !time.equals("NO DATA");
            } else if (displayIndex >= 0) {
                enabled = displayList.getItem(displayIndex).length() > 0;
            }
        } else if (displayIndex >= 0) {
            enabled = displayList.getItem(displayIndex).length() > 0;
        }

        setLoadBtnEnabled(enabled);
    }

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

    private void displayHelpList(ListType type, List list) {
        int height = list.getClientArea().height;
        String[] items = list.getItems();
        ArrayList<IAction> actions = new ArrayList<IAction>();
        int startIndex = list.getTopIndex();

        // list.getItemHeight() can be off by a few pixels depending on
        // height/style, in terms of the few list entries the difference should
        // be miniscule
        int endIndex = startIndex + (height / list.getItemHeight());

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
        }
        }

        // if there are actions, display pop up menu
        if (actions.size() > 0) {
            menuMgr = new MenuManager("#PopupMenu");

            for (IAction action : actions) {
                menuMgr.add(action);
            }

            Menu menu = menuMgr.createContextMenu(list);
            menu.setVisible(true);
            list.setMenu(menu);
        }
    }

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

    private void setAFOSCommand(String command) {
        // save off command so whenever load and continue is used, the current
        // selected command is executed
        currentAfosCommand = command;
        callbackClient.setAfosCmdField(currentAfosCommand);
    }

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

    private class ListMouseHandler extends MouseAdapter {
        final private ListType type;

        int previousOffset = -Integer.MAX_VALUE;

        int previousIndex = -1;

        public ListMouseHandler(ListType type) {
            this.type = type;
        }

        @Override
        public void mouseDown(MouseEvent e) {
            if (menuMgr != null) {
                menuMgr.dispose();
                menuMgr = null;
            }

            // ignore button 2
            if (e.button == 2) {
                resetSelection(type);
            } else {
                // List list = (List) e.getSource();
                // int index = list.getSelectionIndex();
                // int offset = list.getVerticalBar().getSelection() + e.y;
                //
                // boolean clickedOffList = isOffListClick(e);
                //
                // // only care about button 1 in case of same indexes where we
                // // need to select last item in list instead
                // if (e.button == 1) {
                // if (clickedOffList) {
                // // select last item
                // selectListItem(type, list.getItemCount() - 1);
                // }
                // } else {
                // // button 3
                // if (!clickedOffList) {
                // displayHelpText(type, list, index);
                // resetSelection(type);
                // }
                // }
                //
                // previousIndex = list.getSelectionIndex();
                // previousOffset = offset;
                leftMouse = false;
                rightMouse = false;
                if (e.button == 1) {
                    leftMouse = true;
                } else if (e.button == 3) {
                    rightMouse = true;
                }
            }
        }

        private boolean isOffListClick(MouseEvent e) {
            List list = (List) e.getSource();
            int index = list.getSelectionIndex();
            int offset = list.getVerticalBar().getSelection() + e.y;

            if (index == previousIndex
                    && index >= 0
                    && Math.abs(offset - previousOffset) >= list
                            .getItemHeight()) {
                return true;
            }

            return false;
        }
    }

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
            } else if (selection > (designatorBar.getMaximum() - designatorBar
                    .getThumb())) {
                selection = designatorBar.getMaximum()
                        - designatorBar.getThumb();
            }

            designatorBar.setSelection(selection);
            designatorTimeList.getVerticalBar().setSelection(selection);
        }

    }
}