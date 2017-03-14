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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
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

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.texteditor.AwipsBrowserModel;
import com.raytheon.viz.texteditor.TextDisplayModel;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.ICommand;
import com.raytheon.viz.texteditor.command.IProductQueryCallback;
import com.raytheon.viz.texteditor.command.ProductQueryJob;
import com.raytheon.viz.texteditor.msgs.IAwipsBrowserCallback;
import com.raytheon.viz.texteditor.msgs.ITextWorkstationCallback;
import com.raytheon.viz.texteditor.util.AFOS_CLASS;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The AWIPS browser dialog allows the user to look through and choose text
 * products by Category, Designator and site to be displayed in the text window.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 12, 2016  4716       rferrel     Initial creation.
 * Nov 02, 2016  5975       rferrel     Fix designator help and disabled Site's help.
 * </pre>
 * 
 * @author rferrel
 */
public class AwipsBrowserDlg extends CaveSWTDialog
        implements ITextWorkstationCallback, IProductQueryCallback {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AwipsBrowserDlg.class);

    private final AwipsBrowserModel browserData = AwipsBrowserModel
            .getInstance();

    /**
     * The different lists displayed in the browser.
     */
    private enum ListType {
        CATEGORY, DESIGNATOR, SITE, HDR_TIME
    };

    /**
     * custom mouse handling so we can do the required vertical bar
     * non-scrolling.
     */
    private boolean leftMouse = false;

    private boolean rightMouse = false;

    /**
     * Class combo box.
     */
    private Combo categoryClassCombo;

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
     * List control displaying sites for selected category and designator.
     */
    private List siteList;

    /**
     * List control displaying header times for selected catgegory, designator
     * and site.
     */
    private List hdrTimeList;

    /**
     * The category that is selected.
     */
    private String selectedCategory;

    /**
     * The designator that is selected.
     */
    private String selectedDesignator;

    /**
     * The site that is selected.
     */
    private String selectedSite;

    /**
     * Product's site to select after loading siteList.
     */
    private String prodSelectSite;

    /**
     * Mapping of products by site for the current selected category and
     * designator.
     */
    private final Map<String, java.util.List<StdTextProduct>> prodMap = new HashMap<>();

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
     * Interface variable for Awips Browser callback
     */
    private IAwipsBrowserCallback callbackClient = null;

    /**
     * Flag that indicates whether the Browser is active or not
     */
    private boolean isActive = true;

    private String currentAfosCommand = null;

    private MenuManager menuMgr = null;

    private final int NUM_ITEMS = 10;

    /**
     * Job to handle query for products off the UI thread.
     */
    private final ProductQueryJob productQueryJob = new ProductQueryJob(this);

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param browserHdr
     *            Browser name.
     * @param cbClient
     *            Interface to callback for Awips Browser.
     */
    public AwipsBrowserDlg(Shell parent, String browserHdr,
            IAwipsBrowserCallback cbClient, String token) {
        super(parent, SWT.DIALOG_TRIM | SWT.MODELESS | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);

        setText(browserHdr + " AWIPS Browser");

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

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                // Block the disposal of this dialog.
                AwipsBrowserDlg.this.hide();
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

        // Create the composite that will hold the controls.
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(4, false);
        controlComp.setLayout(gridLayout);

        // Add the class label.
        Label classLbl = new Label(controlComp, SWT.NONE);
        classLbl.setText("Class: ");

        // Populate the category class dialog
        // Add the class combo box.
        categoryClassCombo = new Combo(controlComp,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        for (AFOS_CLASS c : AFOS_CLASS.values()) {
            categoryClassCombo.add(c.value());
        }
        categoryClassCombo.select(0);

        // Use this listener to update the category list based on the class.
        categoryClassCombo.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                loadCategoryList();
            }
        });
    }

    /**
     * Create the List box controls.
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

        ToolBar toolBar = new ToolBar(categoryGroup, SWT.FLAT | SWT.RIGHT);

        ToolItem toolItem = new ToolItem(toolBar, SWT.DROP_DOWN);
        toolItem.setText("Help");
        toolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.CATEGORY, categoryList);
            }
        });

        categoryList = new List(categoryGroup,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Rectangle trim = categoryList.computeTrim(0, 0, charWidth * 3,
                categoryList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        categoryList.setLayoutData(gd);

        categoryList.addMouseListener(new ListMouseHandler(ListType.CATEGORY));
        categoryList.addSelectionListener(
                new ListSelectionHandler(ListType.CATEGORY));
        categoryList.setData(new boolean[0]);

        // ------------------------------------
        // Create the Designator component
        // ------------------------------------
        Group designatorGroup = new Group(listComp, SWT.NONE);
        gridLayout = new GridLayout(1, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        gridLayout.verticalSpacing = 0;
        designatorGroup.setLayout(gridLayout);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        designatorGroup.setLayoutData(gd);
        designatorGroup.setText("Designator");

        toolBar = new ToolBar(designatorGroup, SWT.FLAT | SWT.RIGHT);

        toolItem = new ToolItem(toolBar, SWT.DROP_DOWN);
        toolItem.setText("Help");
        toolItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayHelpList(ListType.DESIGNATOR, designatorList);
            }
        });

        designatorList = new List(designatorGroup,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        trim = designatorList.computeTrim(0, 0, charWidth * 3,
                designatorList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        designatorList.setLayoutData(gd);

        designatorList
                .addMouseListener(new ListMouseHandler(ListType.DESIGNATOR));
        designatorList.addSelectionListener(
                new ListSelectionHandler(ListType.DESIGNATOR));
        designatorList.setData(new boolean[0]);

        // ------------------------------------
        // Create the Site component
        // ------------------------------------
        Group siteGroup = new Group(listComp, SWT.NONE);
        gridLayout = new GridLayout(2, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        gridLayout.verticalSpacing = 0;
        siteGroup.setLayout(gridLayout);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        siteGroup.setLayoutData(gd);
        siteGroup.setText("Site");

        /*
         * Keep as place holder so this lines up with Category and Designator
         * columns and resizes correctly.
         */
        toolBar = new ToolBar(siteGroup, SWT.FLAT | SWT.RIGHT);

        toolItem = new ToolItem(toolBar, SWT.DROP_DOWN);
        toolItem.setText("Help");
        toolItem.setEnabled(false);

        // dummy label to fill the spot in the grid
        new Label(siteGroup, SWT.NONE);

        siteList = new List(siteGroup, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        trim = siteList.computeTrim(0, 0, charWidth * 3,
                siteList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        siteList.setLayoutData(gd);

        siteList.addMouseListener(new ListMouseHandler(ListType.SITE));
        siteList.addSelectionListener(new ListSelectionHandler(ListType.SITE));
        siteList.addMouseWheelListener(new DesignatorMouseWheelHandler());
        siteList.setData(new boolean[0]);

        hdrTimeList = new List(siteGroup,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        trim = hdrTimeList.computeTrim(0, 0, charWidth * 12,
                hdrTimeList.getItemHeight() * NUM_ITEMS);
        gd.widthHint = trim.width;
        gd.heightHint = trim.height;
        hdrTimeList.setLayoutData(gd);

        hdrTimeList.addMouseListener(new ListMouseHandler(ListType.HDR_TIME));
        hdrTimeList.addSelectionListener(
                new ListSelectionHandler(ListType.HDR_TIME));
        hdrTimeList.setData(new boolean[0]);

        siteList.getVerticalBar().setVisible(false);
        scrollListsTogether(siteList, hdrTimeList);
    }

    /**
     * Create the bottom control buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalAlignment = SWT.CENTER;
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(3, true);
        buttonComp.setLayout(gridLayout);
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.horizontalAlignment = SWT.FILL;
        loadContinueBtn = new Button(buttonComp, SWT.PUSH);
        loadContinueBtn.setText("Load and Continue");
        loadContinueBtn.setEnabled(false);
        loadContinueBtn.setLayoutData(gd);
        loadContinueBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callbackClient.executeCommand(getAwipsCommand());
                loadContinueBtn.setFocus();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.horizontalAlignment = SWT.FILL;
        loadCloseBtn = new Button(buttonComp, SWT.PUSH);
        loadCloseBtn.setText("Load and Close");
        loadCloseBtn.setEnabled(false);
        loadCloseBtn.setLayoutData(gd);
        loadCloseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                callbackClient.executeCommand(getAwipsCommand());

                setReturnValue(false);
                hide();
                isActive = false;
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.horizontalAlignment = SWT.FILL;
        closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                hide();
                isActive = false;
            }
        });
    }

    private ICommand getAwipsCommand() {
        String hdrTime = hdrTimeList.getSelection()[0];
        return CommandFactory.getAwipsCommand(
                selectedCategory + selectedDesignator, null, selectedSite,
                hdrTime, null);
    }

    /**
     * Allow parent dialog to display the hidden dialog or just force it to the
     * top of the display.
     */
    public void showDialog() {
        bringToTop();
        isActive = true;
    }

    @Override
    public boolean isBrowserActive() {
        return isActive;
    }

    /**
     * Load lists based on common elements of in the product list and select
     * elements.
     * 
     * @param prodList
     */
    public void select(java.util.List<StdTextProduct> prodList) {
        Iterator<StdTextProduct> prodIter = prodList.iterator();
        StdTextProduct prod = prodIter.next();
        String nnn = prod.getNnnid();
        String xxx = prod.getXxxid();
        prodSelectSite = prod.getSite();
        while (prodIter.hasNext()) {
            prod = prodIter.next();
            if (xxx != null) {
                if (!xxx.equals(prod.getXxxid())) {
                    xxx = null;
                }
            }
            if (prodSelectSite != null) {
                if (!prodSelectSite.equals(prod.getSite())) {
                    prodSelectSite = null;
                }
            }
        }

        leftMouse = true;
        rightMouse = false;
        categoryList.setSelection(new String[] { nnn });
        int catIndex = categoryList.getSelectionIndex();
        if (catIndex >= 0) {
            selectListItem(ListType.CATEGORY, catIndex);
            if (xxx != null) {
                designatorList.setSelection(new String[] { xxx });
                int xxxIndex = designatorList.getSelectionIndex();
                if (xxxIndex >= 0) {
                    selectListItem(ListType.DESIGNATOR, xxxIndex);
                }
            }
        }
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
     * Convince method for keeping the handling of load buttons the same.
     * 
     * @param flag
     */
    private void setLoadBtnEnabled(boolean flag) {
        loadContinueBtn.setEnabled(flag);
        loadCloseBtn.setEnabled(flag);
    }

    /**
     * Populate the category list. Assumes query for the list will not hang up
     * the UI thread.
     */
    private void loadCategoryList() {
        setLoadBtnEnabled(false);
        categoryList.removeAll();
        designatorList.removeAll();
        siteList.removeAll();
        hdrTimeList.removeAll();

        String categoryClass = categoryClassCombo
                .getItem(categoryClassCombo.getSelectionIndex());

        for (String category : browserData
                .getFilteredCategoryList(categoryClass)) {
            categoryList.add(category);

            if (category.equals(selectedCategory)) {
                selectCategory(categoryList.getItemCount() - 1);
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
        siteList.removeAll();
        hdrTimeList.removeAll();

        SortedSet<String> xxx = browserData.getDesignatorList(selectedCategory);

        if (xxx != null) {

            // Get the designator list - ensure that the entries are three
            // characters in length.
            for (String s : xxx) {
                if (s.length() == 1) {
                    s = s + "  ";
                } else if (s.length() == 2) {
                    s = s + " ";
                }
                designatorList.add(s);
            }

            // select a designator if applicable
            String items[] = designatorList.getItems();
            for (int i = 0; i < items.length; i++) {
                if (items[i].equals(selectedDesignator)) {
                    selectDesignator(i);
                }
            }

            // add a blank entry to allow deselection
            designatorList.add("");
            hdrTimeList.add("");
        }
        updateSelectionList(designatorList);
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
     * Execute command to retrieve all sites and hdr times only when theree is a
     * selected category and designator.
     */
    private void loadSiteList() {
        siteList.removeAll();
        hdrTimeList.removeAll();
        setLoadBtnEnabled(false);
        if ((categoryList.getSelectionIndex() > 0)
                && (designatorList.getSelectionIndex() > 0)) {
            String nnn = categoryList.getItem(categoryList.getSelectionIndex());
            String xxx = designatorList
                    .getItem(designatorList.getSelectionIndex());
            String awipsId = nnn + xxx;
            ICommand command = CommandFactory.getAwipsCommand(awipsId, null,
                    null, "000000", null);
            executeCommand(command);
        }
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
        setAWIPSCommand(selectedCategory);
        loadDesignatorList();
    }

    /**
     * Select desired site and update hdr times to its values.
     * 
     * @param index
     */
    private void selectSite(int index) {
        selectedSite = siteList.getItem(index);
        siteList.setSelection(index);
        setAWIPSCommand(selectedSite + selectedCategory + selectedDesignator);

        hdrTimeList.removeAll();
        for (StdTextProduct prod : prodMap.get(selectedSite)) {
            hdrTimeList.add(prod.getHdrtime());
        }
        updateSelectionList(hdrTimeList);
        if (hdrTimeList.getItemCount() == 1) {
            hdrTimeList.setSelection(0);
            selectListItem(ListType.HDR_TIME, 0);
        }
        checkLoadBtn();
    }

    /**
     * Update the selected designator, the AWIPS command and site list which
     * will also update the hdr time list.
     * 
     * @param index
     */
    private void selectDesignator(int index) {
        if ((index < 0) || index >= designatorList.getItemCount()) {
            return;
        }
        String tmp = designatorList.getItem(index);
        if (tmp.length() > 0) {
            try {
                selectedDesignator = designatorList.getItem(index);
                designatorList.setSelection(index);
                setAWIPSCommand(selectedCategory + selectedDesignator);

            } catch (IllegalArgumentException ex) {
                designatorList.deselectAll();
                hdrTimeList.deselectAll();
                setAWIPSCommand(selectedCategory);
            }
        } else {
            designatorList.deselectAll();
            setAWIPSCommand(selectedCategory);
        }

        loadSiteList();
    }

    /**
     * Check the selected designator and determine if the load buttons should be
     * enabled.
     */
    private void checkLoadBtn() {
        int designatorIndex = hdrTimeList.getSelectionIndex();
        int siteIndex = siteList.getSelectionIndex();
        int hdrTimeIndex = hdrTimeList.getSelectionIndex();

        boolean enabled = (designatorIndex >= 0) && (siteIndex >= 0)
                && (hdrTimeIndex >= 0);
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
        case CATEGORY: {
            list = categoryList;
            break;
        }
        case DESIGNATOR: {
            list = designatorList;
            break;
        }

        case SITE: {
            list = siteList;
            break;
        }
        case HDR_TIME: {
            list = hdrTimeList;
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
        if (items.length == 0) {
            return;
        }
        ArrayList<IAction> actions = new ArrayList<>();
        int startIndex = list.getTopIndex();

        // list.getItemHeight() can be off by a few pixels depending on
        // height/style, in terms of the few list entries the difference should
        // be miniscule
        int endIndex = startIndex + (height / list.getItemHeight());

        // sanity check
        if (endIndex >= items.length) {
            if (items.length == 0) {
                endIndex = 0;
            } else {
                endIndex = items.length - 1;
            }
        }

        // dispose of current menu
        if (menuMgr != null) {
            menuMgr.dispose();
            menuMgr = null;
        }

        switch (type) {
        case CATEGORY: {
            for (int i = startIndex; i <= endIndex; i++) {
                final int index = i;
                String helpText = checkHelpText(
                        browserData.getCategoryHelp(items[index]));
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
                String helpText = checkHelpText(
                        browserData.getDesignatorHelp(items[index]));
                actions.add(new Action(items[index] + ": " + helpText) {
                    @Override
                    public void run() {
                        selectDesignator(index);
                    }
                });
            }
            break;
        }
        case SITE: {
            break;
        }
        case HDR_TIME: {
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
            menu.setVisible(true);
            list.setMenu(menu);
        }
    }

    private String checkHelpText(String helpText) {
        if (helpText == null) {
            return "No help found";
        }
        return helpText;
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
                case CATEGORY: {
                    helpText = browserData.getCategoryHelp(field);
                    if (helpText != null && helpText.length() > 0) {
                        action = new Action(helpText) {
                            @Override
                            public void run() {
                                selectListItem(
                                        AwipsBrowserDlg.ListType.CATEGORY,
                                        index);
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
                                        AwipsBrowserDlg.ListType.DESIGNATOR,
                                        index);
                                selectDesignator(index);
                            }
                        };
                    }
                    break;
                }
                case SITE: {
                    break;
                }

                case HDR_TIME: {
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

    /**
     * Based on the mouse click either select the desired item in the list
     * populating lists impacted by the selection or display help for the item.
     * 
     * @param type
     * @param index
     */
    private void selectListItem(ListType type, int index) {
        List list = null;
        switch (type) {
        case CATEGORY: {
            list = categoryList;
            break;
        }
        case DESIGNATOR: {
            list = designatorList;
            break;
        }
        case SITE: {
            list = siteList;
            break;
        }
        case HDR_TIME: {
            list = hdrTimeList;
            break;
        }
        default: {
            statusHandler.equals("Unsported List Type: " + type);
            return;
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
            case CATEGORY: {
                selectCategory(index);
                break;
            }
            case SITE: {
                selectSite(index);
                break;
            }
            case DESIGNATOR:
                selectDesignator(index);
                break;
            case HDR_TIME: {
                hdrTimeList.setSelection(index);
                break;
            }
            }
            list.showSelection();
            checkLoadBtn();
        } else if (rightMouse) {
            displayHelpText(type, list, index);
        }
    }

    /**
     * Set the AFOS Cmd in the parent dialog.
     * 
     * @param command
     */
    private void setAWIPSCommand(String command) {
        // save off command so whenever load and continue is used, the current
        // selected command is executed
        currentAfosCommand = command;
        callbackClient.setAwipsCmdField(currentAfosCommand);
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

            // ignore button 2
            if (e.button == 2) {
                resetSelection(type);
            } else {
                leftMouse = false;
                rightMouse = false;
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
            ScrollBar designatorBar = siteList.getVerticalBar();
            int selection = designatorBar.getSelection();

            if (e.count > 0) {
                selection -= designatorBar.getIncrement();
            } else {
                selection += designatorBar.getIncrement();
            }

            // verify bound
            if (selection < 0) {
                selection = 0;
            } else if (selection > (designatorBar.getMaximum()
                    - designatorBar.getThumb())) {
                selection = designatorBar.getMaximum()
                        - designatorBar.getThumb();
            }

            designatorBar.setSelection(selection);
            hdrTimeList.getVerticalBar().setSelection(selection);
        }

    }

    @Override
    protected void preOpened() {
        super.preOpened();
        loadCategoryList();
    }

    public void executeCommand(ICommand command) {
        if (isDisposed()) {
            return;
        }
        // busy here
        productQueryJob.addRequest(command, false, false);
    }

    @Override
    public void requestDone(ICommand command,
            java.util.List<StdTextProduct> prodList, boolean isObsUpdated) {

        siteList.removeAll();
        hdrTimeList.removeAll();
        prodMap.clear();
        if ((prodList != null) && !prodList.isEmpty()) {
            for (StdTextProduct prod : prodList) {
                String site = prod.getSite();
                java.util.List<StdTextProduct> pmList = prodMap.get(site);
                if (pmList == null) {
                    pmList = new ArrayList<>();
                    prodMap.put(site, pmList);
                    siteList.add(prod.getSite());
                }
                pmList.add(prod);
            }
            updateSelectionList(siteList);
            if (siteList.getItemCount() == 1) {
                siteList.select(0);
                selectListItem(ListType.SITE, 0);
            } else if (prodSelectSite != null) {
                siteList.setSelection(new String[] { prodSelectSite });
                int siteIndex = siteList.getSelectionIndex();
                if (siteIndex >= 0) {
                    selectListItem(ListType.SITE, siteIndex);
                }
                prodSelectSite = null;
            }
        }
    }
}