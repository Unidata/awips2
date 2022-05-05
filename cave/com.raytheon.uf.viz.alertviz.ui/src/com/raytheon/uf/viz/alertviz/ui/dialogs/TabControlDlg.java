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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

/**
 * Tab control dialog that displays the messages for the different mode layouts.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * 2008                   mschenke   Initial creation
 * Apr 02, 2009           lvenable   TTR fixes.
 * Dec 01, 2010  5632     cjeanbap   Added sort based on category.
 * Mar 02, 2011  5632     cjeanbap   Added sort based on category.
 * Feb 06, 2013  14501    Xiaochuan  Using getCategoriesFromConfig() to set
 *                                   categoryList[] in clearOptionCbo.
 * Jan 29, 2016  5289     tgurney    Add missing close button in trim
 * Apr 01, 2016  5517     randerso   Fix GUI sizing issues
 * Feb 27, 2017  6029     randerso   Ensure dialog appears on correct monitor
 * Apr 03, 2018  6646     randerso   Allow double click in log pane to show/hide
 *                                   details pane
 * Sep 27, 2018  7454     randerso   Clean up management of tabs
 * Sep 28, 2018  7455     randerso   Bring TabControlDlg to top if already open.
 *                                   Use CTabFolder so individual tabs can be
 *                                   close.
 * Dec 06, 2018  7513     randerso   Changed combo box to display on categories
 *                                   currently present in the log.
 *
 * </pre>
 *
 * @author mschenke
 */
public class TabControlDlg extends Dialog {

    /**
     * Class is a singleton, here is the instance
     */
    private static TabControlDlg instance;

    /**
     * The shell for the dialog
     */
    private Shell shell;

    /**
     * The composite that is contained in the dialog
     */
    private Composite mainComp;

    /**
     * The composite that holds the tabFolder and detailsText
     */
    private SashForm topComp;

    /**
     * The TabFolder that is in the composite
     */
    private CTabFolder tabFolder;

    /**
     * Combo box containing the list of current categories in the log
     */
    private Combo catCombo;

    /**
     * Button to show or hide details
     */
    private Button showHide;

    /**
     * Styled text for details
     */
    private StyledText detailsText;

    private static Rectangle bounds;

    private static int[] weights = { 500, 500 };

    private static boolean visible = false;

    /**
     * Get the instance of the TabControl dialog.
     *
     * If parent is null, null will be returned if no instance is open.
     *
     * If parent is non-null, a new dialog will be created if no instance is
     * open.
     *
     * @param parent
     *            Parent shell.
     * @return Instance of this class, may be null.
     */
    public static synchronized TabControlDlg getInstance(Shell parent) {

        if (parent != null && (instance == null || instance.isDisposed())) {
            instance = new TabControlDlg(parent);
        }

        return instance;
    }

    /**
     * Basic constructor
     *
     * @param parent
     *            The parent shell
     */
    private TabControlDlg(Shell parent) {
        super(parent, SWT.TITLE);

        initShell();
    }

    /**
     * Initialize shell data.
     */
    private void initShell() {
        Shell parent = getParent();

        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
        shell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                synchronized (TabControlDlg.class) {
                    instance = null;
                }
            }
        });

        GridLayout mainLayout = new GridLayout(1, false);
        shell.setLayout(mainLayout);
        shell.addListener(SWT.Close, new Listener() {

            @Override
            public void handleEvent(Event event) {
                synchronized (TabControlDlg.class) {
                    if (visible) {
                        int[] currentWeights = topComp.getWeights();
                        weights[0] = currentWeights[0];
                        weights[1] = currentWeights[1];
                    }
                    bounds = shell.getBounds();
                }
            }
        });

        initComponents();
    }

    /**
     * Initialize components in shell.
     */
    private void initComponents() {
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayoutData(gd);

        topComp = new SashForm(mainComp, SWT.HORIZONTAL);
        topComp.setLayout(new GridLayout(2, false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        topComp.setLayoutData(gd);

        tabFolder = new CTabFolder(topComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tabFolder.setLayoutData(gd);

        tabFolder.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = tabFolder.getSelectionIndex();
                if (index >= 0) {
                    tabSelected(tabFolder.getItem(index));
                }
            }
        });

        detailsText = new StyledText(topComp,
                SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        detailsText.setLayoutData(gd);
        detailsText.setEditable(false);

        createBottomButtons();

        topComp.setWeights(weights);

        handleShowHide(visible);
    }

    /**
     * Creates the bottom buttons of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(mainComp, SWT.NONE);
        buttonComp.setLayoutData(gd);
        buttonComp.setLayout(new GridLayout(4, true));

        catCombo = new Combo(buttonComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        catCombo.setLayoutData(gd);
        catCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                Combo combo = (Combo) e.widget;

                if (combo.getItemCount() > 1) {
                    TextMsgLog log = (TextMsgLog) tabFolder.getSelection()
                            .getData();
                    int position = combo.getSelectionIndex();
                    String category = combo.getItem(position);
                    log.setCategoryFilter(category);
                }
            }
        });
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button clearBtn = new Button(buttonComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = tabFolder.getSelectionIndex();
                if (index < 0) {
                    return;
                }
                TextMsgLog log = (TextMsgLog) tabFolder.getItem(index)
                        .getData();
                log.clearMessages();
                updateDetails(log.getDetails());
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        showHide = new Button(buttonComp, SWT.PUSH);
        showHide.setText("Show Details");
        showHide.setLayoutData(gd);
        showHide.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                toggleDetails();
            }
        });
    }

    /**
     * Toggles the details view on/off
     */
    public void toggleDetails() {
        handleShowHide(!visible);
    }

    private void handleShowHide(boolean show) {
        visible = show;
        detailsText.setVisible(visible);
        SashForm sf = topComp;
        if (visible) {
            showHide.setText("Hide Details");
            sf.setWeights(weights);
        } else {
            showHide.setText("Show Details");
            int[] currentWeights = topComp.getWeights();
            weights[0] = currentWeights[0];
            weights[1] = currentWeights[1];
            sf.setWeights(new int[] { 1000, 0 });
        }

        topComp.layout();
        mainComp.layout();
    }

    /**
     * Populates the clear options combo box with values from log
     *
     * @param log
     *            TextMsgLog that is in current tab.
     */
    private void populateCatCombo(TextMsgLog log) {
        List<String> categories = new LinkedList<>(log.getCurrentCategories());
        catCombo.setEnabled(false);

        if (categories.size() > 1) {
            Collections.sort(categories);
            catCombo.setEnabled(true);
        }
        categories.add(0, "All");

        catCombo.removeAll();
        for (String cat : categories) {
            catCombo.add(cat);
        }

        String selection = log.getCategoryFilter();
        int item = catCombo.indexOf(selection);
        if (item >= 0) {
            catCombo.select(item);
        }
    }

    /**
     * Update category combo if the log is displayed in the currently selected
     * tab
     *
     * @param log
     */
    public void updateCatCombo(TextMsgLog log) {
        CTabItem selectedTab = tabFolder.getSelection();

        if (selectedTab.getData() == log) {
            populateCatCombo(log);
        }
    }

    /**
     * Open the dialog.
     */
    public void open() {
        if (isOpened()) {
            bringToTop();
        } else {
            shell.layout();
            shell.pack();

            if (bounds != null) {
                shell.setBounds(bounds);
                shell.setFocus();
            } else {
                setInitialDialogLocation();
            }

            shell.open();

            Display display = shell.getDisplay();
            while (!shell.isDisposed()) {
                if (!display.readAndDispatch()) {
                    display.sleep();
                }
            }
        }
    }

    /**
     * Bring dialog to the top
     */
    public void bringToTop() {
        if (!isDisposed()) {
            shell.setVisible(true);
            shell.forceFocus();
            shell.forceActive();
        }
    }

    /**
     * Sets the initial Location of the dialog, above parent if parent is on
     * bottom half of monitor or below parent if parent is on top half of
     * monitor.
     */
    private void setInitialDialogLocation() {
        Shell parent = getParent().getShell();

        // get any monitors height
        Monitor m = parent.getMonitor();
        Rectangle monSize = m.getClientArea();
        int monY = monSize.height;

        Point p = parent.getLocation();
        Point size = parent.getSize();

        if ((p.y + size.y) > (monY / 2)) {
            Point use = new Point(p.x, p.y - shell.getSize().y);
            shell.setLocation(use);
        } else {
            Point use = new Point(p.x, p.y + size.y);
            shell.setLocation(use);
        }
    }

    /**
     * Check is the dialog is open or not.
     *
     * @return True if shell is not null, not disposed and visible, false
     *         otherwise.
     */
    public boolean isOpened() {
        return !isDisposed() && shell.isVisible();
    }

    /**
     * @param tabText
     * @param log
     * @return the tabItem added
     */
    public Item addTab(String tabText, TextMsgLog log) {
        int index = getTabIndex(log.getIndex());
        CTabItem tabItem = new CTabItem(tabFolder, SWT.CLOSE, index);
        tabItem.setData(log);
        tabItem.setText(tabText);
        tabItem.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (tabFolder.getItemCount() == 0) {
                    close();
                }
            }
        });

        tabItem.setControl(log.createControls(tabFolder));
        tabFolder.setSelection(tabItem);
        tabSelected(tabItem);
        return tabItem;
    }

    /**
     * Removes tab associated with Log
     *
     * @param log
     */
    public void removeTab(TextMsgLog log) {
        for (CTabItem item : tabFolder.getItems()) {
            if (item.getData().equals(log)) {
                item.dispose();
                break;
            }
        }
    }

    private void tabSelected(CTabItem item) {
        TextMsgLog log = (TextMsgLog) item.getData();
        shell.setText("Log list for: " + log.getFullText());
        populateCatCombo(log);
        detailsText.setText(log.getDetails());
    }

    /**
     * Gets the tab index to use for this textMsgLog's index. Say the log is Q3,
     * and Q1 and Q4 are up already, this function would return 1 because it is
     * the index the tab should be inserted at
     *
     * @param textMsgIdx
     *            index of textMsgIdx (1 for Q1, 2 for Q2, etc...)
     * @return index for tab
     */
    private int getTabIndex(int textMsgIdx) {
        int i;
        for (i = 0; i < tabFolder.getItemCount()
                && ((TextMsgLog) tabFolder.getItem(i).getData())
                        .getIndex() < textMsgIdx; ++i) {
        }
        return i;
    }

    /**
     * Update the details text box
     *
     * @param details
     *            details to display
     */
    public void updateDetails(String details) {
        if (details != null) {
            detailsText.setText(details);
        } else {
            detailsText.setText("");
        }
    }

    /**
     * Check if dialog is disposed
     *
     * @return true if disposed, false if not
     */
    private boolean isDisposed() {
        return shell == null || shell.isDisposed();
    }

    /**
     * Close of the dialog
     */
    public static synchronized void close() {
        if (instance != null && !instance.shell.isDisposed()) {
            instance.shell.close();
            instance = null;
        }
    }
}
