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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;

/**
 * Tab control dialog that displays the messages for the different mode layouts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2008                    Max S.      Initially create by Max Schenkelberg
 * Apr 2, 2009             lvenable    TTR fixes.
 * Dec 1, 2010  5632       cjeanbap    Added sort based on category.
 * Mar 2, 2011  5632       cjeanbap    Added sort based on category.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
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
    private TabFolder tabFolder;

    /**
     * Combo Box that gets updated depending on Tab Selected
     */
    private Combo clearOptionCbo;

    /**
     * Button to show or hide details
     */
    private Button showHide;

    /**
     * List of textMsgLogs
     */
    private List<TextMsgLog> logs;

    /**
     * Styled text for details
     */
    private StyledText detailsText;

    private static int[] weights = { 50, 50 };

    /**
     * Get the instance of the TabControl dialog
     * 
     * @param parent
     *            Parent shell.
     * @return Instance of this class.
     */
    public static TabControlDlg getInstance(Shell parent) {

        if (instance == null || instance.isDisposed()) {
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

        logs = new ArrayList<TextMsgLog>();

        initShell();
    }

    /**
     * Initialize shell data.
     */
    private void initShell() {
        Shell parent = getParent();

        shell = new Shell(parent, SWT.TITLE | SWT.RESIZE);

        GridLayout mainLayout = new GridLayout(1, false);
        shell.setLayout(mainLayout);

        initComponents();
    }

    /**
     * Initialize components in shell.
     */
    private void initComponents() {
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 800;
        mainComp.setLayoutData(gd);

        topComp = new SashForm(mainComp, SWT.HORIZONTAL);
        topComp.setLayout(new GridLayout(2, false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 285;
        topComp.setLayoutData(gd);

        tabFolder = new TabFolder(topComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tabFolder.setLayoutData(gd);

        tabFolder.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                logs.clear();
            }
        });

        tabFolder.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = tabFolder.getSelectionIndex();
                if (index < 0 || logs.size() == 0)
                    return;
                TextMsgLog log = logs.get(index);
                shell.setText("Log list for: " + log.getFullText());
                populateClearOptionsCombo(log);
                detailsText.setText(log.getLogText());
                clearOptionCbo.select(logs.get(index)
                        .getClearOptionCboSelectedIndex());
            }
        });

        detailsText = new StyledText(topComp, SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 400;
        gd.heightHint = 285;
        detailsText.setLayoutData(gd);
        detailsText.setEditable(false);

        detailsText.setVisible(false);
        ((GridData) detailsText.getLayoutData()).exclude = true;

        createBottomButtons();
    }

    /**
     * Creates the bottom buttons of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(mainComp, SWT.NONE);
        buttonComp.setLayoutData(gd);
        buttonComp.setLayout(new GridLayout(4, false));

        clearOptionCbo = new Combo(buttonComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(100, SWT.DEFAULT);
        clearOptionCbo.setLayoutData(gd);
        clearOptionCbo.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = tabFolder.getSelectionIndex();
                if (index < 0) {
                    return;
                }
                if (clearOptionCbo.getItemCount() >= 2) {
                    int position = clearOptionCbo.getSelectionIndex();
                    String category = clearOptionCbo.getItem(position);
                    logs.get(index).displayCategoryMessages(category);
                    if (index == 0) {
                        logs.get(index).populateClearOptionsCombo();
                        clearOptionCbo.select(position);
                    }
                    logs.get(index).setClearOptionCboSelectedIndex(position);
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
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
                logs.get(index).clearMessages();
                updateDetails("");
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dispose();
            }
        });

        gd = new GridData(SWT.END, SWT.DEFAULT, false, false);
        showHide = new Button(buttonComp, SWT.PUSH);
        showHide.setText("Show Details...");
        showHide.setLayoutData(gd);
        // TODO: Make this work, right now not working
        showHide.addSelectionListener(new SelectionAdapter() {
            boolean visible = false;

            @Override
            public void widgetSelected(SelectionEvent e) {
                visible = !visible;
                if (visible == true) {
                    showHide.setText("Hide Details...");
                } else {
                    showHide.setText("Show Details");
                }
                detailsText.setVisible(visible);

                SashForm sf = (SashForm) topComp;
                if (!visible) {
                    cacheCurrentWeights();
                    sf.setWeights(new int[] { 100, 0 });
                } else {
                    sf.setWeights(weights);
                }
                topComp.layout();
                mainComp.layout();
            }
        });
    }

    private void cacheCurrentWeights() {
        int[] currentWeights = topComp.getWeights();
        weights[0] = currentWeights[0];
        weights[1] = currentWeights[1];
    }

    /**
     * Populates the clear options combo box with values of current LogDlg tab
     * being displayed, called when tabitem has changed
     * 
     * @param dlg
     *            LogDlg that is in current tab.
     */
    public void populateClearOptionsCombo(TextMsgLog log) {
        clearOptionCbo.removeAll();

        clearOptionCbo.add("All");

        Set<String> keySet = log.getCatKeySet();

        for (String key : keySet) {
            clearOptionCbo.add(key);
        }

        clearOptionCbo.select(0);
        log.setClearOptionCbo(clearOptionCbo);
    }

    /**
     * Open the dialog.
     */
    public void open() {
        Display display = shell.getDisplay();

        shell.layout();
        shell.pack();

        setInitialDialogLocation();

        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
    }

    /**
     * Sets the initial Location of the dialog, above parent if parent is on
     * bottom half of monitor or below parent if parent is on top half of
     * monitor.
     */
    private void setInitialDialogLocation() {
        Shell parent = getParent().getShell();
        Display display = parent.getDisplay();

        // get any monitors height
        Monitor any = (display.getMonitors())[0];
        Rectangle monSize = any.getBounds();
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
        if (shell == null || shell.isDisposed() || shell.isVisible() == false) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Return the tab folder that is contained in the shell, this is used if a
     * new TabItem is to be added to the dialog.
     * 
     * @return The tab folder.
     */
    public TabFolder getTabFolder() {
        return this.tabFolder;
    }

    /**
     * Notify the TabControlDlg when a new tab has been added. TODO: Replace
     * with event handler?
     * 
     * @param log
     *            The log that is the new tab.
     */
    public void addedTab(TextMsgLog log) {
        if (logs.contains(log) == false) {
            logs.add(this.getTabIndex(log.getIndex()), log);
            populateClearOptionsCombo(log);
        }
    }

    /**
     * Removes tab associated with Log
     * 
     * @param log
     */
    public void removeTab(TextMsgLog log) {
        logs.remove(log);
        log.disposeDialog();

        if (logs.size() == 0) {
            dispose();
        }
    }

    /**
     * Removes the currently selected tab
     */
    public void removeTab() {
        removeTab(logs.get(tabFolder.getSelectionIndex()));
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
    public int getTabIndex(int textMsgIdx) {
        int i;
        for (i = 0; i < logs.size() && logs.get(i).getIndex() < textMsgIdx; ++i) {
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
    public boolean isDisposed() {
        return shell.isDisposed();
    }

    /**
     * Dispose of the dialog
     */
    public static void dispose() {
        if (instance != null && instance.shell.isDisposed() == false) {
            instance.shell.close();
            instance = null;
        }
    }
}
