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
package com.raytheon.uf.viz.datadelivery.notification;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.datadelivery.common.ui.ITableFind;
import com.raytheon.uf.viz.datadelivery.common.ui.TableDataManager;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Find dialog for the Notification Table.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 01, 2012    452      jpiatt     Initial creation.
 * Jun  1, 2012    645      jpiatt     Added tooltips.
 * Jun 07, 2012    687      lvenable   Table data refactor.
 * Dec 12. 2012   1418      mpduff     Change label.
 *
 * </pre>
 *
 * @author jpiatt
 * @version 1.0
 */

public class FindDlg extends CaveSWTDialog {

    /** Filename text field */
    private Text findTxt;

    /** Case sensitive check */
    private Button caseBtn;

    /** Exclusion check */
    private Button exclusionBtn;

    /** Message column check */
    private Button msgBtn;

    /** Category column check */
    private Button categoryBtn;

    /** Highlight button */
    private Button highlightBtn;

    /** Find button */
    private Button findBtn;

    /** NotificationTableData object */
    private final TableDataManager<NotificationRowData> filteredTableList;

    /** ITableFind callback */
    private final ITableFind callback;

    /** Table row Index */
    int tableIndex = -1;

    /** Table row start Index */
    int startIndex = 0;

    /** Table row end Index */
    int endIndex = 0;

    /** Table row selected Index */
    int selectedIndex = 0;

    /** Message Checkbox flag */
    boolean msgFlag = false;

    /** Category Checkbox flag */
    boolean categoryFlag = false;

    /** Case Sensitive flag */
    boolean caseFlag = false;

    /** Yes continue search flag */
    boolean yesFlag = false;

    /** Found Item flag */
    boolean exists = false;

    /** Exclude search flag */
    boolean excludeFlag = false;

    /** Message string */
    String msg = null;

    /** Subscription string */
    String sub = null;

    /**
     * Constructor.
     *
     * @param parent
     *            The parent shell
     * @param filteredTableList
     *            Table data containing the text to be searched.
     * @param sIndex
     *            Start table index
     * @param eIndex
     *            End table index
     * @param selected
     *            Selected table index
     * @param callback
     *            ITableFind callback
     *
     */
    public FindDlg(Shell parent, TableDataManager<NotificationRowData> filteredTableList, int sIndex, int eIndex,
            int selected, ITableFind callback) {
        super(parent, SWT.DIALOG_TRIM, CAVE.NONE);
        this.setText("Find");

        this.filteredTableList = filteredTableList;
        sIndex = startIndex;
        eIndex = endIndex;
        selected = selectedIndex;
        this.callback = callback;

    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;

        return mainLayout;
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

        createFindLayout();
        createBottomButtons();

    }

    /**
     * Create the Find Dialog pop up.
     */
    private void createFindLayout() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 275;
        GridLayout gl = new GridLayout(2, false);

        // Main Composite
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        // Find label & text box
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Label lbl = new Label(mainComp, SWT.LEFT);
        lbl.setText("Find: ");
        lbl.setToolTipText("Enter text for search");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        findTxt = new Text(mainComp, SWT.BORDER);
        findTxt.setLayoutData(gd);
        findTxt.selectAll();
        findTxt.setLayoutData(gd);

        findTxt.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                // handleFocusLost(e);
            }
        });
        findTxt.addKeyListener(new KeyListener() {
            @Override
            // change
            public void keyReleased(KeyEvent e) {

                findText();
            }

            @Override
            public void keyPressed(KeyEvent e) {
                // Not Called

            }
        });

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite checkComp = new Composite(shell, SWT.NONE);
        checkComp.setLayout(gl);
        checkComp.setLayoutData(gd);

        // Case Sensitive check box
        caseBtn = new Button(checkComp, SWT.CHECK);
        caseBtn.setText("Case Sensitive");
        caseBtn.setToolTipText("Rows matching upper and lower case");

        // Exclude check box
        exclusionBtn = new Button(checkComp, SWT.CHECK);
        exclusionBtn.setText("Exclude");
        exclusionBtn.setToolTipText("Rows not containing search text");

        // Column selection group
        Group columnGroup = new Group(shell, SWT.NONE);

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        columnGroup.setLayout(gl);
        columnGroup.setLayoutData(gd);
        columnGroup.setText(" Column Selection ");

        Composite columnComp = new Composite(columnGroup, SWT.NONE);
        columnComp.setLayout(gl);
        columnComp.setLayoutData(gd);

        // Message radio button
        msgBtn = new Button(columnComp, SWT.RADIO);
        msgBtn.setText("Message");
        msgBtn.setSelection(true);
        msgBtn.setToolTipText("Search Message column");

        // Subscription radio button
        categoryBtn = new Button(columnComp, SWT.RADIO);
        categoryBtn.setText("Category");
        categoryBtn.setToolTipText("Search Category column");

    }

    /**
     * Create the bottom buttons on the Find Dialog pop up.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);

        Composite bottomComp = new Composite(shell, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        // Find Next Button
        int btnWidth = 115;
        GridData btnData = new GridData(btnWidth, SWT.DEFAULT);
        findBtn = new Button(bottomComp, SWT.PUSH);
        findBtn.setText("Find Next");
        findBtn.setLayoutData(btnData);
        findBtn.setToolTipText("Find next row matching search criteria");
        findBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleFindBtn();
            }
        });

        // Highlight All button
        btnData = new GridData(btnWidth, SWT.DEFAULT);
        highlightBtn = new Button(bottomComp, SWT.PUSH);
        highlightBtn.setText("Highlight All");
        highlightBtn.setLayoutData(btnData);
        highlightBtn.setToolTipText("Highlight all rows matching search criteria");
        highlightBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleHighlightBtn();
            }

        });

        // Close Button
        btnData = new GridData(btnWidth, SWT.DEFAULT);
        Button closeBtn = new Button(bottomComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(btnData);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

    }

    /**
     * Find text on key release. Check all pages of filtered list.
     */
    private void findText() {

        // Text in the find text box
        String text = findTxt.getText();

        // Get button selections
        msgFlag = msgBtn.getSelection();
        categoryFlag = categoryBtn.getSelection();
        caseFlag = caseBtn.getSelection();
        excludeFlag = exclusionBtn.getSelection();

        // Set to true if item exists
        exists = false;

        int itemCount = filteredTableList.getDataArray().size();
        tableIndex = 0;

        if (filteredTableList != null) {

            // Check rows in the entire filtered list - all pages
            for (NotificationRowData row : filteredTableList.getDataArray()) {

                // Column data
                msg = row.getMessage();
                sub = row.getCategory();

                if (tableIndex <= itemCount) {

                    tableIndex++;

                    if (caseFlag) {
                        if (excludeFlag) {
                            // Select index if does not match message or
                            // subscription
                            if ((!msg.contains(text) && msgFlag) || (!sub.contains(text) && categoryFlag)) {
                                exists = true;
                                callback.selectIndex(tableIndex);
                                break;
                            }
                            // Select index if matches message or subscription
                        }
                        else if ((msg.contains(text) && msgFlag) || (sub.contains(text) && categoryFlag)) {
                            exists = true;
                            callback.selectIndex(tableIndex);
                            break;
                        }

                    }
                    else {
                        if (excludeFlag) {
                            // Select index if matches non case sensitive
                            // message or subscription
                            if ((!msg.toUpperCase().contains(text.toUpperCase()) && msgFlag)
                                    || (!sub.toLowerCase().contains(text.toLowerCase()) && categoryFlag)) {
                                exists = true;
                                callback.selectIndex(tableIndex);
                                break;
                            }
                        }
                        else if ((msg.toUpperCase().contains(text.toUpperCase()) && msgFlag)
                                || (sub.toLowerCase().contains(text.toLowerCase()) && categoryFlag)) {
                            exists = true;
                            callback.selectIndex(tableIndex);
                            break;
                        }
                    }
                }

            }
        }
    }

    /**
     * Find Button action handler. Find the next matching row upon button click.
     */
    private void handleFindBtn() {

        // Text in the find text box
        String text = findTxt.getText();

        // Get button selections
        msgFlag = msgBtn.getSelection();
        categoryFlag = categoryBtn.getSelection();
        caseFlag = caseBtn.getSelection();
        excludeFlag = exclusionBtn.getSelection();

        int itemCount = filteredTableList.getDataArray().size();
        int findTextLength = findTxt.getText().length();

        boolean search = false;
        selectedIndex = selectedIndex + 1;

        // If no text is entered
        if (findTextLength == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Find Warning");
            mb.setMessage("Please enter text in the 'Find' field.");
            mb.open();
            return;
        }

        while (search == false) {

            if (tableIndex < itemCount) {

                // Get the row data starting at the currently highlighted row
                NotificationRowData row = filteredTableList.getDataArray().get(tableIndex);

                // Column data
                msg = row.getMessage();
                sub = row.getCategory();

                if (tableIndex < itemCount) {
                    tableIndex++;
                }

                if (caseFlag) {
                    if (excludeFlag) {
                        // Select index if does not match message or
                        // subscription
                        if ((!msg.contains(text) && msgFlag) || (!sub.contains(text) && categoryFlag)) {
                            search = true;
                            callback.selectIndex(tableIndex);
                        }
                    }
                    else if ((msg.contains(text) && msgFlag) || (sub.contains(text) && categoryFlag)) {
                        // Select index if matches message or subscription
                        search = true;
                        callback.selectIndex(tableIndex);
                    }

                }
                else {
                    if (excludeFlag) {
                        // Select index if matches non case sensitive message or
                        // subscription
                        if ((!msg.toUpperCase().contains(text.toUpperCase()) && msgFlag)
                                || (!sub.toLowerCase().contains(text.toLowerCase()) && categoryFlag)) {
                            search = true;
                            callback.selectIndex(tableIndex);
                        }
                    }
                    else if ((msg.toUpperCase().contains(text.toUpperCase()) && msgFlag)
                            || (sub.toLowerCase().contains(text.toLowerCase()) && categoryFlag)) {
                        search = true;
                        callback.selectIndex(tableIndex);
                    }
                }

                // If the item was found set exists to true
                if (search) {
                    exists = true;
                }

            }
            else {

                int answer = DataDeliveryUtils
                        .showMessage(getShell(), SWT.YES | SWT.NO, "Search from Beginning",
                                "The end of the table has been reached.  Would you like to search from the beginning of the table?");
                if (answer == SWT.NO || (answer == SWT.YES && !exists)) {
                    break;
                    // Start search over at beginning of table
                }
                else if (answer == SWT.YES && exists) {
                    tableIndex = 0;
                    search = false;
                }

            }

        }

        // No items matching search criteria
        if (!exists) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Find Warning");
            mb.setMessage("No item matching your search was found.");
            mb.open();
        }

    }

    /**
     * Highlight Button action handler. Highlight all rows in table matching
     * text.
     */
    private void handleHighlightBtn() {

        // Text in the find text box
        String text = findTxt.getText();

        // Get button selections
        msgFlag = msgBtn.getSelection();
        categoryFlag = categoryBtn.getSelection();
        caseFlag = caseBtn.getSelection();
        excludeFlag = exclusionBtn.getSelection();

        ArrayList<Integer> items = new ArrayList<Integer>();
        int[] indices = new int[0];

        // Start search at beginning of table
        tableIndex = 0;

        if (filteredTableList != null) {

            for (int i = 0; i < filteredTableList.getDataArray().size(); i++) {

                NotificationRowData row = filteredTableList.getDataArray().get(i);
                // Message Column
                msg = row.getMessage();
                // Subscription Name column
                sub = row.getCategory();

                if (caseFlag) {
                    if (excludeFlag) {
                        // Select index if does not match message or
                        // subscription
                        if ((!msg.contains(text) && msgFlag) || (!sub.contains(text) && categoryFlag)) {
                            items.add(i);
                        }
                    }
                    else if ((msg.contains(text) && msgFlag) || (sub.contains(text) && categoryFlag)) {
                        // Select index if matches message or subscription
                        items.add(i);
                    }

                }
                else {
                    if (excludeFlag) {
                        // Select index if matches non case sensitive message or
                        // subscription
                        if ((!msg.toUpperCase().contains(text.toUpperCase()) && msgFlag)
                                || (!sub.toLowerCase().contains(text.toLowerCase()) && categoryFlag)) {
                            items.add(i);
                        }
                    } else if ((msg.toUpperCase().contains(text.toUpperCase()) && msgFlag)
                            || (sub.toLowerCase().contains(text.toLowerCase()) && categoryFlag)) {
                        items.add(i);
                    }
                }

                tableIndex++;

            }

            indices = new int[items.size()];

            // Create an int array of the rows to highlight
            for (int i = 0; i < items.size(); i++) {
                indices[i] = items.get(i);
            }

            callback.selectIndices(indices);

        }

    }

}
