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
import java.util.List;

import org.eclipse.swt.SWT;
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
 * Aug 30, 2013   2314      mpduff     Fixed find, filter, and various other bugs.
 * Sep 26, 2013   2417      mpduff     Reset the highlight all indices on close.
 * Feb 07, 2014   2453      mpduff     Refactored dialog.
 * Mar 18, 2014   2433      mpduff     Update javadoc.
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

    /** Table row selected Index */
    private int selectedIndex = 0;

    /** Message Checkbox flag */
    private boolean msgFlag = false;

    /** Category Checkbox flag */
    private boolean categoryFlag = false;

    /** Case Sensitive flag */
    private boolean caseFlag = false;

    /** Exclude search flag */
    private boolean excludeFlag = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param filteredTableList
     *            Table data containing the text to be searched.
     * @param selected
     *            Selected table index
     * @param callback
     *            ITableFind callback
     */
    public FindDlg(Shell parent,
            TableDataManager<NotificationRowData> filteredTableList,
            int selected, ITableFind callback) {
        super(parent, SWT.DIALOG_TRIM, CAVE.NONE | CAVE.DO_NOT_BLOCK);
        this.setText("Find");

        this.filteredTableList = filteredTableList;
        selectedIndex = selected;
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
        highlightBtn
                .setToolTipText("Highlight all rows matching search criteria");
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
     * Find Button action handler. Find the next matching row upon button click.
     */
    private void handleFindBtn() {
        int prevSelectedIndex = selectedIndex;

        // Text in the find text box
        String text = findTxt.getText();

        // If no text is entered
        if (text.isEmpty()) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Find Warning");
            mb.setMessage("Please enter text in the 'Find' field.");
            mb.open();
            return;
        }

        // Get button selections
        msgFlag = msgBtn.getSelection();
        categoryFlag = categoryBtn.getSelection();
        caseFlag = caseBtn.getSelection();
        excludeFlag = exclusionBtn.getSelection();

        int itemCount = filteredTableList.getDataArray().size();
        boolean continueSearch = true;
        boolean exists = false;
        boolean hitEnd = false;
        selectedIndex = callback.getCurrentSelectionIndex() + 1;

        while (continueSearch) {
            if (selectedIndex < itemCount) {
                NotificationRowData row = filteredTableList.getDataArray().get(
                        selectedIndex);

                boolean matchFound = checkForMatch(text, row);
                if (matchFound) {
                    continueSearch = false;
                    exists = true;
                    callback.selectRow(row);
                }
                selectedIndex++;
            } else {
                if (!hitEnd) {
                    int answer = DataDeliveryUtils
                            .showMessage(
                                    getShell(),
                                    SWT.YES | SWT.NO,
                                    "Search from Beginning",
                                    "The end of the table has been reached.  Would you like to search from the beginning of the table?");
                    if (answer == SWT.NO) {
                        exists = true;
                        selectedIndex = prevSelectedIndex;
                        break;
                    } else if (answer == SWT.YES) {
                        // Start search over at beginning of table
                        selectedIndex = 0;
                        continueSearch = true;
                    }
                    hitEnd = true;
                } else {
                    break;
                }
            }
        }

        // No items matching search criteria
        if (!exists) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Find Warning");
            mb.setMessage("No item matching your search was found.");
            mb.open();
            selectedIndex = prevSelectedIndex;
            callback.clearSelections();
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

        List<NotificationRowData> items = new ArrayList<NotificationRowData>();

        if (filteredTableList != null) {
            for (int i = 0; i < filteredTableList.getSize(); i++) {
                NotificationRowData row = filteredTableList.getDataArray().get(
                        i);

                boolean matchFound = checkForMatch(text, row);
                if (matchFound) {
                    items.add(row);
                }
            }

            callback.selectRows(items);
        }
    }

    /**
     * Check if the matchText matches the row.
     * 
     * @param matchText
     *            The text to match
     * @param row
     *            The row to check
     * @return true if matches
     */
    private boolean checkForMatch(String matchText, NotificationRowData row) {
        boolean matchFound = false;
        String msg = row.getMessage();
        String sub = row.getCategory();

        if (!caseFlag) {
            msg = msg.toUpperCase();
            sub = sub.toUpperCase();
            matchText = matchText.toUpperCase();
        }

        if (excludeFlag) {
            if ((!msg.contains(matchText) && msgFlag)
                    || (!sub.contains(matchText) && categoryFlag)) {
                matchFound = true;
            }
        } else {
            if ((msg.contains(matchText) && msgFlag)
                    || (sub.contains(matchText) && categoryFlag)) {
                matchFound = true;
            }
        }
        return matchFound;
    }
}