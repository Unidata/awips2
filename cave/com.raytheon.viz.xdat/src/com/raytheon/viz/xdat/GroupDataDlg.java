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
package com.raytheon.viz.xdat;

import java.io.File;
import java.util.ArrayList;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Group Data dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10 Nov 2008             lvenable    Initial creation.
 * 10 Feb 2009             wkwock      Added functions.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */

public class GroupDataDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Group list control.
     */
    private List groupList;

    /**
     * Database manager class.
     */
    private XdatDB databaseMgr;

    /**
     * Callback interface used to display the data on the screen.
     */
    private ITextDisplay displayCB;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param database
     *            Database manager
     * @param displayCB
     *            Display callback interface.
     */
    public GroupDataDlg(Shell parentShell, XdatDB database,
            ITextDisplay displayCB) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("Group Data");

        this.databaseMgr = database;
        this.displayCB = displayCB;
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createGroupListControl();

        createCloseButton();

        ReadGroupList readgroups = new ReadGroupList();
        ArrayList<String> groupNamesList = readgroups.getGroups();
        for (int i = 0; i < groupNamesList.size(); i++) {
            groupList.add(groupNamesList.get(i));
        }
    }

    /**
     * Create the group list control.
     */
    private void createGroupListControl() {
        Label groupLbl = new Label(shell, SWT.NONE);
        groupLbl.setText("Select a Group:");

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 250;
        gd.heightHint = 300;
        groupList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        groupList.setLayoutData(gd);
        groupList.setFont(controlFont);
        groupList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                retrieveAndDisplayGroupData();
            }
        });
    }

    /**
     * Create the close button at the bottom of the dialog.
     */
    private void createCloseButton() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Retrieve and display the group data.
     */
    private void retrieveAndDisplayGroupData() {
        String selectedGroup = groupList.getItem(groupList.getSelectionIndex());
        
        String groupsDir = AppsDefaults.getInstance().getToken("xdat_groups_dir");

        File file = new File(groupsDir + File.separator + selectedGroup);

        ReadIDsList IDsList = new ReadIDsList(file);
        ArrayList<String[]> idList = IDsList.getIDsList();

        StringBuilder strBld = new StringBuilder();
        for (int i = 0; i < idList.size(); i++) {

            Vector<String[]> results = databaseMgr.getGroupData(idList.get(i),
                    displayCB.getStartDate(), displayCB.getEndDate());

            if (results == null) {
                return;
            }

            // Format the group data and display the data on the screen.
            strBld.append(formatGroupData(results));
            strBld.append("\n\n");
        }
        displayCB.setDisplayText(strBld.toString());
    }

    /**
     * Format the group data.
     * 
     * @param dataArray
     *            Array of data to be formatted for the display.
     * @return StringBuilder class containing the formatted data.
     */
    private StringBuilder formatGroupData(Vector<String[]> dataArray) {

        StringBuilder strBld = new StringBuilder();

        boolean dataLine = false;

        String idDesFmt = "\t\t\t %-10S  %S";

        String dataLineFmt = "%-9S  %2S  %4S %2S  %1S   %19S   %12S %6S   %7S";

        String hdr = "  ID       PE  DUR  TS  E       OBSTIME               PRODUCT   VALUE    CHANGE";
        String dashLine = "-------------------------------------------------------------------------------";

        for (String[] rowData : dataArray) {

            if (rowData.length == 2) {

                if (dataLine == true) {
                    dataLine = false;
                    strBld.append(hdr).append("\n");
                }

                strBld.append(String.format(idDesFmt, rowData[0], rowData[1]))
                        .append("\n\n");
            } else {
                if (dataLine == false) {
                    strBld.append(hdr).append("\n");
                    strBld.append(dashLine).append("\n");
                }

                dataLine = true;

                strBld.append(
                        String.format(dataLineFmt, rowData[0], rowData[1],
                                rowData[2], rowData[3], rowData[4], rowData[5],
                                rowData[6], rowData[7], rowData[8])).append(
                        "\n");
            }
        }

        return strBld;
    }
}
