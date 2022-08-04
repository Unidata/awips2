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

import com.raytheon.uf.common.util.StringUtil;
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
 * 31 May 2015  4501       skorolev    Got rid of Vector.
 * 13 Dec 2017  6778       mduff       Fixed the output of the group data.
 * 12 Mar 2018  DCS18260   astrakovsky Moved retrieveAndDisplayGroupData and formatGroupData
 *                                     methods to XdatDlg to avoid repeating code.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */

public class GroupDataDlg extends CaveSWTDialog {
    private static final String ID_DES_FORMAT = "\t\t\t %-10S  %S";

    private static final String DATA_LINE_FORMAT = "%-9S  %2S  %4S %2S  %1S   %19S   %12S %6S   %7S";

    private static final String HEADER_LINE = "  ID       PE  DUR  TS  E       OBSTIME               PRODUCT   VALUE    CHANGE";

    private static final String DASHED_LINE = "-------------------------------------------------------------------------------";

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Group list control.
     */
    private List groupList;

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
    public GroupDataDlg(Shell parentShell, ITextDisplay displayCB) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("Group Data");

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
        java.util.List<String> groupNamesList = readgroups.getGroups();
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
                displayCB.retrieveAndDisplayGroupData(groupList.getItem(groupList.getSelectionIndex()));
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
                close();
            }
        });
    }

}
