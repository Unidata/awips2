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
package com.raytheon.viz.hydrobase.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.hydrocommon.data.LocationData;
import com.raytheon.viz.hydrocommon.datamanager.AddModifyLocationDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Copy New Location dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 05 Dec 2008  1744       askripsky   Add database functionality.
 * 27 Oct 2010  5519       Judy Wang   Converted lower case to upper
 *                                     case in Destination box.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CopyNewLocationDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CopyNewLocationDlg.class);

    /**
     * Source text control.
     */
    private Text sourceTF;

    /**
     * Destination text control.
     */
    private Text destinationTF;

    /**
     * All data radio button.
     */
    private Button allDataRdo;

    /**
     * Reference data only radio button.
     */
    private Button refDataOnlyRdo;

    /**
     * Holds the source location data
     */
    private LocationData sourceData;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public CopyNewLocationDlg(Shell parent, LocationData sourceData) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Copy to New Location");

        this.sourceData = sourceData;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Initialize all of the controls and layouts
        initializeComponents();

        initializeData();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createSourceDescControls();

        createDataIncludeGroup();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();
    }

    /**
     * Create the source and description controls.
     */
    private void createSourceDescControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label sourceLbl = new Label(controlComp, SWT.RIGHT);
        sourceLbl.setText("Source: ");
        sourceLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        sourceTF = new Text(controlComp, SWT.BORDER);
        sourceTF.setLayoutData(gd);
        sourceTF.setEditable(false);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = 120;
        Label destLbl = new Label(controlComp, SWT.RIGHT);
        destLbl.setText("Destination: ");
        destLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        destinationTF = new Text(controlComp, SWT.BORDER);
        destinationTF.setLayoutData(gd);
        destinationTF.setFocus();
        /* Only allow 8 characters to be entered */
        destinationTF.setTextLimit(8);
        destinationTF.addListener(SWT.Verify, new Listener() {
            public void handleEvent(Event e) {
                String newStr = e.text;
                char[] newChars = new char[newStr.length()];
                newStr.getChars(0, newChars.length, newChars, 0);
                for (int i = 0; i < newChars.length; i++) {
                    if (!('0' <= newChars[i] && newChars[i] <= '9')
                            && !('a' <= newChars[i] && newChars[i] <= 'z')
                            && !('A' <= newChars[i] && newChars[i] <= 'Z')) {
                        e.doit = false;
                    }
                }
                e.text = e.text.toUpperCase();
            }
        });
    }

    /**
     * Create the data to include group and controls.
     */
    private void createDataIncludeGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group dataIncludeGroup = new Group(shell, SWT.NONE);
        dataIncludeGroup.setLayout(new GridLayout(1, false));
        dataIncludeGroup.setLayoutData(gd);
        dataIncludeGroup.setText(" Data to Include ");

        allDataRdo = new Button(dataIncludeGroup, SWT.RADIO);
        allDataRdo.setText("All Data");
        allDataRdo.setSelection(true);

        refDataOnlyRdo = new Button(dataIncludeGroup, SWT.RADIO);
        refDataOnlyRdo.setText("Reference Data ONLY");
    }

    /**
     * Create buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button copyBtn = new Button(buttonComp, SWT.PUSH);
        copyBtn.setText("Copy");
        copyBtn.setLayoutData(gd);
        copyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (copyLocation()) {
                    close();
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Populate dialog with information in source data.
     */
    private void initializeData() {
        sourceTF.setText(sourceData.getLid());
    }

    /**
     * Calls the actual copy methods based on the user's selections.
     * 
     * @return True if the data was copied successfully, otherwise False
     */
    private boolean copyLocation() {
        boolean successfulCopy = false;

        String destinationLid = destinationTF.getText();

        if (!destinationLid.equals("")) {
            try {
                if (allDataRdo.getSelection()) {
                    AddModifyLocationDataManager.getInstance()
                            .CopyAllDataForLid(sourceData, destinationLid);
                } else if (refDataOnlyRdo.getSelection()) {
                    AddModifyLocationDataManager
                            .getInstance()
                            .CopyReferenceDataForLid(sourceData, destinationLid);
                }
                successfulCopy = true;
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Copy Failure", e);
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Name");
            mb.setMessage("Please enter a destination location name");
            mb.open();
        }

        return successfulCopy;
    }
}
