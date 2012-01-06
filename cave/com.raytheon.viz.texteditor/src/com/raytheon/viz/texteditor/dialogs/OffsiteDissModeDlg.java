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
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Offset mode dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 9/13/07      368         lvenable    Initial creation.
 * 10/11/2007   482         grichard    Reformatted file.
 * 
 * </pre>
 * 
 * @author lvenable
 */
public class OffsiteDissModeDlg extends CaveSWTDialog {

    /**
     * Text window id.
     */
    private String textWindowId;

    /**
     * Operational radio button.
     */
    private Button operationalRdo;

    /**
     * Non-operational radio button.
     */
    private Button nonOperationalRdo;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param textWindowId
     *            Text window ID.
     */
    public OffsiteDissModeDlg(Shell parent, String textWindowId) {
        super(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.PERSPECTIVE_INDEPENDENT);
        setText(this.textWindowId
                + ": Text Product Offsite Dissemination Operational Mode");

        this.textWindowId = textWindowId;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        createInformationLabels();
        createOptionControls();
        createBottomButtons();

        shell.setSize(600, 185);
    }

    /**
     * Create the information labels.
     */
    private void createInformationLabels() {

        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        labelComp.setLayout(gridLayout);

        Label noteLbl = new Label(labelComp, SWT.NONE);
        noteLbl.setText("PLEASE NOTE:");

        Label noteInfoLbl = new Label(labelComp, SWT.NONE);
        noteInfoLbl
                .setText("The TextWS MHS system is used to disseminate official "
                        + "forecast products offsite.");
    }

    /**
     * Create the option radio button controls.
     */
    private void createOptionControls() {
        Group controlsGroup = new Group(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, true);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        controlsGroup.setLayout(gridLayout);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        controlsGroup.setLayoutData(gd);
        controlsGroup.setText(" Choose an appropriate operational mode for "
                + textWindowId + ": ");

        // Create the operation radio button.
        gd = new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
        gd.grabExcessHorizontalSpace = true;
        operationalRdo = new Button(controlsGroup, SWT.RADIO);
        operationalRdo.setText("operational");
        operationalRdo.setLayoutData(gd);

        // Create the non-operational radio button.
        gd = new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
        gd.grabExcessHorizontalSpace = true;
        nonOperationalRdo = new Button(controlsGroup, SWT.RADIO);
        nonOperationalRdo.setText("non-operational");
        nonOperationalRdo.setSelection(true);
        nonOperationalRdo.setLayoutData(gd);
    }

    /**
     * Create the bottom buttons.
     */
    private void createBottomButtons() {
        // Create a composite that will center added controls/composites.
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));
        buttonArea.setLayout(new GridLayout(1, false));

        // Create a composite to hold the enter and cancel buttons.
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayout(new GridLayout(2, true));

        // Create the OK button.
        Button okBtn = new Button(buttons, SWT.PUSH);
        okBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        okBtn.setText("OK");
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(true);
                shell.dispose();
            }
        });

        // Create the OK button.
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                shell.dispose();
            }
        });
    }
}
