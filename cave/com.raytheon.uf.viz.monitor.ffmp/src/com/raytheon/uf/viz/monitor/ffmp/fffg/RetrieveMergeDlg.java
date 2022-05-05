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
package com.raytheon.uf.viz.monitor.ffmp.fffg;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog used to bring data in for the FFFG dialog. There are 3 options:
 * 
 * Retrieve - Clear current data entirely and load data from the selected file.
 * 
 * Merge - Retain current data and load data from the selected file. Current
 * data will NOT be overwritten by incoming data when the two conflict.
 * 
 * Merge/Overwrite - Retain current data and load data from the selected file.
 * Incoming data will overwrite current data when the two conflict.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2010            lvenable     Initial creation
 * Nov 29, 2012 1353       rferrel     Convert to CavSWTDialog
 *                                      and make non-blocking
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class RetrieveMergeDlg extends CaveSWTDialog {

    /**
     * Enumeration of the retrieve options.
     */
    public static enum RetrieveMergeAction {
        RETRIEVE("Retrieve"), MERGE_OVERWRITE("Merge/Overwrite"), MERGE("Merge"), CANCEL(
                "Cancel");

        private String actionName;

        RetrieveMergeAction(String name) {
            actionName = name;
        }

        public String getActionName() {
            return actionName;
        }
    }

    /**
     * Retrieve radio button.
     */
    private Button retrieveRdo;

    /**
     * Merge/Overwrite button.
     */
    private Button mergeOvrWriteRdo;

    /**
     * Merge button.
     */
    private Button mergeRdo;

    /**
     * The selected button.
     */
    private Button selectedRdo;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent control.
     */
    public RetrieveMergeDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Retrieve/Merge FFG File");
    }

    @Override
    protected void initializeComponents(Shell shell) {

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the controls on the dialog.
     */
    private void initializeComponents() {
        createRetrieveMergeControls();
        createBottomButtons();
    }

    /**
     * Create the Retrieve, Merge/Overwrite, and Merge controls.
     */
    private void createRetrieveMergeControls() {
        Group controlGrp = new Group(shell, SWT.BORDER);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 20;
        controlGrp.setLayout(gl);
        controlGrp.setText(" Please choose your selection: ");

        SelectionListener listener = new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                Button button = (Button) e.widget;
                if (button.getSelection()) {
                    selectedRdo = button;
                }
            }
        };

        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        retrieveRdo = new Button(controlGrp, SWT.RADIO);
        retrieveRdo.setText("Retrieve: ");
        retrieveRdo.setSelection(true);
        selectedRdo = retrieveRdo;
        selectedRdo.addSelectionListener(listener);
        selectedRdo.setData(RetrieveMergeAction.RETRIEVE);
        retrieveRdo.setLayoutData(gd);

        Label retrieveLbl = new Label(controlGrp, SWT.NONE);
        retrieveLbl
                .setText("Clear current data entirely and load data\nfrom the selected file.");

        /*
         * ** NOTE: The merge features are disabled due to the limitations of
         * having 4 sources to edit. If 3 unique sources are to be merged with 3
         * other unique sources on the display there would be 6 sources trying
         * to fit into 4 available areas. I have ideas for a more elegant
         * solution that will allow more than just 4 sources.
         */

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        mergeOvrWriteRdo = new Button(controlGrp, SWT.RADIO);
        mergeOvrWriteRdo.setText("Merge Overwrite: ");
        mergeOvrWriteRdo.addSelectionListener(listener);
        mergeOvrWriteRdo.setData(RetrieveMergeAction.MERGE_OVERWRITE);
        mergeOvrWriteRdo.setLayoutData(gd);

        Label mergeOvrWriteLbl = new Label(controlGrp, SWT.NONE);
        mergeOvrWriteLbl.setText("Retain current data and load data from the\n"
                + "selected file.  Incoming data will overwrite\n"
                + "current data when the two conflict.");

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        mergeRdo = new Button(controlGrp, SWT.RADIO);
        mergeRdo.setText("Merge: ");
        mergeRdo.addSelectionListener(listener);
        mergeRdo.setData(RetrieveMergeAction.MERGE);
        mergeRdo.setLayoutData(gd);

        Label mergeLbl = new Label(controlGrp, SWT.NONE);
        mergeLbl.setText("Retain current data and load data from the\n"
                + "selected file.  Current data will NOT be\n"
                + "overwritten by incoming data when the two\n" + "conflict.");
    }

    /**
     * Create the buttons on the bottom of the display.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(selectedRdo.getData());
                close();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }
}