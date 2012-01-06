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
package com.raytheon.viz.mpe.ui.dialogs;

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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.mpe.ui.actions.GageQcSelect;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class SendtoDatabaseDialog extends CaveSWTDialog {

    private GageQcSelect select = new GageQcSelect();

    private Font font;

    private String ds = "";

    private int mode = 0;

    private String btext = "";

    private String mtext = "";

    private Button okBtn;

    private Button cancelBtn;

    private Button precipBtn, freezeBtn, tempBtn;

    private String atext;

    protected SendtoDatabaseDialog(Shell parentShell) {
        this(parentShell, null, 0);
    }

    public SendtoDatabaseDialog(Shell parentShell, String datasets, int v) {
        super(parentShell);
        setText("Send to Database");
        ds = datasets;
        mode = v;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(0);

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        if (mode == 0) {
            atext = "OK";
            btext = "Cancel";
            mtext = "The following datasets need to be QC'd!";
        } else {
            atext = "Yes";
            btext = "No";
            mtext = "Save the point, gridded and basin averaged datasets for the following dates?";
        }
        createDBListComp();
        createButtonComp();
    }

    /**
     * Create the data options group and controls.
     */
    private void createDBListComp() {

        // Create a container to hold the label and the combo box.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite dbListComp = new Composite(shell, SWT.NONE);
        GridLayout dbListCompLayout = new GridLayout(1, false);
        dbListComp.setLayout(dbListCompLayout);
        dbListComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label dataSetsLbl = new Label(dbListComp, SWT.CENTER);
        dataSetsLbl.setText("Choose the following data type/types to save");
        dataSetsLbl.setLayoutData(gd);

        precipBtn = new Button(dbListComp, SWT.CHECK | SWT.LEFT);
        precipBtn.setText("Precipitation");
        precipBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                select.qctype_select(0);
            }
        });

        tempBtn = new Button(dbListComp, SWT.CHECK | SWT.LEFT);
        tempBtn.setText("Temperature");
        tempBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                select.qctype_select(1);
            }
        });

        freezeBtn = new Button(dbListComp, SWT.CHECK | SWT.LEFT);
        freezeBtn.setText("Freezing Level");
        freezeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                select.qctype_select(2);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataSetsLbl = new Label(dbListComp, SWT.CENTER);
        dataSetsLbl.setText(mtext);
        dataSetsLbl.setLayoutData(gd);

        gd = new GridData(300, 150);
        Text dataSetTxt = new Text(dbListComp, SWT.LEFT | SWT.MULTI
                | SWT.READ_ONLY | SWT.BORDER | SWT.V_SCROLL);
        dataSetTxt.setLayoutData(gd);
        dataSetTxt.setText(ds);
    }

    private void createButtonComp() {
        // Create a container to hold the button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite okBtnComp = new Composite(shell, SWT.NONE);
        GridLayout okBtnCompLayout = new GridLayout(2, false);
        okBtnComp.setLayout(okBtnCompLayout);
        okBtnComp.setLayoutData(gd);

        GridData bd = new GridData(110, 25);
        okBtn = new Button(okBtnComp, SWT.PUSH);
        okBtn.setText(atext);
        okBtn.setLayoutData(bd);
        okBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(1);
                shell.dispose();
            }
        });

        cancelBtn = new Button(okBtnComp, SWT.PUSH);
        cancelBtn.setText(btext);
        cancelBtn.setLayoutData(bd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(0);
                shell.dispose();
            }

        });
    }

}
