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
package com.raytheon.viz.mpe.ui.dialogs.hourlyradar;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
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
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.mpe.ui.dialogs.gagetable.GageTableDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 21, 2009            mpduff     Initial creation
 * May 20, 2013  15962     lbousaidi  changed getActiveRadarIds() call to
 *                                    getRadarIdsTrue(). 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RadarSiteSelectionDlg extends CaveSWTDialog {

    /**
     * Normal font.
     */
    private Font font = null;

    /**
     * Radar List Text Field.
     */
    private List radarListBox = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            The parent shell
     */
    public RadarSiteSelectionDlg(Shell parentShell) {
        super(parentShell);
        setText("Radar Sites");
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
        setReturnValue(false);
        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize all of the controls and layout
        initializeComponents();

        // populate the list box
        populateBox();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(225, SWT.DEFAULT);
        comp.setLayoutData(gd);

        // Top Label
        Label selectLbl = new Label(comp, SWT.NONE);
        selectLbl.setText("Select Site:  ");

        // Radar List Box
        radarListBox = new List(comp, SWT.BORDER | SWT.V_SCROLL);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        gd.widthHint = 220;
        gd.heightHint = 350;
        radarListBox.setLayoutData(gd);
        radarListBox.addMouseListener(new MouseListener() {

            @Override
            public void mouseDoubleClick(MouseEvent e) {
                launch();
            }

            @Override
            public void mouseDown(MouseEvent e) {
                // TODO Auto-generated method stub

            }

            @Override
            public void mouseUp(MouseEvent e) {
                // TODO Auto-generated method stub

            }
        });

        // Buttons
        Composite buttonComp = new Composite(comp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false, 2, 1);
        buttonComp.setLayoutData(gd);

        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        gd = new GridData(75, SWT.DEFAULT);
        okBtn.setAlignment(SWT.CENTER);
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                launch();
            }
        });

        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        gd = new GridData(75, SWT.DEFAULT);
        cancelBtn.setAlignment(SWT.CENTER);
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Populate the radar list box.
     */
    private void populateBox() {
        String[] radarIds = null;
        try {
            radarIds = GageTableDataManager.getInstance().getRadarIdsTrue();
            for (String s : radarIds) {
                radarListBox.add(s);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Launch the Review Hourly Radar 4 panel dialog
     */
    private void launch() {
        if (radarListBox.getItem(radarListBox.getSelectionIndex())
                .equalsIgnoreCase("ZZZ")) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a valid site.");
            mb.open();
            return;
        }

        ReviewHourlyRadarDlg dlg = new ReviewHourlyRadarDlg(getParent(),
                radarListBox.getItem(radarListBox.getSelectionIndex()));
        shell.dispose();
        dlg.open();
    }
}
