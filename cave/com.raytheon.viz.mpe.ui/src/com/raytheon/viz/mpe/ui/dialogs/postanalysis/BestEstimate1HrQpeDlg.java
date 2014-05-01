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
package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

/**
 * 1hr Best Estimate QPE Fields dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2011            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class BestEstimate1HrQpeDlg extends BasePostAnalysisDlg {

    /**
     * File combo box.
     */
    private Combo fileCbo = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public BestEstimate1HrQpeDlg(Shell parentShell) {
        super(parentShell);

        setText("1hr Best Estimate QPE Fields");
    }

    /*
     * Create the Display 1hr QPE Fields menu item.
     */
    @Override
    protected void createControlMenuItem(Menu controlMenu) {

        MenuItem saveQpeItem = new MenuItem(controlMenu, SWT.CASCADE);
        saveQpeItem.setText("Save 1hr QPE Fields");

        Menu saveQpeSubMenu = new Menu(shell, SWT.DROP_DOWN);
        saveQpeItem.setMenu(saveQpeSubMenu);

        MenuItem saveSeparateQpeMI = new MenuItem(saveQpeSubMenu, SWT.NONE);
        saveSeparateQpeMI.setText("Save/Separate");
        saveSeparateQpeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveSeperateAction();
            }
        });

        MenuItem saveOverwriteQpeMI = new MenuItem(saveQpeSubMenu, SWT.NONE);
        saveOverwriteQpeMI.setText("Save/Overwrite");
        saveOverwriteQpeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveOverwriteAction();
            }
        });
    }

    /*
     * Get the names for the labels that appear over the maps.
     */
    @Override
    protected String[] getMapLabelNames() {
        String[] names = new String[] { "1hr Best Estimate QPE", "1hr Best Estimate QPE(Grid Bias Applied)" };
        return names;
    }

    /**
     * Save separate action.
     */
    private void saveSeperateAction() {

    }

    /**
     * Save overwrite action.
     */
    private void saveOverwriteAction() {

    }

    /**
     * Add a combo control to the bottom of the dialog.
     */
    @Override
    protected void addBottomControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 300;
        fileCbo = new Combo(shell, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        fileCbo.setLayoutData(gd);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.mpe.ui.dialogs.postanalysis.BasePostAnalysisDlg#
     * getNumberOfColorLegends()
     */
    @Override
    protected int getNumberOfColorLegends() {
        return 1;
    }
}
