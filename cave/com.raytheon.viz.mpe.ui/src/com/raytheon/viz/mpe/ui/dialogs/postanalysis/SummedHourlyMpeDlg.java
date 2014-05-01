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
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * Summed Hourly MPE Field & Gage Only Field dialog.
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

public class SummedHourlyMpeDlg extends BasePostAnalysisDlg {

    /**
     * Merge Data dialog.
     */
    MergedGridBiasDlg mergeDataDlg = null;

    public SummedHourlyMpeDlg(Shell parentShell) {
        super(parentShell);

        setText("Summed Hourly MPE Field & Gage Only Field");
    }

    /*
     * Create the Merge Data menu item.
     */
    @Override
    protected void createControlMenuItem(Menu controlMenu) {
        /*
         * Create custom items in the Control dropdown menu
         */
        MenuItem mergeDataMI = new MenuItem(controlMenu, SWT.NONE);
        mergeDataMI.setText("Merge Data");
        mergeDataMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                mergeDataAction();
            }
        });
    }

    /*
     * Get the names for the labels that appear over the maps.
     */
    @Override
    protected String[] getMapLabelNames() {
        String[] names = new String[] { "Summed Hourly MPE Field", "Gage Only Field" };
        return names;
    }

    /**
     * Merge Data action.
     */
    private void mergeDataAction() {
        if (mergeDataDlg == null || mergeDataDlg.isDisposed() == true) {
            Shell platformShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            mergeDataDlg = new MergedGridBiasDlg(platformShell);
            mergeDataDlg.open();
        }
    }

    /**
     * Not used
     */
    @Override
    protected void addBottomControls() {
        // Not used
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
