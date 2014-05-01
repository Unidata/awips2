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
 * Merged and Grid Bias Fields dialog.
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

public class MergedGridBiasDlg extends BasePostAnalysisDlg {

    /**
     * Best Estimate dialog.
     */
    private BestEstimate1HrQpeDlg bestEstimateDlg = null;

    public MergedGridBiasDlg(Shell parentShell) {
        super(parentShell);

        setText("Merged and Grid Bias Fields");
    }

    /*
     * Create the Display 1hr QPE Fields menu item.
     */
    @Override
    protected void createControlMenuItem(Menu controlMenu) {
        /*
         * Create custom items in the Control dropdown menu
         */
        MenuItem displayQpeMI = new MenuItem(controlMenu, SWT.NONE);
        displayQpeMI.setText("Display 1hr QPE Fields");
        displayQpeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                display1HrQpeAction();
            }
        });
    }

    /*
     * Get the names for the labels that appear over the maps.
     */
    @Override
    protected String[] getMapLabelNames() {
        String[] names = new String[] { "Merged Field", "Grid Bias Field" };
        return names;
    }

    private void display1HrQpeAction() {
        if (bestEstimateDlg == null || bestEstimateDlg.isDisposed() == true) {
            Shell platformShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            bestEstimateDlg = new BestEstimate1HrQpeDlg(platformShell);
            bestEstimateDlg.open();
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
        return 2;
    }
}
