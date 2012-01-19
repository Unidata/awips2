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
/**
 * 
 */
package com.raytheon.viz.hydro.colorscalemgr;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.hydro.gagedisplay.StationDisplay;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.ColorScaleMgrDlg;
import com.raytheon.viz.hydrocommon.colorscalemgr.HydroColorManager;
import com.raytheon.viz.hydrocommon.colorscalemgr.NamedColorSetGroup;

/**
 * Action for unimplemented features. To be used temporarily until final
 * behavior is implemented.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/06                  lvenable    Initial Creation.
 * 04/07/2010   4671        mpduff      Have the map update upon closure of the dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class ColorScaleMgrAction extends AbstractHandler {

    private ColorScaleMgrDlg colorScaleDlg = null;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        String username = LocalizationManager.getInstance().getCurrentUser();

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        colorScaleDlg = new ColorScaleMgrDlg(shell, username);

        HydroColorManager colorManager = HydroColorManager.getInstance();
        NamedColorSetGroup ncsg = colorManager.getDefaultColorSetGroup();
        colorManager.populateDefaultColorUseSets(ncsg.getColorGroupArray());
        colorManager.readColorValuesFromDatabase();

        colorScaleDlg.setTitle("Hydroview Color Scale Manager - User: "
                + username);
        colorScaleDlg.setColorManager(colorManager);
        boolean dataChanged = (Boolean) colorScaleDlg.open();
        colorScaleDlg = null;

        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        displayManager.setColorChanged(dataChanged);

        // redraw the main display
        displayManager.setDataChanged(dataChanged);
        StationDisplay sd = StationDisplay.getInstance();
        sd.redraw();

        return null;
    }
}
