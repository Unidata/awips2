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
package com.raytheon.viz.hydro;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2008            mpduff     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public abstract class CaveHydroSWTDialog extends CaveSWTDialog {
    /**
     * Currently selected lid.
     */
    protected String currentLid = null;
    ;
    
    /**
     * Currently selected TS.
     */
    protected String currentTs = null;    
    protected String currentPe = null;    
    /**
     * Constructor.
     * 
     * @param parentShell
     */
    protected CaveHydroSWTDialog(Shell parentShell) {
        super(parentShell);
    }

    /**
     * Constructor.
     * 
     * @param parentShell
     * @param style
     */   
    protected CaveHydroSWTDialog(Shell parentShell, int style) {
        super(parentShell, style);    
    }
   
    protected CaveHydroSWTDialog(Shell parentShell, int style, int caveStyle) {
        super(parentShell, style, caveStyle);
    }
    
    /**
     * Get the currently selected lid. If no lid selected display error message
     * pop up.
     * 
     * @return the current lid or null if nothing selected
     */
    protected boolean isCurrentLidSelected() {
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();

        currentLid = displayManager.getCurrentLid();

        if (currentLid == null) {
            MessageDialog.openError(getParent(), "Invalid Selection",
                    "You must select a site first");
            return false;
        }

        return true;
    }

    /**
     * Get the currently selected lid.
     * 
     * @return The selected lid, or null if no selection has been made
     */
    protected String getCurrentLid() {
        return currentLid;
    }      
   
    
    protected String getTs() {
        return currentTs;
    }     
    protected String getPe() {
        return currentPe;
    }      
}
