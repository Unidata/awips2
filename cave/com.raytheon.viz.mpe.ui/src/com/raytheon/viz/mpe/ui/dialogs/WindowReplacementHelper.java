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

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class WindowReplacementHelper {
    // variables having to do with a redisplay
    private boolean isDisposed = false;

    private Shell previousShell = null;

    private Point previousLocation = null;

    // ----------------------------------------------------------------------------------

    public void manageWindows(AbstractMPEDialog newDialog) {
        /**
         * This method closes and disposes any previously opened instance of
         * this window and puts the new instance at the same location as the old
         * one if there is one. This class is to be used with
         * EditPrecipStationsDialog, EditTemperatureStationsDialog, and
         * EditFreezingStationsDialog
         */

        Shell currentShell = newDialog.getShell();

        // if there is still a window displayed, then
        // store its location and dispose of it

        if ((previousShell != null) && (!isDisposed())) {
            previousLocation = previousShell.getLocation();

            try {

                previousShell.dispose();
                setIsDisposed(true);

            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        // put the currentShell at the same location as the last saved location
        if (previousLocation != null) {
            currentShell.setLocation(previousLocation);
        }
        setIsDisposed(false);

        previousShell = currentShell; // assign the new shell to previous for
                                      // use next time
        previousLocation = previousShell.getLocation();
    }

    // ----------------------------------------------------------------------------------

    public void setIsDisposed(boolean isDisposed) {
        this.isDisposed = isDisposed;
    }

    // ----------------------------------------------------------------------------------

    private boolean isDisposed() {
        return isDisposed;
    }
    // ----------------------------------------------------------------------------------

} // end class WindowReplacementHelper
