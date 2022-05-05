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
package com.raytheon.viz.gfe.core;

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.internal.GFESpatialDisplayManager;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * UI based {@link DataManager} factory. Uses {@link IWorkbenchWindow} objects
 * as the discriminator for keeping track of instances
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * 
 * 
 * @version 1.0
 */

public class DataManagerUIFactory extends DataManagerFactory {

    private static DataManagerUIFactory instance = new DataManagerUIFactory();

    /**
     * Find the DataManager associated with a specific workbench window
     * 
     * If it does not exist, return null.
     * 
     * @param window
     *            the workbench window
     * @return the DataManager associated with the specified window
     */
    public static DataManager findInstance(IWorkbenchWindow window) {
        return DataManagerFactory.findInstance(window);
    }

    /**
     * Get the DataManager associated with the currently active workbench
     * window.
     * 
     * If no workbench window is active, or the GFE perspective has not been
     * activated in the active window, this method will return null.
     * 
     * @return the active DataManager
     */
    public static DataManager getCurrentInstance() {
        final IWorkbenchWindow[] window = new IWorkbenchWindow[1];

        window[0] = VizWorkbenchManager.getInstance().getCurrentWindow();

        if (window[0] == null && PlatformUI.isWorkbenchRunning()) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    window[0] = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow();
                }

            });
        }

        return findInstance(window[0]);
    }

    /**
     * Get the DataManager associated with a specific workbench window
     * 
     * If it does not exist, create one.
     * 
     * @param window
     *            the window the DataManager is associated with
     * @return a DataManager associated to a specific window
     */
    public static DataManager getInstance(IWorkbenchWindow window) {
        DataManager dm = DataManagerFactory.getInstance(instance, window);
        if (window != null) {
            postInitialize(dm);
        }
        return dm;
    }

    /**
     * Tell ParmManager which parms to load initially
     * 
     * @param dataManager
     */
    private static void postInitialize(DataManager dataManager) {
        IParmManager parmMgr = dataManager.getParmManager();
        ParmID[] parmIDs = dataManager.getWEGroupManager().getParmIDs(
                dataManager.getWEGroupManager().getDefaultGroup(),
                parmMgr.getAllAvailableParms());

        parmMgr.setDisplayedParms(parmIDs);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.DataManagerFactory#createSpatialDisplayManager
     * (com.raytheon.viz.gfe.core.DataManager, java.lang.Object)
     */
    @Override
    protected ISpatialDisplayManager createSpatialDisplayManager(
            DataManager dm, Object discriminator) {
        return new GFESpatialDisplayManager(dm);
    }

}
