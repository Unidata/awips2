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

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.gfe.core.internal.OffscreenSpatialDisplayManager;

/**
 * Manages windowless DataManagers for {@link IRenderableDisplay}s. Mostly used
 * for offscreen rendering applications
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
 * @version 1.0
 */

public class DataManagerOffscreenFactory extends DataManagerFactory {

    private static DataManagerOffscreenFactory instance = new DataManagerOffscreenFactory();

    /**
     * Gets (will create if none) the DataManager for the given display
     * 
     * @param display
     * @return
     */
    public static DataManager getInstance(GFEMapRenderableDisplay display) {
        DataManager dm = DataManagerFactory.getInstance(instance, display);
        display.setDataManager(dm);
        return dm;
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
        return new OffscreenSpatialDisplayManager(
                (GFEMapRenderableDisplay) discriminator, dm);
    }

}
