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
package com.raytheon.viz.radar.rsc.graphic;

import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class PreviousGraphicPage extends AbstractRightClickAction {

    @Override
    public void run() {
        RadarGraphicsResource resource = ((RadarGraphicsResource) getSelectedRsc());
        goToPrevious(resource);
    }

    @Override
    public String getText() {
        return "Previous Page";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.cmenu.AbstractRightClickAction#isHidden()
     */
    @Override
    public boolean isHidden() {
        if (getSelectedRsc() instanceof RadarGraphicsResource) {
            RadarGraphicsResource resource = (RadarGraphicsResource) getSelectedRsc();
            RadarGraphicsDisplay rgd = resource.getRadarGraphicsDisplay().get(
                    resource.getDescriptor().getTimeForResource((resource)));
            if (rgd != null && rgd.getNumPages() > 1) {
                return false;
            }
        }
        return true;
    }

    public static void goToPrevious(RadarGraphicsResource resource) {
        RadarGraphicsDisplay rgd = resource.getRadarGraphicsDisplay().get(
                resource.getDescriptor().getTimeForResource(resource));
        if (rgd.getCurrentPage() == 0) {
            rgd.setCurrentPage(rgd.getNumPages() - 1);
        } else {
            rgd.setCurrentPage(rgd.getCurrentPage() - 1);
        }
    }
}
