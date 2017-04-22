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
package com.raytheon.uf.viz.monitor.scan.resource;

import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * Input handler for Scan
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2009  2037       dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ScanMouseAdapter extends InputAdapter {

    private IDisplayPaneContainer container;

    public ScanMouseAdapter(IDisplayPaneContainer container) {
        this.container = container;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        if (mouseButton == 3) {
            boolean trend = false;
            IRenderableDisplay display = container.getActiveDisplayPane()
                    .getRenderableDisplay();
            List<ScanResource> scans = display.getDescriptor()
                    .getResourceList()
                    .getResourcesByTypeAsType(ScanResource.class);
            for (ScanResource scan : scans) {
                if (scan.isTrend()) {
                    trend = scan.trendClick(new double[] { x, y });
                }
            }

            if (trend) {
                return trend;
            }
        }

        return super.handleMouseUp(x, y, mouseButton);
    }
}
