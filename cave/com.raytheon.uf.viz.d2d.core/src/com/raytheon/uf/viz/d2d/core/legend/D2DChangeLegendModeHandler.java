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
package com.raytheon.uf.viz.d2d.core.legend;

import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;

/**
 * This handler is responsible for picking up right clicks on the map and change
 * the d2d legend display mode
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class D2DChangeLegendModeHandler extends AbstractD2DLegendInputHandler {

    private static final String TOGGLE_LEGEND_PREF = "com.raytheon.viz.d2d.ui.toggleLegend";

    private boolean cancel = false;

    /**
     * @param resource
     */
    protected D2DChangeLegendModeHandler(D2DLegendResource resource) {
        super(resource);
    }

    public boolean handleMouseDown(int x, int y, int mouseButton) {
        cancel = false;
        return super.handleMouseDown(x, y, mouseButton);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseDownMove(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        cancel = true;
        return super.handleMouseDownMove(x, y, mouseButton);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseUp(int, int, int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        if (!cancel && prefManager.handleClick(TOGGLE_LEGEND_PREF, mouseButton)) {
            switch (resource.getLegendMode()) {
            case MAP: {
                resource.getLegendAction(LegendMode.NONE).run();
                break;
            }
            case NONE: {
                resource.getLegendAction(LegendMode.PRODUCT).run();
                break;
            }
            case PRODUCT: {
                resource.getLegendAction(LegendMode.NONE).run();
                break;
            }
            }
            resource.issueRefresh();
        }
        return super.handleMouseUp(x, y, mouseButton);
    }
}
