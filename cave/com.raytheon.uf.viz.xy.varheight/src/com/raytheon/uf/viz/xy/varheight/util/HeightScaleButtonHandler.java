/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.xy.varheight.util;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.d2d.ui.AbstractHeightDisplay;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.ScaleButtonHandler;

/**
 * Handler for the height scale button in the toolbar.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2022 8946       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class HeightScaleButtonHandler extends ScaleButtonHandler {

    public HeightScaleButtonHandler() {
        super(VizConstants.HEIGHT_SCALE_ID);
    }

    @Override
    public void setEnabled(Object evaluationContext) {
        boolean enabled = false;

        /*
         * Enabled if the active editor contains a height graph pane.
         */
        IDisplayPaneContainer cont = EditorUtil.getActiveVizContainer();
        if (cont != null) {
            for (IDisplayPane canvas : cont.getMainCanvases()) {
                if (canvas
                        .getRenderableDisplay() instanceof AbstractHeightDisplay) {
                    enabled = true;
                    break;
                }
            }
        }

        setBaseEnabled(enabled);
    }

    @Override
    protected String getTooltip(String scale) {
        return "Height Scale: " + scale;
    }
}
