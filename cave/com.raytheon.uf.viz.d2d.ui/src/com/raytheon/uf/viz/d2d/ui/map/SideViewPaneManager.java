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
package com.raytheon.uf.viz.d2d.ui.map;

import java.util.List;
import java.util.stream.Collectors;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.viz.ui.panes.PaneManager;

/**
 * {@link PaneManager} extension for managing panes in the D2D side view.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2022 8955       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class SideViewPaneManager extends PaneManager {

    @Override
    public List<IDisplayPane> getCanvasesCompatibleWithActive() {
        /*
         * Overridden to support combo editor panes of different types being
         * swapped in.
         */
        IDisplayPane activeCanvas = getActiveDisplayPane();
        IDescriptor activeDescriptor = activeCanvas.getDescriptor();
        return mainCanvasToPaneMap.keySet().stream().filter(
                canvas -> canvas.getDescriptor().isCompatible(activeDescriptor))
                .collect(Collectors.toUnmodifiableList());
    }
}
