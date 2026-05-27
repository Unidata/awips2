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
package com.raytheon.uf.viz.xy.varheight.hodo;

import java.util.List;

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IPane;
import com.raytheon.uf.viz.core.IPaneCreator;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightXyPaneCreator;

/**
 * {@link IPaneCreator} implementation for creating var height graph panes that
 * include an inset hodograph canvas.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2022 8791       mapeters    Initial creation
 * Sep 30, 2022 8792       mapeters    Refactored from com.raytheon.uf.viz.xy.hodo.HodoXyPaneCreator,
 *                                     extend VarHeightXyPaneCreator
 *
 * </pre>
 *
 * @author mapeters
 */
public class VarHeightHodoXyPaneCreator extends VarHeightXyPaneCreator {

    @Override
    public IPane createPane(IDisplayPaneContainer paneContainer, Composite comp,
            IRenderableDisplay display, List<IPane> panes) throws VizException {
        return new VarHeightHodoXyPane(paneContainer, comp, display, panes);
    }
}
