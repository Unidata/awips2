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
package com.raytheon.uf.viz.xy.timeseries.display;

import java.util.List;

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.AbstractCustomHandlingXyPane;
import com.raytheon.uf.viz.xy.timeseries.util.TimeSeriesPanHandler;
import com.raytheon.uf.viz.xy.timeseries.util.TimeSeriesZoomHandler;
import com.raytheon.uf.viz.xy.util.AbstractGraphPanHandler;
import com.raytheon.uf.viz.xy.util.AbstractGraphZoomHandler;

/**
 * Represents a single time series pane, containing the main time series graph
 * canvas and an inset map canvas.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 08, 2022 8792       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
public class TimeSeriesXyPane extends AbstractCustomHandlingXyPane {

    /**
     * {@inheritDoc}
     */
    public TimeSeriesXyPane(IDisplayPaneContainer paneContainer,
            Composite composite, IRenderableDisplay renderableDisplay,
            List<IPane> panes) throws VizException {
        super(paneContainer, composite, renderableDisplay, panes);
    }

    @Override
    protected AbstractGraphZoomHandler createZoomHandler(
            IRenderableDisplay display) {
        return new TimeSeriesZoomHandler(display);
    }

    @Override
    protected AbstractGraphPanHandler createPanHandler(
            IRenderableDisplay display) {
        return new TimeSeriesPanHandler(display);
    }
}
