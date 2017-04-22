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
package com.raytheon.uf.viz.kml.export;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.maps.actions.AbstractMapHandler;
import com.raytheon.uf.viz.kml.export.KmlExportOptions.KmlExportTimeMode;
import com.raytheon.viz.ui.EditorUtil;

/**
 * 
 * Handler for events from the KML Export menu item.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun  1, 2012            bsteffen     Initial creation
 * Nov 13, 2012      #1326 randerso     Moved setEnabled method into
 *                                      AbstractMapHandler for reuse
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlExportHandler extends AbstractMapHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        new KmlExportDialog(HandlerUtil.getActiveShell(event),
                getDefaultOptions()).open();
        return null;
    }

    protected static KmlExportOptions getDefaultOptions() {
        KmlExportOptions options = new KmlExportOptions();
        options.setFirstFrameIndex(Integer.MIN_VALUE);
        options.setLastFrameIndex(Integer.MAX_VALUE);
        options.setPreserveVisibility(true);
        options.setShadeEarth(false);
        options.setTimeMode(KmlExportTimeMode.TIME_SPAN);
        options.setKmzFileLocation(new File(System.getProperty("user.home"),
                "caveExport.kmz"));
        options.setPlotIconScale(3.5);
        options.setFillPlotBackground(false);
        options.setPaintSleepMillis(10);
        options.setMaxRefreshSeconds(60);
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        List<KmlPane> panes = new ArrayList<KmlPane>();
        for (IDisplayPane pane : container.getDisplayPanes()) {
            AbstractRenderableDisplay display = (AbstractRenderableDisplay) pane
                    .getRenderableDisplay();
            panes.add(new KmlPane(display, pane.getBounds()));
        }
        options.setPanes(panes);
        return options;
    }

}
