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

package com.raytheon.uf.viz.d2d.ui.map.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.maps.actions.NewMapEditor;
import com.raytheon.uf.viz.core.maps.scales.IMapScaleDisplay;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Load the scale bundle and merge it into the existing bundle
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 24, 2007             randerso    Initial Creation.
 * Oct 21, 2008   #1450     randerso    Fixed to support multipane editors
 * Mar 21, 2013       1638  mschenke    Changed map scales not tied to d2d
 * Oct  9, 2013       2104  mschenke    Switched to use logic in the IMapScaleDisplay
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ScaleHandler extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScaleHandler.class);

    public ScaleHandler() {
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String scale = event.getParameter("scale");

        setScale(scale);

        return null;
    }

    /**
     * @param scale
     */
    public static void setScale(String scale) {
        // retrieve the existing map descriptor
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor == null) {
            try {
                editor = new NewMapEditor().execute(null);
            } catch (ExecutionException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error creating new map editor for scale: " + scale);
            }
        }
        setScale(editor, scale);
    }

    /**
     * Set the scale on the container passed in
     * 
     * @param editor
     * @param scale
     */
    public static void setScale(IDisplayPaneContainer editor, String scale) {
        if (editor == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not set scale on null editor");
        }

        for (IDisplayPane pane : editor.getDisplayPanes()) {
            IRenderableDisplay display = pane.getRenderableDisplay();
            if (display instanceof IMapScaleDisplay) {
                ((IMapScaleDisplay) display).changeScale(scale);
            }
        }

        VizGlobalsManager.getCurrentInstance().updateUI(editor);
    }
}
