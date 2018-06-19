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

package com.raytheon.uf.viz.xy.varheight.util;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
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
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ScaleHandler extends AbstractHandler {

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
        // retrieve the existing editor to set on
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor != null) {
            setScale(editor, scale);
        }
    }

    /**
     * @param scale
     */
    static void setScale(IDisplayPaneContainer editor, String scale) {
        for (IDisplayPane pane : editor.getDisplayPanes()) {
            if (pane.getRenderableDisplay() instanceof ID2DRenderableDisplay) {
                ID2DRenderableDisplay disp = (ID2DRenderableDisplay) pane
                        .getRenderableDisplay();
                if (scale.equals(disp.getScale())) {
                    // don't set the scale if it is the same as the display's
                    // current scale
                    return;
                }

                disp.setScale(scale);
                if (pane == editor.getActiveDisplayPane()) {
                    VizGlobalsManager.getCurrentInstance().updateChanges(
                            editor.getActiveDisplayPane()
                                    .getRenderableDisplay().getGlobalsMap());
                }
            }
        }

        ICommandService service = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);
        service.refreshElements("com.raytheon.uf.viz.xy.height.scalebutton",
                null);
    }

}
