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
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.viz.core.imagery.ImageCombiner;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 22, 2009            mschenke     Initial creation
 * Jun 25, 2010 947        rferrel      Added setEnabled and call to execute's
 *                                      super to allow proper refreshing of 
 *                                      elements associated with the action.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ImageCombinationAction extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        super.execute(event);

        // toggle current boolean value
        boolean combine = !this.isEnabled;
        setEnabled(combine);

        // ImageCombiner.setCombineImages(combine);
        ImageCombiner.setCombineImages(combine);
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#setEnabled(boolean)
     */
    public void setEnabled(boolean isEnabled) {
        super.setEnabled(isEnabled);
        ICommandService service = (ICommandService) VizWorkbenchManager
                .getInstance().getCurrentWindow()
                .getService(ICommandService.class);

        if (service != null) {
            service.refreshElements(commandId, null);
        }
    }

}
