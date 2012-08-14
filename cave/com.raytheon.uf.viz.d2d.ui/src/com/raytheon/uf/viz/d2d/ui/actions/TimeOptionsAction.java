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

import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.map.D2DMapRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.d2d.ui.time.dialogs.D2DTimeMatchingConfigurationFactory;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Opens the Time Options dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class TimeOptionsAction extends AbstractTool {

    /**
     * 
     */
    public TimeOptionsAction() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        super.execute(event);
        if (this.editor == null) {
            return null;
        }
        IRenderableDisplay iDisplay = this.editor.getActiveDisplayPane()
                .getRenderableDisplay();
        D2DTimeMatcher matcher = ((D2DTimeMatcher) iDisplay.getDescriptor()
                .getTimeMatcher());

        // tell the dialog manager who we are
        D2DTimeMatchingConfigurationFactory manager = (D2DTimeMatchingConfigurationFactory) matcher
                .getTimeMatchingConfigurationFactory();
        manager.setTimeOptionsAction(this);

        // toggle current boolean value
        boolean newToggle = !this.isEnabled;
        setEnabled(newToggle);
        if (newToggle) {
            if (iDisplay instanceof D2DMapRenderableDisplay) {
                matcher.setTimeOptionsSelected(true);
            }
        } else {
            matcher.setTimeOptionsSelected(false);
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        element.setChecked(this.isEnabled);
    }

    public void setEnabled(boolean isEnabled) {
        this.isEnabled = isEnabled;
        ICommandService service = (ICommandService) VizWorkbenchManager
                .getInstance().getCurrentWindow()
                .getService(ICommandService.class);

        if (service != null) {
            service.refreshElements(commandId, null);
        }
    }
}
