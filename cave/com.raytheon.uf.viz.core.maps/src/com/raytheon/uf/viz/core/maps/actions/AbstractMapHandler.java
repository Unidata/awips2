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
package com.raytheon.uf.viz.core.maps.actions;

import org.eclipse.core.commands.AbstractHandler;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Abstract handler for actions that only apply to Map Editors. Disables itself
 * if the current editor is not a map editor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2012      #1326 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractMapHandler extends AbstractHandler {

    @Override
    public void setEnabled(Object evaluationContext) {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container == null) {
            super.setBaseEnabled(false);
            return;
        }
        IDisplayPane pane = container.getActiveDisplayPane();
        if (pane == null) {
            super.setBaseEnabled(false);
            return;
        }
        super.setBaseEnabled(pane.getRenderableDisplay() instanceof MapRenderableDisplay);
    }

}
