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

import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Same as LaodModeHandler but disabled when there is no time match basis.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class LoadModeOverlayHandler extends LoadModeHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#setEnabled(java.lang.Object)
     */
    @Override
    public void setEnabled(Object evaluationContext) {
        IDisplayPaneContainer ieditor = EditorUtil.getActiveVizContainer();
        if (ieditor == null) {
            setBaseEnabled(false);
            return;
        }
        IDisplayPane activeDisplay = ieditor.getActiveDisplayPane();
        if (activeDisplay == null) {
            setBaseEnabled(false);
            return;
        }
        IDescriptor descriptor = activeDisplay.getDescriptor();
        if (descriptor == null) {
            setBaseEnabled(false);
            return;
        }
        AbstractTimeMatcher timeMatcher = descriptor.getTimeMatcher();
        if (timeMatcher instanceof D2DTimeMatcher == false) {
            setBaseEnabled(false);
            return;
        }
        if (((D2DTimeMatcher) timeMatcher).getTimeMatchBasis() == null) {
            setBaseEnabled(false);
            return;
        }
        setBaseEnabled(true);
    }

}
