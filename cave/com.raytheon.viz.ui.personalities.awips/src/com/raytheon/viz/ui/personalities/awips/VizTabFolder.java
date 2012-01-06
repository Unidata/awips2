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
package com.raytheon.viz.ui.personalities.awips;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.internal.presentations.defaultpresentation.DefaultTabFolder;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Smaller tabs for editors and views
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 18, 2008            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class VizTabFolder extends DefaultTabFolder {

    public VizTabFolder(Composite parent, int flags, boolean allowMin,
            boolean allowMax) {
        super(parent, flags, allowMin, allowMax);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.internal.presentations.defaultpresentation.DefaultTabFolder
     * #computeTabHeight()
     */
    @Override
    protected int computeTabHeight() {
        return 17;
    }

    @Override
    protected void handleDragStarted(Point displayPos, Event e) {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.getRenderableDisplay().setSwapping(true);
            }
        }
        super.handleDragStarted(displayPos, e);
        container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.getRenderableDisplay().setSwapping(false);
            }
        }
    }

}
