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

package com.raytheon.viz.gfe.perspective;

import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.IViewLayout;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.GFEPreference;

/**
 * The GFE window layout
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 01, 2006           chammack  Initial Creation.
 * Dec 14, 2015  5193     bsteffen  Specifically make Grid Manager moveable.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author chammack
 */
public class GFEPerspective implements IPerspectiveFactory {

    /** <code>ID_PERSPECTIVE</code> field */
    public static final String ID_PERSPECTIVE = "com.raytheon.viz.ui.GFEPerspective"; //$NON-NLS-1$

    @Override
    public void createInitialLayout(IPageLayout layout) {
        // Get the editor area.
        String editorArea = layout.getEditorArea();

        layout.setFixed(false);

        int width = GFEPreference.getInt("TimeScale_horizSize", 360);
        int height = GFEPreference.getInt("TimeScale_vertSize", 220);

        Rectangle windowSize = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell().getClientArea();

        String GM_TE_Layout = GFEPreference.getString("GM_TE_Layout");
        int relationship = IPageLayout.TOP;
        float ratio = (float) height / windowSize.height;
        if ("OnLeft".equals(GM_TE_Layout)) {
            relationship = IPageLayout.LEFT;
            ratio = (float) width / windowSize.width;
        } else if ("OnRight".equals(GM_TE_Layout)) {
            relationship = IPageLayout.RIGHT;
            ratio = 1.0f - ((float) width / windowSize.width);
        } else if ("OnBottom".equals(GM_TE_Layout)) {
            relationship = IPageLayout.BOTTOM;
            ratio = 1.0f - ((float) height / windowSize.height);
        }

        layout.addStandaloneView(
                "com.raytheon.viz.gfe.GridManagerView:GridManager", true,
                relationship, ratio, editorArea);
        IViewLayout viewLayout = layout.getViewLayout(
                "com.raytheon.viz.gfe.GridManagerView:GridManager");
        if (viewLayout != null) {
            viewLayout.setCloseable(false);
            viewLayout.setMoveable(true);
        }
    }
}
