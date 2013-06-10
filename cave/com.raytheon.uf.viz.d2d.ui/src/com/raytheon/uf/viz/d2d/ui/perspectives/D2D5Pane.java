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

package com.raytheon.uf.viz.d2d.ui.perspectives;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

import com.raytheon.uf.viz.core.maps.scales.MapScales;
import com.raytheon.uf.viz.core.maps.scales.MapScales.MapScale;
import com.raytheon.uf.viz.core.maps.scales.MapScales.PartId;
import com.raytheon.uf.viz.d2d.ui.actions.ChangeD2DLayoutAction;
import com.raytheon.uf.viz.d2d.ui.map.SideView;
import com.raytheon.viz.ui.UiUtil;

/**
 * The default window layout, shouldn't be named D2D5Pane as it handles both 5
 * and 3 pane now. Keeping original name for bundle compatibility
 * 
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * Mar 21, 2013       1638  mschenke    Changed map scales not tied to d2d
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class D2D5Pane implements IPerspectiveFactory {

    /** <code>ID_PERSPECTIVE</code> field */
    public static final String ID_PERSPECTIVE = "com.raytheon.uf.viz.d2d.ui.perspectives.D2D5Pane"; //$NON-NLS-1$

    private static final String BASE_VIEW_ID_PREFIX = SideView.ID
            + UiUtil.SECONDARY_ID_SEPARATOR + "sideView";

    private static final float THREE_PANE_WIDTH = 0.4f;

    private static final float FIVE_PANE_WIDTH = 0.2f;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui
     * .IPageLayout)
     */
    public void createInitialLayout(IPageLayout layout) {
        // Get the editor area.
        String editorArea = layout.getEditorArea();
        layout.setFixed(false);

        List<String> baseViewsToAdd = new ArrayList<String>();
        Set<String> addedViews = new HashSet<String>();

        for (MapScale scale : MapScales.getInstance().getScales()) {
            for (PartId part : scale.getPartIds()) {
                if (part.getId().startsWith(BASE_VIEW_ID_PREFIX)
                        && baseViewsToAdd.contains(part.getId()) == false) {
                    baseViewsToAdd.add(part.getId());
                }
            }
        }

        int numViews = ChangeD2DLayoutAction.getViewCount() == 2 ? 2
                : baseViewsToAdd.size();

        String lastAdded = null;

        Collections.sort(baseViewsToAdd);

        for (int i = 0; i < baseViewsToAdd.size(); ++i) {
            String baseView = baseViewsToAdd.get(i);
            if (baseViewsToAdd.contains(baseView)) {
                if (lastAdded == null) {
                    layout.addStandaloneView(
                            baseView,
                            false,
                            IPageLayout.LEFT,
                            ChangeD2DLayoutAction.getViewCount() == 2 ? THREE_PANE_WIDTH
                                    : FIVE_PANE_WIDTH, editorArea);
                } else {
                    layout.addStandaloneView(baseView, false,
                            IPageLayout.BOTTOM, (i >= numViews) ? 1.0f
                                    : 1.0f / (numViews - i + 1), lastAdded);
                }
                lastAdded = baseView;
                addedViews.add(lastAdded);
            }
        }

        addedViews.addAll(baseViewsToAdd);

        List<String> extraViews = new ArrayList<String>();
        for (MapScale scale : MapScales.getInstance().getScales()) {
            for (PartId part : scale.getPartIds()) {
                if (part.isView() && addedViews.contains(part.getId()) == false) {
                    extraViews.add(part.getId());
                    addedViews.add(part.getId());
                }
            }
        }

        if (extraViews.size() > 0) {
            IFolderLayout folder = layout.createFolder(
                    "com.raytheon.uf.viz.d2d.ui.extrasFolder",
                    IPageLayout.BOTTOM, .7f, editorArea);

            for (int i = 0; i < extraViews.size(); ++i) {
                String id = extraViews.get(i);
                folder.addView(id);
            }
        }

    }
}
