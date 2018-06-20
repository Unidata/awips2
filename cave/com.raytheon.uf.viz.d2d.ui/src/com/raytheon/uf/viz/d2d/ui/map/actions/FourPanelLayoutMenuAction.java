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

import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Switch to a Four Panel layout in the editor pane. There are two variations of
 * the switch. First, when there was a prior four panel layout and the user
 * selected rotate panes. In this case, the other panes are preserved and will
 * be redisplayed in a four panel layout. The second when only viewing a single
 * pane, and the user selects a four panel layout. In this case, the current
 * pane will be put in the upper left corner, and the other panes will be blank
 * with the same projection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *   
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 19, 2009             bgonzale    Initial Creation.
 * Apr 07, 2015  4204       njensen     Keep part name if renamed
 * Oct 21, 2015  5023       njensen     Removed unnecessary part name code (undid previous change)
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1
 */
public class FourPanelLayoutMenuAction extends AbstractRightClickAction {

    /**
     * Default constructor.
     */
    public FourPanelLayoutMenuAction() {
        super("Four Panel Layout");
    }

    @Override
    public void run() {
        if (getContainer() instanceof IMultiPaneEditor == false
                || getContainer().getDisplayPanes()[0].getRenderableDisplay() 
                instanceof ID2DRenderableDisplay == false) {
            return;
        }
        IMultiPaneEditor editor = (IMultiPaneEditor) getContainer();
        IRenderableDisplay definiteDisplay = getContainer().getDisplayPanes()[0]
                .getRenderableDisplay();

        if (editor.getNumberofPanes() > 1) {
            for (IDisplayPane pane : getContainer().getDisplayPanes()) {
                editor.showPane(pane);
            }
        } else {
            for (int i = 1; i < 4; ++i) {
                editor.addPane(definiteDisplay.createNewDisplay());
            }
        }

        for (IDisplayPane pane : editor.getDisplayPanes()) {
            List<D2DLegendResource> rscs = pane.getDescriptor()
                    .getResourceList()
                    .getResourcesByTypeAsType(D2DLegendResource.class);
            for (D2DLegendResource rsc : rscs) {
                rsc.setLegendMode(LegendMode.PRODUCT);
            }
        }

        editor.setSelectedPane(IMultiPaneEditor.IMAGE_ACTION, null);
        editor.setSelectedPane(IMultiPaneEditor.VISIBLE_PANE, null);
        editor.setSelectedPane(IMultiPaneEditor.LOAD_ACTION, null);

        editor.refresh();
    }
}
