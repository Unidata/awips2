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

import org.eclipse.ui.part.EditorPart;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;
import com.raytheon.viz.ui.IRenameablePart;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Switch to a Two Panel layout in the editor pane.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Apr 20, 2016             mjames@ucar Copied from FourPanelLayoutMenuAction
 *    
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *   
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Apr 20, 2016             mjames@ucar Copied from FourPanelLayoutMenuAction
 * Jul 19, 2016             mjames@ucar Removed unnecessary part name code (undid previous change)
 * </pre>
 * 
 * @author bgonzale
 * @version 1
 */
public class TwoPanelLayoutMenuAction extends AbstractRightClickAction {

    /**
     * Default constructor.
     */
    public TwoPanelLayoutMenuAction() {
        super("Two Panel Layout");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
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
            for (int i = 1; i < 2; ++i) {
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
