package gov.noaa.nws.ncep.viz.ui.perspectives;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;


/**
 * The National Centers perspective window layout
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/2008		22			M. Li		initial creation
 * 03/2009      75          B. Hebbard  Bring forward from TO9; rename all NMAP-->NC
 * 09/27/2009   169         G. Hull     require NCMapEditor
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class NCPerspective implements IPerspectiveFactory {

    /** <code>ID_PERSPECTIVE</code> field */
    public static final String ID_PERSPECTIVE = "gov.noaa.nws.ncep.viz.ui.NCPerspective"; //$NON-NLS-1$
    
    
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
     */
    public void createInitialLayout(IPageLayout layout) {
    	
    	String refId = layout.getEditorArea();
        // Get the editor area.
        layout.getEditorArea();

        layout.setFixed(false);

        layout.addPlaceholder(PgenUtil.VIEW_ID, IPageLayout.LEFT, 0.15f, refId);
        layout.addPlaceholder("gov.noaa.nws.ncep.ui.nsharp", IPageLayout.LEFT, 0.15f, refId);
        
        layout.addActionSet("gov.noaa.nws.ncep.viz.ui.personalities.NCActionSet");

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.IVizPerspectiveFactory#getEditorContributions(com.raytheon.viz.core.IDisplayPaneContainer,
     *      com.raytheon.viz.core.IDisplayPane)
     */
    // [TO10 HOLD] @Override
    // [TO10 HOLD] Following is no longer in IVizPerspectiveFactory as of TO10 --
    // [TO10 HOLD] need to understand the implications of this
    public AbstractRightClickAction[] getEditorContributions(
            IDisplayPaneContainer container, IDisplayPane pane) {
 //       zoomMenuAction.setContainer(container);
 //       return new AbstractRightClickAction[] { zoomMenuAction };
    	return null;
    }

}
