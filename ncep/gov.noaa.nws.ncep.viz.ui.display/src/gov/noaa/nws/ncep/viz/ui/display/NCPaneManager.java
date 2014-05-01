package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.display.INcPaneLayout;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * Natl Cntrs extention of PaneManager.
 * 
 * Note that this uses a slightly different method of selecting panes.
 * IPaneManager allows for different kind of pane selections (ie actions of
 * LOAD, IMAGE, ....) but one one may be selected for each action.)
 * NCPaneManager ignores the action but will allow for more than one pane to be
 * selected at one time. selectPane() and deselectPane() should be called
 * instead of setSelectedPane().
 * 
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer     Description 
 * ------------ ----------  -----------  -------------------------- 
 * 03/07/11      R1G2-9 	Greg Hull 	 Created
 * 07/18/12      #649       Shova Gurung Fixed echo/virtual cursor display issue.
 * 09/13/12			?		B. Yin		 Refresh only for multiple panes
 * 01/28/12      #972       Greg Hull    moved all but paneLayout to AbstractNcPaneManager.
 * 
 * </pre>
 * 
 * @author ghull
 * 
 */
public class NCPaneManager extends AbstractNcPaneManager implements IInputHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NCPaneManager.class);
    
    public NCPaneManager( INcPaneLayout playout, NcDisplayType dispType ) {
    	// must be an NcPaneLayout
    	super( playout, dispType );
    }

    // called from AbstractEditor.createPartControl
    @Override
    public void initializeComponents(IDisplayPaneContainer container,
            Composite parent) {
        // super.initializeComponents( container, parent );
    	//
    	if (!(paneContainer instanceof AbstractNcEditor)) {

        }
        
        paneContainer = container;
        GridLayout gl = new GridLayout( ((NcPaneLayout)paneLayout).getColumns(), true);
        gl.horizontalSpacing = 3;
        gl.verticalSpacing = 3;
        gl.marginHeight = 0;
        gl.marginWidth = 0;

        composite = parent;
        composite.setLayout(gl);
        
        // Enable the inspect adapters
        // handles the VirtualCursor and selecting the panes
        if (paneContainer instanceof AbstractNcEditor) {
            inputManager.registerMouseHandler( new NcPaneMouseHandler(
                    (AbstractNcEditor) paneContainer), InputPriority.PERSPECTIVE);
        }

        // create the Composites for the panes ahead of time be
        // Composite canvasComp = new Composite(composite, SWT.NONE);
        // GridLayout gl = new GridLayout(1, false);
        // gl.marginHeight = 0;
        // gl.marginWidth = 0;
        // canvasComp.setLayout(gl);
        // canvasComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
        // true));

        // final AbstractEditor editor = this;
        //
        // displayPanes = new ArrayList<NCDisplayPane>();
        // displayPaneMap = new HashMap<String, NCDisplayPane>();
        // selectedPanes = new HashMap<NcPaneID,NCDisplayPane>();

        // for (int r = 0; r < paneLayout.getRow(); r++) {
        // for (int c = 0; c < paneLayout.getColumn(); c++) {
        // NcPaneID paneId = new NcPaneID(r, c);
        //
        // NCMapRenderableDisplay display = displaysToLoad.get(paneId
        // .toString());
        //
        // editorOwnDisplay = display;
        // addPane(paneId, display);
        // }
        // }
        //
        // activatedPane = displayPaneMap.get(new NcPaneID(0, 0).toString());
        // currentMouseHoverPane = displayPaneMap.get(new NcPaneID(0,
        // 0).toString());
        //
        // // if there is more
        // if (displayPaneMap.size() > 0) {
        // selectPane(displayPaneMap.get(new NcPaneID(0, 0).toString()), true);
        // }

        //
        composite.addListener(SWT.Resize, new Listener() {
            private boolean waiting = false;

            @Override
            public void handleEvent(Event event) {
                // PaneManager includes code to adjust the paneLayout here .....
            }
        });

        displayPanes.clear();
    }    
}
