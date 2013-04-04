package gov.noaa.nws.ncep.viz.resourceManager.ui.loadRbd;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;

import gov.noaa.nws.ncep.viz.resources.manager.RbdBundle;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.PaneID;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea.AreaSource;

import java.util.ArrayList;

import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;

/**
 *  
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/26/12      #568       G. Hull     Created to replace separate code in 
 * 										LoadControl, SelectRbd and ManageSpfControl
 * 12/01/12      #630       G. Hull     Show Area based on Source (Resource, Predefined...)
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class RbdViewComposite extends Composite {

	private ListViewer rscLviewer = null;
	private Label      rbdNameLabel = null;
	private Label      rbdLocationLabel = null;
	private Boolean    viewSelectedPane = false;

	public RbdViewComposite( Composite parent ) {
		super( parent, SWT.SHADOW_NONE  );
				
        Composite topComp = this;
        
		topComp.setLayout( new FormLayout() );

		rscLviewer = new ListViewer( topComp, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
      	
		FormData fd = new FormData();
        fd.top = new FormAttachment( 0, 20 );
        fd.left  = new FormAttachment(0, 0);
        fd.right = new FormAttachment( 100, 0 );
        fd.bottom = new FormAttachment( 100, -20 );
        rscLviewer.getList().setLayoutData( fd );
        
        // do this as an indication that the list is view-only
        rscLviewer.getList().setBackground( parent.getBackground() );

        rbdNameLabel = new Label( topComp, SWT.NONE);
        rbdNameLabel.setText("View RBD");
       	fd = new FormData();
        fd.bottom  = new FormAttachment( rscLviewer.getList(), -3, SWT.TOP );
        fd.left  = new FormAttachment( rscLviewer.getList(), 0, SWT.LEFT );
        fd.right  = new FormAttachment( rscLviewer.getList(), 0, SWT.RIGHT );
        rbdNameLabel.setLayoutData( fd );

        rbdLocationLabel = new Label( topComp, SWT.NONE);
        rbdLocationLabel.setText("");
       	fd = new FormData();
        fd.top  = new FormAttachment( rscLviewer.getList(), 3, SWT.BOTTOM );
        fd.left  = new FormAttachment( rscLviewer.getList(), 0, SWT.LEFT );
        fd.right  = new FormAttachment( rscLviewer.getList(), 0, SWT.RIGHT );
        rbdLocationLabel.setLayoutData( fd );

        rscLviewer.setContentProvider( new IStructuredContentProvider() {
			@Override
			public Object[] getElements(Object inputElement) {
				RbdBundle selRbd = (RbdBundle)inputElement;
				if( selRbd == null ) {
					return new String[0];
				}
				
				ArrayList<String> rscNames = new ArrayList<String>();
				boolean isMultiPane = (selRbd.getPaneLayout().getNumberOfPanes() > 1 );
			
				// loop thru all of the resources in all of the panes and create the
				// list of items for the viewer. These will depend on whether we are
				// importing a single pane (in which case only the selected pane's 
				// resources are displayed) and whole RBDs (in which case all pane's
				// resources are displayed) For multi-pane RBDs the format will include
				// the name of the pane.
				for( int r=0 ; r<selRbd.getPaneLayout().getRows() ; r++ ) {
					for( int c=0 ; c<selRbd.getPaneLayout().getColumns() ; c++ ) {
						PaneID paneId = new PaneID(r,c)	;
						
						NCMapRenderableDisplay disp = selRbd.getDisplayPane(paneId);
						AbstractDescriptor mapDescr = (AbstractDescriptor)disp.getDescriptor();

						boolean showPane = (!viewSelectedPane || 
										    (viewSelectedPane && (paneId.compare( selRbd.getSelectedPaneId() ) == 0)));
					
						// show this pane only if its a single pane or if this is the 
						if( showPane ) {
					
							if( isMultiPane ) {
								rscNames.add( "Pane ("+ paneId.toString() +")" );
							}
							else { 
								rscNames.add( "Single Pane" );
							}
							
							// TODO: show the actual center/proj/zoomLevel???
							PredefinedArea area = disp.getInitialArea();
							
							if( area == null ) {
								rscNames.add("Unspecified initial Area");
							}
							else if( area.getAreaSource() == AreaSource.PREDEFINED_AREA ) {
								rscNames.add("Predefined Area "+area.getAreaName() );
							}
							else {
								rscNames.add("Area From "+area.getAreaName() );
							}
							
							for( ResourcePair rp : mapDescr.getResourceList() ) {
								if( rp.getResourceData() instanceof INatlCntrsResourceData ) {
									INatlCntrsResourceData ncRsc = 
										(INatlCntrsResourceData)rp.getResourceData();
									String rscName = ncRsc.getResourceName().toString();

									if( ncRsc.getIsEdited() ) {
										rscName = rscName + "(E)";
									}
	//								if( ncRsc instanceof AbstractNatlCntrsRequestableResourceData &&
	//										((AbstractNatlCntrsRequestableResourceData)ncRsc).getIsDominant() ) {
	//									qualRscName = qualRscName + "(D)";
	//								}

									if( isMultiPane ) {
										rscName = "   " + rscName;
									}

									rscNames.add( rscName );
								}
							}
						}
					}
				}
		    	return rscNames.toArray();
			}
			@Override
			public void dispose() { }

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			}
        });    

        // make this View-only
        rscLviewer.addSelectionChangedListener(new ISelectionChangedListener() {
       		public void selectionChanged(SelectionChangedEvent event) {
       			rscLviewer.getList().deselectAll();
            } 
       	});     
        
        rscLviewer.addDoubleClickListener( new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
				rscLviewer.getList().deselectAll();				
			}
   		});

	}
	
	public void viewSelectedPane() {
		viewSelectedPane = true;
	}
	
	public void viewRbd( RbdBundle rbd ) {
		
		if( rbd == null ) {
			rbdNameLabel.setText( "" );
			rbdLocationLabel.setText("");
		}
		else {
			rbdNameLabel.setText( "RBD: " + rbd.getRbdName() );

			LocalizationFile lFile = rbd.getLocalizationFile();
			if( lFile == null ) {
				rbdLocationLabel.setText("");
			}
			else {
				rbdLocationLabel.setText( "Localization="+
				   lFile.getContext().getLocalizationLevel().toString()+":"+
				   lFile.getContext().getContextName() );
			}
		}
		
		rscLviewer.setInput( rbd );
		rscLviewer.refresh();
	}

	public void refresh() {
		rscLviewer.refresh( true );
	}
}
