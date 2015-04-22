package gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources;

import gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources.ManageResourceControl.IEditResourceComposite;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup.RscAndGroupName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.internal.misc.StringMatcher;

import com.raytheon.uf.viz.core.exception.VizException;


/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09/10		  #273		Greg Hull	 Created
 * 07/22/11       #450      Greg Hull    Save to User Localization
 * 06/03/12       #816      Greg Hull    Add Filter for attr set list
 * 06/07/12       #816      Greg Hull    Add ability to apply to other resources
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
class EditAttrSetGroupComp extends Composite implements IEditResourceComposite {
	ResourceDefnsMngr rscDefnMngr;
	ManageResourceControl mngrControl; // the parent composite 

	ResourceName       seldRscName=null;
	ResourceDefinition seldRscDefn;
	AttrSetGroup       seldAttrSetGroup;
	
	Text attrSetGroupNameTxt;

	Text       resourceTxt;
	ListViewer applyForRscsLViewer;
	Button     applyForAllRscsBtn;
	
	Text       filterTxt;
	Button     addAttrSetBtn;
	Button     removeAttrSetBtn;
	ListViewer availAttrSetsLViewer;
	ListViewer seldAttrSetsLViewer;
	
	Button     saveAttrSetGroupBtn;
	Button     newAttrSetGroupBtn;
	Button     cancelBtn;
	
	ArrayList<String> availRscsForGroup;
	ArrayList<String> availAttrSets;
	ArrayList<String> seldAttrSets;
		
	// used for all 3 of the ListViewers
	IStructuredContentProvider stringArrayListContentProvider = new IStructuredContentProvider() {
		@Override
		public Object[] getElements(Object inputElement) {				
			ArrayList<String> stringArrayList = (ArrayList<String>) inputElement;
			return stringArrayList.toArray();			
		}
		@Override
		public void dispose() { }

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) { } 
	};
	
	
	protected class AttrSetViewerFilter extends ViewerFilter {

		private StringMatcher filterMatcher = null;
		
		@Override
		public boolean select(Viewer viewer, Object parentElement, Object element) {
	
			// We could use the match() method but I think the find() will
			// be more user-friendly
//			return (filterMatcher == null ? true : filterMatcher.match( (String)element ) );
			String elemStr = (String)element;
			return (filterMatcher == null ? true : 
				    filterMatcher.find( elemStr, 0, elemStr.length() ) != null );
		}
		
		public void setFilterMatcher( StringMatcher sm ) {
			filterMatcher = sm;
		}
	}
	
	private AttrSetViewerFilter attrSetFilter = new AttrSetViewerFilter();

	
	public EditAttrSetGroupComp( Composite parent, int style, ManageResourceControl mgrCtl ) {
		super( parent, style );
		Composite top_form = this;      

		FormData fd = new FormData();
    	fd.top = new FormAttachment( 0, 12 );   // offset so the title shows up
    	fd.left = new FormAttachment( 0, 0 );
    	fd.right = new FormAttachment( 100, 0 );
    	fd.bottom = new FormAttachment( 100, 0 );
    	top_form.setLayoutData(fd);

		setLayoutData( fd );

		top_form.setLayout( new FormLayout() );

		mngrControl = mgrCtl;
		rscDefnMngr = mngrControl.getRscDefnMngr();
		
		attrSetGroupNameTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER );
		attrSetGroupNameTxt.setText("");		

    	fd = new FormData();
    	fd.width = 120;
    	fd.top = new FormAttachment( 0, 25 );
    	fd.left = new FormAttachment( 0, 15 );
    	attrSetGroupNameTxt.setLayoutData( fd );

		Label attrSetGroupNameLbl = new Label( top_form, SWT.NONE );
		attrSetGroupNameLbl.setText("Group Name");
		fd = new FormData();
    	fd.bottom = new FormAttachment( attrSetGroupNameTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( attrSetGroupNameTxt, 0, SWT.LEFT );
    	attrSetGroupNameLbl.setLayoutData( fd );

		resourceTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY );
		resourceTxt.setText("");		
		resourceTxt.setBackground( getParent().getBackground() ); // indicate readonly
//		resourceTxt.setVisible( false );
		
    	fd = new FormData();
    	fd.width = 120;
    	fd.top = new FormAttachment( attrSetGroupNameTxt, 35, SWT.BOTTOM );
    	fd.left = new FormAttachment( attrSetGroupNameTxt, 0, SWT.LEFT );
    	resourceTxt.setLayoutData( fd );
    	
    	resourceTxt.setVisible( false );
    	 
    	Label applyToLbl = new Label( top_form, SWT.NONE );
    	applyToLbl.setText("Applies For:" );
    	fd = new FormData();
    	fd.bottom = new FormAttachment( resourceTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( resourceTxt, 0, SWT.LEFT );
    	applyToLbl.setLayoutData( fd );


    	applyForRscsLViewer = new ListViewer( top_form, 
    			SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData(120, 300);
    	fd.width = 120;
    	fd.top = new FormAttachment( attrSetGroupNameTxt, 35, SWT.BOTTOM );
    	fd.left = new FormAttachment( attrSetGroupNameTxt, 0, SWT.LEFT );
    	fd.bottom = new FormAttachment( 100, -95 );
    	applyForRscsLViewer.getList().setLayoutData(fd);
    	applyForRscsLViewer.getList().setToolTipText("Use <Control> to Multi-Select or <Shift> to select a group.");
    	
//    	applyForRscsLViewer.getList().setVisible( false );
    	
    	applyForAllRscsBtn = new Button( top_form, SWT.PUSH );
    	applyForAllRscsBtn.setText("Apply for All");
		fd = new FormData();
    	fd.top = new FormAttachment( applyForRscsLViewer.getList(), 10, SWT.BOTTOM );
    	fd.left = new FormAttachment( applyForRscsLViewer.getList(), -50, SWT.CENTER );
    	applyForAllRscsBtn.setLayoutData( fd );

//    	applyForAllRscsBtn.setVisible( false );
    	
    	Label filterLbl = new Label( top_form, SWT.NONE );
    	filterLbl.setText("Filter Attribute Sets" );
    	fd = new FormData();
    	fd.left = new FormAttachment( 26, 0 );
    	fd.top = new FormAttachment( 0, 22 );
    	filterLbl.setLayoutData( fd );


    	filterTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER );
    	filterTxt.setToolTipText("Enter a regular expression using '*' and '?'.");
    	
    	fd = new FormData();
    	fd.width = 115;
    	fd.top = new FormAttachment( filterLbl, -2, SWT.TOP );
    	fd.left = new FormAttachment( filterLbl, 5, SWT.RIGHT );
    	filterTxt.setLayoutData( fd );

    	Button filterBtn = new Button( top_form, SWT.PUSH );
    	filterBtn.setText(" Filter ");
		fd = new FormData();
    	fd.top = new FormAttachment(  filterTxt, 0, SWT.TOP );
    	fd.right = new FormAttachment( filterTxt,  -20, SWT.LEFT );
    	filterBtn.setLayoutData( fd );
    	filterBtn.setVisible( false );
    	
    	availAttrSetsLViewer = new ListViewer( top_form, 
				   SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData();//100,200);
    	fd.top = new FormAttachment( filterTxt, 35, SWT.BOTTOM );
    	fd.left = new FormAttachment( 26, 0 );
    	fd.right = new FormAttachment( 61, 0 );
    	fd.bottom = new FormAttachment( 100, -95 );
    	availAttrSetsLViewer.getList().setLayoutData(fd);
    	availAttrSetsLViewer.getList().setToolTipText("Use <Control> to Multi-Select or <Shift> to select a group.");

		Label addAttrSetsLbl = new Label( top_form, SWT.NONE );
		addAttrSetsLbl.setText("Available Attribute Sets");
		fd = new FormData();
    	fd.bottom = new FormAttachment( availAttrSetsLViewer.getList(), -3, SWT.TOP );
    	fd.left = new FormAttachment( availAttrSetsLViewer.getList(), 0, SWT.LEFT );
    	addAttrSetsLbl.setLayoutData( fd );

    	
    	seldAttrSetsLViewer = new ListViewer( top_form, 
    			SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData();
    	fd.top = new FormAttachment( availAttrSetsLViewer.getList(), 0, SWT.TOP );
    	fd.left = new FormAttachment( 63, 0 );
    	fd.right = new FormAttachment( 98, 0 );
    	fd.bottom = new FormAttachment( availAttrSetsLViewer.getList(), 0, SWT.BOTTOM );
    	seldAttrSetsLViewer.getList().setLayoutData(fd);
//    	seldAttrSetsLViewer.getList().setToolTipText("Use <Control> to Multi-Select or <Shift> to select a group.");

    	Label seldAttrSetsLbl = new Label( top_form, SWT.NONE );
    	seldAttrSetsLbl.setText("Selected Attribute Sets In Group");
    	fd = new FormData();
    	fd.bottom = new FormAttachment( seldAttrSetsLViewer.getList(), -3, SWT.TOP );
    	fd.left = new FormAttachment( seldAttrSetsLViewer.getList(), 0, SWT.LEFT );
    	seldAttrSetsLbl.setLayoutData( fd );

    	addAttrSetBtn = new Button( top_form, SWT.PUSH );
    	addAttrSetBtn.setText("Add ->");
		fd = new FormData();
		fd.width = 100;
    	fd.top = new FormAttachment( availAttrSetsLViewer.getList(), 10, SWT.BOTTOM );
    	fd.left = new FormAttachment( availAttrSetsLViewer.getList(), -50, SWT.CENTER );
    	addAttrSetBtn.setLayoutData( fd );

    	removeAttrSetBtn = new Button( top_form, SWT.PUSH );
    	removeAttrSetBtn.setText("<- Remove");
		fd = new FormData();
		fd.width = 100;
    	fd.top = new FormAttachment( seldAttrSetsLViewer.getList(), 10, SWT.BOTTOM );
    	fd.left = new FormAttachment( seldAttrSetsLViewer.getList(), -50, SWT.CENTER );
    	removeAttrSetBtn.setLayoutData( fd );
    	
       	saveAttrSetGroupBtn = new Button( top_form, SWT.PUSH );
    	saveAttrSetGroupBtn.setText("Save");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( 100, -30 );
    	saveAttrSetGroupBtn.setLayoutData( fd );
    	
    	newAttrSetGroupBtn = new Button( top_form, SWT.PUSH );
    	newAttrSetGroupBtn.setText("Create");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( 100, -30 );
    	newAttrSetGroupBtn.setLayoutData( fd );

    	cancelBtn = new Button( top_form, SWT.PUSH );
    	cancelBtn.setText( "Cancel");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( saveAttrSetGroupBtn, -20, SWT.LEFT );
    	cancelBtn.setLayoutData( fd );
    	
    	applyForRscsLViewer.setContentProvider( stringArrayListContentProvider );

    	availAttrSetsLViewer.setContentProvider( stringArrayListContentProvider );
    	seldAttrSetsLViewer.setContentProvider( stringArrayListContentProvider );

    	applyForRscsLViewer.setInput( availRscsForGroup );
    	availAttrSetsLViewer.setInput( availAttrSets );
    	seldAttrSetsLViewer.setInput( seldAttrSets );

    	applyForRscsLViewer.setComparator( new ViewerComparator() {    		
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
            	if( seldRscName.getRscType().equals( (String)e1 ) ) {
            		return -1;
            	}
            	else if( seldRscName.getRscType().equals( (String)e2 ) ) {
            		return 1;
            	}
            	else {
            		return ((String)e1).compareTo( (String)e2 );
            	}
            }
    	});
    	
    	// check to see if there is already an ASG for this resource with this name
    	// and prompt the user if they want to replace it
    	applyForRscsLViewer.addSelectionChangedListener( new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				// Can't remove the currently selected ASG from the 'active' resource.
				// This is basically removing the ASG. 
				applyForRscsLViewer.getList().select(0);				
				//StructuredSelection sel = (StructuredSelection) event.getSelection();				
//				StructuredSelection sel = (StructuredSelection)applyForRscsLViewer.getSelection();
//            	Iterator selIter = sel.iterator();
//
//            	while( selIter.hasNext() ) {
//            		String selRsc = (String)selIter.next();
//            		AttrSetGroup
//            	}
			}
    	});
    	
    	availAttrSetsLViewer.setComparator( new ViewerComparator() {
    		
    		// TODO : implement this if we want to group definitions according to 
    		// some meaningful category....
    	    public int category(Object element) {
     	        return super.category(element);
    	    }

            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
            	return ((String)e1).compareTo( (String)e2 );
            }
    	});

    	// just use the same one since they do the same thing (unless we implemented the category)
    	seldAttrSetsLViewer.setComparator( availAttrSetsLViewer.getComparator() );
    	
		attrSetGroupNameTxt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {				
				String newTextStr = attrSetGroupNameTxt.getText().trim();
				
				if( newTextStr.isEmpty() ) {
					saveAttrSetGroupBtn.setEnabled( false );
					saveAttrSetGroupBtn.setEnabled( false );
				}
				else {
					if( seldRscDefn != null ) {
						saveAttrSetGroupBtn.setEnabled( true );

						// if the name has been changed, the 'save' button acts as a 'Rename' or Save As
						//					
						// disable the New button if the name hasn't been changed.
						//
						if( seldAttrSetGroup == null ) {
							saveAttrSetGroupBtn.setEnabled( false );
							newAttrSetGroupBtn.setEnabled( false );							
						}
						else if( seldAttrSetGroup.getAttrSetGroupName().equals( newTextStr ) ) {
							saveAttrSetGroupBtn.setEnabled( true );
							saveAttrSetGroupBtn.setText("Save" );

							newAttrSetGroupBtn.setEnabled( false );
						}
						else {
							saveAttrSetGroupBtn.setEnabled( true );
							saveAttrSetGroupBtn.setText("Save As" );
							newAttrSetGroupBtn.setEnabled( true );

							// disable the Save button if the new name already exists 
							if( rscDefnMngr.getAttrSetGroupNamesForResource( 
									           seldRscDefn.getResourceDefnName() ).contains( 
									        		   attrSetGroupNameTxt.getText().trim() ) ) {								
								saveAttrSetGroupBtn.setEnabled( false );
							}
						}
					}
				}
			}
    	});		
		
		filterTxt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				String filterExpr = filterTxt.getText().trim();
				try {
					attrSetFilter.setFilterMatcher( 
							new StringMatcher( filterExpr, true, false ) );
					
	        		availAttrSetsLViewer.refresh();
				}
				catch( IllegalArgumentException iaex ) {
					System.out.println("StringMatcherSyntaxError: "+filterExpr );
				}
			}
		});
		
		ViewerFilter vFilters[] = new ViewerFilter[]{ attrSetFilter };
		
		availAttrSetsLViewer.setFilters( vFilters );		
		
		applyForAllRscsBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		applyForRscsLViewer.getList().selectAll();
        	}
		});
		
//		filterBtn.addSelectionListener( new SelectionAdapter() {
//        	public void widgetSelected( SelectionEvent ev ) {        		
//        		availAttrSetsLViewer.refresh();
//        	}
//		});

		saveAttrSetGroupBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		saveAttrSetGroup( );
        	}
		});

		newAttrSetGroupBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		saveAttrSetGroup( );
        	}
		});
		
		cancelBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		mngrControl.editActionCanceled();
        	}
		});
    	

    	addAttrSetBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		// get the selected attr sets and move them to the seld list        		
            	StructuredSelection attrSetSels = (StructuredSelection)availAttrSetsLViewer.getSelection();
            	Iterator selIter = attrSetSels.iterator();

            	while( selIter.hasNext() ) {
            		String attrSet = (String)selIter.next();
            		availAttrSets.remove( attrSet );
            		seldAttrSets.add( attrSet );
            	}

            	availAttrSetsLViewer.setInput(availAttrSets);
            	seldAttrSetsLViewer.setInput(seldAttrSets);
            	availAttrSetsLViewer.refresh();
            	seldAttrSetsLViewer.refresh();
        	}
		});
    	
    	availAttrSetsLViewer.getList().addListener(SWT.MouseDoubleClick, new Listener() {
			public void handleEvent(Event event) {
            	StructuredSelection attrSetSels = 
            		 (StructuredSelection)availAttrSetsLViewer.getSelection();
				String attrSet = (String)attrSetSels.getFirstElement();
				availAttrSets.remove( attrSet );
				seldAttrSets.add( attrSet );

            	availAttrSetsLViewer.setInput(availAttrSets);
            	seldAttrSetsLViewer.setInput(seldAttrSets);
            	availAttrSetsLViewer.refresh();
            	seldAttrSetsLViewer.refresh();
			}
       	});

    	removeAttrSetBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		// get the selected attr sets and move them to the seld list        		
            	StructuredSelection attrSetSels = (StructuredSelection)seldAttrSetsLViewer.getSelection();
            	Iterator selIter = attrSetSels.iterator();

            	while( selIter.hasNext() ) {
            		String attrSet = (String)selIter.next();
            		seldAttrSets.remove( attrSet );
            		availAttrSets.add( attrSet );
            	}

            	availAttrSetsLViewer.setInput(availAttrSets);
            	seldAttrSetsLViewer.setInput(seldAttrSets);
            	availAttrSetsLViewer.refresh();
            	seldAttrSetsLViewer.refresh();
        	}
		});

    	seldAttrSetsLViewer.getList().addListener(SWT.MouseDoubleClick, new Listener() {
			public void handleEvent(Event event) {
            	StructuredSelection attrSetSels = 
            		(StructuredSelection)seldAttrSetsLViewer.getSelection();
            	String attrSet = (String)attrSetSels.getFirstElement();
            	seldAttrSets.remove( attrSet );
            	availAttrSets.add( attrSet );

            	availAttrSetsLViewer.setInput(availAttrSets);
            	seldAttrSetsLViewer.setInput(seldAttrSets);
            	availAttrSetsLViewer.refresh();
            	seldAttrSetsLViewer.refresh();
			}
       	});
	}
	

	@Override
	public void activate() {
		setVisible( true );
		if( getParent() instanceof Group ) {
			((Group)getParent()).setText( getTitle() );
		}
	}
	
	public void copySelectedResource( ResourceName rscName ) {
		setSelectedResource( rscName );
		newAttrSetGroupBtn.setVisible( true );
		saveAttrSetGroupBtn.setVisible( false );
		attrSetGroupNameTxt.setEditable( true );
		attrSetGroupNameTxt.setBackground( availAttrSetsLViewer.getList().getBackground() );
		
		attrSetGroupNameTxt.setText( "CopyOf"+attrSetGroupNameTxt.getText() );
		attrSetGroupNameTxt.setSelection(0, attrSetGroupNameTxt.getText().length() );
		attrSetGroupNameTxt.setFocus();
	}
	
	public void editSelectedResource( ResourceName rscName ) {
		setSelectedResource( rscName );
		newAttrSetGroupBtn.setVisible( false );
		saveAttrSetGroupBtn.setVisible( true );
		
		attrSetGroupNameTxt.setEditable( false );
		attrSetGroupNameTxt.setBackground( getParent().getBackground() );
	}

	public void setSelectedResource( ResourceName rscName ) {

		seldRscName = rscName;
		
		attrSetGroupNameTxt.setEditable( false );
		
		if( seldRscName.getRscGroup().isEmpty() ) {
			attrSetGroupNameTxt.setText( "" );
			return;
		}
		
		attrSetGroupNameTxt.setText( seldRscName.getRscGroup() );

		seldRscDefn = rscDefnMngr.getResourceDefinition( seldRscName );//new ResourceDefinition( rscDefnMngr.getResourceDefinition( seldRscName ) );
		
		if( !seldRscDefn.applyAttrSetGroups() ) {

		}
		
		seldAttrSetGroup = rscDefnMngr.getAttrSetGroupForResource( seldRscName );
			
		if( seldAttrSetGroup == null ) {
			System.out.println("sanity check: can't find AttrSetGroup for "+seldRscName.toString() );
			return;
		}
		
		resourceTxt.setText( seldRscName.getRscType() );
		
		filterTxt.setText("");
		
		availRscsForGroup = new ArrayList<String>( 
				rscDefnMngr.getRscTypesForRscImplementation( seldRscDefn.getRscImplementation() ) );
		
		seldAttrSets = new ArrayList<String>();
		seldAttrSets.addAll( seldAttrSetGroup.getAttrSetNames() );
		
		availAttrSets = new ArrayList<String>();
		availAttrSets.addAll( rscDefnMngr.getAvailAttrSetsForRscImpl( 
								seldRscDefn.getRscImplementation() ) );

		for( String as : seldAttrSets ) {
			availAttrSets.remove( as );				
		}
			
    	applyForRscsLViewer.setInput( availRscsForGroup );
    	
    	// select the first entry in the list. This will be 
    	// seldRscName since it will be sorted to be at the top.
    	//
    	applyForRscsLViewer.getList().select(0);
//    	for( int r=0 ; r<availRscsForGroup.size() ; r++ ) {
//    		if( seldRscName.getRscType().equals( availRscsForGroup.get(r) ) ) {
//    			applyForRscsLViewer.getList().select( r );
////    			applyForRscsLViewer.getList().se
//    		}
//    	}
//    	if( applyForRscsLViewer.getList().getSelectionCount() == 0 ) {
//    		System.out.println("sanity check: No applicable resources selected????");
//    	}
    	
    	applyForRscsLViewer.refresh();
    	
    	availAttrSetsLViewer.setInput( availAttrSets );
    	seldAttrSetsLViewer.setInput( seldAttrSets );
		availAttrSetsLViewer.refresh();
		seldAttrSetsLViewer.refresh();
	}
	
	@Override
	public ResourceName getSelectedResourceName( ) {
		return seldRscName;
	}
	
	// TODO : not implemented
	@Override
	public boolean isModified( ) {
		return false; 
	}
	

	@Override
	public void deactivate() {
		setVisible( false );
		// good to clear out all of the selections even if they can't be seen?
	}

	@Override
	public String getTitle() {
		return "Edit Attribute Set Group";			
	}
	
	
	private void saveAttrSetGroup( ) {
		try {
			String attrSetGroupName = attrSetGroupNameTxt.getText().trim();

			List<String> applyToRscsList = 
				Arrays.asList( applyForRscsLViewer.getList().getSelection() );		

// If the user want to do this then they need to Remove the ASG explicitly (if in the USER level)
//			 if we are removing this group from the currently selected resource
//					if( !applyToRscsList.contains( seldAttrSetGroup.getResource() ) ) {
//						MessageDialog confirmDlg = new MessageDialog( getShell(), "Confirm", null, 
//								"Are you sure you want to remove this Attribute Set Group for.\n\n"+
//								seldAttrSetGroup.getResource(), MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
//						confirmDlg.open();//			
//					}
			AttrSetGroup asg = null;
			
			for( String rscName : applyToRscsList ) {

				// create the new AttrSetGroup from the GUI selections.
				AttrSetGroup newAttrSetGroup = 
					rscDefnMngr.getAttrSetGroupForResource( new RscAndGroupName( rscName, attrSetGroupName ) );

				if( newAttrSetGroup == null ) {
					newAttrSetGroup = new AttrSetGroup();

					newAttrSetGroup.setAttrSetGroupName( attrSetGroupName );
					newAttrSetGroup.setResource( rscName );
				}
				else {
					// if adding this asg for another resource, first check to see if the resource
					// already has an asg with this name and if so confirm that they really want 
					// to override it.
					if( !seldAttrSetGroup.getResource().equals( rscName ) ) {
						MessageDialog confirmDlg = new MessageDialog( getShell(), 
								"Confirm", null, 
								"The Attribute Set Group " +attrSetGroupName + " for resource "+
								rscName + " already exists.\n\n"+
								"Are you sure you want to override it?",
								MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
						confirmDlg.open();

						if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
							continue;
						}
					}
				}

				newAttrSetGroup.removeAllAttrSets();

				for( String asName : seldAttrSets ) {
					newAttrSetGroup.addAttrSetName( asName );
				}

				rscDefnMngr.saveAttrSetGroup(  newAttrSetGroup );	    		
			}

			ResourceName newSeldRscName = new ResourceName();
			newSeldRscName.setRscCategory( seldRscDefn.getResourceCategory() );
			newSeldRscName.setRscType( seldRscDefn.getResourceDefnName() );		
			newSeldRscName.setRscGroup( attrSetGroupName );

			mngrControl.updateResourceSelections( newSeldRscName );

			String msgStr = "Saved Attribute Set Group " + attrSetGroupName+".";

			MessageDialog saveMsgDlg = new MessageDialog( getShell(), 
					"Done", null, msgStr+"\n\n",
					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
			saveMsgDlg.open();

			return;
		}
		catch( VizException ve ) {
			MessageDialog errDlg = new MessageDialog( getShell(), 
					"Error", null, 
					"Error Saving AttrSetGroup:\n"+ve.getMessage()+"\n"+ve.getCause(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
		}
	}
}