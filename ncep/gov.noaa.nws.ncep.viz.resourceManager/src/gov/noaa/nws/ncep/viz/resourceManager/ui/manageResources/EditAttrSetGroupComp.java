package gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources.ManageResourceControl.IEditResourceComposite;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimeMatchMethod;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;


/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09/10		  #273		Greg Hull	 Created
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
//	Button     addApplicableRscBtn;
//	Button     removeApplicableRscBtn;
//	ListViewer availRscsLViewer;
//	ListViewer applyForRscsLViewer;

	Button     addAttrSetBtn;
	Button     removeAttrSetBtn;
	ListViewer availAttrSetsLViewer;
	ListViewer seldAttrSetsLViewer;
	
	Button     saveAttrSetGroupBtn;
	Button     newAttrSetGroupBtn;
	Button     cancelBtn;
	
	ArrayList<String> availAttrSets;
	ArrayList<String> seldAttrSets;
	
//	ArrayList<String> availResourcesList;
//	ArrayList<String> seldResourcesList;
	// a saved copy of seldResourcesList to determine new selections
//	ArrayList<String> origSeldResourcesList;
	
	// used for all 4 of the ListViewers
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
    	fd.width = 140;
    	fd.top = new FormAttachment( 0, 35 );
    	fd.left = new FormAttachment( 0, 15 );
    	attrSetGroupNameTxt.setLayoutData( fd );

		Label attrSetGroupNameLbl = new Label( top_form, SWT.NONE );
		attrSetGroupNameLbl.setText("Attribute Set Group Name");
		fd = new FormData();
    	fd.bottom = new FormAttachment( attrSetGroupNameTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( attrSetGroupNameTxt, 0, SWT.LEFT );
    	attrSetGroupNameLbl.setLayoutData( fd );

    	
		resourceTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY );
		resourceTxt.setText("");		
		resourceTxt.setBackground( getParent().getBackground() ); // indicate readonly
		
    	fd = new FormData();
    	fd.width = 140;
    	fd.top = new FormAttachment( attrSetGroupNameTxt, 35, SWT.BOTTOM );
    	fd.left = new FormAttachment( attrSetGroupNameTxt, 0, SWT.LEFT );
    	resourceTxt.setLayoutData( fd );

    	
    	Label applyToLbl = new Label( top_form, SWT.NONE );
    	applyToLbl.setText("Applies To:" );
    	fd = new FormData();
    	fd.bottom = new FormAttachment( resourceTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( resourceTxt, 0, SWT.LEFT );
    	applyToLbl.setLayoutData( fd );


//    	availRscsLViewer = new ListViewer( top_form, 
//    			SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
//    	fd = new FormData(80, 120);
//    	fd.top = new FormAttachment( applyToLbl, 35, SWT.BOTTOM );
//    	fd.left = new FormAttachment( applyToLbl, 0, SWT.LEFT );
//    	fd.right = new FormAttachment( 17, 0 );
//      	fd.bottom = new FormAttachment( 100, -95 );
//    	availRscsLViewer.getList().setLayoutData(fd);
//
//    	Label availRscsLbl = new Label( top_form, SWT.NONE );
//    	availRscsLbl.setText("Available" );
//    	fd = new FormData();
//    	fd.bottom = new FormAttachment( availRscsLViewer.getList(), -3, SWT.TOP );
//    	fd.left = new FormAttachment( availRscsLViewer.getList(), 0, SWT.LEFT );
//    	availRscsLbl.setLayoutData( fd );
//
//    	
//    	applyForRscsLViewer = new ListViewer( top_form, 
//    			SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
//    	fd = new FormData();
//    	fd.top = new FormAttachment( availRscsLViewer.getList(), 0, SWT.TOP );
//    	fd.left = new FormAttachment( 20, 0 );
//    	fd.right = new FormAttachment( 35, 0 );
//    	fd.bottom = new FormAttachment( availRscsLViewer.getList(), 0, SWT.BOTTOM );
//    	applyForRscsLViewer.getList().setLayoutData(fd);
//
//    	Label applyForResourceTypeLbl = new Label( top_form, SWT.NONE );
//    	applyForResourceTypeLbl.setText("Selected" );
//    	fd = new FormData();
//    	fd.bottom = new FormAttachment( applyForRscsLViewer.getList(), -3, SWT.TOP );
//    	fd.left = new FormAttachment( applyForRscsLViewer.getList(), 0, SWT.LEFT );
//    	applyForResourceTypeLbl.setLayoutData( fd );
//
//    	
//    	addApplicableRscBtn = new Button( top_form, SWT.PUSH );
//    	addApplicableRscBtn.setText("Add ->");
//		fd = new FormData();
//		fd.width = 90;
//    	fd.top = new FormAttachment( availRscsLViewer.getList(), 10, SWT.BOTTOM );
//    	fd.left = new FormAttachment( availRscsLViewer.getList(), -50, SWT.CENTER );
//    	addApplicableRscBtn.setLayoutData( fd );
//
//    	removeApplicableRscBtn = new Button( top_form, SWT.PUSH );
//    	removeApplicableRscBtn.setText("<- Remove");
//		fd = new FormData();
//		fd.width = 90;
//    	fd.top = new FormAttachment( applyForRscsLViewer.getList(), 10, SWT.BOTTOM );
//    	fd.left = new FormAttachment( applyForRscsLViewer.getList(), -50, SWT.CENTER );
//    	removeApplicableRscBtn.setLayoutData( fd );

    	
    	Label consistsOfLbl = new Label( top_form, SWT.NONE );
    	consistsOfLbl.setText("Consists Of the Selected Attribute Sets:" );
    	fd = new FormData();
    	fd.bottom = new FormAttachment( attrSetGroupNameTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( 40, 0 );
    	consistsOfLbl.setLayoutData( fd );

    	
    	availAttrSetsLViewer = new ListViewer( top_form, 
				   SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData();//100,200);
//    	fd.top = new FormAttachment( attrSetGroupNameTxt, 0, SWT.TOP );
    	fd.top = new FormAttachment( consistsOfLbl, 35, SWT.BOTTOM );
    	fd.left = new FormAttachment( 40, 0 );
    	fd.right = new FormAttachment( 67, 0 );
//    	fd.bottom = new FormAttachment( availRscsLViewer.getList(), 0, SWT.BOTTOM );
    	fd.bottom = new FormAttachment( 100, -95 );
      	//    	fd.height = 190;
    	availAttrSetsLViewer.getList().setLayoutData(fd);

		Label addAttrSetsLbl = new Label( top_form, SWT.NONE );
		addAttrSetsLbl.setText("Available");
		fd = new FormData();
    	fd.bottom = new FormAttachment( availAttrSetsLViewer.getList(), -3, SWT.TOP );
    	fd.left = new FormAttachment( availAttrSetsLViewer.getList(), 0, SWT.LEFT );
    	addAttrSetsLbl.setLayoutData( fd );

    	
    	seldAttrSetsLViewer = new ListViewer( top_form, 
    			SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    	fd = new FormData();
    	fd.top = new FormAttachment( availAttrSetsLViewer.getList(), 0, SWT.TOP );
    	fd.left = new FormAttachment( 70, 0 );
    	fd.right = new FormAttachment( 97, 0 );
//    	fd.left = new FormAttachment( availAttrSetsLViewer.getList(), 15, SWT.RIGHT );
    	fd.bottom = new FormAttachment( availAttrSetsLViewer.getList(), 0, SWT.BOTTOM );
    	seldAttrSetsLViewer.getList().setLayoutData(fd);

    	Label seldAttrSetsLbl = new Label( top_form, SWT.NONE );
    	seldAttrSetsLbl.setText("Selected");
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
//    	fd.bottom = new FormAttachment( 100, -10 );
//    	fd.right = new FormAttachment( saveAttrSetGroupBtn, -20, SWT.LEFT );
    	newAttrSetGroupBtn.setLayoutData( fd );

    	cancelBtn = new Button( top_form, SWT.PUSH );
    	cancelBtn.setText( "Cancel");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( saveAttrSetGroupBtn, -20, SWT.LEFT );
    	cancelBtn.setLayoutData( fd );
    	
    	availAttrSetsLViewer.setContentProvider( stringArrayListContentProvider );
    	seldAttrSetsLViewer.setContentProvider( stringArrayListContentProvider );

    	availAttrSetsLViewer.setInput(availAttrSets);
    	seldAttrSetsLViewer.setInput(seldAttrSets);

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
							if( rscDefnMngr.getAllAttrSetGroupsForResource( 
									           seldRscDefn.getResourceDefnName() ).contains( 
									        		   attrSetGroupNameTxt.getText().trim() ) ) {								
								saveAttrSetGroupBtn.setEnabled( false );
							}
						}
					}
				}
			}
    	});		

		saveAttrSetGroupBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		saveAttrSetGroup( false );
        	}
		});

		newAttrSetGroupBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		saveAttrSetGroup( true );
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
		attrSetGroupNameTxt.setText( "CopyOf"+attrSetGroupNameTxt.getText() );
		attrSetGroupNameTxt.setSelection(0, attrSetGroupNameTxt.getText().length() );
		attrSetGroupNameTxt.setFocus();
	}
	
	public void editSelectedResource( ResourceName rscName ) {
		setSelectedResource( rscName );
		newAttrSetGroupBtn.setVisible( false );
		saveAttrSetGroupBtn.setVisible( true );
	}

	public void setSelectedResource( ResourceName rscName ) {

		seldRscName = rscName;
		
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
		
		seldAttrSets = new ArrayList<String>();
		seldAttrSets.addAll( seldAttrSetGroup.getAttrSetNames() );
		
		availAttrSets = new ArrayList<String>();
		availAttrSets.addAll( rscDefnMngr.getAvailAttrSetsForRscImpl( 
								seldRscDefn.getRscImplementation() ) );

		for( String as : seldAttrSets ) {
			availAttrSets.remove( as );				
		}
			
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
	
	
	private void saveAttrSetGroup( boolean newGroup ) {
		String origName = seldAttrSetGroup.getAttrSetGroupName();
		
		String attrSetGroupName = attrSetGroupNameTxt.getText().trim();
			
		boolean nameChanged = (!origName.equals( attrSetGroupName ) );
		
		// if a new group is being created make sure it doesn't already exist.
		//	
		if( nameChanged ) {
			if( rscDefnMngr.getAvailAttrSetsForRscImpl( 
					seldRscDefn.getRscImplementation() ).contains( attrSetGroupName ) ) {
//			if( rscDefnMngr.getResourceAttrSetGroups( 
//					seldAttrSetGroup.getResource() ).contains( attrSetGroupName ) ) {
//				
	    		MessageDialog confirmDlg = new MessageDialog( 
	    				NmapUiUtils.getCaveShell(), 
	    				"Confirm", null, 
	    				"The Attribute Set Group " +attrSetGroupName + " already exists.\n\n"+
	    				"Do you want to replace this Group?",
				MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
				confirmDlg.open();
				
				if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
					return;
				}
			}
			
			// make a copy
			if( newGroup ) {
				AttrSetGroup newAttrSetGroup = new AttrSetGroup( seldAttrSetGroup );
				seldAttrSetGroup = newAttrSetGroup;
			}
			
			seldAttrSetGroup.setAttrSetGroupName( attrSetGroupName );
		}
		
		seldAttrSetGroup.removeAllAttrSets();
		
		// update the seldAttrSetGroup with the GUI selections
		//		
		for( String asName : seldAttrSets ) {
			seldAttrSetGroup.addAttrSetName( asName );
		}

		rscDefnMngr.saveAttrSetGroup( seldAttrSetGroup, true );	    		

		// if renaming the group delete the old entry in the map 
		if( !newGroup && nameChanged ) {
			rscDefnMngr.removeAttrSetGroup( origName, seldAttrSetGroup.getResource() );
		}
		
		seldRscDefn.addAttrSetGroupName( seldAttrSetGroup.getAttrSetGroupName() );
				
       	try {
       		rscDefnMngr.findAvailAttrSets( seldRscDefn );

    		rscDefnMngr.writeResourceDefnTable();
    		rscDefnMngr.writeAttrSetGroups();
    		
		} catch (VizException e) {
    		MessageDialog confirmDlg = new MessageDialog( 
    				NmapUiUtils.getCaveShell(), 
    				"Save Attribute Set Group", null, 
    				"Error Writing new Resource Definitions Table\n\n"+
    				e.getMessage(),
    				MessageDialog.ERROR, new String[]{"OK"}, 0);
    		confirmDlg.open();

			System.out.println("Error saving Resource Defns Table."+ e.getMessage() );
		}

		ResourceName newSeldRscName = new ResourceName();
		newSeldRscName.setRscCategory( seldRscDefn.getResourceCategory() );
		newSeldRscName.setRscType( seldRscDefn.getResourceDefnName() );		
		newSeldRscName.setRscGroup( attrSetGroupName );
		
		mngrControl.updateResourceSelections( newSeldRscName );
		
		String msgStr;
		
		if( newGroup ) {
			msgStr = "Created new Attribute Set Group "+attrSetGroupName + 
			  " for Resource " + seldRscDefn.getResourceDefnName();
		}
		else {
			msgStr = (nameChanged ? "Saved " : "Saved " ) +
			         "Attribute Set Group " + attrSetGroupName +
					  " for Resource " + seldRscDefn.getResourceDefnName();
		}
		
		MessageDialog saveMsgDlg = new MessageDialog( 
				NmapUiUtils.getCaveShell(), 
				"Done", null, msgStr+"\n\n",
				MessageDialog.INFORMATION, new String[]{"OK"}, 0);
		saveMsgDlg.open();

		return;
	}

}