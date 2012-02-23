package gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources.ManageResourceControl.IEditResourceComposite;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup;
import gov.noaa.nws.ncep.viz.resources.manager.AttributeSet;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
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
 * 12/22/11       #365      Greg Hull    Changes to support dynamic resources
 * 07/22/11       #450      Greg Hull    Save to User Localization
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
class EditAttrSetComp extends Composite implements IEditResourceComposite {
	ResourceDefnsMngr rscDefnMngr;
	ManageResourceControl mngrControl; // the parent composite 

	ResourceName       seldRscName=null;
	ResourceDefinition seldRscDefn=null;
	AttributeSet       seldAttrSet=null;
	
	Text attrSetNameTxt;	
	Text editAttrSetValuesTxt;
	
//	Text rscImplTxt; 
	org.eclipse.swt.widgets.List groupsList;
	Label groupsLbl;

	Button     saveAttrSetBtn;
	Button     newAttrSetBtn;
	Button     cancelBtn;

	public EditAttrSetComp( Composite parent, int style, ManageResourceControl mgrCtl ) {
		super( parent, style );
		Composite top_form = this;      

		FormData fd = new FormData();
    	fd.top = new FormAttachment( 0, 12 );  // offset so the title shows up
    	fd.left = new FormAttachment( 0, 0 );
    	fd.right = new FormAttachment( 100, 0 );
    	fd.bottom = new FormAttachment( 100, 0 );
    	top_form.setLayoutData(fd);

		setLayoutData( fd );

		top_form.setLayout( new FormLayout() );

		mngrControl = mgrCtl;
		rscDefnMngr = mngrControl.getRscDefnMngr();
		
		attrSetNameTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER );
		attrSetNameTxt.setText("");		

    	fd = new FormData();
    	fd.width = 200;
    	fd.top = new FormAttachment( 0, 30 );
    	fd.left = new FormAttachment( 0, 15 );
    	attrSetNameTxt.setLayoutData( fd );

		Label attrSetNameLbl = new Label( top_form, SWT.NONE );
		attrSetNameLbl.setText("Attribute Set Name");
		fd = new FormData();
    	fd.bottom = new FormAttachment( attrSetNameTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( attrSetNameTxt, 0, SWT.LEFT );
    	attrSetNameLbl.setLayoutData( fd );

		editAttrSetValuesTxt = new Text( top_form, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
		editAttrSetValuesTxt.setText("");		
    	
    	fd = new FormData();
    	fd.top = new FormAttachment( attrSetNameTxt, 0, SWT.TOP );
    	fd.left = new FormAttachment( 40, 0 ); // attrSetNameTxt, 0, SWT.LEFT );
    	fd.right = new FormAttachment( 100, -10 );
    	fd.bottom = new FormAttachment( 100, -60 );
    	editAttrSetValuesTxt.setLayoutData( fd );

    	
		Label editLbl = new Label( top_form, SWT.NONE );
		editLbl.setText("Edit Attribute Set Values");
		fd = new FormData();
    	fd.bottom = new FormAttachment( editAttrSetValuesTxt, -3, SWT.TOP );
    	fd.left = new FormAttachment( editAttrSetValuesTxt, 0, SWT.LEFT );
    	editLbl.setLayoutData( fd );

//    	rscImplTxt = new Text( top_form, SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY );    	
//    	rscImplTxt.setBackground( getParent().getBackground() ); // make look read-only
//    	fd = new FormData();
//    	fd.width = 150;
//    	fd.top = new FormAttachment( attrSetNameTxt, 60, SWT.BOTTOM );
//    	fd.left = new FormAttachment( attrSetNameTxt, 0, SWT.LEFT );    	
//    	rscImplTxt.setLayoutData( fd );
//    	Label rscImplLbl = new Label( top_form, SWT.NONE );
//    	rscImplLbl.setText("Applies to \nResource Implementation");
//		fd = new FormData();
//    	fd.bottom = new FormAttachment( rscImplTxt, -3, SWT.TOP );
//    	fd.left = new FormAttachment( rscImplTxt, 0, SWT.LEFT );
//    	rscImplLbl.setLayoutData( fd );
    	
    	groupsList = new org.eclipse.swt.widgets.List( top_form, SWT.READ_ONLY | SWT.BORDER | SWT.V_SCROLL );
    	groupsList.setBackground( getParent().getBackground() ); // make look read-only
    	
    	fd = new FormData();
    	fd.width = 150;
//    	fd.top = new FormAttachment( rscImplTxt, 50, SWT.BOTTOM );
//    	fd.left = new FormAttachment( rscImplTxt, 0, SWT.LEFT );    	
    	fd.top = new FormAttachment( attrSetNameTxt, 50, SWT.BOTTOM );
    	fd.left = new FormAttachment( attrSetNameTxt, 0, SWT.LEFT );    	
       	fd.bottom = new FormAttachment( 100, -60 );    	
       	groupsList.setLayoutData( fd );

    	groupsLbl = new Label( top_form, SWT.NONE );
    	groupsLbl.setText("In Attribute Set Groups");
		fd = new FormData();
    	fd.bottom = new FormAttachment( groupsList, -3, SWT.TOP );
    	fd.left = new FormAttachment( groupsList, 0, SWT.LEFT );
    	groupsLbl.setLayoutData( fd );

    	groupsLbl.setVisible( false );
    	groupsList.setVisible( false );
    	
    	
    	saveAttrSetBtn = new Button( top_form, SWT.PUSH );
    	saveAttrSetBtn.setText("Save");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( 100, -30 );
    	saveAttrSetBtn.setLayoutData( fd );
    	
    	newAttrSetBtn = new Button( top_form, SWT.PUSH );
    	newAttrSetBtn.setText("Create");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( 100, -30 );
//    	fd.bottom = new FormAttachment( 100, -10 );
//    	fd.right = new FormAttachment( saveAttrSetBtn, -20, SWT.LEFT );
    	newAttrSetBtn.setLayoutData( fd );

    	cancelBtn = new Button( top_form, SWT.PUSH );
    	cancelBtn.setText( "Cancel");
		fd = new FormData();
		fd.width = 100;
    	fd.bottom = new FormAttachment( 100, -10 );
    	fd.right = new FormAttachment( saveAttrSetBtn, -20, SWT.LEFT );
    	cancelBtn.setLayoutData( fd );

	
    	attrSetNameTxt.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				
				String newTextStr = attrSetNameTxt.getText().trim();
				
				if( newTextStr.isEmpty() ) {
					saveAttrSetBtn.setEnabled( false );
					saveAttrSetBtn.setEnabled( false );
				}
				else {
					if( seldAttrSet != null ) {
						saveAttrSetBtn.setEnabled( true );

						// if the name has been changed, the 'save' button acts as a 'Rename' or Save As
						// disable the New button if the name hasn't been changed.
						//
						if( seldAttrSet.getName().equals( newTextStr /*+ ".attr"*/ ) ) {
							saveAttrSetBtn.setText("Save" );

							newAttrSetBtn.setEnabled( false );
						}
						else {
							saveAttrSetBtn.setText("Save As" );
							newAttrSetBtn.setEnabled( true );

							// disable the Save button if the new name already exists 
//							if( rscDefnMngr.getAllAttrSetsForRscImpl( 
//									           seldAttrSet.getRscImpl() ).contains( 
//									        		   attrSetNameText.getText().trim() ) ) {								
//								saveAttrSetBtn.setEnabled( false );
//							}
						}
					}
				}
			}
    	});		

		saveAttrSetBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		saveAttrSet( false );
        	}
		});

		newAttrSetBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		saveAttrSet( true );
        	}
		});

		cancelBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected( SelectionEvent ev ) {
        		mngrControl.editActionCanceled();
        	}
		});
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
	public void activate() {
		setVisible( true );
		if( getParent() instanceof Group ) {
			((Group)getParent()).setText( getTitle() );
		}
	}

	public void copySelectedResource( ResourceName rscName ) {
		setSelectedResource( rscName );
		attrSetNameTxt.setText( "CopyOf"+attrSetNameTxt.getText() );
		attrSetNameTxt.setSelection(0, attrSetNameTxt.getText().length() );
		attrSetNameTxt.setEditable(true);
		attrSetNameTxt.setBackground( editAttrSetValuesTxt.getBackground() );
		attrSetNameTxt.setFocus();
		newAttrSetBtn.setVisible( true );
		saveAttrSetBtn.setVisible( false );

	}
	
	public void editSelectedResource( ResourceName rscName ) {
		setSelectedResource( rscName );
		attrSetNameTxt.setEditable(false);
		attrSetNameTxt.setBackground( getParent().getBackground() );
		newAttrSetBtn.setVisible(false);
		saveAttrSetBtn.setVisible( true );
		editAttrSetValuesTxt.setFocus();
	}
	
	public void setSelectedResource( ResourceName rscName ) {

		seldRscName = rscName;
		
		if( seldRscName.getRscAttrSetName().isEmpty() ) {
			attrSetNameTxt.setText( "" );
			return;
		}
		
		seldAttrSet = rscDefnMngr.getAttrSet( seldRscName );

		if( seldAttrSet == null || 
		   !seldAttrSet.getFile().getFile().exists() ) {
			
			attrSetNameTxt.setText("");
			return;
		}
		
		seldRscDefn = rscDefnMngr.getResourceDefinition( seldRscName );
		
		attrSetNameTxt.setText( seldRscName.getRscAttrSetName() );
		
		groupsList.removeAll();
		
		if( seldRscDefn.applyAttrSetGroups() ) {
			groupsLbl.setVisible( true );
			groupsList.setVisible( true );
			if( seldRscDefn.isPgenResource() ) {
				groupsList.add( "All PGEN Resources");
			}
			else {
				ArrayList<AttrSetGroup> attrSetGroupsList = rscDefnMngr.getAttrSetGroupsForResource( 
						seldRscDefn.getResourceDefnName() );
				for( AttrSetGroup asg : attrSetGroupsList ) {
					if( asg.getAttrSetNames().contains( seldRscName.getRscAttrSetName() ) ) {
						groupsList.add( asg.getAttrSetGroupName() );					
					}
				}
			}
		}
		else {
	    	groupsLbl.setVisible( false );
	    	groupsList.setVisible( false );
	    	groupsList.removeAll();
		}
		
		
		try {
			FileReader fr = new FileReader( seldAttrSet.getFile().getFile() );
			char[] attrsSetStr = new char[(int) seldAttrSet.getFile().getFile().length()];
			fr.read(attrsSetStr);
			fr.close();
			
			editAttrSetValuesTxt.setText( new String( attrsSetStr ) );
		} catch (FileNotFoundException fnf ) {
			editAttrSetValuesTxt.setText( "Error Getting AttrSet values" );
		} catch (IOException ioe ) {
			editAttrSetValuesTxt.setText( "Error Getting AttrSet values" );
			System.out.println("I/O error reading attr set file");
		}
	}

	@Override
	public void deactivate() {
		setVisible( false );
	}

	@Override
	public String getTitle() {
		return "Edit Attribute Set";			
	}

	// NOTE : Currently this is written to not delete an attributeSet if the 
	// name is changed. 
	//
	private void saveAttrSet( boolean newAttrSet ) {
    	
    	HashMap<String, String> attrsMap;
    	
    	// get and validate the new attibutes.
    	//
		try {
			String newAttrsStr = editAttrSetValuesTxt.getText();
			NcPathManager pathMngr = NcPathManager.getInstance();

			// First validate by writing to a tmp file and parsing the attributes
			// 
			File tmpAttrSetFile = File.createTempFile("tempAttrSet-", ".attr");

			FileWriter fwriter = new FileWriter( tmpAttrSetFile );

			fwriter.write( newAttrsStr );
			fwriter.close();

			attrsMap = ResourceDefnsMngr.readAttrSetFile( tmpAttrSetFile ); // throws exception on parse error

			tmpAttrSetFile.delete();
			
		} catch ( Exception e ) {
			MessageDialog errDlg = new MessageDialog( getShell(), 
					"Error", null, "Error Parsing Attributes: "+e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
			return;
		} 

		String attrSetName = attrSetNameTxt.getText().trim();
		
//    	String applicableRsc = seldAttrSet.getApplicableResource();    	
//    	AttributeSet existingAttrSet = rscDefnMngr.getAttrSet( seldRscName );
    	
    	// if copying then check to make sure that the new name doesn't already exist
    	if( newAttrSet ) {
    		ResourceName newRscName = new ResourceName( seldRscName );
    		newRscName.setRscAttrSetName( attrSetName );
    		AttributeSet aset = rscDefnMngr.getAttrSet( newRscName  );
    		if( aset != null ) {
    			// TODO : allow user to confirm. for now just fail..
	    		MessageDialog confirmDlg = new MessageDialog( 
	    				getShell(), 
	    				"Error", null, 
	    				"The Attribute Set " + attrSetName + " already exists.\n"+
	    				"Either enter another name or delete the existing Attribute Set",
				MessageDialog.ERROR, new String[]{"Ok"}, 0);
				confirmDlg.open();
				return;
//				if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {						
//					return;
//				}
    		}
    		
    	}

		try {
			rscDefnMngr.saveAttrSet( seldRscDefn, attrSetName, 
									 editAttrSetValuesTxt.getText() );//attrsMap );
		} catch (VizException e) {
			MessageDialog errDlg = new MessageDialog( NmapUiUtils.getCaveShell(), 
					"Error", null, "Error Saving AttrSet, "+attrSetName+"\n"+
					e.getMessage(),
					MessageDialog.ERROR, new String[]{"OK"}, 0);
			errDlg.open();
			return;
		}
		
		// Next if this is a new AttrSet and if attrSetGroups apply, then 
		// prompt if the user wants to add it to the selected group
		if( newAttrSet && seldRscDefn.applyAttrSetGroups() ) {
			boolean addToGroup = true;

			// for new pgen AttrSets and for renamed attrSets we will automatically add to the attrSetGroup
			//    otherwise for other renamed attrSets prompt the user
			//
			if( !seldRscDefn.isPgenResource() ) {
				MessageDialog confirmDlg = new MessageDialog( 
						NmapUiUtils.getCaveShell(), 
						"New Attribute Set", null, 
						"Do you want to add this Attribute Set\nto the group "+
						seldRscName.getRscGroup()+"?",
						MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
				confirmDlg.open();

				addToGroup = ( confirmDlg.getReturnCode() == MessageDialog.OK );
			}
			
			try {
				AttrSetGroup seldAsGroup = 
					rscDefnMngr.getAttrSetGroupForResource( seldRscName );

				if( addToGroup ) {						
					if( seldAsGroup == null ) {
						throw new VizException("??failed to get AttributeSetGroup for resource, "+ seldRscName.toString() );
					}
					seldAsGroup.addAttrSetName(attrSetName);
					rscDefnMngr.saveAttrSetGroup( seldAsGroup );
				}
			} catch (VizException e) {
				MessageDialog errDlg = new MessageDialog( NmapUiUtils.getCaveShell(), 
						"Error", null, "Error Saving AttrSet to AttrSetGroup\n"+
							e.getMessage(),
						MessageDialog.ERROR, new String[]{"OK"}, 0);
				errDlg.open();
				return;
			}
		}
	
		MessageDialog infoDlg = new MessageDialog( getShell(), 
				"Info", null, "The Attribute Set  "+ attrSetName + "  has been "+
				( newAttrSet ? "Created" : "Saved" ),
				MessageDialog.INFORMATION, new String[]{"OK"}, 0);
		infoDlg.open();


		ResourceName newSeldRscName = new ResourceName();
		newSeldRscName.setRscCategory( seldRscName.getRscCategory() );
		newSeldRscName.setRscType( seldRscName.getRscType() );		
		newSeldRscName.setRscGroup( seldRscName.getRscGroup() );
		newSeldRscName.setRscAttrSetName( attrSetName );
		mngrControl.updateResourceSelections( newSeldRscName );
	}
}