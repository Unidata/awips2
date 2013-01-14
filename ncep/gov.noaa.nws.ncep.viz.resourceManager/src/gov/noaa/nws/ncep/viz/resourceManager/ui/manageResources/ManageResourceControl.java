package gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources;

import java.util.List;

import gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources.ResourceEditSelectionComposite.EditResourceAction;
import gov.noaa.nws.ncep.viz.resourceManager.ui.manageResources.ResourceEditSelectionComposite.IEditResourceListener;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;


/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09/10		  #273		Greg Hull	 Created
 * 10/05/10       #307      Greg Hull    adjust the sash when edit forms are shown.
 * 12/17/10       #365      Greg Hull    add updateDialog
 * 06/07/11       #445       Xilin Guo   Data Manager Performance Improvements
 * 07/25/11       #450      Greg Hull    Save to User Localization
 * 07/03/12       #568      Greg Hull    Set size on updateDialog
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class ManageResourceControl extends Composite {

	private Shell shell;

	private ResourceDefnsMngr rscDefnMngr;
		
    private SashForm sashForm = null;
    private Group selRscGrp = null;
    private Group editRscGrp = null;
    
    private Point initDlgSize = new Point( 750, 860 );

    private ResourceEditSelectionComposite selectResourceComp = null;
    
   	public interface IEditResourceComposite {
   		
   		public abstract String getTitle();
   		
   		public abstract void activate();
		
   		public abstract void copySelectedResource( ResourceName rscName );

   		public abstract void editSelectedResource( ResourceName rscName );
   		
   		public abstract ResourceName getSelectedResourceName();
   		
   		public abstract boolean isModified();

   		public abstract void deactivate();
   	}
   	

    private IEditResourceComposite activeEditComposite = null;
    
    private EditResourceTypeComp editRscTypeComp;
    
    private EditAttrSetGroupComp editAttrSetGroupComp;
    
    private EditAttrSetComp editAttrSetComp;
    
//    private NoEditActionComp nullEditActionComp;
    
    public ManageResourceControl(Composite parent )   throws VizException {
        super(parent, SWT.NONE);
        
        rscDefnMngr = ResourceDefnsMngr.getInstance();

        shell = parent.getShell();

        Composite top_comp = this;        
        top_comp.setLayout( new GridLayout(1,true) );
        
        top_comp.setSize( 500, 450 );
        
        sashForm = new SashForm( top_comp, SWT.VERTICAL );
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        
        sashForm.setLayoutData( gd );
        sashForm.setSashWidth(10);
                
        selRscGrp = new Group( sashForm, SWT.SHADOW_NONE );
        selRscGrp.setText("Select Resource");
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;

        selRscGrp.setLayoutData( gd );

        selRscGrp.setLayout( new FormLayout() );
        
    	try {
    		selectResourceComp = new ResourceEditSelectionComposite( selRscGrp, editActionListener );
		} catch (VizException e) {
			e.printStackTrace();
		} catch ( Exception e ) {
			e.printStackTrace();
		}
		
//		sel_rsc_cntrl.addResourceSelectionListener( new IResourceSelectedListener() {
//			@Override
//			public void resourceSelected(ResourceName rscName,
//					DataTime fcstTime, boolean done) {
//				
//			}
//		});

		editRscGrp = new Group( sashForm, SWT.SHADOW_NONE );
		editRscGrp.setLayout( new FormLayout() );
		
		editRscTypeComp = new EditResourceTypeComp( 
				editRscGrp, SWT.SHADOW_NONE, this );

		editAttrSetGroupComp = new EditAttrSetGroupComp( 
				editRscGrp, SWT.SHADOW_NONE, this );

		
		editAttrSetComp = new EditAttrSetComp( 
				editRscGrp, SWT.SHADOW_NONE, this );

		// Start with a blank section 
		editRscTypeComp.deactivate();
		editAttrSetGroupComp.deactivate();
		editAttrSetComp.deactivate();
				
		sashForm.setWeights( new int[] { 4, 3 } );
        
        initWidgets();        
    }
      	
    public ResourceDefnsMngr getRscDefnMngr() {
    	return rscDefnMngr;
    }
    
   	// 
   	public void initWidgets() {

   	}

   	private IEditResourceListener editActionListener = new IEditResourceListener() {
		@Override
		public void editResourceAction( ResourceName rscName,
				                        EditResourceAction action) {
			boolean copyFlag=false;
			EditResourceAction prevAction = EditResourceAction.NULL_ACTION;
			
			if( activeEditComposite != null ) {
				if( activeEditComposite.isModified() ) {  // TODO :isModified() not implemented
					// prompt for confirmation and if denied then reselect the previously
					// selected rscName
				}
				// if this is the same action and resource that is already selected then 
				// don't activate 
//				if( activeEditComposite.getSelectedResourceName().equals( rscName ) ) {
//					return;
//				}
				activeEditComposite.deactivate();
			}
			activeEditComposite = null;
			
			if( action == EditResourceAction.COPY_RESOURCE_TYPE ) {
				sashForm.setWeights( new int[] { 5, 6 } );
				activeEditComposite = editRscTypeComp;			
				copyFlag = true;
			}
			else if( action == EditResourceAction.EDIT_RESOURCE_TYPE ) {
				sashForm.setWeights( new int[] { 5, 6 } );
				activeEditComposite = editRscTypeComp;				
			}
			// Give more room since the lists of attr sets can be long for
			// grids
			else if( action == EditResourceAction.COPY_RESOURCE_GROUP ) {
				sashForm.setWeights( new int[] { 2, 3 } );
				activeEditComposite = editAttrSetGroupComp;
				copyFlag = true;
			}
			else if( action == EditResourceAction.EDIT_RESOURCE_GROUP ) {
				sashForm.setWeights( new int[] { 2, 3 } );
				activeEditComposite = editAttrSetGroupComp;
			}
			else if( action == EditResourceAction.COPY_RESOURCE_ATTR_SET ) {
				sashForm.setWeights( new int[] { 5, 6 } );
				activeEditComposite = editAttrSetComp;
				copyFlag = true;
			}
			else if( action == EditResourceAction.EDIT_RESOURCE_ATTR_SET ) {
				sashForm.setWeights( new int[] { 5, 6 } );
				activeEditComposite = editAttrSetComp;
			}
			else { // remove a type/group/attrSet
				  if( action == EditResourceAction.REMOVE_RESOURCE_TYPE ) { 
					  removeResourceType( rscName );
				  }
				  else if( action == EditResourceAction.REMOVE_RESOURCE_GROUP ) { 
					  removeAttrSetGroup( rscName );
				  }
				  else if( action == EditResourceAction.REMOVE_RESOURCE_ATTR_SET ) { 
					  removeAttrSet( rscName );
				  }
			}
			
			
			if( activeEditComposite != null ) {
				activeEditComposite.activate();
				if( copyFlag ) {
					activeEditComposite.copySelectedResource( rscName );
				}
				else {
					activeEditComposite.editSelectedResource( rscName );				
				}
			}
		}
   	};
   	
   	public void setResourceEnable( ResourceName rscName, boolean isEnabled ) {
   		rscDefnMngr.setResourceEnabled( rscName.getRscType(), isEnabled );
   		
		MessageDialog okDlg = new MessageDialog( getShell(), 
				"", null, 
				"The "+rscName.getRscType()+" Resource has been " + 
				(isEnabled ? "Enabled" : "Disabled") + "\nand will "+
				(isEnabled ? "now" : "not") + " show up in the Select " +
				"Resources Dialog",				
				MessageDialog.INFORMATION, new String[]{"OK"}, 0);
		okDlg.open();
   	}
   	
   	public void removeResourceType( ResourceName rscName ) {
   		try {
   			if( rscName == null ||
   				rscDefnMngr.getResourceDefinition( rscName ) == null ) {
   				throw new VizException( "Selected Resource/Group is not valid:" +
   						(rscName == null ? "null" : rscName.toString() ) );
   				
   			}

   			//boolean isPgen = rscName.getRscCategory().equals("PGEN" );
   			ResourceDefinition rscDefn = rscDefnMngr.getResourceDefinition( rscName );
   			
//   		List<String> asgList = rscDefn.getAttrSetGroupNames()
   			List<AttrSetGroup> asgList = 
   						rscDefnMngr.getAttrSetGroupsForResource( 
   									rscDefn.getResourceDefnName() );
   			
   			// true if another resource from a higher context level replaced the deleted one.
   			//
   			Boolean rscReplaced = rscDefnMngr.removeResourceDefn( rscDefn );

   			if( !rscReplaced && !asgList.isEmpty() ) {
   				// TODO : do we want to prompt user if they want to delete any AttrSetGroups that
   				// apply to this resource?
//   				MessageDialog confirmDlg = new MessageDialog( getShell(), 
//   						"Confirm", null, 
//   						"Do you also want to remove the "+
//   						   Integer.toString( asgList.size() ) +"\nAttribute Set Groups?",				
//   						MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
//   				confirmDlg.open();
//
//   				if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
//   					return;
//   				}
//
//   				// This won't work because the ResourceDefinition is gone now. 
//   				// 
//   				for( AttrSetGroup asg : asgList ) {
//   					try {
//   						rscDefnMngr.removeAttrSetGroup( 
//   								rscName.getRscGroup(), rscName.getRscType() ); 
//   					}
//   					catch (VizException e ) {
//   						MessageDialog msgDlg = new MessageDialog( getShell(), 
//   								"Error", null, 
//   								"Failed to remove the "+rscName.getRscGroup()+" Attribute Set Group.",				
//   								MessageDialog.ERROR, new String[]{"OK"}, 0);
//   						msgDlg.open();
//   					}
//
//   				}
   			}
   		}
   		catch ( VizException e ) {
   			MessageDialog msgDlg = new MessageDialog( getShell(), 
   					"Error", null, 
   					"Failed to remove Resource Type "+rscName.getRscType()+"\n"+e.getMessage(),				
   					MessageDialog.ERROR, new String[]{"OK"}, 0);
   			msgDlg.open();
   			rscName.setRscType("");
   		}

   		updateResourceSelections( rscName );
   	}
   	
   	public void removeAttrSetGroup( ResourceName rscName ) {   		
   		try {
   			if( rscName == null || !rscName.isValid() || 
   			    rscName.getRscGroup().isEmpty() ) {
   				System.out.println( "Selected Resource/Group is not valid:" +
   						(rscName == null ? "null" : rscName.toString() ) );
   			}

   			boolean isPgen = rscName.getRscCategory().equals("PGEN" );
   				
   			// TODO : determine if this is the last group available and issue a 
   			// warning (or error?) to the user.   			
   			//
   			MessageDialog confirmDlg;
   			if( isPgen ) {
   				confirmDlg = new MessageDialog( getShell(), 
   						"Confirm", null, 
   						"This will permanently remove the PGEN Resource, "+
   						rscName.getRscGroup()+
   						".\nAre you sure you want to do this?",				
   						MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
   				confirmDlg.open();
   				
   				if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
   					return;
   				}
   			}
   			else {
//   				
//   				confirmDlg = new MessageDialog( NmapUiUtils.getCaveShell(), 
//   						"Confirm", null, 
//   						"This will remove the Attribute Set Group, " +
//   						rscName.getRscGroup()+
//   						"  from the  "+ rscName.getRscType() +" Resource."
//   						". Are you sure you want to do this?",				
//   						MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
//   				
   			}

			if( isPgen ) {
			//	rscDefnMngr.removePgenResource( rscName );
			}
			else {
				rscDefnMngr.removeAttrSetGroup( 
						rscName.getRscGroup(), rscName.getRscType() );

				MessageDialog msgDlg = new MessageDialog( getShell(), 
						"Done", null, 
						"The User-Level Attrribute Set Group, "+rscName.getRscGroup()+", has been " + 
						"removed for Resource, " + rscName.getRscType()+".",				
						MessageDialog.INFORMATION, new String[]{"OK"}, 0);
				msgDlg.open();
			}
   		}
   		catch( VizException e ) {
   			MessageDialog msgDlg = new MessageDialog( getShell(), 
   					"Error", null, 
   					"Failed to remove the "+rscName.getRscGroup()+" Attribute Set Group.",				
   					MessageDialog.ERROR, new String[]{"OK"}, 0);
   			msgDlg.open();
   		}
   		
   		updateResourceSelections( rscName );
   	}

   	
   	public void removeAttrSet( ResourceName rscName ) {   		
   		try {
   			ResourceDefinition seldRscDefn = rscDefnMngr.getResourceDefinition( rscName );
   			if( seldRscDefn == null ) {
   				throw new VizException("Can't find rsc defn for:"+rscName.toString() );
   			}

   			String attrSetName = rscName.getRscAttrSetName();
   			String attrSetGroup = rscName.getRscGroup();
   			
   			// if removing an attr set that is part of a group then check for references
   			// to it in other groups 
   			//
   			if( seldRscDefn.applyAttrSetGroups() ) {
   				
   	   			// check if there are references to this attribute set and prompt the user
   	   			// whether they want to do this
   				MessageDialog confirmDlg = new MessageDialog( getShell(), 
   						"Confirm", null, 
   						"This will permanently remove the Attribute Set, "+ 
   						  rscName.getRscAttrSetName()+" and will remove references to "+
   						  "it in the  "+attrSetGroup+" and any other Attribute Set Groups"+
   						  "that reference it.\n\n"+
   						"Are you sure you want to delete the attribute set?",				
   						MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
   				confirmDlg.open();
   				
   				if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
   					return;
   				}
   			}
   			
  // 			else if this is the last attribute set prompt a warning that they will not
   			// be able to create a new one. This will only be allowed if the resource defn 
   			// was copied; otherwise the attr set will be in the base context and will not 
   			// be deletable.
   			
   			rscDefnMngr.removeAttrSet( rscName );
   	   		
   	   		updateResourceSelections( rscName );

   			MessageDialog msgDlg = new MessageDialog( getShell(), 
   					"Removed", null, 
   					"The Attrribute Set "+ attrSetName+" has been removed",				
   					MessageDialog.INFORMATION, new String[]{"OK"}, 0);
   			msgDlg.open();
   		}
   		catch( VizException e ) {
   			MessageDialog msgDlg = new MessageDialog( getShell(), 
   					"Error", null, 
   					"Failed to remove the Attribute Set:\n"+ e.getMessage(),				
   					MessageDialog.ERROR, new String[]{"OK"}, 0);
   			msgDlg.open();
   		} 
   	}

   	// this is called by the 'edit/create' components when a new Type,Group, or attrSet
   	// is created. 
   	// It is also called with null rscName when an existing Type/Group/AttrSet is edited
   	// this is treated as a 'refresh'.
   	public void updateResourceSelections( ResourceName seldRscName ) {
   		selectResourceComp.updateResourceSelections( seldRscName );
   		editActionCanceled();
   	}
   	
   	public void editActionCanceled( ) {
		sashForm.setWeights( new int[] { 5, 4 } );

		if( activeEditComposite != null ) {
   			activeEditComposite.deactivate();
   			selectResourceComp.cancelEditAction();
   		}
   	}
   	
    public void updateDialog() {
    	shell.setSize( initDlgSize );
    }
}