/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * Janurary 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import java.util.*;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.sigmet.*;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * The class for Volcano Ash Cloud Info Source dialog 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 *
 * </pre>
 * 
 * @author	G. Zhang
 */

public class AshCloudInfoSourceDlg   extends AttrDlg{
	
	/**
	 * instance of this class
	 */
	private static AshCloudInfoSourceDlg INSTANCE;
	
	/**
	 * top Composite of this class
	 */
	protected Composite top = null;
	
	/**
	 * Label for info sources selecting hints
	 */
	private Label lblHint = null;
	
	/**
	 * hint message for info sources selecting.
	 */
	private static final String SELECT_HINTS = 	"Single mouse click:	  select one row;\n"+
												"Single click+Shift: 	  continuous rows;\n"+
												"Single click+Ctrl : 	  separate rows;";
		
	/**
	 * widgets.List for holding the info sources
	 */
	private org.eclipse.swt.widgets.List listInfoSour = null;
	
	/**
	 * constructor of this class
	 * @param Shell: parent Shell for this class
	 * @throws VizException
	 */
	public AshCloudInfoSourceDlg(Shell parShell) throws VizException {		
        super(parShell);                   
	}
	
	/**
	 * singleton creation method for this class
	 * @param Shell: parent Shell for this class
	 * @return
	 */
	public static AshCloudInfoSourceDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){					
			try {
				INSTANCE = new AshCloudInfoSourceDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}			
		}		
		return INSTANCE;		
	}
	
	/**
	 * method necessary with the super class
	 */
	public HashMap<String, Object> getAttrFromDlg(){ return new HashMap<String,Object>(); }
	
	/**
	 * method necessary with the super class
	 */
	public void setAttrForDlg(IAttribute ia){ }
	
	/**
	 * button listener overridden from the super class.
	 * once OK button is clicked the selected info sources
	 * is copied to VolcanoVaaAttrDlg class
	 */
	@Override
	public void okPressed(){
		VolcanoVaaAttrDlg.getInstance(this.getParentShell()).setInfoSource(listInfoSour.getSelection());
		cancelPressed();
	}
	
	/**
	 * button listener overridden from the super class
	 */
	@Override
	public void cancelPressed(){		
		setReturnCode(CANCEL);
		close();		
	}
	
	/**
	 * method for creating the OK/Cancel buttons
	 */
	@Override
	public void createButtonsForButtonBar(Composite parent){	 		
		
		createButton(parent, IDialogConstants.OK_ID, "OK", true);
    	createButton(parent, IDialogConstants.CANCEL_ID, "Cancel",true); 
    	
    	getButton(IDialogConstants.OK_ID).setEnabled(true);
  		getButton(IDialogConstants.CANCEL_ID).setEnabled(true);  		 

  	}
	
	/**
	 * method overridden from the super class
	 * to create the dialog area for this class. 
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
		top = (Composite) super.createDialogArea(parent);
		
		lblHint = new Label(top, SWT.LEFT);
		lblHint.setText(SELECT_HINTS);
		
		listInfoSour = new org.eclipse.swt.widgets.List(top, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL);
		GridData gData = new GridData(256,512);
		gData.horizontalSpan = 7;
		listInfoSour.setLayoutData( gData);		
		listInfoSour.setItems( getInfoSourItems() );//Multi-selecting: ctrl+click / shift+click
		
		
		return top;
	}
	
	
	/**
	 * The following four methods are all static,
	 * since they all use VaaInfo maps
	 * 
	 * TODO: check if this is a good idea!
	 */
	
	public static String[] getInfoSourItems(){
		String[] s = VaaInfo.VAA_INFO_SINGLE_MAP.get("information-source");

		return s;
	}	

}
