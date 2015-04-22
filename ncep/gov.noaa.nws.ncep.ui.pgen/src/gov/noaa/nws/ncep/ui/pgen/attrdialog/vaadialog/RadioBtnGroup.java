/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * February 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;

import java.util.*;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;

/**
 * The class for dialogs Radio Buttons handling 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/10		#165		G. Zhang   	Initial Creation.
 *
 * </pre>
 * 
 * @author	G. Zhang
 */


public class RadioBtnGroup {
	
	/**
	 * array for holding the related buttons
	 */
	private Button[] btnGroup = null;
	

	/**
	 * constructor for RadioBtnGroup.
	 * 
	 * Usage: 
	 * 
	 * Generally used in:
	 * public void setAttrForDlg(IAttribute ia) of dialogs:
	 * 
	 * 		RadioBtnGroup rBtnGrp = 
	 * 			new RadioBtnGroup( new Button[]{ btnArea,btnLine,btnNotSeen,btnFcst } );
	 * 	
	 * 		if(type.contains("Area")) 
	 * 			rBtnGrp.enableBtn(btnArea, null, new Control[]{ txtWidth, comboFcst}); 
	 * 
	 */	
	public RadioBtnGroup(Button[] btns){
		btnGroup = btns;
	}		
	
	/**
	 * @parameter: btn:			Button to be enabled
	 * @parameter: tbEnabled:	Controls to be enabled
	 * @parameter: tbDisabled:	Controls to be disabled
	 */		
	public  void enableBtn(Button btn, Control[] tbEnabled, Control[] tbDisabled){
		
		for (Button button : btnGroup){
			if(button == btn){
				button.setSelection(true);				
				
				if(tbEnabled != null){
					for(Control cen : tbEnabled)
						cen.setEnabled(true);					
				}
				
				if(tbDisabled != null){
					for(Control cdis : tbDisabled)
						cdis.setEnabled(false);
				}
				
			}else{
				if(button != null && ! button.isDisposed()) 
					button.setSelection(false);
			}
		}
	}

}
