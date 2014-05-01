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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * The class for Volcano Ash Cloud dialog
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

public class AshCloudInfoDlg  extends AttrDlg{
	
	/**
	 * the single instance of this type
	 */
	private static AshCloudInfoDlg INSTANCE;
	
	/**
	 * top composite of the type	
	 */
	protected Composite top = null;
	
	/**
	 * Text fields for OBS info
	 */
	private Text txtObs = null;

	/**
	 * Text fields for Fcst 6 hour info
	 */
	private Text txtFcst6 = null;

	/**
	 * Text fields for Fcst 12 hour info
	 */
	private Text txtFcst12 = null;

	/**
	 * Text fields for Fcst 18 hour info
	 */
	private Text txtFcst18 = null;
	
	/**
	 * date for internal use
	 */
	private String date = null;

	/**
	 * time for internal use
	 */
	private String time = null;
	
	/**
	 * reference for parent dialog; need to set some fields
	 */
	private static VolcanoVaaAttrDlg  vaaAttrDlg = null;
	
	/**
	 * constructor of this dialog class
	 * @param Shell: parent shell for this dialog
	 * @throws VizException
	 */
	public AshCloudInfoDlg(Shell parShell) throws VizException {		
        super(parShell);                   
	}
	
	/**
	 * singleton method for returning the instance
	 * @param Shell: parent shell of the dialog
	 * @return AshCloudDlg: the instance of this dialog
	 */
	public static AshCloudInfoDlg getInstance( Shell parShell){		
		if ( INSTANCE == null ){					
			try {
				INSTANCE = new AshCloudInfoDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}			
		}		
		return INSTANCE;		
	}	

	/**
	 * empty method 
	 */
	public HashMap<String, Object> getAttrFromDlg(){ 
		return new HashMap<String,Object>(); 
	}
	
	/**
	 * empty method 
	 */
	public void setAttrForDlg(IAttribute ia){ }
	
	/**
	 * set VolcanoVaaAttrDlg related fields;
	 * mainly for user manual inputs.
	 * Obs info does NOT take manual input
	 */
	private void setFcstInfo(){
		vaaAttrDlg.setVacInfoFhr6(txtFcst6.getText().trim());
		vaaAttrDlg.setVacInfoFhr12(txtFcst12.getText().trim());
		vaaAttrDlg.setVacInfoFhr18(txtFcst18.getText().trim());
	}
	
	/**
	 * clear all the texts for next open up
	 */	
	private void reset(){
		txtObs.setText("");
		txtFcst6.setText("");
		txtFcst12.setText("");
		txtFcst18.setText("");
	}
	
	/**
	 * close the dialog	
	 */
	public void cancelPressed(){
		
		setFcstInfo();
		reset();
		setReturnCode(CANCEL);
		close();		
	}	
	
	/**
	 * button creation method overridden the super class'
	 * just one button: close
	 */
	@Override
	public void createButtonsForButtonBar(Composite parent){	 		
		
    	createButton(parent, IDialogConstants.CANCEL_ID, "Close",true);    	
  		getButton(IDialogConstants.CANCEL_ID).setEnabled(true);  		 

  	}
	
	/**
	 * create the dialog area for this dialog type;
	 * overridden the super class' method of the same name.
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
		top = (Composite) super.createDialogArea(parent);
		
		this.getShell().setText( "VAA Ash Cloud Info" );
		
		GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);
		
		Label lblObs = new Label(top, SWT.LEFT);
		lblObs.setText("Obs Ash Cloud:");
		
		txtObs = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP);
		txtObs.setLayoutData(new GridData(400,64));
		txtObs.setEditable(false);
		
		Label lblFcst6 = new Label(top, SWT.LEFT);
		lblFcst6.setText("Fcst Ash Cloud +06hr:");
		
		txtFcst6 = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP);
		txtFcst6.setLayoutData(new GridData(400,64));
		
		Label lblFcst12 = new Label(top, SWT.LEFT);
		lblFcst12.setText("Fcst Ash Cloud +12hr:");
		
		txtFcst12 = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP);
		txtFcst12.setLayoutData(new GridData(400,64));
		
		Label lblFcst18 = new Label(top, SWT.LEFT);
		lblFcst18.setText("Fcst Ash Cloud +18hr:");
		
		txtFcst18 = new Text(top, SWT.BORDER | SWT.MULTI | SWT.WRAP);
		txtFcst18.setLayoutData(new GridData(400,64));
		
		String[] fhrDT = VaaInfo.getFhrTimes(date, time);
		String txt00 = VaaInfo.getAshCloudInfo("00");
		txtObs.setText(txt00);	//VaaInfo.LAYERS[1] "OBS" NOT F00		
		txtFcst6.setText(fhrDT[0]+"  "+  (isNotSeen(txt00) ? "" : VaaInfo.getAshCloudInfo(VaaInfo.LAYERS[2])));
		txtFcst12.setText(fhrDT[1]+"  "+ (isNotSeen(txt00) ? "" : VaaInfo.getAshCloudInfo(VaaInfo.LAYERS[3])));
		txtFcst18.setText(fhrDT[2]+"  "+ (isNotSeen(txt00) ? "" : VaaInfo.getAshCloudInfo(VaaInfo.LAYERS[4])));
		
		return top;		
	}
	
	/**
	 * set the date/time for the dialog
	 * @param date: date string
	 * @param time: time string
	 */
	public void setDateTimeForDlg(String date, String time){
		this.date = date;
		this.time = time;	
	}
	
	/**
	 * set the parent dialog, VolcanoVaaAttrDlg in this case
	 * @param vaDlg: parent dialog, VolcanoVaaAttrDlg type
	 */
	public void setVaaAttrDlg(VolcanoVaaAttrDlg vaDlg){
		vaaAttrDlg = vaDlg;
	}
	
	/**
	 * 20100819 workshop issue: if NotSeen in OBS
	 * others only contain Date/Time.
	 */
	private boolean isNotSeen(String txt00){		
		
		if(txt00 == null)
			return false;
		
		return txt00.contains(VaaInfo.VA_NOT_IDENTIFIABLE);
	}

}
