/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * Janurary 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.transform.dom.DOMSource;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.sigmet.*;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The class for VAA text product dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/10		#165		G. Zhang   	Initial Creation.
 * 12/12		#889		B. Yin		Made the dialog modal..
 *
 * </pre>
 * 
 * @author	G. Zhang
 */

public class SaveMsgDlg  extends CaveJFACEDialog{
	
	/**
	 * singleton instance of this class
	 */
	private static SaveMsgDlg INSTANCE = null;
	
	/**
	 * the parent class of this class
	 */
	private static VolcanoVaaAttrDlg volAttrDlgInstance = null;
	
	/**
	 * the Volcano with its text product this dialog is handling
	 */
	private Volcano volcano = null;
	
	/**
	 * the xml file Volcano from JAXB for persistence this dialog is creating
	 */
	private gov.noaa.nws.ncep.ui.pgen.file.Volcano fVol = null;
	
	/**
	 * Text field for displaying the text product
	 */
	Text txtInfo;
	
	/**
	 * the Text field displaying the name of the file
	 * of to be saved text product.
	 */
	Text txtSave;
	
	/**
	 * variables holding directory and file content.
	 */
	String dirLocal = ".", txtFileContent = "";
	
	/**
	 * constructor for this class
	 * @param Shell: parent Shell of this class
	 * @throws VizException
	 */
	SaveMsgDlg(Shell parShell) throws VizException {
		super(parShell);		
		this.setShellStyle(SWT.TITLE | SWT.APPLICATION_MODAL | SWT.CLOSE );

	}
	
	/**
	 * singleton creation method for this class
	 * @param Shell: parent Shell of this class
	 * @return
	 */
	public static SaveMsgDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){					
			try {
				INSTANCE = new SaveMsgDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}			
		}		
		return INSTANCE;		
	}
	
	/**
	 * method necessary from the super class
	 */
	public HashMap<String, Object> getAttrFromDlg(){
		HashMap<String,Object> attr = null;
    	return attr;
	}
	
	/**
	 * method overridden from the super class
	 * for Save/Cancel buttons of this class
	 */
	@Override
    public void createButtonsForButtonBar(Composite parent) { 			
		createButton(parent, IDialogConstants.OK_ID, "Save",	true);	    	
		createButton(parent, IDialogConstants.CANCEL_ID,IDialogConstants.CANCEL_LABEL, false);
	}
	
	/**
	 * method listener overridden from the super class
	 * for Cancel button of this class
	 */
	@Override
	public void cancelPressed(){			
		setReturnCode(CANCEL);
		close();		
	}
	
	/**
	 * method listener overridden from the super class
	 * for Save button of this class: 
	 * it saves the displayed text with the displayed name
	 * as a text file in local directory.
	 */
	@Override
	public void okPressed() {  
		
		FileTools.writeFile( PgenUtil.getPgenActivityTextProdPath()+File.separator+txtSave.getText(), 
				PgenUtil.wrap( txtInfo.getText(), 51, null, false));

			setReturnCode(OK);
			close();
				
	}
	
	/**
	 * setter for the field of VolcanoVaaAttrDlg
	 * @param vaDlg: the VolcannoVaaAttrDlg instance to be set
	 */
	public void setVolAttrDlgInstance(VolcanoVaaAttrDlg vaDlg){
		volAttrDlgInstance = vaDlg;
	}

	/**
	 * method overridden from the super class
	 * to create the dialog area for this class
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		Composite top = (Composite) super.createDialogArea(parent);
		
		GridLayout mainLayout = new GridLayout(3, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        top.setLayout(mainLayout);
        
        this.getShell().setText("VAA Save");	  
        
        this.volcano = this.volAttrDlgInstance.getVolcano();//TODO: already set in attrDlg before this is open 20100309
        
	    txtInfo = new Text( top, SWT.MULTI | SWT.BORDER | SWT.WRAP);
	    GridData gData = new GridData(400,640);
	    gData.horizontalSpan = 3;
	    txtInfo.setLayoutData( gData );	    
        txtInfo.setText(this.getFileContent2());//getFileContent());
        
        txtSave = new Text(top, SWT.BORDER | SWT.READ_ONLY);
        txtSave.setLayoutData(new GridData(SWT.FILL,SWT.CENTER,true,false,3,1));
        txtSave.setText(getFileName());	
		
		return top;
	}
	
	/**
	 * calculate the file name for the to be saved text file.
	 * 
	 * @return String: the file name
	 */
	public String getFileName(){
		String connector = "_";
		
		StringBuilder sb = new StringBuilder();
		sb.append( volcano.getName() );//getVolNameId().split(VaaInfo.SEPERATER)[0] );
		sb.append(connector);
		sb.append(VaaInfo.getDateTime("yyyyMMdd"));
		sb.append(connector);
		sb.append(VaaInfo.getDateTime("HHmm"));
		
		//handle Corrections
		String corr = volcano.getCorr();
		if( corr!=null && corr.length()>0)
			sb.append(connector).append(corr);
		
		//TODO: see @4103 of nmap_pgvolw.c if(corr)
		
		sb.append(".txt");//2010-04-12 save txt NOT xml
		
		return sb.toString();
	}

	private String getVolNameId(){
		
		return volcano==null ? "" : volcano.getName()+VaaInfo.SEPERATER+volcano.getNumber();
	}
	

	
	private String getVolcanoLoc(){
		//TODO: see @3805 of nmap_gpvolw.c
		return volcano==null ? "" : volcano.getTxtLoc();
	}

	
	private String getAdvisoryNo(){
		//TODO: see @3859 of nmap_pgvolw.c
		return volAttrDlgInstance.getYear()+"/"+volAttrDlgInstance.getAdvisoryNo();
	}


	public Volcano getVolcano() {
		return volcano;
	}

	public void setVolcano(Volcano volcano) {
		this.volcano = volcano;
	}
	
	//2010-03-09
	public String getFileContent2(){
		return txtFileContent;		
	}
	
	/**
	 * setter for the content of the dialog
	 * @param txt
	 */
	
	public void setTxtFileContent(String txt){
		txtFileContent = txt.trim();
	}

}
