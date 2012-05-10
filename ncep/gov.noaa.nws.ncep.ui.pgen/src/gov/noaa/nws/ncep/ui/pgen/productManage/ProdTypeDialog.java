/*
 * gov.noaa.nws.ncep.ui.pgen.productManage
 * 
 * 10 January 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.productManage;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.productTypes.ProdType;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Implements a dialog to define a ProdType such as XML/KML/Text etc..
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/11			?		B. Yin   	Initial Creation.
 * 11/11			?		B. Yin		Put the style sheets in localization
 *
 * </pre>
 * 
 */

public class ProdTypeDialog  extends Dialog {

	//product types
	private static String[] typeStr = {"GEMPAK/GIF", "XML",  "Text Prod", "CAP", "KML" };
	
	//Production configuration dialog instance 
	private ProductConfigureDialog pcd;
	
	//ProdType instance for the dialog
	private ProdType pt;
	
	//ProdType button of pt in the Product configuration dialog
	private Button	ptBtn;
	
	//ProdType type label of pt in the Product configuration dialog
	private Label typeLbl;
	
	//ProdType combo list
	private Combo typeCbo;

	//ProdType name
	private Text nameTxt;
	
	//Style Sheet text field
	private Text ssTxt;
	
	//Output file text field
    private Text outputTxt;
	
    //Constructor
	protected ProdTypeDialog(ProductConfigureDialog pcd ) {
		super( pcd.shell );
		this.pcd = pcd;
		this.pt = null;
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE );
	}
	
	//Constructor
	protected ProdTypeDialog(ProductConfigureDialog pcd, ProdType pt, Button ptBtn, Label typeLbl ) {
		super( pcd.shell );
		this.pcd = pcd;
		this.pt = pt;
		this.ptBtn = ptBtn;
		this.typeLbl = typeLbl;
		
		this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE );
	}
	
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {

		//set title
		this.getShell().setText("Product Type");

		Composite top = (Composite) super.createDialogArea(parent);

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(2, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);

		// Initialize all of the menus, controls, and layouts
		
		//Create the list of types
		Label typeLbl = new Label( top, SWT.None );
		typeLbl.setText("Type:");
		typeCbo = new Combo( top, SWT.DROP_DOWN | SWT.READ_ONLY );

		typeCbo.setItems(typeStr);

		if ( pt!= null ){
			for ( int ii =0; ii < typeStr.length; ii++ ){
				if ( pt.getType().equalsIgnoreCase(typeStr[ii])){
					typeCbo.select(ii);
					break;
				}
			}
		}
		else {
			typeCbo.select( 2 );
		}

		//Name widgets
		Label nameLbl = new Label( top, SWT.None );
		nameLbl.setText("Name:");
		nameTxt = new Text( top, SWT.SINGLE | SWT.BORDER  );
		nameTxt.setLayoutData( new GridData( 60, 18 ) );

		if ( pt != null ){
			nameTxt.setText( pt.getName() );
		}

		//Style Sheet file widget and the 'browse' button
		Label styleSheetLbl = new Label( top, SWT.None );
		styleSheetLbl.setText("Style Sheet File:");

		Composite ssComp = new Composite( top, SWT.NONE);
		GridLayout ssLayout = new GridLayout(2, false);
		ssLayout.marginWidth = 0;
		ssComp.setLayout( ssLayout );

		ssTxt = new Text( ssComp, SWT.SINGLE | SWT.BORDER  );
		if ( pt != null ) ssTxt.setText(pt.getStyleSheetFile());

		Button ssBtn = new Button( ssComp, SWT.PUSH );
		ssBtn.setText( "Browse");

		ssBtn.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected( SelectionEvent event ) {

				String[] filterNames = new String[] { "XSLT Files (*.xlt)" };
				String[] filterExtensions = new String[] { "*.xlt", "*.xslt", "*.*" };
//				String filterPath = PgenUtil.CURRENT_WORKING_DIRECTORY;
				String filterPath = PgenUtil.getWorkingDirectory();

				String selectedFile = selectFile( ProdTypeDialog.this.getShell(), SWT.OPEN, filterNames,
						filterExtensions, filterPath, null, false );
				if ( selectedFile != null ) {
					ssTxt.setText(  selectedFile );
				}           	
			}
		});      
		
		//output file widgets
		Label outputLbl = new Label( top, SWT.None );
		outputLbl.setText("Output File:");
		outputTxt = new Text( top, SWT.SINGLE | SWT.BORDER  );
		if ( pt!=null) outputTxt.setText(pt.getOutputFile());
		
		//outputLbl.setEnabled(false);
		//outputTxt.setEnabled(false);

		return top;

	}

	@Override
	/**
	 * Add a new type in the Product Configuration dialog or
	 * update the information there
	 */
	public void okPressed(){
		
		//Warning if name is blank
		if (nameTxt.getText().isEmpty()){
			MessageDialog infoDlg = new MessageDialog( 
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
					"Error", null, "Please provide a name!",
					MessageDialog.ERROR, new String[]{"OK"}, 0);
					infoDlg.open();
			return;
		}
		
		if ( pt == null ){
			//create a new type
			ProdType ptype =  new ProdType();
			ptype.setName(nameTxt.getText() );
			ptype.setType(typeCbo.getText());
			ptype.setStyleSheetFile(ssTxt.getText());
			loadStyleSheet(ssTxt.getText());
			ptype.setOutputFile(outputTxt.getText());
			pcd.addProdType( ptype );
		}
		else {
			//update an existed type
			pt.setName(nameTxt.getText());
			pt.setType(typeCbo.getText());
			pt.setStyleSheetFile(ssTxt.getText());
			loadStyleSheet(ssTxt.getText());
			pt.setOutputFile(outputTxt.getText());
			ptBtn.setText(pt.getName());
			typeLbl.setText(pt.getType());
			//ptBtn.pack();
		}
		
		super.okPressed();
	}
	
	/**
	 * Create a file selection dialog
	 * 
	 * @param sh
	 * @param mode
	 * @param nameFilter
	 * @param extensionFilter
	 * @param pathFilter
	 * @param defaultFile
	 * @param overWrite
	 * @return dialog.open()
	 */
	private String selectFile( Shell sh, int mode, String[] nameFilter,
								String[] extensionFilter, String pathFilter,
								String defaultFile, boolean overWrite ) {
		
        FileDialog dialog = new FileDialog( sh, mode );
        dialog.setFilterNames( nameFilter );
        dialog.setFilterExtensions( extensionFilter );
        dialog.setFilterPath( pathFilter );
        if ( defaultFile != null ) {
            dialog.setFileName ( defaultFile );
        }
        dialog.setOverwrite( overWrite );               
                
        return dialog.open();
            	               
	}
	
	private void loadStyleSheet(String filePath){
		
		File sFile = new File( filePath ); 
		if ( !sFile.canRead() ){
    			MessageDialog infoDlg = new MessageDialog( 
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
						"Warning", null, "Stlyle sheet file " + filePath + " does not exist!",
						MessageDialog.WARNING, new String[]{"OK"}, 0);

				infoDlg.open();
		}
		else {
	    	//put into localization
		    LocalizationContext userContext = PgenStaticDataProvider.getProvider().getLocalizationContext(
					LocalizationType.CAVE_STATIC, LocalizationLevel.USER );
		    
		    LocalizationFile lFile = PgenStaticDataProvider.getProvider().getLocalizationFile( 
		    		userContext, getStyleSheetFileName(nameTxt.getText()));
		    
		    //check file
		    InputStream is;
		    try {
		    	is = new FileInputStream( sFile );

		    	// Get the size of the file
		    	long length = sFile.length();

		    	// ensure that file is not larger than Integer.MAX_VALUE.
		    	if (length > Integer.MAX_VALUE) {
		    		return;
		    	}

		    	// Create the byte array to hold the data
		    	byte[] bytes = new byte[(int)length];

		    	// Read in the bytes
		    	int offset = 0;
		    	int numRead = 0;
		    	while (offset < bytes.length
		    			&& (numRead=is.read(bytes, offset, bytes.length-offset)) >= 0) {
		    		offset += numRead;
		    	}

		    	lFile.write( bytes );

		    } catch (FileNotFoundException e1) {
		    	// TODO Auto-generated catch block
		    	e1.printStackTrace();
		    } catch (LocalizationException e) {
		    	// TODO Auto-generated catch block
		    	e.printStackTrace();
		    } catch (IOException e) {
		    	// TODO Auto-generated catch block
		    	e.printStackTrace();
		    }
		}
		
	}
	
	static public String getStyleSheetFileName( String prodName ){
		String ret = "";
		if ( prodName == null || prodName.isEmpty() ) return ret;
		
		String pn = prodName.replaceAll(" ", "_");
		
		return PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + File.separator + "xslt" + File.separator + "prod" + File.separator + pn  + ".xslt";
	}
	
}
