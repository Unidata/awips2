/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenProd
 * 
 * 12 January 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.productmanage.ProductConfigureDialog;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProdType;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;

import java.io.File;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.LinkedHashMap;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * Implements PGEN "Prod" tool to generate products
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/11			?		B. Yin   	Initial Creation.
 * 11/11			? 		B. Yin		Write the text products to a predefined location 
 * 03/12		#616		B. Yin		Write the product to activity/prod/prod type/file
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenProd extends AbstractPgenTool {
	
    public PgenProd(){
    	
    	super();
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool() {

    	super.activateTool();

    	ProdDialog dlg = new ProdDialog();
    	dlg.open();
        
        //NmapUiUtils.setPanningMode();

    }
    
    /**
     * Dialog that contains a list of check boxes of ProdTypes for the 
     * active product.
     * @author bingfan
     *
     */
    private class ProdDialog extends Dialog  {
    	
    	//private ProductType curType;
    	
    	//Map between check box in the GUI and ProdType
    	private HashMap<Button, ProdType> typeMap;

    	/**
    	 * Constructor
    	 */
    	private ProdDialog(){
    		super(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
    		typeMap= new LinkedHashMap<Button, ProdType>();
    	}
    	
    	/**
    	 * Creates the dialog area
    	 */
    	@Override
    	public Control createDialogArea(Composite parent) {
    		
            	this.getShell().setText("Products");

    	        Composite top = (Composite) super.createDialogArea(parent);

    	        // Create the main layout for the shell.
    	        GridLayout mainLayout = new GridLayout(2, false);
    	        mainLayout.marginHeight = 3;
    	        mainLayout.marginWidth = 3;
    	        mainLayout.horizontalSpacing = 30;
    	        top.setLayout(mainLayout);

    	        // Initialize all of the menus, controls, and layouts
    	        String typeName = drawingLayer.getActiveProduct().getType();
    	        ProductType curType = ProductConfigureDialog.getProductTypes().get(typeName);
    	        
    	        //create a list of check boxes
    	        if ( curType != null ){
    	        	
    	        	for ( ProdType pt : curType.getProdType() ) {
    	        		Button ptBtn = new Button( top, SWT.CHECK);
    	        		
    	        		//Label nameLbl = new Label( top, SWT.None );
    	        		ptBtn.setText(pt.getName());
    	        		ptBtn.setSelection(true);
    	        		
    	        		typeMap.put(ptBtn, pt);
    	        		
    	        		Label typeLbl = new Label( top, SWT.None );
    	        		typeLbl.setText( (pt.getType() == null)?"":pt.getType() );
    	        		
    	        	}
    	        }
    	        else {
    	        	Label nothing = new Label( top, SWT.NONE );
    	        	nothing.setText("No product type in current product.");
    	        }
    	        
				return top;
    	}
    	
    	/**
    	 * Create buttons on the button bar
    	 */
    	@Override
    	public void createButtonsForButtonBar(Composite parent){

    		super.createButtonsForButtonBar(parent);
    		getButton(IDialogConstants.OK_ID).setText("Go");
    		getButton(IDialogConstants.CANCEL_ID).setText("Close");

    	}
    	
       	@Override
    	/**
    	 * Set the location of the dialog
    	 */
    	public int open(){

    		if ( this.getShell() == null ){
    			this.create();
    		}
    		
       	    this.getShell().setLocation(this.getShell().getParent().getLocation().x + 30,
       	    		this.getShell().getParent().getLocation().y);
      	    
       	    return super.open();
    		
    	}
       	
       	@Override
       	public boolean close(){
       		//remove the all PGEN tool from tool manager list
       		PgenSession.getInstance().getPgenResource().deactivatePgenTools();
       		return super.close();
       	}
       	
    	/*
    	 * 
    	 * (non-Javadoc)
    	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
    	 */
    	@Override
    	public void okPressed(){
    		for ( Button btn : typeMap.keySet()) {
    			if ( btn.getSelection() ){
    				//create and open the text message dialog
    				ProdTextDlg msgDlg = new ProdTextDlg(ProdDialog.this.getShell(),
									typeMap.get(btn));
    				
					msgDlg.setBlockOnOpen(true);
					msgDlg.message = typeMap.get(btn).generateProd(drawingLayer.getActiveProduct());
					
					int rt = msgDlg.open();
					if ( rt == Dialog.CANCEL ) return;
    			}
    		}
    	}
    }
    
    /**
     * Window that displays the contents of a PGEN product.
     * @author bingfan
     *
     */
    private class ProdTextDlg extends Dialog {

    	ProdType pt ;
    	
    	//top level container for all widgets
    	private Composite top;

    	//Text widget for the contents
    	private Text messageBox;
    	
    	private String message;
    	
    	//dialog size
    	private final int NUM_LINES = 25;
    	private final int NUM_COLUMNS = 80;
    	
    	/*
    	 * constructor
    	 */
    	private ProdTextDlg(Shell parentShell, ProdType pt ) {
    		super(parentShell);		
    		this.setShellStyle(SWT.TITLE | SWT.APPLICATION_MODAL | SWT.CLOSE );
    		this.pt = pt;
    		message ="";
    	}

    	/**
    	 * Creates the dialog area
    	 */
    	@Override
    	public Control createDialogArea(Composite parent) {
    		
    		// Set title
    		getShell().setText( pt.getName() );
    		
    		top = (Composite) super.createDialogArea(parent);
    		
            /*
             *  Create the main layout for the dialog area.
             */
            GridLayout mainLayout = new GridLayout(1, true);
            mainLayout.marginHeight = 3;
            mainLayout.marginWidth = 3;
            top.setLayout(mainLayout);

            /*
             *  Create a text box for the message
             */
            messageBox = new Text(top, SWT.MULTI | SWT.BORDER | SWT.READ_ONLY 
            					| SWT.H_SCROLL | SWT.V_SCROLL);
            messageBox.setFont(new Font(messageBox.getDisplay(),"Courier",12, SWT.NORMAL) );
    		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
    		
    		//  Calculate approximate size of text box to display 25 lines at 80 characters each
    		gd.heightHint = NUM_LINES * messageBox.getLineHeight();       
    		GC gc = new GC (messageBox);
    		FontMetrics fm = gc.getFontMetrics ();
    		gd.widthHint = NUM_COLUMNS * fm.getAverageCharWidth ();

            messageBox.setLayoutData(gd);
            messageBox.setText(message);
            
            //  Make sure to dispose of font
            messageBox.addDisposeListener(new DisposeListener() {

    			@Override
    			public void widgetDisposed(DisposeEvent e) {
    				Text w = (Text)e.widget;
    				w.getFont().dispose();
    			}
            	
            });
    		
    		return top;
    	}
    	
    	@Override
    	/**
    	 * Set the location of the dialog
    	 */
    	public int open(){

    		if ( this.getShell() == null ){
    			this.create();
    		}
    		
       	    this.getShell().setLocation(this.getShell().getParent().getLocation().x + 300,
       	    		this.getShell().getParent().getLocation().y);
      	    
       	    return super.open();
    		
    	}
    	
       	@Override
    	public void okPressed(){
       		
       		
       		if ( pt.getOutputFile() != null && !pt.getOutputFile().isEmpty()) {
       			
				String pd = ProductConfigureDialog.getProductTypes().get(drawingLayer.getActiveProduct().getType()).getType();
				String pd1 = pd.replaceAll(" ", "_");
       			
       			String dirPath = PgenUtil.getPgenOprDirectory() + 
       							File.separator + pd1 +
       							File.separator + "prod" +
       							File.separator + pt.getType().replaceAll(" ", "_");
       			
       			String filePath = dirPath +File.separator + pt.getOutputFile();
       			
        		File out = new File(  filePath );
        		
        		int code = MessageDialog.OK;
        		
        		if ( out.exists() ) {
        			String msg = "File " + out + " exists. Do you want to overwrite it?";

        			MessageDialog confirmDlg = new MessageDialog( 
        					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
        					"Output PGEN Products", null, msg,
        					MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);

        			code = confirmDlg.open();
        			
        		}
        		
        		
        		if ( code == MessageDialog.OK ){
        			
        			FileTools.writeFile(filePath, message);
        			
        		}
        	}
       		
       		super.okPressed();
       	}
    }

	@Override
	public IInputHandler getMouseHandler() {
		return null;                 // none
	}
}