/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.SymbolAttrDlg
 * 
 * 20 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrDialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Singleton attribute dialog with the volcano list window.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09		#116			B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class VolcanoAttrDlg extends LabeledSymbolAttrDlg {
	
	static private VolcanoAttrDlg INSTANCE = null;
	static private VolcanoListDlg volList = null;

	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private VolcanoAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);
        volList = new VolcanoListDlg(parShell);

    }
	
	/**
	 * Creates a volcano attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static SymbolAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new VolcanoAttrDlg( parShell );
				
			} catch (VizException e) {
				
				e.printStackTrace();
				
			}	
		}
		
		return INSTANCE;
		
	} 	
	
	@Override
	public int open(){
		
		int rt = super.open();
		
    	volList.setBlockOnOpen( false );
    	
    	Point loc = this.getShell().getLocation();
    	
    	volList.create();
    	volList.getShell().setLocation(loc.x+this.getShell().getSize().x, loc.y);
		volList.open();
		
		return rt;
		
	}
	
	@Override
	public boolean close(){
		
		volList.close();
		return super.close();
		
	}
	
	/**
	 * Dialog class for volcano list
	 * @author bingfan
	 *
	 */
	private class VolcanoListDlg  extends CaveJFACEDialog {

		protected VolcanoListDlg(Shell parentShell) {
			super(parentShell);
	        setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
		}
		
		/**
		 * Creates the dialog area
		 */
		@Override
		public Control createDialogArea(Composite parent) {
			
		        top = (Composite) super.createDialogArea(parent);

		        // Create the main layout for the shell.
		        GridLayout mainLayout = new GridLayout(2, false);
		        mainLayout.marginHeight = 3;
		        mainLayout.marginWidth = 3;
		        top.setLayout(mainLayout);

		        // Initialize all of the menus, controls, and layouts
		        this.initializeComponents();
		 
		        return top;
		        
		}
		
		/**
		 * Creates buttons, menus, and other controls in the dialog area
		 */
		protected void initializeComponents() {
			
	        this.getShell().setText("Volcano List");
	        
	        Text volText = new Text(top, SWT.LEFT);
	        volText.setLayoutData(new GridData(120, 20));

	        Combo volMenu = new Combo(top, SWT.DROP_DOWN|SWT.READ_ONLY);
	   /*     Menu vol = new Menu( volMenu );
	        
	        MenuItem notL = new MenuItem(SWT.CASCADE);
	        volMenu.add
	        */
	        volMenu.add("Not listed");
	        volMenu.add("Aa-Am");
	        volMenu.add("An-Az");

	        volMenu.select(0);
	        volMenu.pack();
	        
		}
		
		@Override
		public Control createButtonBar(Composite parent){
			return parent;
			
		}
	}
	
}
