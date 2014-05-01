/*
 * gov.noaa.nws.ncep.ui.pgen.controls.PgenRestoreDialog
 * 
 * 31 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import java.io.File;
import java.io.FilenameFilter;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;

import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;

/**
 * Dialog used for PGEN Restore feature.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/09		#158		S. Gilbert	Initial creation
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class PgenRestoreDialog extends Dialog {
    
	private final String dlgTitle = "PGEN Restore";
    
	private Composite dlgArea = null;
    private Table fileTable = null;
    
	private static final int RESTORE_ID = IDialogConstants.CLIENT_ID + 7597;
	private static final int EMPTY_ID = IDialogConstants.CLIENT_ID + 7598;
	
	private Button restoreBtn = null;   
    private Button emptyBtn = null;
    private Button cancelBtn = null;
 
    private String tmpdir;
    private FilenameFilter filter;
          
    private boolean filesFound;
    
    /*
     *  Constructor
     */
    public PgenRestoreDialog( Shell parShell ) throws VizException {
    	super ( parShell );
        setShellStyle( SWT.RESIZE | SWT.PRIMARY_MODAL ); 
    	tmpdir = PgenUtil.getTempWorkDir();

    	/*
    	 * Create filter for PGEN temporary recovery files
    	 */
    	filter = new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return (  name.startsWith(PgenUtil.RECOVERY_PREFIX) &&
					      name.endsWith(PgenUtil.RECOVERY_POSTFIX) );
			};
    	};
    	
    }  
 
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {

        super.configureShell( shell );
        
        shell.setText( dlgTitle );
    }
            
    /*
     * (non-Javadoc)
     * Create all of the widgets on the Dialog
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {
        
    	dlgArea = (Composite) super.createDialogArea(parent);

		GridLayout gl = new GridLayout( 1, true);
		gl.marginTop = 5;
		gl.marginBottom = 5;
		gl.verticalSpacing = 10;
    	dlgArea.setLayout( gl );
           	
        /*
         *  Create a label for the table
         */
        Label info = new Label(dlgArea, SWT.NONE);
        info.setText("Select a PGEN Session to restore:");
        info.setLayoutData( new GridData() );
        
        fileTable = new Table(dlgArea, SWT.SINGLE | SWT.BORDER );
    	fileTable.setHeaderVisible (true);
    	fileTable.setLinesVisible (true);
    	
    	/*
    	 * Add two columns to table
    	 */
    	String[] titles = {"PGEN Session", "Timestamp"};
    	for (int i=0; i<titles.length; i++) {
    		TableColumn column = new TableColumn (fileTable, SWT.NONE);
    		column.setText (titles [i]);
    	}

    	SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
		File dir = new File(tmpdir);
		
		/*
		 * Add recovery files to table
		 */
    	for ( File f : dir.listFiles(filter) ) {
    		TableItem item = new TableItem (fileTable, SWT.NONE);
    		item.setText (0, f.getName());
    		Calendar time = Calendar.getInstance();
    		time.setTimeInMillis( f.lastModified() );
    		item.setText (1, sdf.format(time.getTime()) );
    		item.setData(f);
    	}
    	
    	/*
    	 * If no recovery files, let user know
    	 */
    	filesFound = true;
    	if ( dir.listFiles(filter).length == 0 ) {
    		TableItem item = new TableItem (fileTable, SWT.NONE);
    		item.setText (0, "No Data Available.");
    		item.setData(null);
        	filesFound = false;
    	}
    	
    	/*
    	 * pack each column
    	 */
    	for (int i=0; i<titles.length; i++) {
    		fileTable.getColumn(i).pack();
    	}
    	
        fileTable.setLayoutData( new GridData(SWT.FILL, SWT.FILL, true, true) );

        return dlgArea;
     }
    
    /*
     *  Create Restore/Empty/Done buttons
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {       
        
    	restoreBtn = createButton( parent, RESTORE_ID, "Restore", false );

    	emptyBtn = createButton( parent, EMPTY_ID, "Empty List", false );
    	
    	//cancelBtn = createButton( parent, IDialogConstants.CANCEL_ID, "Done", false );//TTR 47
    	cancelBtn = createButton( parent, IDialogConstants.CANCEL_ID, "Cancel", false );
    	
    	restoreBtn.setEnabled(filesFound);
    	emptyBtn.setEnabled(filesFound);

    }

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		super.buttonPressed(buttonId);
		
		if ( buttonId == RESTORE_ID ) {

			TableItem[] items = fileTable.getSelection();
			/*
			 * Read elements from recovery file and add to PGEN resource
			 */
			String fileName = new String(items[0].getData().toString());
	   	    Products products = FileTools.read( fileName );
	   	    PgenSession.getInstance().getPgenResource().replaceProduct( ProductConverter.convert( products ) );     
	   	    super.buttonPressed(IDialogConstants.OK_ID);

		}
		else if ( buttonId == EMPTY_ID ) {
			/*
			 * remove all recovery files
			 */
			fileTable.removeAll();
			deleteFiles();
			restoreBtn.setEnabled(false);
			emptyBtn.setEnabled(false);
		}
		
	}

	/*
	 * Remove all PGEN recovery files in the temp directory
	 */
	private void deleteFiles() {
		
		File dir = new File(tmpdir);
		for ( File f : dir.listFiles(filter) ) {
			f.delete();
		}
		
	}
    
}



