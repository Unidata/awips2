package gov.noaa.nws.ncep.standalone.vgfConverter;

import java.io.File;
import java.io.IOException;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * ConvertDialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/25/2010   203         Q. Zhou     Initial created
 * 1/25/2011   137         Q. Zhou     Modified getColorMin, and getColorTag
 * 11/2/2011   480         Q. Zhou     Added Activity and subActivity input fields to vgfConverter
 * 12/12/2011  548         Q. Zhou     Added contour table file argument.
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class VgfConvertDialog extends  TitleAreaDialog { 
	
	private Text text1;
	private Text text2;
	private Text text3;
	private Text text4;
	private Text text5;

	//private String in = "/export-1/cdbsrv/nawdev/nawips/gempak/data/vgf"; //"/export/cdbsrv/qzhou/work/data1";  
	//private String out = NmapCommon.getLutsDir(); //"/export/cdbsrv/qzhou/work/data2"; 
	private String in = "";  
	private String out = "";  
	private String activity = "";
	private String subActivity = "";
	private String contTbl = "";

	public VgfConvertDialog(Shell parentShell) {
		super(parentShell);
		
		}

	
	protected Control createContents(Composite parent) {
		Control contents = super.createContents(parent);
		setTitle("To convert the binary VGF files that end with vgf to XML files.\n" +
				"Please input the source file directory or source file name, and the destination directory.");
		
		setMessage("When click the Convert button, the converted files are saved to the destination directory.\n" +
        		"It converts one file, or converts all files in the directory if a source directory was input.");
		return contents;
	} 

	@Override
	protected Control createDialogArea(Composite parent) {

		// return super.createDialogArea(parent);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		parent.setLayout(layout);
		
		Label label0 = new Label(parent, SWT.BOLD);
        label0.setText(" ");
        Label label00 = new Label(parent, SWT.BOLD);
        label00.setText(" ");
        
		Label label1 = new Label(parent, SWT.NONE);
		label1.setText("Source Directory File: ");
		text1 = new Text(parent, SWT.BORDER);
		text1.setText(in);
        text1.setLayoutData(new GridData(600, SWT.DEFAULT));
		 
		Label label2 = new Label(parent, SWT.NONE);
		label2.setText("Destination Directory: ");
		text2 = new Text(parent, SWT.BORDER);
		text2.setText(out);
        text2.setLayoutData(new GridData(600, SWT.DEFAULT));
             
        Label label3 = new Label(parent, SWT.NONE);
		label3.setText("Activity: ");
		text3 = new Text(parent, SWT.BORDER);
		text3.setText(activity);
        text3.setLayoutData(new GridData(600, SWT.DEFAULT));
        
        Label label4 = new Label(parent, SWT.NONE);
		label4.setText("Sub activity: ");
		text4 = new Text(parent, SWT.BORDER);
		text4.setText(subActivity);
        text4.setLayoutData(new GridData(600, SWT.DEFAULT));
        
        Label label5 = new Label(parent, SWT.NONE);
		label5.setText("Contour Tbl: ");
		text5 = new Text(parent, SWT.BORDER);
		text5.setText(contTbl);
        text5.setLayoutData(new GridData(600, SWT.DEFAULT));
                
        Label label6 = new Label(parent, SWT.BOLD);
        label6.setText(" ");
        Label label7 = new Label(parent, SWT.BOLD);
        label7.setText(" ");
        Label label8 = new Label(parent, SWT.BOLD);
        label8.setText(" ");
        
		return parent;
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createOkButton(parent, IDialogConstants.OK_ID,
		IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID,
		IDialogConstants.CANCEL_LABEL, false);
	}

	protected Button createOkButton(Composite parent, int id, String label,
		boolean defaultButton) {
		// increment the number of columns in the button bar
		((GridLayout) parent.getLayout()).numColumns++;
		Button btOK = new Button(parent, SWT.PUSH);
		btOK.setText("Convert");
		btOK.setFont(JFaceResources.getDialogFont());
		btOK.setData(new Integer(id));
		btOK.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				String s1 = text1.getText().trim();
				String s2 = text2.getText().trim();
				String s3 = text3.getText().trim();
				String s4 = text4.getText().trim();
				String s5 = text5.getText().trim();
				
				String s1Tem ;
				if (s1.endsWith("*")|| s1.endsWith(".vgf")) {
					s1Tem = s1.substring(0, s1.lastIndexOf("/"));
				} else {
					s1Tem = s1;
				}
								
				int fileconverted = 0;
								
				if (s1.length() == 0 || s2.length() == 0 ) {
					setErrorMessage("Please input the directory.");
				} 
				else if (!new File(s1Tem).exists()) {
					setErrorMessage("The Source directory does not exist");
				} 
				else if (!new File(s2).exists()) {
					setErrorMessage("The Destination directory does not exist");
				}				
				
				else {															
					try {
						fileconverted = (new Convert()).convertMap(s1, s2, s3, s4, s5);
					} catch (IOException e) {
						System.out.println("The convertion failed.");
						//e.printStackTrace();
					} //File can handle it
							
					setErrorMessage(fileconverted +" files are converted.  " + "The Convertion is finished.");					
				}
			}
		});
		
		if (defaultButton) {
			Shell shell = parent.getShell();
			if (shell != null) {
				shell.setDefaultButton(btOK);
			}
		}
	
		setButtonLayoutData(btOK);
		return btOK;
	}

}
