package gov.noaa.nws.ncep.standalone.xmlConverter;

import java.io.File;
import java.io.IOException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Control;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.resource.JFaceResources;

/**
 * XMLConvertDialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/25/2009   137         Q. Zhou     Initial created
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class XmlConvertDialog extends  TitleAreaDialog { 
	
	private Text text1;
	private Text text2;

	private String in = "";  
	private String out = "";  

	public XmlConvertDialog(Shell parentShell) {
		super(parentShell);
		
		}

	
	protected Control createContents(Composite parent) {
		Control contents = super.createContents(parent);
		setTitle("To convert the XML files ending with xml to the text files.\n" +
				"Please input the source file name or source file directory, and the destination directory.");
		
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
              
        Label label4 = new Label(parent, SWT.BOLD);
        label4.setText(" ");
        Label label5 = new Label(parent, SWT.BOLD);
        label5.setText(" ");
        Label label6 = new Label(parent, SWT.BOLD);
        label6.setText(" ");
        
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
				String s1Tem ;
				if (s1.endsWith("*")|| s1.endsWith("*.xml")) {
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
					fileconverted = (new XmlLoading().loading(s1, s2)); 
							
					setErrorMessage(fileconverted +" files are converted.  " + "The Convertion is finished.");		
					} catch(IOException e) {
					}
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
