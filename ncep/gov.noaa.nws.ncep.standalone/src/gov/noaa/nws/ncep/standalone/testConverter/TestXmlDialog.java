package gov.noaa.nws.ncep.standalone.testConverter;

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
 * TestXmlDialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/21/2010   271         Q. Zhou     Initial created
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class TestXmlDialog extends  TitleAreaDialog  {
	
	private Text text1;
	private Text text2;
; 
	private String in = "";  
	private String out = "";  

	public TestXmlDialog(Shell parentShell) {
		super(parentShell);
		
		}

	
	protected Control createContents(Composite parent) {
		Control contents = super.createContents(parent);
		setTitle("To compare an XML file and an XML file converted from the vgf file.\n" +
				"Please input two XML files in the Original and the Converted text fields.");
		
		setMessage("Click the Compare button and observe the testing results on the background.");
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
		label1.setText("Original File: ");
		text1 = new Text(parent, SWT.BORDER);
		text1.setText(in);
        text1.setLayoutData(new GridData(600, SWT.DEFAULT));
		 
		Label label2 = new Label(parent, SWT.NONE);
		label2.setText("Converted File: ");
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
		btOK.setText("Compare");
		btOK.setFont(JFaceResources.getDialogFont());
		btOK.setData(new Integer(id));
		btOK.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				String s1 = text1.getText().trim();
				String s2 = text2.getText().trim();
				String s1Tem ;
//				if ( !s1.endsWith(".xml")|| s1.endsWith(".vgf")) {
//					s1Tem = s1.substring(0, s1.lastIndexOf("/"));
//				} else {
//					s1Tem = s1;
//				}
								
				boolean fileconverted = false;
								
				if (s1.length() == 0 || s2.length() == 0 ) {
					setErrorMessage("Please input the file path.");
				} 
				else if (!new File(s1).exists()) {
					setErrorMessage("The original file does not exist");
				} 
				else if (!new File(s2).exists()) {
					setErrorMessage("The converted file does not exist");
				}				
				
				else {															
					try {
						fileconverted = (new TestXmlConvert().testXml(s1, s2));
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} //File can handle it
					
					if (fileconverted)
						setErrorMessage( "The two files contain the same information.");	
					else
						setErrorMessage( "The two files contain the different information.");
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
