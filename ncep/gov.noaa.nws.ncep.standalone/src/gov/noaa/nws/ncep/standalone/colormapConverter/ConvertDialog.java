package gov.noaa.nws.ncep.standalone.colormapConverter;

import java.io.File;
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
 * 11/30/2009   197         Q. Zhou     Initial created
 * 03/04/2009				M. Gao		Using localization extension to replace NmapCommon class
 * 08/12/2010               Q. Zhou     removed localization
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class ConvertDialog extends  TitleAreaDialog { //CaveSWTDialog { //
	
	private Text text1;
	private Text text2;
	private Text text3;
//	private Shell shell;
	private String lookup = "/export-1/cdbsrv/nawdev/software/gempak/tables/luts/enhance.tbl";
	private String in = "/export-1/cdbsrv/nawdev/software/gempak/tables/luts/";
	
	String fileDir = ConvertDialog.class.getProtectionDomain().getCodeSource().getLocation().toString(); // file:/usr1/qzhou/to11dr3/workspace/gov.noaa.nws.ncep.pgen/bin/	
	String dir = fileDir.substring(5, (fileDir.length()-5));
	String dir1 = dir.substring(0, dir.lastIndexOf("/"));

	private String out = dir1 + "/build.cave/static/common/cave/ncep/base/luts";
	//private String out = LocalizationManager.getInstance().getLocalizationFileNameDirectly("build.cave/static/common/cave/ncep", "luts", "base");
	
	public ConvertDialog(Shell parentShell) {
		super(parentShell);
		
		}

//	public Object open() {
//		shell = new Shell(getParent(), SWT.DIALOG_TRIM);
//		shell.setText("Colormap Converter");
//		shell.setSize(800, 600);
//	    shell.setLayout(new GridLayout(1, true));
//
//	    createDialog(shell);
//	    
//	    shell.pack();
//	    shell.open();
//	
//	    // Wait until the shell is disposed.
//	    Display display = getParent().getDisplay();
//	    while (!shell.isDisposed()) {
//	        if (!display.readAndDispatch()) {
//	            display.sleep();
//	        }
//	    }
//
//	    return null;
//	}
//	
//	private void createDialog(final Shell shell) {
//		Composite mainComp = new Composite(shell, SWT.NONE);
//        mainComp.setLayout(new GridLayout(1, false));
//        mainComp
//                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
//
//        
//        
//        Label label0 = new Label(shell, SWT.BOLD);
//        label0.setText("Please input the source directory and the destination directory, or use the default directories. \n" +
//        		"When click the OK button, all the color maps in the source directory will be converted and be recognized as satellite, \n" +
//        		"radar and other color maps.  They are saved to the related sub-directories of the destination directory.\n" +
//        		"Sample of directory:  /usr1/project/ \n\n");
//        Label label1 = new Label(shell, SWT.NONE);
//        label1.setText("Source Directory: ");
//               
//		text1 = new Text(shell, SWT.BORDER);
//		text1.setText(in);
//        text1.setLayoutData(new GridData(700, SWT.DEFAULT));
//          
//        Label label2 = new Label(shell, SWT.BOLD);
//        label2.setText("Destination Directory: ");
//
//		text2 = new Text(shell, SWT.BORDER);
//		text2.setText(out);
//        text2.setLayoutData(new GridData(700, SWT.DEFAULT));
//        
//        Label label3 = new Label(shell, SWT.BOLD);
//        label3.setText(" ");
//        
//        Composite body = new Composite(shell, SWT.NONE);
//        body.setLayout(new GridLayout(3, false));
//        
//        btOK = new Button(body, SWT.PUSH); 
//        btOK.setText("OK");
//        btOK.setLayoutData(new GridData(100, SWT.DEFAULT));
//        btOK.addSelectionListener(new SelectionAdapter() {
//        @Override
//            public void widgetSelected(SelectionEvent e) {
//                
//                new ColormapConvert().convertMap(text1.getText().trim(), text2.getText().trim());
//                }
//
//            });
//
//        btReset = new Button(body, SWT.PUSH); 
//        btReset.setText("Reset");
//        btReset.setLayoutData(new GridData(100, SWT.DEFAULT));
//        btReset.addSelectionListener(new SelectionAdapter() {
//        @Override
//            public void widgetSelected(SelectionEvent e) {
//        		text1.setText(in);
//        		text2.setText(out);
//            }
//        });
//        
//        btCancel = new Button(body, SWT.PUSH); 
//        btCancel.setText("Cancel");
//        btCancel.setLayoutData(new GridData(100, SWT.DEFAULT));
//        btCancel.addSelectionListener(new SelectionAdapter() {
//        @Override
//            public void widgetSelected(SelectionEvent e) {
//        		shell.dispose();
//            }
//        });
//        Label label4 = new Label(shell, SWT.BOLD);
//        label4.setText(" ");
//	}
	
	protected Control createContents(Composite parent) {
		Control contents = super.createContents(parent);
		setTitle("To convert the color map files that end with tbl.\n" +
				"Please input the source directory, the destination directory and the lookup file, or use the default.");
		
		setMessage("When click the Convert button, all the color map files in the source directory will be converted. \n" +
        		"The converted files are saved to the Satellite, Radar and Other sub-directories of the destination directory accordingly.");
		return contents;
	} 

	@Override
	protected Control createDialogArea(Composite parent) {
		// get etc/colormaps/ dir.  Remove these lines if using ncep/luts/
//		String out1 = out.substring(0, out.lastIndexOf("/"));
//		String out2 = out1.substring(0, out1.lastIndexOf("/"));
//		String out3 = out2.substring(0, out2.lastIndexOf("/"));
//		out = out3 + "/etc/colormaps/";
		
		// return super.createDialogArea(parent);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		parent.setLayout(layout);
		
		Label label0 = new Label(parent, SWT.BOLD);
        label0.setText(" ");
        Label label00 = new Label(parent, SWT.BOLD);
        label00.setText(" ");
        
		Label label1 = new Label(parent, SWT.NONE);
		label1.setText("Source Directory: ");
		text1 = new Text(parent, SWT.BORDER);
		text1.setText(in);
        text1.setLayoutData(new GridData(600, SWT.DEFAULT));
		 
		Label label2 = new Label(parent, SWT.NONE);
		label2.setText("Destination Directory: ");
		text2 = new Text(parent, SWT.BORDER);
		text2.setText(out);
        text2.setLayoutData(new GridData(600, SWT.DEFAULT));
        
        Label label3 = new Label(parent, SWT.NONE);
		label3.setText("Lookup File: ");
		text3 = new Text(parent, SWT.BORDER);
		text3.setText(lookup);
        text3.setLayoutData(new GridData(600, SWT.DEFAULT));
               
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
				String s3 = text3.getText().trim();
				int fileconverted = 0;
								
				if (s1.length() == 0 || s2.length() == 0 || s3.length() == 0) {
					setErrorMessage("Please input the directory or file name");
				} 
				else if (!new File(s1).exists()) {
					setErrorMessage("The Source directory does not exist");
				} 
				else if (!new File(s2).exists()) {
					setErrorMessage("The Destination directory does not exist");
				}
				else if (!new File(s3).exists()) {
					setErrorMessage("The lookup file does not exist");
				}
				
				else {
					try {
						if (s2.endsWith("/")) {
							if (!new File(s2 + "Satellite/").exists()) {
								new File(s2+ "Satellite/").mkdir();
							}
							if (!new File(s2 + "Radar/").exists()) {
								new File(s2+ "Radar/").mkdir();
							}
							if (!new File(s2 + "Other/").exists()) {
								new File(s2+ "Other/").mkdir();
							}
							
							fileconverted = (new ColormapConvert().convertMap(s1, s2, s3));
							
						}
						else if (!s2.endsWith("/") ) {
							if (!new File(s2 + "/Satellite/").exists()) {
								new File(s2+ "/Satellite/").mkdir();
							}
							if (!new File(s2 + "/Radar/").exists()) {
								new File(s2+ "/Radar/").mkdir();
							}
							if (!new File(s2 + "/Other/").exists()) {
								new File(s2+ "/Other/").mkdir();
							}
							
							fileconverted = (new ColormapConvert().convertMap(s1+"/", s2+"/", s3+"/"));
							
						}
						
						setErrorMessage(fileconverted +" files are converted.  " + "The Convertion is finished.");
					}
						
					catch (SecurityException e) {
						setErrorMessage("Can not create sub-directories Sat, Radar and Other.");
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
