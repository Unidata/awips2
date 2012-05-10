/*
 * gov.noaa.nws.ncep.standalone.fop.FOPDialog
 * 
 * Date created (as Jan 29, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.fop;

import java.io.File;
import java.io.IOException;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/**
 * Flood Outlook Product (FOP) - GUI part of fop application.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer      Description
 * ------------ ----------  ------------  --------------------------
 * 01/29/2010   220         mlaryukhin    Initial created
 * 
 * </pre>
 * 
 * @author mlaryukhin
 */
public class FOPDialog extends TitleAreaDialog {

	private Text		inputFilename;
	private Text		outputFilename;
	private Label		statusLbl;
	private File		currentDir;
	private final int	TEXT_SIZE	= 600;
	private final Color	RED			= new Color(null, 255, 0, 0);
	private final Color	DARK_GREEN	= new Color(null, 0, 100, 0);

	/**
	 * Constructor.
	 * 
	 * @param parentShell
	 */
	public FOPDialog(Shell parentShell) {
		super(parentShell);
		currentDir = new File(".");
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createContents(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		Control contents = super.createContents(parent);
		setTitle("Flood Outlook Product (FOP)");
		setMessage("This program translates flood locations ASCII file to PGen XML format file.");
		return contents;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(final Composite parent) {
		StyledText area = new StyledText(parent, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
		GridData gd = new GridData(GridData.VERTICAL_ALIGN_FILL);
		gd.horizontalSpan = 3;
		gd.heightHint = 200;
		gd.widthHint = TEXT_SIZE + 100;

		area.setLayoutData(gd);
		area.setText(FOP.getProgramDescription().toString());
		Font font = new Font(parent.getDisplay(), "Courier", 10, SWT.NORMAL);
		area.setFont(font);
		area.setEditable(false);

		Composite comp = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 3;
		comp.setLayout(layout);

		new Label(comp, SWT.NONE).setText("Input file name 1: ");
		inputFilename = new Text(comp, SWT.LEFT);
		gd = new GridData(600, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		inputFilename.setLayoutData(gd);

		new Label(comp, SWT.NONE).setText("Output file name: ");
		outputFilename = new Text(comp, SWT.LEFT);
		gd = new GridData(TEXT_SIZE, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		outputFilename.setLayoutData(gd);

		statusLbl = new Label(comp, SWT.RIGHT);
		gd = new GridData(SWT.RIGHT, SWT.TOP, true, false, 3, 1);
		gd.widthHint = TEXT_SIZE;
		statusLbl.setLayoutData(gd);

		// default values
		try {
			inputFilename.setText(currentDir.getCanonicalPath() + File.separator + "input");
			outputFilename.setText(currentDir.getCanonicalPath() + File.separator + "output.xml");
		} catch (IOException e) {
			inputFilename.setText("");
			outputFilename.setText("");
		}

		return parent;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createOkButton(parent, IDialogConstants.OK_ID);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/**
	 * Creates a button.
	 * 
	 * @param parent
	 * @param id
	 * @return
	 */
	private Button createOkButton(Composite parent, int id) {
		// increment the number of columns in the button bar
		((GridLayout) parent.getLayout()).numColumns++;
		Button button = new Button(parent, SWT.PUSH);
		button.setText(" Convert ");
		button.setData(id);
		button.addSelectionListener(new OkListener());
		Shell shell = parent.getShell();
		if (shell != null) {
			shell.setDefaultButton(button);
		}
		setButtonLayoutData(button);
		return button;
	}

	/**
	 * Listener implementation
	 * 
	 * @author mlaryukhin
	 */
	class OkListener extends SelectionAdapter {

		@Override
		public void widgetSelected(SelectionEvent event) {

			String[] args = { inputFilename.getText(), outputFilename.getText() };

			try {
				
				new FOP(args).execute();
				
			} catch (Exception e) {
				statusLbl.setForeground(RED);
				statusLbl.setText("Error");
				return;
			}
			statusLbl.setForeground(DARK_GREEN);
			statusLbl.setText("Complete");
		}
	}
}
