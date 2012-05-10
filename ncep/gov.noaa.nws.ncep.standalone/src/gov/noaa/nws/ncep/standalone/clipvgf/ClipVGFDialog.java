/*
 * gov.noaa.nws.ncep.standalone.clipvgf.ClipVGFDialog
 * 
 * Date created (as Jan 12, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.clipvgf;

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
 * ClipVGFDialog - GUI part of ClipVGF application.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer      Description
 * ------------ ----------  ------------  --------------------------
 * 01/21/2010   197         mlaryukhin    Initial created
 * 
 * </pre>
 * 
 * @author mlaryukhin
 */
public class ClipVGFDialog extends TitleAreaDialog {

	private Text			server;
	private Text			inputFilename;
	private Text			clipBound;
	private Text			keepFlag;
	private Text			outputFilename;
	private Label			statusLbl;
	private File			currentDir;
	private final int		TEXT_SIZE		= 700;
	private final String	DEFAULT_SERVER	= "http://localhost:9581/services";
	private final Color		RED				= new Color(null, 255, 0, 0);
	private final Color		DARK_GREEN			= new Color(null, 0, 100, 0);

	public ClipVGFDialog(Shell parentShell) {
		super(parentShell);
		currentDir = new File(".");
	}

	@Override
	protected Control createContents(Composite parent) {
		Control contents = super.createContents(parent);
		setTitle("Clip PGen XML");
		setMessage("This program clips elements in a VGF file based on a bounds specification.");
		return contents;
	}

	@Override
	protected Control createDialogArea(final Composite parent) {
		StyledText area = new StyledText(parent, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
		GridData gd = new GridData(GridData.VERTICAL_ALIGN_FILL);
		gd.horizontalSpan = 3;
		gd.heightHint = 200;
		gd.widthHint = TEXT_SIZE + 100;

		area.setLayoutData(gd);
		area.setText(ClipVGF.getProgramDescription());
		Font font = new Font(parent.getDisplay(), "Courier", 10, SWT.NORMAL);
		area.setFont(font);
		area.setEditable(false);

		Composite comp = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 3;
		comp.setLayout(layout);

		new Label(comp, SWT.NONE).setText("EDEX http server: ");
		server = new Text(comp, SWT.LEFT);
		gd = new GridData(TEXT_SIZE, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		server.setLayoutData(gd);

		new Label(comp, SWT.NONE).setText("Input file name: ");
		inputFilename = new Text(comp, SWT.LEFT);
		gd = new GridData(TEXT_SIZE, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		inputFilename.setLayoutData(gd);

		new Label(comp, SWT.NONE).setText("Clip bound: ");
		clipBound = new Text(comp, SWT.LEFT);
		gd = new GridData(TEXT_SIZE, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		clipBound.setLayoutData(gd);

		new Label(comp, SWT.NONE).setText("Keep flag: ");
		keepFlag = new Text(comp, SWT.LEFT);
		gd = new GridData(TEXT_SIZE, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		keepFlag.setLayoutData(gd);

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
		server.setText(DEFAULT_SERVER);
		try {
			inputFilename.setText(currentDir.getCanonicalPath() + File.separator + "input.xml");
			outputFilename.setText(currentDir.getCanonicalPath() + File.separator + "output.xml");
		} catch (IOException e) {
			inputFilename.setText("");
			outputFilename.setText("");
		}
		keepFlag.setText("keep");

		return parent;
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createOkButton(parent, IDialogConstants.OK_ID);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	private Button createOkButton(Composite parent, int id) {
		// increment the number of columns in the button bar
		((GridLayout) parent.getLayout()).numColumns++;
		Button button = new Button(parent, SWT.PUSH);
		button.setText(" Clip ");
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

			String[] args = { inputFilename.getText(), clipBound.getText(), keepFlag.getText(),
					outputFilename.getText(), "exact", server.getText() };

			try {
				// new thread so it would not freeze GUI
				ClipVGF thread = new ClipVGF(args);
				thread.start();
			} catch (Exception e) {
				e.printStackTrace();
				statusLbl.setForeground(RED);
				statusLbl.setText("Error");
				return;
			}
			statusLbl.setForeground(DARK_GREEN);
			statusLbl.setText("Complete");
		}
	}
}
