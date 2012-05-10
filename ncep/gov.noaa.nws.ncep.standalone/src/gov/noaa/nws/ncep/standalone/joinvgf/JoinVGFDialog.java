/*
 * gov.noaa.nws.ncep.standalone.joinvgf.JoinVGFDialog
 * 
 * Date created (as Jan 12, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.joinvgf;

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
 * JoinVGFDialog - GUI part of JoinVGF application.
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
public class JoinVGFDialog extends TitleAreaDialog {

	private Text			inputFilename1;
	private Text			inputFilename2;
	private Text			outputFilename;
	private Text			tolerance;
	private Label			statusLbl;
	private File			currentDir;
	private final int		TEXT_SIZE		= 600;
	private final Color		RED				= new Color(null, 255, 0, 0);
	private final Color		DARK_GREEN			= new Color(null, 0, 100, 0);

	public JoinVGFDialog(Shell parentShell) {
		super(parentShell);
		currentDir = new File(".");
	}

	@Override
	protected Control createContents(Composite parent) {
		Control contents = super.createContents(parent);
		setTitle("Join PGen XML");
		setMessage("This program joins two XML files");
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
		area.setText(JoinVGF.getProgramDescription());
		Font font = new Font(parent.getDisplay(), "Courier", 10, SWT.NORMAL);
		area.setFont(font);
		area.setEditable(false);

		Composite comp = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 3;
		comp.setLayout(layout);

		new Label(comp, SWT.NONE).setText("Input file name 1: ");
		inputFilename1 = new Text(comp, SWT.LEFT);
		gd = new GridData(600, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		inputFilename1.setLayoutData(gd);

		new Label(comp, SWT.NONE).setText("Input file name 2: ");
		inputFilename2 = new Text(comp, SWT.LEFT);
		gd = new GridData(600, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		inputFilename2.setLayoutData(gd);

		new Label(comp, SWT.NONE).setText("Output file name: ");
		outputFilename = new Text(comp, SWT.LEFT);
		gd = new GridData(TEXT_SIZE, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		outputFilename.setLayoutData(gd);

		new Label(comp, SWT.NONE).setText("[Tolerance in km]: ");
		tolerance = new Text(comp, SWT.LEFT);
		gd = new GridData(TEXT_SIZE, SWT.DEFAULT);
		gd.horizontalSpan = 2;
		tolerance.setLayoutData(gd);

		statusLbl = new Label(comp, SWT.RIGHT);
		gd = new GridData(SWT.RIGHT, SWT.TOP, true, false, 3, 1);
		gd.widthHint = TEXT_SIZE;
		statusLbl.setLayoutData(gd);

		// default values
		try {
			inputFilename1.setText(currentDir.getCanonicalPath() + File.separator + "input1.xml");
			inputFilename2.setText(currentDir.getCanonicalPath() + File.separator + "input2.xml");
			outputFilename.setText(currentDir.getCanonicalPath() + File.separator + "output.xml");
		} catch (IOException e) {
			inputFilename1.setText("");
			inputFilename2.setText("");
			outputFilename.setText("");
		}
		tolerance.setText("" + JoinVGF.tolerance);

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
		button.setText(" Join ");
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

			String[] args = { inputFilename1.getText(), inputFilename2.getText(), outputFilename.getText(),
					tolerance.getText() };

			try {
				// new thread so it would not freeze GUI
				JoinVGF thread = new JoinVGF(args);
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
