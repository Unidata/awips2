package gov.noaa.nws.ncep.ui.nsharp.palette;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

public class NsharpBoundaryMotionDialog extends Dialog {
	private static NsharpBoundaryMotionDialog thisDialog = null;
	public static NsharpBoundaryMotionDialog getAccess() {
		return thisDialog;
	}
	protected NsharpBoundaryMotionDialog(Shell parentShell) {
		super(parentShell);
		thisDialog = this;
	}
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite top;
		top = (Composite) super.createDialogArea(parent);

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(2, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);

		// Initialize all of the menus, controls, and layouts
		//createDialogContents(top);

		return top;
	}
	@Override
	public boolean close() {
		// TODO Auto-generated method stub
		return super.close();
	}
	@Override
	public int open() {
		// TODO Auto-generated method stub
		return super.open();
	}

}
