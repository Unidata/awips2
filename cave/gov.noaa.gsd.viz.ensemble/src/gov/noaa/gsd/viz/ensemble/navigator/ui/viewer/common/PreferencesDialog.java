package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/*
 * A dialog which allows the user to choose create an ensemble relative 
 * frequency product from different probability ranges.
 * 
 * @author polster
 * @author jing
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2015   6863      polster     Initial creation
 * 
 * </pre>
 * 
 * @version 1.0
 */

public class PreferencesDialog extends CaveJFACEDialog {

    private Composite mainPanelComposite = null;

    private GlobalPreferencesComposite prefsComp = null;

    public PreferencesDialog(Shell parentShell) {
        super(parentShell);
        setBlockOnOpen(false);
        addCloseCallback(new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                prefsComp.dispose();
            }

        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#isResizable()
     */
    @Override
    protected boolean isResizable() {
        return true;
    }

    /**
     * Create contents of the dialog.
     * 
     * @param parent
     */
    @Override
    protected Control createDialogArea(Composite parent) {

        setBlockOnOpen(true);

        mainPanelComposite = new Composite(parent, SWT.BORDER);

        GridData rootCalculatorPanel_gd = new GridData(SWT.FILL, SWT.FILL,
                false, true, 1, 1);
        mainPanelComposite.setLayoutData(rootCalculatorPanel_gd);
        mainPanelComposite.setLayout(new GridLayout(1, false));

        prefsComp = new GlobalPreferencesComposite(mainPanelComposite, SWT.None);

        return parent;

    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {

        createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
                false);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Ensemble Tool Preferences");
    }

    @Override
    protected Control createContents(Composite parent) {
        Control contents = super.createContents(parent);
        return contents;
    }

}
