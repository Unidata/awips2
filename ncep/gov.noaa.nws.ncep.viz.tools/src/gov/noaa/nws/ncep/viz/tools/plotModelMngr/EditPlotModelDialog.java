package gov.noaa.nws.ncep.viz.tools.plotModelMngr;

import gov.noaa.nws.ncep.viz.common.ui.UserEntryDialog;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.PlotModelMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.EditPlotModelComposite;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * UI for editing Plot Models.
 * 
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/15/2009    172        M. Li       Initial creation.
 * 12/05/2009    217        Greg Hull   Use EditPlotModelComposite
 * 07/26/2010    285        Q. Zhou     modified editPlotModelComposit
 * 03/08/2011    425        Greg Hull   add a Save As button
 * 03/10/2013    921        S. Russell  TTR 921. Commented out "Save" button
 * 03/11/2013    921        S. Russell  TTR 921. Append "COPY" during "Save As"
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
public class EditPlotModelDialog extends Dialog {

    protected Shell shell;

    protected String dlgTitle = "Edit Plot Model";

    // TTR 921
    protected String pltmdlnmsffx = "COPY";

    protected boolean ok = false;

    private PlotModel editedPlotModel = null;

    public EditPlotModelDialog(Shell parentShell, PlotModel pm) {
        super(parentShell);
        dlgTitle = "Edit Plot Model " + pm.getPlugin() + "/" + pm.getName();
        editedPlotModel = new PlotModel(pm);
    }

    public void createShell(int x, int y) {

        shell = new Shell(getParent(), SWT.DIALOG_TRIM | SWT.RESIZE);
        shell.setText(dlgTitle);
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);
        shell.setLocation(x, y);

        Composite editPlotModelComp = new EditPlotModelComposite(shell, SWT.NONE, editedPlotModel, null);

        GridData gd = new GridData();

        Composite okCanComp = new Composite(shell, SWT.NONE);
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        okCanComp.setLayoutData(gd);

        okCanComp.setLayout(new FormLayout());

        Button canBtn = new Button(okCanComp, SWT.PUSH);
        canBtn.setText(" Cancel ");
        FormData fd = new FormData();
        fd.width = 80;
        fd.bottom = new FormAttachment(100, -5);
        fd.left = new FormAttachment(25, -40);
        canBtn.setLayoutData(fd);

        canBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                ok = false;
                shell.dispose();
            }
        });

        /*
         * // TTR 921 Button saveBtn = new Button( okCanComp, SWT.PUSH );
         * saveBtn.setText("  Save  "); fd = new FormData(); fd.width = 80;
         * fd.bottom = new FormAttachment( 100, -5 ); fd.left = new
         * FormAttachment( 50, -40 ); saveBtn.setLayoutData( fd );
         * 
         * saveBtn.addSelectionListener( new SelectionAdapter() { public void
         * widgetSelected(SelectionEvent e) { ok=true; shell.dispose(); } });
         */

        Button saveAsBtn = new Button(okCanComp, SWT.PUSH);
        saveAsBtn.setText("Save As...");
        fd = new FormData();
        fd.width = 90;
        fd.bottom = new FormAttachment(100, -5);
        fd.left = new FormAttachment(75, -40);
        saveAsBtn.setLayoutData(fd);

        saveAsBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {

                String pmname = editedPlotModel.getName() + pltmdlnmsffx;

                // pop up a dialog to prompt for the new name
                UserEntryDialog entryDlg = new UserEntryDialog(shell, "Save As", "Save Plot Model As:", pmname);
                String newPltMdlName = entryDlg.open();

                if (newPltMdlName == null || // cancel pressed
                        newPltMdlName.isEmpty()) {
                    return;
                }

                // if this plotModel already exists, prompt to overwrite
                //
                if (PlotModelMngr.getInstance().getPlotModel(editedPlotModel.getPlugin(), newPltMdlName) != null) {

                    MessageDialog confirmDlg = new MessageDialog(shell, "Confirm", null, "A '" + newPltMdlName + "' Plot Model already exists.\n\nDo you want to overwrite it?",
                            MessageDialog.QUESTION, new String[] { "Yes", "No" }, 0);
                    confirmDlg.open();

                    if (confirmDlg.getReturnCode() == MessageDialog.CANCEL) {
                        return;
                    }
                }

                editedPlotModel.setName(newPltMdlName);

                ok = true;
                shell.dispose();
            }
        });

    }

    public void open() {
        open(getParent().getLocation().x + 10, getParent().getLocation().y + 10);
    }

    public Object open(int x, int y) {
        Display display = getParent().getDisplay();

        createShell(x, y);

        initWidgets();

        shell.pack();
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return (ok ? editedPlotModel : null);
    }

    public void initWidgets() {
    }
}
