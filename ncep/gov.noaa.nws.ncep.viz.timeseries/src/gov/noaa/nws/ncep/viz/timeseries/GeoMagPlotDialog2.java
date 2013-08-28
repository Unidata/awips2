package gov.noaa.nws.ncep.viz.timeseries;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * This class provides cursor selection and editing in National Centers
 * perspective.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 2009  	109        M. Li    	Initial creation.
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */

public class GeoMagPlotDialog2 extends Dialog {
    /**
     * Dialog shell.
     */
    private Shell shell;

    private Display display;

    public GeoMagPlotDialog2(Shell parentShell) {
        super(parentShell);
    }

    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        shell.setText("Magnetometer Plots");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        // mainLayout.marginHeight = 1;
        // mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);
        Label label0 = new Label(shell, SWT.NONE);
        label0.setText("label0");
        GridData data = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        label0.setLayoutData(data);

        Composite composite1 = new Composite(shell, SWT.BORDER);

        Composite composite2 = new Composite(shell, SWT.BORDER);

        Composite composite3 = new Composite(shell, SWT.BORDER);

        shell.pack();
        shell.open();

        // parent.setCursor(arrowCursor);
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return null;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        // createCursorRefControls();
        addSeparator();
        // createCursorTypeControls();
        addSeparator();
        createCloseButton();
        // initialize();
        // update();
    }

    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, true);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd);

        Button ok_btn = new Button(centeredComp, SWT.NONE);
        ok_btn.setText("OK");
        ok_btn.setLayoutData(gd);
        // ok_btn.addSelectionListener(new SelectionAdapter() {
        // public void widgetSelected(SelectionEvent event) {
        // ncCursor.setCursorTypeColorIdx(curTypeSet, curColorSet);
        //
        // Cursor cursor = NCCursors.getCursor(display,
        // NCCursors.CursorRef.DEFAULT);
        // getParent().setCursor(cursor);
        //
        // shell.dispose();
        // }
        // });

        Button default_btn = new Button(centeredComp, SWT.NONE);
        default_btn.setText("Defaults");
        default_btn.setLayoutData(gd);
        // default_btn.addSelectionListener(new SelectionAdapter() {
        // public void widgetSelected(SelectionEvent event) {
        // setDefault();
        // update();
        // }
        // });

        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // if (!IsToPlotLogos()) removeLogosResource();
                shell.dispose();
            }
        });
    }

    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }

}
