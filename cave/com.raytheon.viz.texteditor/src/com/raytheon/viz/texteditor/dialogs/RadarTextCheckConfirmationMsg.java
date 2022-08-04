package com.raytheon.viz.texteditor.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Produces a warning when buffer length exceeds the set length limit.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 10Jul2017    DCS19530    jdynina     Initial development
 * 
 * </pre>
 * 
 * @author jdynina
 */
public class RadarTextCheckConfirmationMsg extends CaveSWTDialog {

    private final static String WARNING_TITLE = "Radar Text Length Warning";

    private final static String WARNING_LABEL = "Buffer length exceeds 3840 character limit";

    protected final static String answer = "Edit";

    protected RadarTextCheckConfirmationMsg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL, CAVE.NONE
                | CAVE.DO_NOT_BLOCK);
        setText(WARNING_TITLE);
        setReturnValue(answer);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // TODO Auto-generated method stub
        Composite mainComposite = new Composite(shell, SWT.NONE);
        mainComposite.setLayout(new GridLayout(1, false));
        createMessageLabel(mainComposite);
        createButtonRow(mainComposite);
    }

    private void createButtonRow(Composite mainComposite) {
        Composite buttonRowComp = new Composite(mainComposite, SWT.NONE);
        buttonRowComp.setLayout(new GridLayout(1, true));
        buttonRowComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // Add the Edit button.
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.widthHint = 100;
        Button editBtn = new Button(buttonRowComp, SWT.PUSH);
        editBtn.setText("Edit");
        editBtn.setLayoutData(gd);
        editBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dispose(answer);
            }
        });
    }

    protected void dispose(String answer2) {
        setReturnValue(answer2);
        close();
    }

    private void createMessageLabel(Composite mainComposite) {
        Composite messageComp = new Composite(mainComposite, SWT.NONE);
        messageComp.setLayout(new GridLayout(1, false));
        Label messageImg = new Label(mainComposite, SWT.NONE);
        messageImg.setImage(mainComposite.getDisplay().getSystemImage(
                SWT.ICON_WARNING));
        messageImg.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                true));

        Label messageLbl = new Label(mainComposite, SWT.NONE);
        messageLbl.setText(WARNING_LABEL);
        messageLbl.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                true));
    }
}
