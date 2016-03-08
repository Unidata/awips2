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
import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase.CAVE;

/**
 * Produces a warning when one or more lines in the text product
 * exceed the set length limit.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 01Mar2016    RM13214  mgamazaychikov Initial development
 * 
 * </pre>
 * 
 * @author mgamazaychikov
 */
public class LineWrapCheckConfirmationMsg extends CaveSWTDialog {

    private final static String WARNING_TITLE = "Line Length Confirmation Warning";

    private final static String WARNING_LABEL = "Length of one or more lines exceed wrapping limit";

    protected static enum AnswerChoices {
        EDIT, FIX, SEND
    }

    protected LineWrapCheckConfirmationMsg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL, CAVE.NONE
                | CAVE.DO_NOT_BLOCK);
        setText(WARNING_TITLE);
        setReturnValue(AnswerChoices.EDIT);
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
        buttonRowComp.setLayout(new GridLayout(3, true));
        buttonRowComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // Add the Edit button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button editBtn = new Button(buttonRowComp, SWT.PUSH);
        editBtn.setText("Edit");
        editBtn.setLayoutData(gd);
        editBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dispose(AnswerChoices.EDIT);
            }
        });

        // Add the Fix button.
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button fixBtn = new Button(buttonRowComp, SWT.PUSH);
        fixBtn.setText("Fix");
        fixBtn.setLayoutData(gd);
        fixBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dispose(AnswerChoices.FIX);
            }
        });

        // Add the Send button.
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button sendBtn = new Button(buttonRowComp, SWT.PUSH);
        sendBtn.setText("Send As Is");
        sendBtn.setLayoutData(gd);
        sendBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dispose(AnswerChoices.SEND);
            }
        });
    }

    protected void dispose(AnswerChoices choice) {
        setReturnValue(choice);
        close();
    }

    private void createMessageLabel(Composite mainComposite) {
        Composite messageComp = new Composite(mainComposite, SWT.NONE);
        messageComp.setLayout(new GridLayout(2, false));
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
