package com.raytheon.viz.texteditor.dialogs;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

/**
 * A message box that is PRIMARY_MODAL. The SWT MessageBox class is
 * APPLICATION_MODAL so a message box for a given text window will interfere
 * with other text windows. (Namely, the WarnGen text window.) This should be
 * replaced with com.raytheon.viz.ui.dialogs.SWTMessageBox once the code has
 * been refactored to not depend on the blocking behavior of open().
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 2015-07-22   ASM #17534  D. Friedman Initial Creation.
 * </pre>
 * 
 * @author David Friedman
 * @version 1
 */
public class TextWSMessageBox extends Dialog {
    String title;
    String information;
    int iconAndButtonStyle;

    public TextWSMessageBox(Shell parentShell, String title,
            String information, int iconAndButtonStyle) {
        super(parentShell);
        this.title = title;
        this.information = information;
        this.iconAndButtonStyle = iconAndButtonStyle;
    }

    @Override
    protected void setShellStyle(int newShellStyle) {
        final int mask = SWT.SYSTEM_MODAL | SWT.APPLICATION_MODAL
                | SWT.PRIMARY_MODAL;
        super.setShellStyle((newShellStyle & ~mask) | SWT.PRIMARY_MODAL);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(title);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        if ((iconAndButtonStyle & SWT.YES) != 0) {
            createButton(parent, IDialogConstants.YES_ID,
                    IDialogConstants.YES_LABEL, true);
        }
        if ((iconAndButtonStyle & SWT.NO) != 0) {
            createButton(parent, IDialogConstants.NO_ID,
                    IDialogConstants.NO_LABEL, true);
        }
        if ((iconAndButtonStyle & SWT.OK) != 0) {
            createButton(parent, IDialogConstants.OK_ID,
                    IDialogConstants.OK_LABEL, true);
        }
        if ((iconAndButtonStyle & SWT.CANCEL) != 0) {
            createButton(parent, IDialogConstants.CANCEL_ID,
                    IDialogConstants.CANCEL_LABEL, true);
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        Layout layout = composite.getLayout();
        if (layout instanceof GridLayout) {
            ((GridLayout) layout).numColumns = 2;
        }
        Label l;
        l = new Label(composite, SWT.LEFT);
        int mask = SWT.ICON_ERROR | SWT.ICON_INFORMATION | SWT.ICON_QUESTION
                | SWT.ICON_WARNING;
        int icon = iconAndButtonStyle & mask;
        if (icon == 0) {
            icon = SWT.ICON_INFORMATION;
        }
        l.setImage(l.getDisplay().getSystemImage(icon));
        l = new Label(composite, SWT.LEFT);
        l.setText(information);
        return composite;
    }

    @Override
    protected void buttonPressed(int buttonId) {
        int returnCode;
        switch (buttonId) {
        case IDialogConstants.OK_ID:
            returnCode = SWT.OK;
            break;
        case IDialogConstants.CANCEL_ID:
            returnCode = SWT.CANCEL;
            break;
        case IDialogConstants.YES_ID:
            returnCode = SWT.YES;
            break;
        case IDialogConstants.NO_ID:
            returnCode = SWT.NO;
            break;
        default:
            return;
        }
        setReturnCode(returnCode);
        close();
    }

    public static int open(Shell parentShell, String title, String message) {
        return open(parentShell, title, message, SWT.ICON_ERROR | SWT.OK);
    }

    public static int open(Shell parentShell, String title, String message,
            int iconAndButtonStyle) {
        TextWSMessageBox mb = new TextWSMessageBox(parentShell, title, message,
                iconAndButtonStyle);
        return mb.open();
    }
}
