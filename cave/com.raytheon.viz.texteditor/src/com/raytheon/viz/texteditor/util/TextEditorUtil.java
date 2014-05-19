/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.viz.texteditor.util;

import java.awt.Dimension;
import java.awt.Toolkit;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.text.dbsrv.IQueryTransport;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.python.PythonLocalizationPathBuilder;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.texteditor.HTTPQueryTransport;
import com.raytheon.viz.texteditor.command.ICommand;

/**
 * Text Editor Utilities.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 8/11/2009    2191        rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 */
public class TextEditorUtil {
    public static final String TEXTEDITOR_PYTHON_DIR = "textws/python";

    private static final String HTTP_SERVER_ADDRESS_PROPERTY = "httpServerAddress";

    private static final String TEXT_DB_SERVICE_NAME = "textdbsrv";

    private static String pythonIncludeDir;

    public static void setCommandField(ICommand command, Text... fields) {
        boolean clearFields = true;

        if (command != null) {
            // text fields may be longer than fields as the start of textFields
            // may be header information
            String[] textFields = command.getCommandTextFields();

            if (textFields != null) {
                clearFields = false;
                int fieldIndex = fields.length - 1;
                int textIndex = textFields.length - 1;

                while (fieldIndex >= 0 && textIndex >= 0) {
                    String text = textFields[textIndex--];
                    Text field = fields[fieldIndex--];
                    field.setText(text);

                    if (text.length() > 0) {
                        field.setSelection(text.length() - 1);
                    }
                }

                // clear any fields not handled
                while (fieldIndex >= 0) {
                    fields[fieldIndex--].setText("");
                }
            }
        }

        if (clearFields) {
            for (Text field : fields) {
                field.setText("");
            }
        }
    }

    /**
     * Create a label that is vertically aligned in a cell.
     * 
     * @param parentComp
     *            Parent composite.
     * @param labelText
     *            Label text.
     */
    public static void createVerticallyCenteredLabel(Composite parentComp,
            String labelText) {
        Composite vertTextRowComp = new Composite(parentComp, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        vertTextRowComp.setLayout(gridLayout);

        GridData gd = new GridData(GridData.VERTICAL_ALIGN_CENTER);
        Label tempLbl = new Label(vertTextRowComp, SWT.NONE);
        tempLbl.setText(labelText);
        tempLbl.setLayoutData(gd);
    }

    public static String getCommandText(ICommand command) {
        String[] textFields = command.getCommandTextFields();
        StringBuilder builder = new StringBuilder();
        for (String text : textFields) {
            builder.append(text);
            builder.append(" ");
        }
        if (builder.length() > 0) {
            builder.deleteCharAt(builder.length() - 1);
        }
        return builder.toString();
    }

    public static void centerOnScreen(Shell shell) {
        try {
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            Point p = shell.getSize();

            if (screenSize.width > screenSize.height * 2) {
                shell.setLocation((screenSize.width / 2 - p.x) / 2,
                        (screenSize.height - p.y) / 2);
            } else {
                shell.setLocation((screenSize.width - p.x) / 2,
                        (screenSize.height - p.y) / 2);
            }
        } catch (Exception ex) {
        }
    }

    public static int determineSelectionStart(StyledText st) {
        int caretOffset = st.getCaretOffset();
        Point selectionRange = st.getSelectionRange();

        // in case of no selection, same as caret offset, in case of
        // LtoR selection.x is still the correct starting point, for
        // RtoL selections need to increment by the amount already
        // selected
        int start = selectionRange.x;

        // LtoR, also matches no selection but selection.y is 0
        if (selectionRange.x == caretOffset) {
            start += selectionRange.y;
        }

        return start;
    }

    public static String getPythonIncludeDir() {
        if (pythonIncludeDir == null) {
            PythonLocalizationPathBuilder builder = new PythonLocalizationPathBuilder();
            builder.append(TEXTEDITOR_PYTHON_DIR, LocalizationType.CAVE_STATIC);
            pythonIncludeDir = builder.getPathString();
        }
        return pythonIncludeDir;
    }

    public static IQueryTransport getTextDbsrvTransport() {
        return new HTTPQueryTransport(LocalizationManager.getInstance()
                .getLocalizationServer(), TEXT_DB_SERVICE_NAME);
    }

    public static void userInformation(Shell shell, String information) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText("Notice");
        mb.setMessage(information);
        mb.open();
    }
}
