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
package com.raytheon.viz.texteditor.alarmalert.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.texteditor.print.PrintDisplay;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            mnash       Initial creation
 * Apr 14, 2010 4734       mhuang      Corrected StdTextProduct import 
 *                                      dependency
 * Jun 29, 2010 5466       cjeanbap    Add SWT.RESIZE type to shell.
 * Oct 29, 2010 7375       cjeanbap    Moved set window size to opened();
 *                                      removed shellListener().
 * Nov 15, 2011 11616      rferrel     Change font to fixed width; and text now
 *                                     uses the font.
 * Feb 03, 2012 14317      mhuang      Make alarm display window wider
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmDisplayWindow extends CaveSWTDialog {

    private Font font;

    private StyledText text;

    private Composite shellComp = null;

    private static boolean accumulate;

    private Button printWindow;

    private Button printBuffer;

    private static String actualText = "";

    java.util.List<StdTextProduct> prods = null;

    /**
     * @param parentShell
     * @param style
     */
    protected AlarmDisplayWindow(Shell parentShell,
            java.util.List<StdTextProduct> prodList) {
        super(parentShell, SWT.RESIZE, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.INDEPENDENT_SHELL);
        setText("Alarm Display Window");
        prods = prodList;
        if (prods == null) {
            prods = new ArrayList<StdTextProduct>();
        }
    }

    @Override
    protected void disposed() {
        super.disposed();
        font.dispose();
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        setReturnValue(false);
        shell.setMinimumSize(300, 100);

        FontData fd = shell.getDisplay().getSystemFont().getFontData()[0];
        fd.setName("Bitstream Vera Sans Mono");
        font = new Font(shell.getDisplay(), fd);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        shellComp = new Composite(shell, SWT.NONE);
        shellComp.setLayout(constructShellLayout());
        shellComp.setLayoutData(gd);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                closeDisplay();
            }
        });
    }

    private void initializeComponents() {
        createMenus();
        createTextArea();
        populateText();
    }

    /**
     * 
     */
    private void createTextArea() {
        GridData textData = new GridData(SWT.FILL, SWT.FILL, true, true, 4, 4);
        text = new StyledText(shellComp, SWT.MULTI | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.WRAP | SWT.BORDER);
        text.setEditable(false);
        textData.heightHint = 300;
        text.setLayoutData(textData);
        text.setSize(text.getSize().x, 300);
        text.setFont(font);
    }

    /**
     * 
     */
    private void createMenus() {
        Composite buttonMenuComp = new Composite(shellComp, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        buttonMenuComp.setLayout(gl);
        // Create three push buttons and a checkbox
        printWindow = new Button(buttonMenuComp, SWT.PUSH);
        printWindow.setText("Print Window");

        printBuffer = new Button(buttonMenuComp, SWT.PUSH);
        printBuffer.setText("Print Entire Buffer");

        final Button clearButton = new Button(buttonMenuComp, SWT.PUSH);
        clearButton.setText("Clear");

        final Button accumButton = new Button(buttonMenuComp, SWT.CHECK);
        accumButton.setText("Accumulate");
        accumButton.setSelection(accumulate);
        if (accumulate) {
            accumButton.setBackground(Display.getCurrent().getSystemColor(
                    SWT.COLOR_YELLOW));
        } else {
            accumButton.setBackground(Display.getCurrent().getSystemColor(
                    SWT.COLOR_WIDGET_BACKGROUND));
        }

        printWindow.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                StringBuilder lines = new StringBuilder();
                int offset = text.getLineHeight() / 2;
                int firstLine = text.getLineIndex(offset);
                int lastLine = text.getLineIndex(text.getSize().y - offset);
                for (int lineIndex = firstLine; lineIndex <= lastLine; ++lineIndex) {
                    lines.append(text.getLine(lineIndex)).append("\n");
                }

                PrintDisplay.print(lines.toString(), text.getFont()
                        .getFontData()[0], UFStatus
                        .getHandler(AlarmDisplayWindow.class));
            }
        });

        printBuffer.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PrintDisplay.print(text.getText(),
                        text.getFont().getFontData()[0],
                        UFStatus.getHandler(AlarmDisplayWindow.class));
            }
        });

        clearButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                text.setText("");
                actualText = text.getText();
            }
        });

        // add action to accumulation button, for appending text products to end
        accumButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                accumulate = !accumulate;
                if (accumulate) {
                    accumButton.setBackground(Display.getCurrent()
                            .getSystemColor(SWT.COLOR_YELLOW));
                } else {
                    accumButton.setBackground(Display.getCurrent()
                            .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
                }
            }
        });
    }

    private void closeDisplay() {
        shell.dispose();
    }

    public void setDialogFocus() {
        shell.setFocus();
    }

    private void populateText() {
        if (prods != null) {
            if (!prods.isEmpty()) {
                for (int i = 0; i < prods.size(); i++) {
                    addText(prods.get(i).getProduct());
                }
            } else {
                addText(actualText);
            }
        } else {
            addText(actualText);
        }
    }

    public void setProds(java.util.List<StdTextProduct> prodList) {
        prods = prodList;
        text.setText(actualText);
        populateText();
    }

    public void addText(String msg) {
        if (accumulate) {
            if (text.getText().isEmpty()) {
                text.setText(msg);
            } else {
                text.setText(text.getText() + "\n\n" + msg);
            }
        } else {
            text.setText(msg);
        }
        actualText = text.getText();
    }

    /**
     * @param accumulate
     *            the accumulate to set
     */
    public void setAccumulate(boolean accumulate) {
        AlarmDisplayWindow.accumulate = accumulate;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO take this method out
        AlarmDisplayWindow curr = new AlarmDisplayWindow(new Shell(), null);
        curr.open();
    }

    @Override
    protected void opened() {
        shell.setSize(600, 300);
    }
}
