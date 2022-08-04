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
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.texteditor.print.PrintDisplay;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This is a dialog to display the desired products from the current alarm
 * queue.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 10, 2009           mnash     Initial creation
 * Apr 14, 2010  4734     mhuang    Corrected StdTextProduct import dependency
 * Jun 29, 2010  5466     cjeanbap  Add SWT.RESIZE type to shell.
 * Oct 29, 2010  7375     cjeanbap  Moved set window size to opened(); removed
 *                                  shellListener().
 * Nov 15, 2011  11616    rferrel   Change font to fixed width; and text now
 *                                  uses the font.
 * Feb 03, 2012  14317    mhuang    Make alarm display window wider
 * Sep 06, 2012  13365    rferrel   Accumulate and Display fix.
 * Sep 25, 2012  1196     lvenable  Dialog refactor for AlarmDisplayWindow.Added
 *                                  DO_NOT_BLOCK.
 * Nov 20, 2013  2488     randerso  Changed to use DejaVu font
 * Jun 23, 2014  3161     lvenable  Added SWT dialog trim to the dialogs for
 *                                  thin client.
 * Jul 24, 2014  3423     randerso  Added setLoading() to indicate waiting on
 *                                  product retrieval. Removed prods from
 *                                  constructor.
 * Nov 09, 2016  5996     tgurney   Add minimize and maximize buttons
 * Feb 21, 2018  6681     tgurney   Stop wrapping text and use fixed size for
 *                                  dialog
 * May 09, 2018  7291     randerso  Changed signature for PrintDisplay.print.
 *                                  Code cleanup.
 * Jul 25, 2018  6748     randerso  Code cleanup.
 * Nov 14, 2018  7624     tgurney   Save the dialog dimensions when closed
 * Dec  6, 2018  7624     tgurney   Change text area size to 80x15 characters
 *
 * </pre>
 *
 * @author mnash
 */

public class AlarmDisplayWindow extends CaveSWTDialog
        implements ControlListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlarmDisplayWindow.class);

    /**
     * State to place the accumulate into after adding products in the preopen.
     */
    public static enum ACCUMULATE_STATE {
        UNCHANGE, TRUE, FALSE
    }

    private ACCUMULATE_STATE accum_state;

    private Font font;

    private StyledText text;

    private Composite shellComp = null;

    private static boolean accumulate;

    private Button accumButton;

    private static String actualText = "";

    private java.util.List<StdTextProduct> prods = null;

    protected AlarmDisplayWindow(Shell parentShell,
            ACCUMULATE_STATE accum_state) {
        super(parentShell, SWT.MAX | SWT.MIN | SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.INDEPENDENT_SHELL
                        | CAVE.DO_NOT_BLOCK);
        setText("Alarm Display Window");
        prods = new ArrayList<>(0);
        this.accum_state = accum_state;
    }

    @Override
    protected void disposed() {
        super.disposed();
        font.dispose();
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        setReturnValue(false);

        FontData fd = shell.getDisplay().getSystemFont().getFontData()[0];
        // TODO not have hard coded font name
        fd.setName("DejaVu Sans Mono");
        font = new Font(shell.getDisplay(), fd);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        shellComp = new Composite(shell, SWT.NONE);
        shellComp.setLayout(constructShellLayout());
        shellComp.setLayoutData(gd);

        // Initialize all of the controls and layouts
        createMenus();
        createTextArea();
        populateText();
        switch (accum_state) {
        case TRUE:
            setAccumulate(true);
            break;
        case FALSE:
            setAccumulate(false);
            break;
        case UNCHANGE:
            // do nothing
        }
        accum_state = ACCUMULATE_STATE.UNCHANGE;
    }

    private void createTextArea() {
        GridData textData = new GridData(SWT.FILL, SWT.FILL, true, true, 4, 4);
        text = new StyledText(shellComp,
                SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        text.setEditable(false);
        text.setFont(font);
        GC gc = new GC(text);
        int width = gc.getFontMetrics().getAverageCharWidth() * 80;
        int height = gc.getFontMetrics().getHeight() * 15;
        gc.dispose();
        Rectangle textBounds = text.computeTrim(0, 0, width, height);
        textData.widthHint = textBounds.width;
        textData.heightHint = textBounds.height;
        text.setLayoutData(textData);
    }

    private void createMenus() {
        Composite buttonMenuComp = new Composite(shellComp, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        buttonMenuComp.setLayout(gl);
        // Create three push buttons and a checkbox
        Button printWindow = new Button(buttonMenuComp, SWT.PUSH);
        printWindow.setText("Print Window");

        Button printBuffer = new Button(buttonMenuComp, SWT.PUSH);
        printBuffer.setText("Print Entire Buffer");

        final Button clearButton = new Button(buttonMenuComp, SWT.PUSH);
        clearButton.setText("Clear");

        accumButton = new Button(buttonMenuComp, SWT.CHECK);
        accumButton.setText("Accumulate");
        setAccumulate(accumulate);

        printWindow.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                StringBuilder lines = new StringBuilder();
                int offset = text.getLineHeight() / 2;
                int firstLine = text.getLineIndex(offset);
                int lastLine = text
                        .getLineIndex(text.getClientArea().height - offset);
                for (int lineIndex = firstLine; lineIndex <= lastLine; ++lineIndex) {
                    lines.append(text.getLine(lineIndex)).append("\n");
                }

                try {
                    PrintDisplay.print(lines.toString());
                } catch (VizException e) {
                    statusHandler.error(e.getLocalizedMessage(), e);
                }
            }
        });

        printBuffer.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    PrintDisplay.print(text.getText());
                } catch (VizException e) {
                    statusHandler.error(e.getLocalizedMessage(), e);
                }
            }
        });

        clearButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                text.setText("");
                synchronized (AlarmDisplayWindow.class) {
                    actualText = text.getText();
                }
            }
        });

        // add action to accumulation button, for appending text products to end
        accumButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setAccumulate(!accumulate);
            }
        });
    }

    /**
     * Set the focus on the dialog shell
     */
    public void setDialogFocus() {
        shell.setFocus();
    }

    private void populateText() {
        if (prods == null || prods.isEmpty()) {
            text.setText(actualText);
        } else {
            if (accumulate) {
                text.setText(actualText);
            } else {
                text.setText("");
            }
            boolean saveAccumulate = accumulate;

            // Make sure all products in the list are displayed then restore
            // accumulate.
            accumulate = true;
            for (StdTextProduct prod : prods) {
                addText(prod.getProduct());
            }
            accumulate = saveAccumulate;
        }
    }

    public void setProds(java.util.List<StdTextProduct> prodList) {
        prods = prodList;
        populateText();
    }

    public void setLoading() {
        synchronized (AlarmDisplayWindow.class) {
            actualText = text.getText();
        }
        text.setText("Loading...");
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
        synchronized (AlarmDisplayWindow.class) {
            actualText = text.getText();
        }
    }

    /**
     * Sets the accumulate to the desired state and if active updates the
     * display.
     *
     * @param accumulate
     *            the accumulate to set
     */
    public void setAccumulate(boolean accumulate) {
        AlarmDisplayWindow.accumulate = accumulate;
        if (accumButton != null && !accumButton.isDisposed()) {
            accumButton.setSelection(accumulate);
            if (accumulate) {
                accumButton.setBackground(
                        Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW));
            } else {
                accumButton.setBackground(Display.getCurrent()
                        .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
            }
        }
    }

    private void restoreDimensions() {
        Rectangle dimensions = TextWsDialogDimensionsHelper.getInstance()
                .getSavedDimensions(getClass().getSimpleName());
        if (dimensions == null) {
            Rectangle monitorArea = shell.getMonitor().getClientArea();
            int width = shell.getSize().x;
            int height = shell.getSize().y;
            /*
             * Default location is immediately to the right of the Current Alarm
             * Window.
             */
            Point parentLoc = getParent().getLocation();
            Point parentSize = getParent().getSize();
            int locX = parentLoc.x + parentSize.x;
            if (locX + width > monitorArea.width) {
                locX -= parentSize.x + width;
                if (locX < 0) {
                    locX += parentSize.x + width;
                }
            }
            int locY = parentLoc.y;
            dimensions = new Rectangle(locX, locY, width, height);
        }
        shell.setBounds(dimensions);
    }

    @Override
    public void controlMoved(ControlEvent e) {
        TextWsDialogDimensionsHelper.getInstance()
                .saveDimensions(shell.getBounds(), getClass().getSimpleName());
    }

    @Override
    public void controlResized(ControlEvent e) {
        TextWsDialogDimensionsHelper.getInstance()
                .saveDimensions(shell.getBounds(), getClass().getSimpleName());
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        restoreDimensions();
        shell.addControlListener(this);
    }
}
