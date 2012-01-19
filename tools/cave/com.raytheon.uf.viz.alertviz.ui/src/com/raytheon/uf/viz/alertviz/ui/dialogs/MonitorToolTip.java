package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

public class MonitorToolTip {

    /**
     * Create a generic Control class that can display the tooltips of a
     * control, ie, org.eclipse.swt.widgets.Label, org.eclipse.swt.widgets.Text
     * above/below the control object in the same foreground and background
     * colors.
     * 
     * This class was modeled after
     * com.raytheon.viz.aviation.monitor.MonitorToolTip.
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * 17Nov2010    5150       cjeanbap    Initial creation
     * 02Dec2010    2235       cjeanbap    Modify to handle the flickering of ToolTipText.
     * 17Jan2011    5150       cjeanbap    Adjusted tooltip location and fix IllegalArgumentException.
     * 18Jan2011    5449       cjeanbap    Fixed NullPointerException.
     * 24Jan 2011   1978       cjeanbap    Removed unused variables.
     * 
     * </pre>
     * 
     * @author cjeanbap
     * @version 1.0
     */

    private Pattern LinePattern = Pattern.compile("^(.*)$", Pattern.MULTILINE);

    private Display display;

    private Shell parentShell;

    private final Control control;

    public static final String tooltipTextKey = "tooltiptext";

    private boolean useBorderWidth = true;

    public MonitorToolTip(Control control) {
        this.control = control;
    }

    public MonitorToolTip(Control control, boolean ignoreDisplayPos) {
        this.control = control;
        this.useBorderWidth = ignoreDisplayPos;
    }

    /**
     * Before calling open() method, set the control's tooltiptext,
     * setToolTipText(null), to null; otherwise two tooltiptext will appear.
     */
    public void open() {
        display = control.getDisplay();
        parentShell = control.getShell();
        setupToolTip(control);
    }

    private void setupToolTip(final Control ctrl) {
        final Listener labelListener = new Listener() {
            public void handleEvent(Event event) {
                Label label = (Label) event.widget;
                Shell shell = label.getShell();
                switch (event.type) {
                case SWT.MouseDown:
                    shell.dispose();
                    break;
                case SWT.MouseExit:
                    shell.dispose();
                    break;
                }
            }
        };

        Listener tableListener = new Listener() {
            Shell tip = null;

            Label label = null;

            public void handleEvent(Event event) {
                switch (event.type) {
                case SWT.MouseExit: {
                    if (tip == null)
                        break;
                    tip.dispose();
                    tip = null;
                    label = null;
                    break;
                }
                case SWT.Paint: {
                    if (tip != null && tip.isDisposed() == false) {
                        if (label != null
                                && (control != null && control.getToolTipText() != null)) {
                            label.setText(control.getToolTipText());
                            tip.pack();
                        }
                    }
                    break;
                }
                case SWT.MouseHover: {
                    if (control != null) {

                        if (tip != null && !tip.isDisposed()) {
                            tip.dispose();
                        }

                        Font textFont = new Font(display, "Monospace", 10,
                                SWT.NORMAL);

                        tip = new Shell(parentShell, SWT.ON_TOP | SWT.NO_FOCUS
                                | SWT.TOOL);
                        tip.setBackground(control.getBackground());
                        FillLayout layout = new FillLayout();
                        layout.marginWidth = 2;
                        tip.setLayout(layout);
                        label = new Label(tip, SWT.NONE);
                        label.setFont(textFont);
                        label.setForeground(control.getForeground());
                        label.setBackground(control.getBackground());
                        if (control != null
                                && control
                                        .getData(MonitorToolTip.tooltipTextKey) != null) {
                            label.setText(control.getData(
                                    MonitorToolTip.tooltipTextKey).toString());
                        }
                        label.addListener(SWT.MouseExit, labelListener);
                        label.addListener(SWT.MouseDown, labelListener);
                        Point size = tip.computeSize(SWT.DEFAULT, SWT.DEFAULT);
                        Rectangle rect = Display.getCurrent().getBounds();
                        Point pt = Display.getCurrent()
                                .map(control, null, 0, 0);

                        final int borderEstimate = (useBorderWidth ? (control
                                .getBorderWidth() * 2 + 1) : 9);
                        String labelText = label.getText();
                        GC gc = new GC(control);
                        int fontHeight = gc.getFontMetrics().getHeight();
                        gc.dispose();
                        Matcher m = LinePattern.matcher(labelText);
                        int numberOfTextLines = 0; // m.groupCount();
                        while (m.find()) {
                            ++numberOfTextLines;
                        }
                        numberOfTextLines = numberOfTextLines > 1 ? numberOfTextLines + 1
                                : numberOfTextLines;
                        int yAdj = fontHeight * numberOfTextLines
                                + borderEstimate + (useBorderWidth ? 4 : 0);
                        if (pt.y + yAdj + fontHeight > rect.height) {
                            pt.y -= yAdj;
                        } else {
                            pt.y += control.getSize().y + borderEstimate;
                        }

                        tip.setBounds(pt.x, pt.y, size.x, size.y);
                        tip.setVisible(true);
                        textFont.dispose();
                    }
                }
                }
            }
        };

        ctrl.addListener(SWT.MouseHover, tableListener);
        ctrl.addListener(SWT.MouseMove, tableListener);
        ctrl.addListener(SWT.MouseExit, tableListener);
        ctrl.addListener(SWT.Paint, tableListener);
    }
}
