package com.raytheon.uf.viz.alertviz.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
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
     * 23Oct 2011   2303       bgonzale    Old patch to fix tool tip layout.
     * 
     * </pre>
     * 
     * @author cjeanbap
     * @version 1.0
     */

    private Display display;

    private Shell parentShell;

    private final Control control;

    public static final String tooltipTextKey = "tooltiptext";

    private boolean ignoreDisplayPos = true;

    public MonitorToolTip(Control control) {
        this.control = control;
    }

    public MonitorToolTip(Control control, boolean ignoreDisplayPos) {
        this.control = control;
        this.ignoreDisplayPos = ignoreDisplayPos;
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
                        setToolTipBounds(label, tip);
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
    
    private void setToolTipBounds(Label label, Shell tip) {
        // size of tool tip
        Point tipSize = tip.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        // bounds of the current display
        Rectangle displayBounds = Display.getCurrent().getBounds();
        // x screen coordinate
        int xCoord = Display.getCurrent().map(control, null, 0, 0).x;
        // y coordinate of widget that the tool tip is next to
        Control widget = ignoreDisplayPos ? control : control.getParent();
        int widgetYCoord = Display.getCurrent().map(widget, null, 0, 0).y;
        Point widgetSize = widget.computeSize(SWT.DEFAULT,
                SWT.DEFAULT);

        int yCoord = widgetYCoord;
        // check if the tip extends past the end of the display
        if (yCoord + widgetSize.y + tipSize.y > displayBounds.height) {
            yCoord = yCoord - tipSize.y - control.getParent().getBorderWidth();
        } else {
            yCoord = yCoord + widgetSize.y;
        }
        tip.setBounds(xCoord, yCoord, tipSize.x, tipSize.y);
    }

}
