package com.raytheon.viz.aviation.monitor;

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
     * TODO Add Description
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Sep 1, 2010            lvenable     Initial creation
     * Sep 8, 2011  10659      rferrel     Added checks to setToolTipBounds
     *                                     so all of the tip is displayed.
     * 
     * </pre>
     * 
     * @author lvenable
     * @version 1.0
     */

    private Display display;

    private Shell shell;

    private final Label lbl;

    public static final String tooltipTextKey = "tooltiptext";

    public MonitorToolTip(Label lbl) {
        this.lbl = lbl;
    }

    public void open() {
        display = lbl.getDisplay();
        shell = lbl.getShell();
        setupToolTip(lbl);
    }

    private void setupToolTip(final Label lbl) {
        Listener tableListener = new Listener() {
            Shell tip = null;

            Label label = null;

            public void handleEvent(Event event) {
                switch (event.type) {
                case SWT.MouseHover: {
                    if (lbl != null) {
                        if (!nullOrDisposed(tip)) {
                            tip.dispose();
                        }

                        Font textFont = new Font(display, "Monospace", 10,
                                SWT.NORMAL);

                        tip = generateTip();
                        label = generateLabel(tip, textFont);
                        setToolTipText(label);
                        setToolTipBounds(tip);

                        tip.setVisible(true);
                        textFont.dispose();
                    }
                }
                case SWT.Paint: {
                    if (!nullOrDisposed(tip)) {
                        setToolTipText(label);
                        tip.pack();
                    }
                    break;
                }
                case SWT.MouseExit: {
                    if (!nullOrDisposed(tip)) {
                        tip.dispose();
                    }
                    tip = null;
                    label = null;
                    break;
                }
                }
            }
        };

        lbl.addListener(SWT.MouseHover, tableListener);
        lbl.addListener(SWT.MouseExit, tableListener);
        lbl.addListener(SWT.Paint, tableListener);
    }

    private boolean nullOrDisposed(Control ctl) {
        return (ctl == null || ctl.isDisposed());
    }

    private Shell generateTip() {
        Shell tip = new Shell(shell, SWT.ON_TOP | SWT.NO_FOCUS | SWT.TOOL);
        tip.setBackground(display.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
        FillLayout layout = new FillLayout();
        layout.marginWidth = 5;
        tip.setLayout(layout);
        return tip;
    }

    private Label generateLabel(Shell tip, Font font) {
        Label label = new Label(tip, SWT.NONE);
        label.setFont(font);
        label.setForeground(display.getSystemColor(SWT.COLOR_INFO_FOREGROUND));
        label.setBackground(display.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
        return label;
    }

    private void setToolTipText(Label label) {
        String tooltipText = (String) lbl.getData(tooltipTextKey);
        if (tooltipText != null) {
            label.setText(tooltipText);
        }
    }

    private void setToolTipBounds(Shell tipShell) {
        Point tipSize = tipShell.computeSize(SWT.DEFAULT, SWT.DEFAULT);

        // Convert relative location to screen coordinates
        Rectangle lblRect = lbl.getBounds();
        Point tipPt = Display.getCurrent().map(lbl.getParent(), null,
                lblRect.x, lblRect.y + lbl.getSize().y);

        // Adjust so all of the tip displays.
        Rectangle screen = Display.getDefault().getBounds();
        if (tipPt.x <= screen.x + 1) {
            // Move to the right
            tipPt.x = screen.x + 2;
        } else if ((tipPt.x + tipSize.x) >= screen.width - 2) {
            // Move to the left
            tipPt.x = screen.width - tipSize.x - 3;
        }

        if (tipPt.y <= screen.y + 1) {
            // Move down
            tipPt.y = screen.y + 2;
        } else if ((tipPt.y + tipSize.y + 5) >= screen.height) {
            // Move up and above the mouse hover point.
            tipPt.y = tipPt.y - tipSize.y - 15;
        }
        tipShell.setBounds(tipPt.x, tipPt.y, tipSize.x, tipSize.y);
    }
}
