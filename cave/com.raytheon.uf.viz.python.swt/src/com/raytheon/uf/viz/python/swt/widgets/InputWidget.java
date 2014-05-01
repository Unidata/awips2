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
package com.raytheon.uf.viz.python.swt.widgets;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * Generic superclass for input widgets such as Text and Numbers
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 5, 2008	1164    	jelkins	Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public abstract class InputWidget extends Widget implements Listener {

    /**
     * Repositions the help when the widget is moved or resized.
     * 
     * <pre>
     * SOFTWARE HISTORY
     * Date			Ticket#		Engineer	Description
     * ------------	----------	-----------	--------------------------
     * Jun 10, 2008				jelkins	Initial creation
     * 
     * </pre>
     * 
     * @author jelkins
     * @version 1.0
     */

    public class HelpRepositioner implements ControlListener {

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.swt.events.ControlListener#controlMoved(org.eclipse.swt
         * .events.ControlEvent)
         */
        @Override
        public void controlMoved(ControlEvent e) {
            setHelpBounds();

        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.swt.events.ControlListener#controlResized(org.eclipse
         * .swt.events.ControlEvent)
         */
        @Override
        public void controlResized(ControlEvent e) {
            setHelpBounds();

        }

    }

    private Text text;

    private Shell helpTip = null;

    private Label helpLabel = null;

    private Composite composite;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.ui.runtimeui.widgets.Widget#buildComposite(org.eclipse
     * .swt.widgets.Composite, int)
     */
    @Override
    public Composite buildComposite(Composite parent) {

        Group group = new Group(parent, SWT.NONE);

        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        gridLayout.marginHeight = 0;
        group.setLayout(gridLayout);
        group.setLayoutData(new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false));

        Label label = new Label(group, SWT.LEFT);
        label.setText(makeGuiLabel(getLabel()));

        String s = "";
        if (getValue() != null) {
            s = getValue().toString();
        }

        int width = 30;
        if (s.length() > 20) {
            width = s.length() + 10;
        }
        GC gc = new GC(parent);
        width = Dialog.convertWidthInCharsToPixels(gc.getFontMetrics(), width);
        gc.dispose();

        text = new Text(group, SWT.BORDER);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, true,
                false);
        layoutData.widthHint = width;
        text.setLayoutData(layoutData);
        text.addListener(SWT.Verify, this);
        text.addListener(SWT.Modify, new Listener() {

            @Override
            public void handleEvent(Event event) {
                setValue(getText());

            }
        });
        text.addListener(SWT.FocusOut, new Listener() {

            @Override
            public void handleEvent(Event event) {
                hideHelpMessage();
            }
        });

        text.setText(s);

        this.composite = group;

        group.getShell().addControlListener(new HelpRepositioner());
        group.addControlListener(new HelpRepositioner());

        return group;

    }

    /**
     * Override this method to provide constrained character input
     * 
     * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void handleEvent(Event event) {
        setValue(getText() + event.text);
    }

    /**
     * @return the text
     */
    protected String getText() {
        return text.getText();
    }

    /**
     * @param text
     *            the text to set
     */
    protected void setText(String string) {
        this.text.setText(string);
    }

    /**
     * 
     */
    protected void hideHelpMessage() {

        if (helpTip == null) {
            return;
        } else {

            helpTip.dispose();
            helpTip = null;
            helpLabel = null;
        }
    }

    /**
     * Display a help message so that the user knows what input is expected.
     * 
     * @param message
     *            the message to display to the user
     */
    protected void showHelpMessage(String message) {
        // @see
        // http://www.java2s.com/Tutorial/Java/0280__SWT/Createfaketooltipsforitemsinatable.htm

        Shell shell = composite.getShell();
        Display display = shell.getDisplay();

        if (helpTip != null && !helpTip.isDisposed()) {
            helpTip.dispose();
        }

        helpTip = new Shell(shell, SWT.ON_TOP | SWT.NO_FOCUS | SWT.TOOL);
        helpTip.setBackground(display.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        helpTip.setLayout(gridLayout);

        helpLabel = new Label(helpTip, SWT.NONE);
        helpLabel.setForeground(display
                .getSystemColor(SWT.COLOR_INFO_FOREGROUND));
        helpLabel.setBackground(display
                .getSystemColor(SWT.COLOR_INFO_BACKGROUND));
        helpLabel.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {
                hideHelpMessage();

            }
        });

        helpLabel.setText(message);

        setHelpBounds();

        helpTip.setVisible(true);

    }

    /**
     * Positions and sizes the help message in the appropriate spot.
     */
    private void setHelpBounds() {

        if (helpTip == null) {
            return;
        } else {

            Point size = helpTip.computeSize(SWT.DEFAULT, SWT.DEFAULT);
            Rectangle rect = text.getBounds();
            Point pt = text.toDisplay(rect.x, rect.y);
            helpTip.setBounds(pt.x, pt.y, size.x, size.y);
        }
    }

}
