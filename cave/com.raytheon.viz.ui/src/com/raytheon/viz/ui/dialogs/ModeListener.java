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

package com.raytheon.viz.ui.dialogs;

import java.lang.ref.WeakReference;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.core.mode.CAVEMode;

/**
 * ModeListener.
 * 
 * A PaintListener that will change the background color depending on the
 * current mode of CAVE.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ----------	----------	-----------	--------------------------
 * 12/20/07     561         Dan Fitch    Initial Creation.
 * 6/7/2013                 mnash        Implementation for Chris Golden to allow instances to be garbage collected.
 * 03/24/14     DR 17186    D. Friedman Do not change colors of most buttons.
 * </pre>
 * 
 * @author Dan Fitch
 * @version 1
 */
public class ModeListener implements PaintListener {
    private static final Color DEFAULT_SWT_BACK;

    private static final Color DEFAULT_SWT_FORE;

    static {
        Display disp = Display.getDefault();
        DEFAULT_SWT_BACK = disp.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND);
        DEFAULT_SWT_FORE = disp.getSystemColor(SWT.COLOR_WIDGET_FOREGROUND);
    }

    private WeakReference<Composite> comp;

    public ModeListener(Composite comp) {
        this.comp = new WeakReference<Composite>(comp);
        comp.addPaintListener(this);
    }

    /**
     * Dispose of this mode listener.
     */
    public void dispose() {
        // If the composite is still reachable and it has not
        // been disposed, remove this listener; then delete
        // the reference.
        Composite comp = this.comp.get();
        if ((comp != null) && (comp.isDisposed() == false)) {
            comp.removePaintListener(this);
        }
        this.comp = null;
    }

    @Override
    public void paintControl(PaintEvent paintevent) {
        Color bgColor = CAVEMode.getBackgroundColor();
        Color fgColor = CAVEMode.getForegroundColor();

        setControl((Control) paintevent.widget, bgColor, fgColor);
    }

    private void setControl(Control control, Color bgColor, Color fgColor) {
        if (!(control instanceof Combo)
                && !(control instanceof List)
                && !((control instanceof Text) && ((((Text) control).getStyle() & SWT.READ_ONLY) == 0))
                && !(control instanceof StyledText)
                && !(control instanceof Slider)
                && !((control instanceof Spinner) && ((((Spinner) control)
                        .getStyle() & SWT.READ_ONLY) == 0))
                && !(control instanceof Table)
                && !((control instanceof Button) && ((((Button) control)
                        .getStyle() & (SWT.PUSH|SWT.TOGGLE|SWT.CHECK|SWT.RADIO)) != 0))) {

            Color back = control.getBackground();
            Color fore = control.getForeground();
            if (back.equals(bgColor) == false && back.equals(DEFAULT_SWT_BACK)) {
                control.setBackground(bgColor);
            }
            if (fore.equals(fgColor) == false && fore.equals(DEFAULT_SWT_FORE)) {
                control.setForeground(fgColor);
            }

            if (control instanceof Composite) {
                for (Control child : ((Composite) control).getChildren()) {
                    setControl(child, bgColor, fgColor);
                }
            }
        }
    }
}
