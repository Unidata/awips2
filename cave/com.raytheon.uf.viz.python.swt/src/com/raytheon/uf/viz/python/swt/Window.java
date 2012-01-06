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
package com.raytheon.uf.viz.python.swt;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.python.swt.widgets.Widget;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class Window extends org.eclipse.jface.window.Window {

    protected String title;

    protected List<Widget> widgetList;

    public Window(String title, List<Widget> widgetList) {
        super(new Shell());
        this.title = title;
        this.widgetList = widgetList;
    }

    public void setTrim(boolean trim) {
        if (trim == true) {
            this.setShellStyle(SWT.SHELL_TRIM);
        } else {
            this.setShellStyle(SWT.NO_TRIM);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(title);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#createContents(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        Composite composite = new DialogAreaComposite(parent, widgetList,
                SWT.NONE);

        return composite;
    }

    public static Window open(String title, List<Widget> widgets, boolean sync) {
        final String dialogTitle = title;
        final List<Widget> widgetList = widgets;

        final Window[] window = new Window[1];
        Runnable r = new Runnable() {

            @Override
            public void run() {
                window[0] = new Window(dialogTitle, widgetList);
                window[0].open();
            }
        };

        if (sync) {
            VizApp.runSync(r);
        } else {
            VizApp.runAsync(r);
        }
        Thread t;

        return window[0];
    }

}
