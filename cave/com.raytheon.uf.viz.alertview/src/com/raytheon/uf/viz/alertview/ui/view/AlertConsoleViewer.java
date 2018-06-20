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
package com.raytheon.uf.viz.alertview.ui.view;

import java.io.IOException;

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IHyperlink;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.console.TextConsoleViewer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.action.SaveToFileAction;

/**
 * 
 * Used for viewing the details of {@link Alert}s. Since most alerts are
 * generated from logged errors this console is created so that links appear in
 * stack traces when the view is used in eclipse.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 18, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertConsoleViewer extends TextConsoleViewer {

    private static Logger logger = LoggerFactory.getLogger(AlertView.class);

    private AlertConsole console;

    private Alert alert;

    public AlertConsoleViewer(Composite parent) {
        this(parent, new AlertConsole());
    }

    private AlertConsoleViewer(Composite parent, AlertConsole console) {
        super(parent, console);
        this.console = console;
        console.setViewer(this);
        initialize();
    }

    private void initialize() {
        StyledText text = getTextWidget();
        text.setEditable(false);
        Menu menu = new Menu(text);
        text.setMenu(menu);
        menu.addMenuListener(new MenuAdapter() {

            @Override
            public void menuShown(MenuEvent e) {
                populateContextMenu(getTextWidget().getMenu());
            }

        });
    }

    protected void populateContextMenu(Menu menu) {
        for (MenuItem item : menu.getItems()) {
            item.dispose();
        }
        if (alert == null) {
            return;
        }
        new ActionContributionItem(new SaveToFileAction(alert)).fill(menu, -1);
    }

    public void setAlert(Alert alert) {
        if (this.alert == alert) {
            return;
        }
        this.alert = alert;
        console.clearConsole();
        if (alert == null) {
            return;
        }
        String details = alert.getDetails();
        if (details.isEmpty()) {
            details = alert.getMessage();
        }
        try (IOConsoleOutputStream os = console.newOutputStream()) {
            os.write(details);
        } catch (IOException e) {
            logger.error("Unexpected IO exception writing to console.", e);
        }
    }

    protected void redraw() {
        Display.getDefault().syncExec(new Runnable() {

            @Override
            public void run() {
                getTextWidget().redraw();
            }

        });
    }

    private static class AlertConsole extends IOConsole {

        private AlertConsoleViewer viewer;

        public AlertConsole() {
            super("AlertViewConsole", "javaStackTraceConsole", null);
            ConsolePlugin.getDefault().getConsoleManager()
                    .createPatternMatchListeners(this);
        }

        public void setViewer(AlertConsoleViewer viewer) {
            this.viewer = viewer;
        }

        @Override
        public void addHyperlink(IHyperlink hyperlink, int offset, int length)
                throws BadLocationException {
            super.addHyperlink(hyperlink, offset, length);
            viewer.redraw();
        }
    }

}
