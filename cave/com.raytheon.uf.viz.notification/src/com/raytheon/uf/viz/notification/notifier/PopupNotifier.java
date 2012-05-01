package com.raytheon.uf.viz.notification.notifier;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.VizApp;

/**
 * Notifications to show up in the bottom right of the monitor, will stack if
 * another is currently displayed
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class PopupNotifier {

    // how long the the popup is displayed
    private static final int TIME_TO_DISPLAY = 4500;

    // what is currently popped up
    private static List<Shell> activeShells = new ArrayList<Shell>();

    private static Shell shell;

    private static Font titleFont = null;

    /**
     * Creates and shows a notification dialog with a specific title and message
     * 
     * For future could allow for different types of notifications to be
     * different colors and different popup types?
     * 
     * @param title
     * @param message
     */
    public static void notify(String title, String message) {
        if (Display.getCurrent() == null
                || Display.getCurrent().getActiveShell() == null) {
            shell = new Shell(Display.getDefault(), SWT.NO_FOCUS | SWT.NO_TRIM
                    | SWT.ON_TOP);
            GridLayout layout = new GridLayout(1, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            shell.setLayout(layout);
            shell.setForeground(Display.getDefault().getSystemColor(
                    SWT.COLOR_BLACK));
            shell.addListener(SWT.Dispose, new Listener() {
                @Override
                public void handleEvent(Event event) {
                    activeShells.remove(shell);
                    if (activeShells.isEmpty()) {
                        titleFont.dispose();
                    }
                }
            });

            final Composite comp = new Composite(shell, SWT.BORDER);

            layout = new GridLayout(1, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
            comp.setLayout(layout);
            comp.setLayoutData(data);

            CLabel titleLabel = new CLabel(comp, SWT.NONE);
            data = new GridData(SWT.FILL, SWT.FILL, true, true);
            data.horizontalSpan = 2;
            titleLabel.setLayoutData(data);

            // storing off the font, make the title font pop out a little more
            // to the user
            if (titleFont == null || titleFont.isDisposed()) {
                titleFont = titleLabel.getFont();
                FontData fd = titleFont.getFontData()[0];
                fd.setStyle(SWT.BOLD);
                fd.height = fd.height + 2;
                titleFont = new Font(Display.getCurrent(), fd);
            }
            titleLabel.setFont(titleFont);
            titleLabel.setText(title);

            // separate the title from the body text
            Label separator = new Label(comp, SWT.HORIZONTAL | SWT.SEPARATOR);
            data = new GridData(SWT.FILL, SWT.FILL, true, true);
            separator.setLayoutData(data);
            separator.setVisible(true);

            // here is the text for the body of the message
            Label text = new Label(comp, SWT.WRAP);
            data = new GridData(SWT.FILL, SWT.FILL, true, true);
            text.setLayoutData(data);
            text.setText(message);

            // maybe this should be minimum size?
            shell.setSize(350, 100);

            // get the bottom right corner of the monitor
            Rectangle clientArea = Display.getDefault().getMonitors()[0]
                    .getClientArea();

            // and offset that may not be needed, only here to keep the box from
            // touching the corner
            int offset = 2;
            int startX = clientArea.x + clientArea.width - shell.getSize().x
                    - offset;
            int startY = clientArea.y + clientArea.height - shell.getSize().y
                    - offset;

            // move other shells up
            synchronized (activeShells) {
                if (!activeShells.isEmpty()) {
                    List<Shell> modifiable = new ArrayList<Shell>(activeShells);
                    Collections.reverse(modifiable);
                    for (Shell shell : modifiable) {
                        if (shell.isDisposed()) {
                            activeShells.remove(shell);
                            continue;
                        }
                        Point curLoc = shell.getLocation();
                        shell.setLocation(curLoc.x, curLoc.y - 100);
                        if (curLoc.y - 100 < 0) {
                            activeShells.remove(shell);
                            shell.dispose();
                        }
                    }
                }
            }

            shell.setLocation(startX, startY);
            shell.setVisible(true);

            activeShells.add(shell);
            startTimer(shell);
        }
    }

    /**
     * @wbp.parser.entryPoint
     */
    private static void startTimer(final Shell currShell) {
        Timer timer = new Timer("Remove notification");
        TimerTask task = new TimerTask() {

            @Override
            public void run() {
                try {
                    if (currShell == null || currShell.isDisposed()) {
                        return;
                    }
                    VizApp.runAsync(new Runnable() {
                        public void run() {
                            currShell.dispose();
                        };
                    });
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
        timer.schedule(task, TIME_TO_DISPLAY);
    }
}
