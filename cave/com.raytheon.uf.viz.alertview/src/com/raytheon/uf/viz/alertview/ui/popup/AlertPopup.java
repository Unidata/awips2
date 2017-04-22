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
package com.raytheon.uf.viz.alertview.ui.popup;

import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.AlertDestination;
import com.raytheon.uf.viz.alertview.filter.FilterManager;
import com.raytheon.uf.viz.alertview.prefs.PopUpPreferences;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;
import com.raytheon.uf.viz.alertview.style.StyleManager;
import com.raytheon.uf.viz.alertview.ui.view.AlertView;
import com.raytheon.uf.viz.alertview.ui.view.OpenAlertViewHandler;

/**
 * 
 * An {@link AlertDestination} which displays {@link Alert}s in a small popup
 * window. The window only display for a few seconds(exact time is configurable)
 * and then disappears. If the user clicks the popup it will open the displayed
 * alert in the {@link AlertView}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * Aug 20, 2015  4717     mapeters  Added null check on Alert's message
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertPopup implements AlertDestination,
        PreferenceFile.Listener<PopUpPreferences> {

    private final PopupAlertTask task = new PopupAlertTask();

    private final PreferenceFile<PopUpPreferences> prefsFile;

    protected final FilterManager filters = new FilterManager();

    protected final StyleManager styles = new StyleManager();

    protected PopUpPreferences prefs;

    public AlertPopup() {
        prefsFile = PopUpPreferences.load(this);
        prefs = prefsFile.get();
    }

    @Override
    public void update(PopUpPreferences preferences) {
        prefs = preferences;
    }

    /**
     * This will get called automatically by declaritive services.
     */
    public void deactivate() {
        prefsFile.close();
        styles.close();
    }

    @Override
    public void handleAlert(Alert alert) {
        if (filters.getFilter(prefs.getFilter()).filter(alert)) {
            task.update(alert);
        }
    }

    /**
     * This task will run on the UI thread and is responsible for managing the
     * popup window. This should not be run directly but will schedule itself
     * when {@link #update(Alert)} or {@link #clear(Alert)} is called.
     */
    private class PopupAlertTask implements Runnable {

        private static final int UNBLOCK = 0;

        private static final int BLOCK = 1;

        /**
         * Counter for generating unique keys for {@link UnblockPopupTask}s.
         */
        private int nextDelayUnblockKey = 2;

        /**
         * The popup becomes blocked and will not close or update if the
         * {@link MouseTracker} detects that the mouse is over the popup.
         */
        private AtomicInteger blockingState = new AtomicInteger(UNBLOCK);

        private final Timer timer = new Timer("Remove Alert Popup");

        private Shell shell;

        private Label label;

        /**
         * This is the alert that should be dispalyed, the most recent alert
         * passed to update which has not been passed to clear.
         */
        private volatile Alert alert;

        /**
         * This is the currently displayed alert, it should only be used from
         * the UI thread. If this task is waiting to be run on the UI thread
         * then this will not be the same as alert.
         */
        private Alert displayedAlert;

        /**
         * Update the popup window with the provided alert. If the window is not
         * currently open then a new popup window will be opened to display the
         * alert. If a popup window is already displayed then this alert will
         * replace any other alert in the popup window. This method schedules an
         * update on the UI thread but it does not block. If multiple calls to
         * {@link #update(Alert)} are made before the UI thread becomes
         * available then only the most recent Alert will be displayed.
         */
        public synchronized void update(Alert alert) {
            this.alert = alert;
            Display.getDefault().asyncExec(this);
        }

        /**
         * Close the popup if the provided alert is the most recent alert that
         * should be displayed. The alert is provided to make clear an atomic
         * operation. If another alert has been provided to
         * {@link #update(Alert)} since clear was called then it will be a no-op
         * because the more recent alert needs to be displayed.
         */
        public synchronized void clear(Alert alert) {
            if (alert == this.alert) {
                this.alert = null;
                Display.getDefault().asyncExec(this);
            }
        }

        public void openInAlertView() {
            if (displayedAlert != null) {
                OpenAlertViewHandler.openInAlertView(displayedAlert);
                blockingState.set(UNBLOCK);
                clear(displayedAlert);
            }
        }

        public void blockChanges() {
            this.blockingState.set(BLOCK);
        }

        public void unblockChanges() {
            if (blockingState.compareAndSet(BLOCK, nextDelayUnblockKey)) {
                timer.schedule(new UnblockPopupTask(nextDelayUnblockKey), 500);
                nextDelayUnblockKey += 1;
            }
        }

        public void unblockChanges(int key) {
            if (blockingState.compareAndSet(key, UNBLOCK)) {
                Display.getDefault().asyncExec(PopupAlertTask.this);
            }

        }

        @Override
        public void run() {
            if (blockingState.get() != UNBLOCK) {
                return;
            } else {
                /* Reset count to prevent wrap-around. */
                nextDelayUnblockKey = 2;
            }
            this.displayedAlert = this.alert;
            if (displayedAlert != null && (shell == null || shell.isDisposed())) {
                shell = new Shell(Display.getDefault(), SWT.NO_FOCUS
                        | SWT.NO_TRIM | SWT.ON_TOP);
                GridLayout layout = new GridLayout(1, false);
                layout.marginHeight = 0;
                layout.marginWidth = 0;
                shell.setLayout(layout);
                shell.setForeground(Display.getDefault().getSystemColor(
                        SWT.COLOR_BLACK));
                shell.addListener(SWT.Dispose, new Listener() {
                    @Override
                    public void handleEvent(Event event) {
                        shell = null;
                    }
                });

                final Composite comp = new Composite(shell, SWT.BORDER);

                layout = new GridLayout(1, false);
                layout.marginHeight = 0;
                layout.marginWidth = 0;
                GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
                comp.setLayout(layout);
                comp.setLayoutData(data);

                label = new Label(comp, SWT.WRAP);
                data = new GridData(SWT.FILL, SWT.FILL, true, true);
                label.setLayoutData(data);
                label.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseUp(MouseEvent e) {
                        openInAlertView();
                    }

                });
                shell.setSize(prefs.getWidth(), prefs.getHeight());

                Rectangle clientArea = Display.getDefault().getMonitors()[0]
                        .getClientArea();

                int offset = 2;
                int startX = offset;
                int startY = offset;
                switch (prefs.getCorner()) {
                case UPPER_LEFT:
                    startX = clientArea.x + offset;
                    startY = clientArea.y + offset;
                    break;
                case UPPER_RIGHT:
                    startX = clientArea.x + clientArea.width
                            - shell.getSize().x - offset;
                    startY = clientArea.y + offset;
                    break;
                case LOWER_LEFT:
                    startX = clientArea.x + offset;
                    startY = clientArea.y + clientArea.height
                            - shell.getSize().y - offset;
                    break;
                case LOWER_RIGHT:
                    startX = clientArea.x + clientArea.width
                            - shell.getSize().x - offset;
                    startY = clientArea.y + clientArea.height
                            - shell.getSize().y - offset;
                    break;
                }

                shell.setLocation(startX, startY);
                new MouseTracker(label);
                shell.setVisible(true);
            }
            if (displayedAlert == null) {
                if (shell != null && !shell.isDisposed()) {
                    shell.dispose();
                }
            } else {
                Display display = shell.getDisplay();
                label.setBackground(styles.getBackgroundColor(display,
                        displayedAlert));
                label.setForeground(styles.getForegroundColor(display,
                        displayedAlert));
                label.setFont(styles.getFont(display, displayedAlert));
                label.setText(displayedAlert.getMessage() == null ? "null"
                        : displayedAlert.getMessage());
                timer.schedule(new ClosePopupTask(displayedAlert),
                        prefs.getDuration());
            }
        }
    }

    /**
     * This task is scheduled from the UI thread whenever an alert is displayed
     * in a popup. It will call clear after the popup has expired. It is never
     * canceled but instead it relies on the atomic nature of
     * {@link #clear(Alert)} to ensure that if another alert arrives it will not
     * close the popup.
     */
    private class ClosePopupTask extends TimerTask {

        private final Alert alert;

        public ClosePopupTask(Alert alert) {
            this.alert = alert;
        }

        @Override
        public void run() {
            task.clear(alert);
        }
    }

    /**
     * This task is scheduled from the UI thread when the {@link MouseTracker}
     * detects that the mouse is no longer over the popup. It will cause the
     * {@link PopupAlertTask} to refresh. If the user moves the mouse over the
     * popup before this task is executed then the {@link PopupAlertTask} will
     * invalidate the key and ignore the request to unblock when this task runs.
     */
    private class UnblockPopupTask extends TimerTask {

        private final int key;

        public UnblockPopupTask(int key) {
            this.key = key;
        }

        @Override
        public void run() {
            task.unblockChanges(key);
        }
    }

    /**
     * Change the cursor and color when the mouse is over the popup. Also
     * prevents the popup from changing when the mouse is over it.
     */
    private class MouseTracker implements MouseTrackListener {

        private final Label label;

        private final Shell shell;

        /** True when mouse is in the popup. */
        private boolean active = false;

        private Color inactiveColor;

        private Color activeColor;

        public MouseTracker(Label label) {
            this.label = label;
            this.shell = label.getShell();
            label.addMouseTrackListener(this);
        }

        private void activate() {
            if (!active) {
                shell.setCursor(shell.getDisplay().getSystemCursor(
                        SWT.CURSOR_HAND));
                if (activeColor == null
                        || !inactiveColor.equals(label.getBackground())) {
                    inactiveColor = label.getBackground();
                    int r = inactiveColor.getRed();
                    int g = inactiveColor.getGreen();
                    int b = inactiveColor.getBlue();
                    if (r + b + g < 32) {
                        /* If its a really dark color, make it brighter */
                        r = Math.min(255, r + 32);
                        g = Math.min(255, g + 32);
                        b = Math.min(255, b + 32);
                    } else {
                        /* If its a bright color make it darker */
                        r = Math.max(0, r - 32);
                        g = Math.max(0, g - 32);
                        b = Math.max(0, b - 32);
                    }
                    if (activeColor != null) {
                        activeColor.dispose();
                    }
                    activeColor = new Color(shell.getDisplay(), r, g, b);
                }
                label.setBackground(activeColor);
                active = true;
                task.blockChanges();
            }
        }

        private void deactivate() {
            if (active) {
                shell.setCursor(shell.getDisplay().getSystemCursor(
                        SWT.CURSOR_ARROW));
                label.setBackground(inactiveColor);
                active = false;
                task.unblockChanges();
            }
        }

        @Override
        public void mouseEnter(MouseEvent e) {
            activate();
        }

        @Override
        public void mouseExit(MouseEvent e) {
            deactivate();
        }

        @Override
        public void mouseHover(MouseEvent e) {
            activate();
        }

    }
}
