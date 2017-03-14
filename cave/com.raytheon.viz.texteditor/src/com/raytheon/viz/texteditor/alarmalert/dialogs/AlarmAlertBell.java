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

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.texteditor.Activator;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmBeepJob;
import com.raytheon.viz.texteditor.alarmalert.util.FlashBellJob;

/**
 * Alarm/Alert Bell
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2009            mnash       Initial creation
 * Aug 16, 2010 2187       cjeanbap    Fixed a NullPointerException
 * Dec 23, 2010 7375       cjeanbap    Force dialog ON TOP of over Dialog/Windows.
 * 03/19/2012              D. Friedman Fix alarming.  Disable runloop in open().
 * May 18, 2012            jkorman     Added flashing alarm image.
 * Jul 25, 2012 15122      rferrel     Add sound delay interval.
 * Mar 06  2013 15827  mgamazaychikov  Prevent Alarm Bell window from stealing focus 
 *                                      from Text Editor.
 * Mar 30, 2016 5513       randerso    Fixed to display on same monitor as parent,
 *                                     significant code cleanup
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmAlertBell implements MouseMoveListener, MouseListener {

    // delay in milliseconds - flash every 1 1/2 seconds
    private static final int FLASH_DELAY = 1500;

    /**
     * Repeat the alarm sound every minute.
     */
    private static final long SOUND_DELAY = TimeUtil.MILLIS_PER_MINUTE;

    private Shell parent;

    private Shell shell;

    private Display display;

    private FlashBellJob flasher;

    private Image[] bellImage = new Image[2];

    private int phase = 0;

    private boolean active = false;

    private Button button;

    private AlarmBeepJob abj = null;

    /**
     * Mouse origin.
     */
    private Point origin;

    /**
     * Actual dialog X, Y coordinate.
     */
    private Point dialogXY = null;

    /**
     * Move label.
     */
    private Label moveLabel;

    /**
     * Font used for labels
     */
    private Font labelFont;

    /**
     * @param parent
     */
    public AlarmAlertBell(Shell parent) {
        this.parent = parent;
    }

    private void initShell() {
        display = parent.getDisplay();

        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.ON_TOP);
        shell.setText("Alarm Alert Bell");
        GridLayout mainLayout = new GridLayout(1, true);
        shell.setLayout(mainLayout);

        labelFont = new Font(shell.getDisplay(), "Monospace", 14, SWT.BOLD);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        moveLabel = new Label(shell, SWT.CENTER | SWT.BORDER);
        moveLabel.setText("Alarm Bell");
        moveLabel.setLayoutData(gd);
        moveLabel.setFont(labelFont);
        moveLabel.addMouseListener(this);
        moveLabel.addMouseMoveListener(this);
        moveLabel.setBackground(display
                .getSystemColor(SWT.COLOR_TITLE_BACKGROUND));
        moveLabel.setForeground(display
                .getSystemColor(SWT.COLOR_TITLE_FOREGROUND));

        setInitialDialogLocation();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                saveLocation();
            }
        });

        shell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                while (shell.isDisposed()) {
                    if (!display.readAndDispatch()) {
                        display.sleep();
                    }
                }
            }
        });
        setDialogImage();

        shell.pack();
    }

    public Object open(boolean alarm) {
        if (alarm && abj == null) {
            // provides the beep to alert the user that an alarm has come in
            abj = new AlarmBeepJob("AlarmBeepJob", SOUND_DELAY);
            abj.schedule();
        }

        if (shell == null || shell.isDisposed()) {
            initShell();
        }

        shell.setVisible(true);
        active = true;
        // Start a new flash job only if one isn't currently running!
        if (flasher == null) {
            phase = 0;
            flasher = new FlashBellJob("FlashBell", this, FLASH_DELAY);
        }

        return null;
    }

    /**
     * Close the AlarmAlertBell and turn off the "flasher" and "abj" job if
     * running.
     */
    public void close() {
        if (shell != null && !shell.isDisposed()) {
            shell.close();
        }
        active = false;
        if (flasher != null) {
            flasher.cancel();
            flasher = null;
        }
        if (abj != null) {
            abj.cancel();
            abj = null;
        }
    }

    private void setInitialDialogLocation() {
        if (dialogXY == null) {
            Rectangle bounds = parent.getMonitor().getBounds();
            dialogXY = new Point(bounds.x, bounds.y);
        }
        shell.setLocation(dialogXY);
    }

    private void setDialogImage() {
        ImageDescriptor id = Activator.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, FileUtil.join("images", "bell0.gif"));
        bellImage[0] = id.createImage();
        id = Activator.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                FileUtil.join("images", "bell1.gif"));
        bellImage[1] = id.createImage();

        button = new Button(shell, SWT.PUSH);
        button.setImage(bellImage[0]);
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                CurrentAlarmQueue.getInstance(shell).show();
                close();
            }
        });
    }

    private void saveLocation() {
        dialogXY = shell.getLocation();
    }

    @Override
    public void mouseDoubleClick(MouseEvent e) {
        // Don't do anything
    }

    @Override
    public void mouseDown(MouseEvent e) {
        origin = new Point(e.x, e.y);
    }

    @Override
    public void mouseUp(MouseEvent e) {
        origin = null;
    }

    /**
     * 
     */
    @Override
    public void mouseMove(MouseEvent e) {
        if (origin != null) {
            // Move the dialog.
            Point mouseLoc = shell.toDisplay(e.x, e.y);
            dialogXY.x = mouseLoc.x - origin.x;
            dialogXY.y = mouseLoc.y - origin.y;
            shell.setLocation(dialogXY.x, dialogXY.y);
        }
    }

    /**
     * Check to see if the dialog is active.
     * 
     * @return
     */
    public boolean isActive() {
        return active;
    }

    /**
     * Alternate between normal and reverse images.
     */
    public void flash() {
        button.setImage(bellImage[phase]);
        phase = (phase + 1) % bellImage.length;
    }
}