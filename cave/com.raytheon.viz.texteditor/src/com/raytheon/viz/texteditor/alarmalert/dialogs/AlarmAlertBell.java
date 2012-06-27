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

import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.File;

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
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmBeepJob;
import com.raytheon.viz.texteditor.alarmalert.util.FlashBellJob;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2009            mnash       Initial creation
 * Aug 16, 2010 2187	   cjeanbap    Fixed a NullPointerException
 * Dec 23, 2010 7375       cjeanbap    Force dialog ON TOP of over Dialog/Windows.
 * 03/19/2012              D. Friedman Fix alarming.  Disable runloop in open().
 * May 18, 2012            jkorman     Added flashing alarm image.
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmAlertBell extends Dialog implements MouseMoveListener,
        MouseListener {
    
    // delay in milliseconds - flash every 1 1/2 seconds
    private static final int FLASH_DELAY = 1500;
    
    private Shell parentShell;

    private Shell alarmShell;

    private Display display;

    private CurrentAlarmQueue alarmDlg;

    private String bellPath;

    private FlashBellJob flasher;
    
    private Image norm_bell;

    private Image revs_bell;

    private boolean invert = false;
    
    private boolean active = false;
    
    private Button button;

    private AlarmBeepJob abj = null;

    private static int locationX = -1;

    private static int locationY = -1;

    /**
     * Mouse origin.
     */
    private Point origin;

    /**
     * Adjusted dialog location.
     */
    private Point dialogLoc;

    /**
     * Actual dialog X, Y coordinate.
     */
    private Point dialogXY;

    /**
     * Move dialog flag.
     */
    private boolean moveDialog = false;

    /**
     * Move label.
     */
    private Label moveLabel;

    /**
     * Font used for labels
     */
    private Font labelFont;

    /**
     * @param parentShell
     */
    public AlarmAlertBell(Shell parentShell) {
        super(parentShell);

        this.parentShell = parentShell;

        initShell();
    }

    public void initShell() {
        Shell parent = getParent();
        display = parentShell.getDisplay();

        alarmShell = new Shell(parent, SWT.DIALOG_TRIM | SWT.ON_TOP);
        alarmShell.setText("Alarm Alert Bell");
        GridLayout mainLayout = new GridLayout(1, true);
        alarmShell.setLayout(mainLayout);

        labelFont = new Font(alarmShell.getDisplay(), "Monospace", 14, SWT.BOLD);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        moveLabel = new Label(alarmShell, SWT.CENTER | SWT.BORDER);
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

        alarmShell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                setLocation();
            }
        });

        alarmShell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                while (alarmShell.isDisposed()) {
                    if (!display.readAndDispatch()) {
                        display.sleep();
                    }
                }
            }
        });
        setDialogImage();
    }

    public Object open(boolean alarm) {
        if (alarm) {
            // provides the beep to alert the user that an alarm has come in
            abj = new AlarmBeepJob("AlarmBeepJob");
            abj.schedule();
        }

        if (alarmShell == null) {
            display = parentShell.getDisplay();
            if (alarmShell == null) {
                initShell();
            }
            alarmShell.setLocation(locationX, locationY);

            alarmShell.pack();
        }
        alarmShell.setVisible(true);
        alarmShell.pack();
        alarmShell.setActive();
        active = true;
        // Start a new flash job only if one isn't currently running!
        if(flasher == null) {
            invert = false;
            flasher = new FlashBellJob("FlashBell", this, FLASH_DELAY);
        }
        return null;
    }

    /**
     * Close the AlarmAlertBell and turn off the "flasher" job
     * if running.
     */
    public void close() {
        if(!alarmShell.isDisposed()) {
            alarmShell.setVisible(false);
        }
        active = false;
        if(flasher != null) {
            flasher.cancel();
            flasher = null;
        }
    }
    
    private void setInitialDialogLocation() {
        if (locationX < 0) {
            Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
            int screenWidth = d.width;
            int screenHeight = d.height;
            if (screenWidth > (screenHeight * 2)) {
                screenWidth /= 2;
            }
            locationX = screenWidth / 2 - 102;
            locationY = screenHeight / 2 - 88;
        }

        dialogXY = new Point(locationX, locationY);
    }

    private void setDialogImage() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext localization = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        LocalizationFile imageFile = PathManagerFactory.getPathManager()
                .getLocalizationFile(localization,
                        "images" + File.separator + "bell.gif");
        bellPath = imageFile.getFile().getAbsolutePath();
        norm_bell = new Image(display, bellPath);
        button = new Button(alarmShell, SWT.IMAGE_GIF);
        if(norm_bell != null) {
            createInvertImage(bellPath);
            button.setImage(norm_bell);
        }
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (alarmDlg == null) {
                    alarmDlg = CurrentAlarmQueue.getInstance(getParent());
                    alarmDlg.setDialogFocus();
                    alarmDlg.open();
                } else {
                    if (alarmDlg.getShell().isDisposed()) {
                        alarmDlg = CurrentAlarmQueue.getInstance(alarmShell);
                        alarmDlg.setDialogFocus();
                        alarmDlg.open();
                    } else {
                        alarmDlg.setDialogFocus();
                    }
                }
                close();
            }
        });
    }

    private void setLocation() {
        Point location = alarmShell.getLocation();
        locationX = location.x;
        locationY = location.y;
    }

    @Override
    public void mouseDoubleClick(MouseEvent e) {
        // Don't do anything
    }

    @Override
    public void mouseDown(MouseEvent e) {
        origin = new Point(e.x, e.y);
        moveDialog = true;
    }

    @Override
    public void mouseUp(MouseEvent e) {
        moveDialog = false;
    }

    /**
     * 
     */
    @Override
    public void mouseMove(MouseEvent e) {
        if (origin != null && moveDialog == true) {
            // Move the dialog.
            dialogLoc = display.map(alarmShell, null, e.x, e.y);
            dialogXY.x = dialogLoc.x - origin.x;
            dialogXY.y = dialogLoc.y - origin.y;
            alarmShell.setLocation(dialogXY.x, dialogXY.y);
        }
    }

    /**
     * Create an inverse image of the bell.
     */
    private void createInvertImage(String path) {
        if (norm_bell != null) {
            ImageData id = new ImageData(path);
            for(int i = 0;i < id.width;i++) {
                for(int j = 0;j < id.height;j++) {
                    if(id.getPixel(i,j) == 0) {
                        id.setPixel(i,j,1);
                    } else {
                        id.setPixel(i,j,0);
                    }
                }
            }
            revs_bell = new Image(display, id);
        }
    }

    /**
     * Check to see if the dialog is active.
     * @return
     */
    public boolean isActive() {
        return active;
    }
    
    /**
     * Alternate between normal and reverse images.
     */
    public void flash() {
        if(invert) {
            button.setImage(revs_bell);
        } else {
            button.setImage(norm_bell);
        }
        invert = !invert;
    }
}