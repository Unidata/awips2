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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Display FFMP Basin Table's splash page dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Dec 7, 2012  1353       rferrel     Covert to CaveSWTDialog and make non-blocking.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class FFMPSplash extends CaveSWTDialog {
    /**
     * The display control.
     */
    private Display display;

    /**
     * Canvas to display the information.
     */
    private Canvas canvas;

    private Font textFont;

    /**
     * Canvas width.
     */
    private int canvasWidth = 300;

    /**
     * Canvas height.
     */
    private int canvasHeight = 200;

    private Image loadImage = null;

    public FFMPSplash(Shell parent) {
        super(parent, SWT.NO_TRIM | SWT.ON_TOP, CAVE.DO_NOT_BLOCK);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        display = shell.getDisplay();

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        centerOnScreen();
    }

    private void initializeComponents() {
        textFont = new Font(display, "Monospace", 50, SWT.BOLD);
        String imageName = loadImage();

        if (imageName != null) {
            loadImage = new Image(display, imageName);
        }

        // Make the calculation for the canvas.
        // makeCalculations();

        // Create the canvas.
        createCanvas();
    }

    private void createCanvas() {
        canvas = new Canvas(shell, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = canvasHeight;
        gd.widthHint = canvasWidth;

        canvas.setSize(canvasWidth, canvasHeight);

        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });
    }

    private void drawCanvas(GC gc) {
        gc.setFont(textFont);
        gc.setTextAntialias(SWT.ON);

        if (loadImage != null) {
            gc.drawImage(loadImage, 0, 0);
        } else {
            gc.setBackground(display.getSystemColor(SWT.COLOR_BLUE));
            gc.fillRectangle(0, 0, canvasWidth, canvasHeight);
        }

        gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        gc.setLineWidth(3);
        gc.drawRectangle(1, 1, canvasWidth - 3, canvasHeight - 3);
    }

    /**
     * Center on the screen that CAVE is on.
     */
    private void centerOnScreen() {
        Shell tmpShell = getParent().getShell();

        Monitor[] monitors = getParent().getDisplay().getMonitors();
        int displayMonitor = 0;

        for (int i = 0; i < monitors.length; i++) {
            displayMonitor = i;
            int rightEdge = monitors[i].getBounds().x
                    + monitors[i].getBounds().width;
            if (tmpShell.getBounds().x < rightEdge) {
                break;
            }
        }

        Monitor monitor = monitors[displayMonitor];
        Rectangle monRect = monitor.getBounds();

        int xCoord = ((monRect.x + (monRect.width / 2)) - (canvasWidth / 2));
        int yCoord = (monRect.height / 2) - (canvasHeight / 2);

        shell.setLocation(xCoord, yCoord);
    }

    private String loadImage() {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getStaticFile(
                "ffmp" + File.separatorChar + "images" + File.separatorChar
                        + "ffmpLoading.png").getAbsolutePath();
        return path;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        textFont.dispose();
        loadImage.dispose();
        shell.dispose();
    }
}
