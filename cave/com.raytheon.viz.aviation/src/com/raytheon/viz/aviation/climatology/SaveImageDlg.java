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
package com.raytheon.viz.aviation.climatology;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

/**
 * This handles the information for the file dialog used for saving images in
 * clmate dialog classes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2011 8861       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class SaveImageDlg {
    /**
     * Array of image filter names.
     */
    private final static String[] FILTER_NAMES = { "Bitmap (*.bmp)",
            "JPEG (*.jpg)", "PNG (*.png)" };

    /**
     * Array of image filters.
     */
    private final static String[] FILTER_EXTS = { "*.bmp", "*.jpg", "*.png" };

    /**
     * Last directory visited.
     */
    private String saveDir = null;

    /**
     * Selected filter; default is PNG.
     */
    private int saveImageFilterIndex = 2;

    Shell shell;

    FileDialog dlg;

    int style = SWT.IMAGE_UNDEFINED;

    public SaveImageDlg(Shell shell) {
        this.shell = shell;
        dlg = new FileDialog(shell, SWT.SAVE);
        dlg.setText("Save Image");
        dlg.setFilterNames(FILTER_NAMES);
        dlg.setFilterExtensions(FILTER_EXTS);
        dlg.setFilterIndex(saveImageFilterIndex);
        dlg.setFilterPath(saveDir);
    }

    public String open() {
        return open(false);
    }

    public String open(boolean kml) {
        style = SWT.IMAGE_UNDEFINED;
        String filename = dlg.open();
        if (filename == null) {
            return null;
        }

        File dir = (new File(filename)).getParentFile();
        saveDir = dir.getAbsolutePath();
        saveImageFilterIndex = dlg.getFilterIndex();

        if (dir.canWrite() == false) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("File Selection");
            mb.setMessage("You do not have permission to create a file in the selected directory.");
            mb.open();
            return null;
        }

        // Default to PNG
        style = SWT.IMAGE_PNG;

        if (filename.endsWith(".jpg") == true && !kml) {
            style = SWT.IMAGE_JPEG;
        } else if (filename.endsWith(".png") == true) {
            style = SWT.IMAGE_PNG;
        } else if (filename.endsWith(".bmp") == true && !kml) {
            style = SWT.IMAGE_BMP;
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
            mb.setText("File Selection");
            mb.setMessage("No extension was provided for the image name.\n"
                    + "Defaulting to PNG format.");
            mb.open();

            filename += ".png";
        }
        return filename;
    }

    public int getStyle() {
        return style;
    }
}
