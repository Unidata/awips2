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
package com.raytheon.uf.viz.alertviz.localization.actions;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.localization.filetreeview.FileTreeEntryData;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Opens a file dialog for importing files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 04, 2011  5853       bgonzale    Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class AlertVizFileImportAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertVizFileImportAction.class, "GDN_ADMIN",
                    "GDN_ADMIN");

    private static final String PLUGIN_ID = "com.raytheon.uf.viz.alertviz.ui";

    private static final String ASTERISK = "*";

    private static final String ALL_FILES = "*.*";

    private LocalizationLevel level;

    private String[] extensions;

    private String path;

    /**
     * @param fileEntry
     * 
     */
    public AlertVizFileImportAction(FileTreeEntryData fileEntry) {
        this(fileEntry, LocalizationLevel.USER);
    }

    /**
     * @param fileEntry
     * @param level
     * 
     */
    public AlertVizFileImportAction(FileTreeEntryData fileEntry,
            LocalizationLevel level) {
        super("Import");
        this.level = level == null ? LocalizationLevel.USER : level;
        this.path = fileEntry.getPath();
        String[] fileEntryExtensions = fileEntry.getPathData().getFilter();
        this.extensions = new String[fileEntryExtensions.length + 1];
        for (int i = 0; i < fileEntryExtensions.length; ++i) {
            this.extensions[i] = ASTERISK + fileEntryExtensions[i];
        }
        this.extensions[this.extensions.length - 1] = ALL_FILES;
    }

    @Override
    public void run() {
        Shell shell = VizWorkbenchManager.getInstance().getCurrentWindow()
                .getShell();
        FileDialog fd = new FileDialog(shell, SWT.OPEN | SWT.MULTI);

        fd.setText("Import " + level + " File");
        fd.setFilterExtensions(extensions);
        fd.setFilterPath(System.getProperty("user.home"));

        String fileName = fd.open();

        if (fileName != null) {
            File file = new File(fileName);

            if (file.exists() && file.isFile()) {
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationContext ctx = pm.getContext(
                        LocalizationType.CAVE_STATIC, level);
                LocalizationFile locFile = pm.getLocalizationFile(ctx, path
                        + File.separator + file.getName());

                try {
                    saveToLocalizationFile(file, locFile);
                } catch (FileNotFoundException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (LocalizationOpFailedException e) {
                    statusHandler.handle(Priority.CRITICAL,
                            "Error Importing file " + fileName, e);
                }
            }
        }
    }

    private void saveToLocalizationFile(File file, LocalizationFile locFile)
            throws IOException, LocalizationOpFailedException {
        File newFile = locFile.getFile();
        InputStream in = new FileInputStream(file);
        OutputStream out = new FileOutputStream(newFile);
        byte[] buff = new byte[1024];
        int len;

        while ((len = in.read(buff)) > 0) {
            out.write(buff, 0, len);
        }
        in.close();
        out.close();
        locFile.save();
    }
}
