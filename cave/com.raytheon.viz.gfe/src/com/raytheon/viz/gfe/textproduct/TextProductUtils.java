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
package com.raytheon.viz.gfe.textproduct;

import java.io.File;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Utilities for text products
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sept 23, 2008			askripsky	Initial creation
 * Sept 09, 2013  #2033     dgilling    Remove dead code.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class TextProductUtils {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextProductUtils.class);

    // Extension of text products
    public static final String EXTENSION = ".py";

    // Key for products
    public final static String PRODUCT = "PRODUCT";

    // Key for utilities
    public final static String UTILITIES = "UTILITIES";

    // Smart sub type text product
    public final static String SMART = "SMART";

    // Table sub type text product
    public final static String TABLE = "TABLE";

    // Root path for text products
    private final static String TEXT_PRODUCTS_ROOT_PATH = "gfe"
            + File.separator + "userPython";

    // Root path for Products
    public final static String PRODUCTS_PATH = TEXT_PRODUCTS_ROOT_PATH
            + File.separator + "textProducts";

    // Root path for Utilities
    public final static String UTILITIES_PATH = TEXT_PRODUCTS_ROOT_PATH
            + File.separator + "utilities";

    /**
     * Copies the source file
     * 
     * @param oldFileName
     *            The file to be copied
     * @param newFileName
     *            The destination file
     * @param type
     *            Designates whether the file is a product or a utility
     * @throws VizException
     *             Exception if the file doesn't get copied correctly
     */
    public static void copyFile(String oldFileName, String newFileName,
            String type) throws VizException {
        // Make sure the new name has the correct extension
        if (!newFileName.endsWith(EXTENSION)) {
            newFileName += EXTENSION;
        }

        // Get file from catalogue
        LocalizationFile oldFile = TextProductCatalogue.getInstance().getEntry(
                oldFileName, type);

        // Get new file for the Site
        LocalizationFile newFile = newTextProductFile(newFileName, type);

        // Make a copy to the Site location
        try {
            // Copy file
            FileUtil.copyFile(oldFile.getFile(), newFile.getFile());

            newFile.save();

            // Add entry to catalogue
            TextProductCatalogue.getInstance().addEntry(newFileName, newFile,
                    type);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error copying text product", e);
        }
    }

    /**
     * Deletes a text product or utility.
     * 
     * @param toolToDelete
     *            the name of the tool
     * @throws VizException
     */
    public static void deleteTextProduct(String textProductToDelete,
            String textProductType) throws VizException {
        // Retrieve a handle to the actual file
        LocalizationFile file = TextProductCatalogue.getInstance().getEntry(
                textProductToDelete, textProductType);

        if (file.getContext().getLocalizationLevel() != LocalizationLevel.BASE) {
            if ((file != null) && file.getFile().exists()) {
                try {
                    file.delete();

                    TextProductCatalogue.getInstance().removeEntry(
                            textProductToDelete, textProductType);
                } catch (Exception e) {
                    throw new VizException(e);
                }
            }
        } else {
            throw new VizException("Can't delete base text product "
                    + textProductToDelete);
        }
    }

    private static LocalizationFile newTextProductFile(String newName,
            String type) throws VizException {

        IPathManager pathMgr = PathManagerFactory.getPathManager();

        // Get context for the Site
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);

        // Build the path to the new file
        String path = (type.equals(PRODUCT) ? PRODUCTS_PATH : UTILITIES_PATH)
                + File.separator + newName;

        // Verify the extension is correct
        if (!path.endsWith(EXTENSION)) {
            path += EXTENSION;
        }

        // the created file
        LocalizationFile rval = PathManagerFactory.getPathManager()
                .getLocalizationFile(cx, path);

        // Check to see if the file exists
        // If not, create it
        File file = rval.getFile();
        if (!file.exists()) {
            try {
                // Check to see if the directory exists
                File dir = file.getParentFile();

                if (!dir.exists()) {
                    dir.mkdirs();
                }

                file.createNewFile();

            } catch (Exception e) {
                throw new VizException(e);
            }
        }

        return rval;
    }

    public static LocalizationLevel getLocalizationLevel(String entryName,
            String entryType) {
        return TextProductCatalogue.getInstance()
                .getEntry(entryName, entryType).getContext()
                .getLocalizationLevel();
    }

    public static void renameFile(String oldName, String newName, String type)
            throws VizException {

        // Verify the extension is correct
        if (!newName.endsWith(EXTENSION)) {
            newName += EXTENSION;
        }

        // Rename the localization file and entry
        try {
            copyFile(oldName, newName, type);
            deleteTextProduct(oldName, type);
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    public static boolean promptForOverwrite(String name, String type) {
        boolean okToOverwrite = true;

        if (!name.endsWith(EXTENSION)) {
            name += EXTENSION;
        }

        LocalizationFile preexistingFile = TextProductCatalogue.getInstance()
                .getEntry(name, type);

        if (preexistingFile != null) {
            // Initialize the confirmation prompt
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();

            if (preexistingFile.getContext().getLocalizationLevel() == LocalizationLevel.BASE) {
                // Do you want to create a site copy?
                String prompt = name
                        + " exists at the BASE Level and cannot be modified.\nDo you"
                        + " want to make a copy that can be modified at the SITE Level?";

                okToOverwrite = MessageDialog.openConfirm(shell,
                        "Item Overwrite", prompt);
            } else {
                // A Site file
                String prompt = name
                        + " already exists.\nDo you want to overwrite it?";

                okToOverwrite = MessageDialog.openConfirm(shell,
                        "Item Overwrite", prompt);
            }
        }

        return okToOverwrite;
    }
}
