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
import java.io.FileWriter;
import java.util.Properties;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
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
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.gfe.PythonUtil;

/**
 * Utilities for text products
 * 
 * <pre>
S * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 23,2008			    askripsky	Initial creation
 * Mar 06,2013  15717       jzeng       Change CAVE_STATIC to COMMON_STATIC 
 *                                      for GFE localization files 
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

    // Root path for Templates
    public final static String TEMPLATES_PATH = TEXT_PRODUCTS_ROOT_PATH
            + File.separator + "templates";

    // Template for Table type Text Product
    public final static String TEXT_PRODUCT_TABLE_TEMPLATES = "textProductTable.vm";

    // Template for Smart type Text Product
    public final static String TEXT_PRODUCT_SMART_TEMPLATES = "textProductSmart.vm";

    // Template for Text Utilities
    public final static String TEXT_UTILITY_TEMPLATES = "textUtility.vm";

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
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

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

    /**
     * Uses Velocity templates to create new text products and utilities
     * 
     * @param name
     *            The name of the new product or utility
     * @param type
     *            Designate whether the file is a product or a utility
     * @param subType
     *            Designate whether the product is a table or smart type or null
     *            if it is a new utility
     */
    public static void createNewFromTemplate(String name, String type,
            String subType) {
        // Verify the extension is correct
        if (!name.endsWith(EXTENSION)) {
            name += EXTENSION;
        }

        // Get path to template file
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext tx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File templateFile = pathMgr.getFile(tx, TEMPLATES_PATH);

        try {
            // Set path to template file
            Properties p = new Properties();
            p.setProperty("file.resource.loader.path", templateFile.getPath());

            // get velocity context
            Velocity.init(p);
            VelocityContext context = new VelocityContext();

            String selectedTemplate = "";
            if (type.compareTo(UTILITIES) == 0) {
                // Choose the Utilities template
                selectedTemplate = TEXT_UTILITY_TEMPLATES;
            } else {
                if (subType.compareTo(TABLE) == 0) {
                    // Choose the Product Table template
                    selectedTemplate = TEXT_PRODUCT_TABLE_TEMPLATES;
                } else {
                    // Choose the Product Smart template
                    selectedTemplate = TEXT_PRODUCT_SMART_TEMPLATES;
                }
            }

            // Retrieve template
            Template template = null;
            template = Velocity.getTemplate(selectedTemplate);

            // set options for template
            String author = LocalizationManager.getInstance().getCurrentUser();
            context.put("author", author);
            context.put("itemName", name.split(EXTENSION)[0]);

            // Create the localization file
            LocalizationFile localizationFile = newTextProductFile(name, type);

            FileWriter fw = new FileWriter(localizationFile.getFile());

            template.merge(context, fw);
            fw.flush();
            fw.close();

            // Save the merged template to the server
            localizationFile.save();

            // Add new product or utility to the catalogue
            TextProductCatalogue.getInstance().addEntry(name, localizationFile,
                    type);

            // Open new file in the Python editor
            PythonUtil.openPythonFile(localizationFile);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating new text product", e);
        }

    }

    /**
     * Prompts for a name for a new utility and creates it.
     */
    public static void promptForNewUtility() {
        InputDialog dlg = new InputDialog(
                Display.getCurrent().getActiveShell(), "New Text Utility",
                "New Utility Name", "", null);
        if (dlg.open() == Dialog.OK) {
            String newName = dlg.getValue();

            if (promptForOverwrite(newName, TextProductUtils.UTILITIES)) {
                TextProductUtils.createNewFromTemplate(newName,
                        TextProductUtils.UTILITIES, null);
            }
        }
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
