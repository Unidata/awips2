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
package com.raytheon.viz.gfe.smarttool;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.python.PythonFileFilter;
import com.raytheon.viz.gfe.GFEOperationFailedException;

/**
 * Utility to replace in a similar way some of the legacy TextFileID features.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TextFileUtil {

    private static final IPathManager PATH_MGR = PathManagerFactory
            .getPathManager();

    public static LocalizationFile getTextFile(String filename, String fileType) {
        LocalizationFile file = null;
        String name = getPathFromType(filename, fileType);

        if (name != null) {
            file = PATH_MGR.getStaticLocalizationFile(name);
            if (file == null) {
                LocalizationContext ctx = PATH_MGR.getContext(
                        LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
                file = PATH_MGR.getLocalizationFile(ctx, name);
            }
        }

        return file;
    }

    public static LocalizationFile getSiteTextFile(String filename,
            String fileType) {
        LocalizationFile file = null;
        String name = getPathFromType(filename, fileType);

        if (name != null) {
            LocalizationContext ctx = PATH_MGR.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
            file = PATH_MGR.getLocalizationFile(ctx, name);
        }
        return file;
    }

    private static String getPathFromType(String filename, String fileType) {
        String name = null;
        if (fileType.equalsIgnoreCase("TextUtility")) {
            name = GfePyIncludeUtil.TEXT_UTILITIES + File.separator + "regular"
                    + File.separator + filename + PythonFileFilter.EXTENSION;
        } else if (fileType.equalsIgnoreCase("TextProduct")) {
            name = GfePyIncludeUtil.TEXT_PRODUCTS + File.separator + filename
                    + PythonFileFilter.EXTENSION;
        } else if (fileType.equalsIgnoreCase("Combinations")) {
            name = GfePyIncludeUtil.GFE + File.separator + "combinations"
                    + File.separator + filename + PythonFileFilter.EXTENSION;
        } else if (fileType.equalsIgnoreCase("EditAreaGroups")) {
            name = "gfe" + File.separator + "editAreaGroups" + File.separator
                    + filename + ".txt";
        } else if (fileType.equalsIgnoreCase("Reference")) {
            name = "editAreas" + File.separator + filename + ".xml";
        }
        return name;
    }

    public static LocalizationFile getTextFile(LocalizationContext context,
            String filename) {
        return PATH_MGR.getLocalizationFile(context, filename);
    }

    public static void makeWritableCopy(String source, String fileType,
            String dest, boolean deleteFlag) throws IOException,
            GFEOperationFailedException {
        LocalizationFile lf = getTextFile(source, fileType);
        if (lf.getContext().getLocalizationLevel() == LocalizationLevel.BASE
                || lf.getContext().getLocalizationLevel() == LocalizationLevel.SITE) {
            File srcFile = lf.getFile();

            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext ctx = pathMgr.getContext(lf.getContext()
                    .getLocalizationType(), LocalizationLevel.USER);

            LocalizationFile destLf = getTextFile(ctx,
                    getPathFromType(dest, fileType));
            File destFile = destLf.getFile();
            copy(srcFile, destFile);
            try {
                destLf.save();
            } catch (Exception e) {
                throw new GFEOperationFailedException(
                        "Unable to save localization file", e);
            }
        } else if (lf.getContext().getLocalizationLevel() == LocalizationLevel.USER) {
            if (deleteFlag) {
                try {
                    lf.delete();
                } catch (Exception e) {
                    throw new GFEOperationFailedException(
                            "Unable to delete localization file "
                                    + lf.toString(), e);
                }
            } else {
                System.out.println("USER version already exists " + source);
            }
        } else {
            throw new GFEOperationFailedException(
                    "No file found at base or site level for " + source + " "
                            + fileType);
        }
    }

    private static void copy(File src, File dest) throws IOException {
        File dir = new File(dest.getParent());
        dir.mkdir();
        InputStream in = new FileInputStream(src);
        OutputStream out = new FileOutputStream(dest);

        // Transfer bytes from in to out
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
    }

    public static void deleteTextFile(LocalizationFile lf)
            throws LocalizationOpFailedException {
        lf.delete();
    }

}
