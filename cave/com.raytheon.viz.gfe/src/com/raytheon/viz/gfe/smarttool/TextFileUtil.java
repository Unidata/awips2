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
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.python.PyCacheUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.GFEOperationFailedException;

/**
 * Utility to replace in a similar way some of the legacy TextFileID features.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 28, 2009           njensen   Initial creation
 * Apr 20, 2015  4027     randerso  Changes to support GFE formatter auto tests
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Feb 05, 2016  5242     dgilling  Remove calls to deprecated Localization
 *                                  APIs.
 * Feb 28, 2018  6602     randerso  Updated for consolidated text utilities.
 * Sep 12, 2019  7917     tgurney   Update handling of pyc files for Python 3
 *
 * </pre>
 *
 * @author njensen
 */

public class TextFileUtil {

    private static final IPathManager PATH_MGR = PathManagerFactory
            .getPathManager();

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextFileUtil.class);

    public static LocalizationFile getTextFile(String filename,
            String fileType) {
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

    public static LocalizationFile getUserTextFile(LocalizationFile source) {
        LocalizationContext ctx = PATH_MGR.getContext(
                source.getContext().getLocalizationType(),
                LocalizationLevel.USER);

        LocalizationFile destLf = getTextFile(ctx, source.getPath());
        return destLf;
    }

    private static String getPathFromType(String filename, String fileType) {
        String name = null;
        if ("TextUtility".equalsIgnoreCase(fileType)) {
            name = LocalizationUtil.join(GfePyIncludeUtil.TEXT_UTILITIES,
                    filename + ".py");
        } else if ("TextProduct".equalsIgnoreCase(fileType)) {
            name = LocalizationUtil.join(GfePyIncludeUtil.TEXT_PRODUCTS,
                    filename + ".py");
        } else if ("Combinations".equalsIgnoreCase(fileType)) {
            name = LocalizationUtil.join(GfePyIncludeUtil.GFE, "combinations",
                    filename + ".py");
        } else if ("EditAreaGroups".equalsIgnoreCase(fileType)) {
            name = LocalizationUtil.join("gfe", "editAreaGroups",
                    filename + ".txt");
        } else if ("Reference".equalsIgnoreCase(fileType)) {
            name = LocalizationUtil.join("gfe", "editAreas", filename + ".xml");
        }
        return name;
    }

    public static LocalizationFile getTextFile(LocalizationContext context,
            String filename) {
        return PATH_MGR.getLocalizationFile(context, filename);
    }

    public static void makeWritableCopy(String source, String fileType,
            String dest, boolean deleteFlag)
            throws GFEOperationFailedException {
        LocalizationFile srcLf = getTextFile(source, fileType);
        if (srcLf.getContext().getLocalizationLevel() == LocalizationLevel.BASE
                || srcLf.getContext()
                        .getLocalizationLevel() == LocalizationLevel.CONFIGURED
                || srcLf.getContext()
                        .getLocalizationLevel() == LocalizationLevel.SITE) {

            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext ctx = pathMgr.getContext(
                    srcLf.getContext().getLocalizationType(),
                    LocalizationLevel.USER);

            LocalizationFile destLf = getTextFile(ctx,
                    getPathFromType(dest, fileType));
            try {
                copy(srcLf, destLf);
            } catch (Exception e) {
                throw new GFEOperationFailedException(
                        "Unable to save localization file", e);
            }
        } else if (srcLf.getContext()
                .getLocalizationLevel() == LocalizationLevel.USER) {
            if (deleteFlag) {
                try {
                    srcLf.delete();
                } catch (Exception e) {
                    throw new GFEOperationFailedException(
                            "Unable to delete localization file "
                                    + srcLf.toString(),
                            e);
                }
            } else {
                statusHandler.debug("USER version already exists " + source);
            }
        } else {
            throw new GFEOperationFailedException(
                    "No file found at base, configured or site level for "
                            + source + " " + fileType);
        }
    }

    private static void copy(LocalizationFile srcLf, LocalizationFile destLf)
            throws IOException, LocalizationException {
        File dir = destLf.getFile(false).getParentFile();
        Files.createDirectories(dir.toPath());

        try (InputStream in = srcLf.openInputStream();
                SaveableOutputStream out = destLf.openOutputStream()) {

            // Transfer bytes from in to out
            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
            out.save();
        }
    }

    public static void deleteTextFile(LocalizationFile lf)
            throws LocalizationException, IOException {
        if (lf.getContext().getLocalizationLevel()
                .equals(LocalizationLevel.USER)
                && "GFETEST".equals(lf.getContext().getContextName())) {
            lf.delete();
            String path = lf.getFile().getAbsolutePath();
            if (path.endsWith(".py")) {
                PyCacheUtil.clean(Paths.get(path));
            }
        } else {
            throw new IllegalArgumentException(
                    "Can only delete USER level files owned by GFETEST: " + lf);
        }
    }

}
