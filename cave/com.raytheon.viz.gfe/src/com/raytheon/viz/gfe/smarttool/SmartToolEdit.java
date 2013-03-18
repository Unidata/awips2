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
import java.io.FileWriter;
import java.util.Properties;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonFileFilter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.gfe.PythonUtil;

/**
 * Utilities for creating new smart tools
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Mar 20, 2008				njensen	    Initial creation
 * Sep 25, 2008 1562        askripsky   Moved methods out to
 * Mar 06, 2013 15717       jzeng        Change CAVE_STATIC to COMMON_STATIC 
 *                                      for GFE localization files 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolEdit {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SmartToolEdit.class);

    public static LocalizationFile initializeNewTool(String toolName,
            String parmToEdit) {
        LocalizationFile localizationFile = null;
        String toolFilename = new String(toolName);
        if (!toolFilename.endsWith(PythonFileFilter.EXTENSION)) {
            toolFilename += PythonFileFilter.EXTENSION;
        }

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext tx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File templateFile = PathManagerFactory.getPathManager().getFile(tx,
                SmartToolConstants.TEMPLATES_DIR);

        try {
            Properties p = new Properties();
            p.setProperty("file.resource.loader.path", templateFile.getPath());
            Velocity.init(p);
            VelocityContext context = new VelocityContext();

            context.put("itemName", toolFilename);
            context.put("parmToEdit", parmToEdit);
            String author = LocalizationManager.getInstance().getCurrentUser();
            context.put("author", author);

            Template template = null;
            template = Velocity.getTemplate("smartTool.vm");

            LocalizationContext cx = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
            localizationFile = PathManagerFactory.getPathManager()
                    .getLocalizationFile(
                            cx,
                            GfePyIncludeUtil.SMART_TOOLS + File.separator
                                    + toolFilename);
            if (localizationFile != null) {
                File file = localizationFile.getFile();
                if (!file.getParentFile().exists()) {
                    file.getParentFile().mkdirs();
                }

                FileWriter fw = new FileWriter(localizationFile.getFile());

                template.merge(context, fw);
                fw.close();
                localizationFile.save();
                PythonUtil.openPythonFile(localizationFile);
            } else {
                // Display Protected file message
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                String msg = toolFilename
                        + " is a protected file and cannot be overridden.";
                MessageDialog.openWarning(shell, "Protected File", msg);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating new smart tool", e);
        }

        return localizationFile;
    }

    public static LocalizationFile initializeNewUtility(String utilityName) {
        LocalizationFile localizationFile = null;

        String toolFilename = new String(utilityName);
        if (!toolFilename.endsWith(PythonFileFilter.EXTENSION)) {
            toolFilename += PythonFileFilter.EXTENSION;
        }

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext tx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File templateFile = pathMgr.getFile(tx,
                SmartToolConstants.TEMPLATES_DIR);

        try {
            Properties p = new Properties();
            p.setProperty("file.resource.loader.path", templateFile.getPath());
            Velocity.init(p);
            VelocityContext context = new VelocityContext();

            context.put("itemName", utilityName);
            String author = LocalizationManager.getInstance().getCurrentUser();
            context.put("author", author);

            Template template = null;
            template = Velocity.getTemplate("utility.vm");

            LocalizationContext cx = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
            localizationFile = PathManagerFactory.getPathManager()
                    .getLocalizationFile(
                            cx,
                            GfePyIncludeUtil.UTILITIES + File.separator
                                    + toolFilename);
            File file = localizationFile.getFile();
            if (!file.getParentFile().exists()) {
                file.getParentFile().mkdirs();
            }

            FileWriter fw = new FileWriter(localizationFile.getFile());

            template.merge(context, fw);
            fw.close();

            localizationFile.save();

            PythonUtil.openPythonFile(localizationFile);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating new utility", e);
        }

        return localizationFile;
    }

}
