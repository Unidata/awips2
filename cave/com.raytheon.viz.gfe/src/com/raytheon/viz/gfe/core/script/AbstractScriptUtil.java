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
/**
 * 
 */
package com.raytheon.viz.gfe.core.script;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Properties;
import java.util.regex.Pattern;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonFileFilter;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.smarttool.SmartToolConstants;

/**
 * An abstract base class that provides a default implementation of IScriptUtil.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 09, 2013  #2033     dgilling    Use new templates directory.
 * 
 * </pre>
 * 
 * @author wldougher
 * 
 */
public abstract class AbstractScriptUtil implements IScriptUtil {

    protected IPathManager pathManager;

    /**
     * Constructor.
     */
    public AbstractScriptUtil() {
        pathManager = PathManagerFactory.getPathManager();
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.IScriptUtil#copy(java.lang.String,
     *      java.lang.String,
     *      com.raytheon.uf.common.localization.LocalizationContext.
     *      LocalizationLevel,
     *      com.raytheon.viz.gfe.core.script.IScriptUtil.Overwrite)
     */
    @Override
    public LocalizationFile copy(String source, String dest,
            LocalizationLevel toLevel, Overwrite mode) throws GFEException {
        LocalizationFile src = find(source, null);
        LocalizationFile dst = null;
        String scriptType = getScriptType();
        if (src == null || !src.exists()) {
            throw new GFEException(String.format("%s %s does not exist.",
                    scriptType, source));
        } else {
            LocalizationLevel srcLevel = src.getContext()
                    .getLocalizationLevel();
            if (source.equals(dest) && srcLevel.equals(toLevel)) {
                String msg = String.format(
                        "Attempt to copy %s %s at %s level to itself.",
                        scriptType, source, srcLevel);
                throw new GFEException(msg);
            } else {
                IPathManager pathManager = PathManagerFactory.getPathManager();
                LocalizationContext ctx = pathManager.getContext(
                        LocalizationType.CAVE_STATIC, toLevel);
                String destPath = getScriptTypePathPrefix() + File.separator
                        + scripted(dest);
                dst = pathManager.getLocalizationFile(ctx, destPath);
                if (mode == Overwrite.SAFE && dst.exists()) {
                    dst = null;
                } else {
                    try {
                        FileUtil.copyFile(src.getFile(), dst.getFile());
                        dst.save();
                    } catch (Exception e) {
                        throw new GFEException(String.format(
                                "Error copying %s %s to %s.", scriptType,
                                source, dest), e);
                    }
                }
            }
        }
        return dst;
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.IScriptUtil#createNew(java.lang.
     *      String,
     *      com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel
     *      , com.raytheon.viz.gfe.core.script.IScriptUtil.Overwrite)
     */
    @Override
    public LocalizationFile createNew(String script, LocalizationLevel level,
            Overwrite mode) throws GFEException {
        String scriptFName;
        scriptFName = scripted(script);
        String path = getScriptTypePathPrefix() + File.separator + scriptFName;
        LocalizationContext context = pathManager.getContext(
                LocalizationType.CAVE_STATIC, level);
        LocalizationFile locFile = pathManager.getLocalizationFile(context,
                path);
        if (locFile == null) {
            String msg = String.format("Cannot find a path for %s at level %s",
                    script, level);
            throw new GFEException(msg);
        } else if (locFile.exists() && Overwrite.SAFE == mode) {
            throw new GFEException(
                    "Attempt to create existing file in SAFE mode");
        } else {
            File outFile = locFile.getFile();
            File parentDir = outFile.getParentFile();
            if (!parentDir.exists()) {
                parentDir.mkdirs();
            }
            FileWriter fileWriter = null;
            try {
                fileWriter = new FileWriter(outFile);
            } catch (IOException e) {
                throw new GFEException("Error creating file writer for "
                        + getScriptType() + " " + script, e);
            }

            // Set up the velocity context and template
            initVelocity();
            VelocityContext vctx = new VelocityContext();
            String author = LocalizationManager.getInstance().getCurrentUser();
            vctx.put("author", author);
            vctx.put("itemName", script);
            Template template = getVelocityTemplate();

            // Generate the local file.
            boolean throwing = false;
            try {
                template.merge(vctx, fileWriter);
            } catch (Exception e) {
                throwing = true;
                throw new GFEException("Error in velocity merge for "
                        + getScriptType() + " " + script, e);
            } finally {
                try {
                    fileWriter.close();
                } catch (IOException e) {
                    if (!throwing) {
                        throw new GFEException("Error closing file writer for "
                                + getScriptType() + " " + script, e);
                    }
                }
            }

            // Propagate local file to the server.
            try {
                locFile.save();
            } catch (Exception e) {
                throw new GFEException("Error saving " + getScriptType() + " "
                        + script, e);
            }
        }
        return locFile;
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.IScriptUtil#delete(java.lang.String,
     *      com
     *      .raytheon.uf.common.localization.LocalizationContext.LocalizationLevel)
     */
    @Override
    public void delete(String name, LocalizationLevel level)
            throws GFEException {
        String srcPath = scripted(name);
        String pycPath = srcPath.replaceAll(Pattern.quote(".py") + "$", ".pyc");
        String pyoPath = srcPath.replaceAll(Pattern.quote(".py") + "$", ".pyo");
        LocalizationLevel fileLevel = LocalizationLevel.BASE;
        LocalizationLevel pycLevel = LocalizationLevel.BASE;
        LocalizationLevel pyoLevel = LocalizationLevel.BASE;

        LocalizationFile file = find(srcPath, level);
        LocalizationFile pycFile = find(pycPath, level);
        LocalizationFile pyoFile = find(pyoPath, level);

        // If level was null we may have source and bytecode files at multiple
        // levels. Find the highest localization level.
        LocalizationLevel highest = LocalizationLevel.BASE;
        if (file != null) {
            fileLevel = file.getContext().getLocalizationLevel();
            highest = fileLevel;
        }
        if (pycFile != null) {
            pycLevel = pycFile.getContext().getLocalizationLevel();
            highest = pycLevel.compareTo(highest) > 0 ? pycLevel : highest;
        }
        if (pyoFile != null) {
            pyoLevel = pyoFile.getContext().getLocalizationLevel();
            highest = pyoLevel.compareTo(highest) > 0 ? pyoLevel : highest;
        }

        // Don't delete files less localized than highest.
        if (fileLevel.compareTo(highest) < 0) {
            file = null;
        }
        if (pycLevel.compareTo(highest) < 0) {
            pycFile = null;
        }
        if (pyoLevel.compareTo(highest) < 0) {
            pyoFile = null;
        }

        innerDelete(file, srcPath, fileLevel);
        innerDelete(pycFile, pycPath, pycLevel);
        innerDelete(pyoFile, pyoPath, pyoLevel);
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.IScriptUtil#find(java.lang.String,
     *      com
     *      .raytheon.uf.common.localization.LocalizationContext.LocalizationLevel)
     */
    @Override
    public LocalizationFile find(String name, LocalizationLevel level)
            throws GFEException {
        LocalizationFile result = null;

        String path = getScriptTypePathPrefix() + File.separator + name;
        path = scripted(path);
        if (level == null) {
            result = pathManager.getStaticLocalizationFile(path);
        } else {
            LocalizationContext ctx = pathManager.getContext(
                    LocalizationType.CAVE_STATIC, level);
            result = pathManager.getLocalizationFile(ctx, path);
            if (result != null && !result.exists()) {
                result = null;
            }
        }
        return result;
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.IScriptUtil#getScriptType()
     */
    @Override
    abstract public String getScriptType();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.script.IScriptUtil#getScriptTypePathPrefix()
     */
    @Override
    abstract public String getScriptTypePathPrefix();

    /**
     * Get the velocity template. This mostly serves to convert the many
     * exception types thrown by Velocity.getTemplate() to a GFEException.
     * 
     * @return The velocity template
     * @throws GFEException
     *             if anything goes wrong in Velocity.
     */
    protected Template getVelocityTemplate() throws GFEException {
        Template template = null;
        String templateName = getVelocityTemplateName();
        try {
            template = Velocity.getTemplate(templateName);
        } catch (Exception e) {
            throw new GFEException("Error obtaining Velocity template", e);
        }
        return template;
    }

    /**
     * Get the simple name of the velocity template used to create new scripts.
     * 
     * @return
     */
    abstract protected String getVelocityTemplateName();

    /**
     * Initialize Velocity to load file resources from the TEMPLATES_DIR
     * directory.
     * 
     * @throws GFEException
     */
    protected void initVelocity() throws GFEException {
        try {
            File templateFile = new File(FileLocator.resolve(
                    FileLocator.find(Activator.getDefault().getBundle(),
                            new Path(SmartToolConstants.TEMPLATES_DIR), null))
                    .getPath());
            Properties properties = new Properties();
            properties.setProperty("file.resource.loader.path",
                    templateFile.getPath());

            try {
                Velocity.init(properties);
            } catch (Exception e) {
                throw new GFEException("Error initializing Velocity", e);
            }
        } catch (IOException e) {
            throw new GFEException(
                    "Error locating script templates directory.", e);
        }
    }

    /**
     * Do the actual deletion of a localization file. If file is null, no action
     * is taken. Otherwise, file is deleted locally, and on the server, if it
     * exists there.
     * 
     * @param file
     *            The file to delete. May be null.
     * @param name
     *            The name of file, for error reporting.
     * @param level
     *            The localization level of file
     * @throws GFEException
     */
    protected void innerDelete(LocalizationFile file, String name,
            LocalizationLevel level) throws GFEException {
        if (file == null) {
            ; // ignore deletion of non-existent file
        } else if (file.isAvailableOnServer()) {
            try {
                file.delete();
            } catch (Exception e) {
                String msg = String.format("Error deleting %s %s at level %s",
                        getScriptType(), name, level);
                throw new GFEException(msg, e);
            }
        } else if (file.exists()) {
            File jFile = file.getFile();
            jFile.delete();
        }
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.IScriptUtil#rename(java.lang.String,
     *      java.lang.String,
     *      com.raytheon.uf.common.localization.LocalizationContext.
     *      LocalizationLevel,
     *      com.raytheon.viz.gfe.core.script.IScriptUtil.Overwrite)
     */
    @Override
    public LocalizationFile rename(String source, String dest,
            LocalizationLevel toLevel, Overwrite mode) throws GFEException {
        LocalizationFile dst;
        LocalizationFile src = find(source, null);
        dst = copy(source, dest, toLevel, mode);
        delete(source, src.getContext().getLocalizationLevel());
        return dst;
    }

    /**
     * Add ".py" to name if it doesn't end with ".py", ".pyc", or ".pyo" and
     * return the result.
     * 
     * @param name
     *            the undecorated script name
     * @return the decorated name.
     */
    @Override
    public String scripted(String name) {
        String result = null;
        if (name.endsWith(PythonFileFilter.EXTENSION) || name.endsWith(".pyo")
                || name.endsWith(".pyc")) {
            result = name;
        } else {
            result = name + PythonFileFilter.EXTENSION;
        }
        return result;
    }
}
