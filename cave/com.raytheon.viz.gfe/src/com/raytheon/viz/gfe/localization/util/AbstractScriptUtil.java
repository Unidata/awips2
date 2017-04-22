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

package com.raytheon.viz.gfe.localization.util;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

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
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.python.PythonFileFilter;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.smarttool.SmartToolConstants;

/**
 * An abstract base class for creating GFE localization files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * ???                    wldougher  Initial creation
 * Sep 09, 2013  2033     dgilling   Use new templates directory.
 * Jan 18, 2016  4834     njensen    Fix creating files, deleted dead code
 * Aug 11, 2016  5816     randerso   Moved to gfe.localization.util.
 *                                   Code cleanup/refactor
 * 
 * </pre>
 * 
 * @author wldougher
 * 
 */
public abstract class AbstractScriptUtil {

    /**
     * Overwrite mode
     * 
     * If mode is DISALLOW the user will not be allowed to overwrite an existing
     * file.
     * 
     * If mode is ALLOW, the user will be prompted to ensure they wish to
     * overwrite an existing file.
     */
    public enum Overwrite {
        /** Allow overwrite of existing file */
        ALLOW,

        /** Disallow overwrite of existing file */
        DISALLOW
    }

    protected IPathManager pathManager;

    /**
     * Constructor.
     */
    protected AbstractScriptUtil() {
        pathManager = PathManagerFactory.getPathManager();
    }

    /**
     * The type of script this utility handles. Implementers should return this
     * in capitalized case, i.e., "Procedure", for use in error messages.
     * 
     * @return the script type string
     */
    public abstract String getScriptType();

    /**
     * Get the path prefix used in building localization filenames. For example,
     * the path prefix for Procedures is "gfe/userPython/procedures", or
     * GfePyIncludeUtil.PROCEDURES.
     * 
     * @return The path prefix
     */
    public abstract String getScriptTypePathPrefix();

    /**
     * Get the LocalizationType to be used for the script
     * 
     * @return the LocalizationType
     */
    public abstract LocalizationType getLocalizationType();

    /**
     * Get the LocalizationLevel to be used for the script
     * 
     * @return the LocalizationLevel
     */
    public abstract LocalizationLevel getLocalizationLevel();

    /**
     * Create a new script at the designated localization level. Implementations
     * may initialize the contents of the new script however they desire.
     * 
     * @param script
     * @param mode
     * @return A reference to the newly created script, or null if the script
     *         could not be created.
     * @throws GFEException
     */
    public LocalizationFile createNew(String script, Overwrite mode)
            throws GFEException {
        return this.createNew(script, mode, null);
    }

    /**
     * Create a new script at the designated localization level. Implementations
     * may initialize the contents of the new script however they desire.
     * 
     * @param script
     * @param mode
     * @param additionalContextItems
     * @return A reference to the newly created script, or null if the script
     *         could not be created.
     * @throws GFEException
     */
    public LocalizationFile createNew(String script, Overwrite mode,
            Map<String, Object> additionalContextItems) throws GFEException {
        String scriptFName;
        scriptFName = normalize(script);
        String path = getScriptTypePathPrefix() + File.separator + scriptFName;
        LocalizationContext context = pathManager.getContext(
                getLocalizationType(), getLocalizationLevel());
        LocalizationFile locFile = pathManager.getLocalizationFile(context,
                path);
        if (locFile == null) {
            String msg = String.format("Unable to create %s at level %s",
                    script, getLocalizationLevel());
            throw new GFEException(msg);
        } else if (locFile.exists() && Overwrite.DISALLOW == mode) {
            throw new GFEException(script + " already exists!");
        } else {
            // Set up the velocity context and template
            initVelocity();
            VelocityContext vctx = new VelocityContext();
            String author = LocalizationManager.getInstance().getCurrentUser();
            vctx.put("author", author);
            vctx.put("itemName", script);

            // process additional context items
            if (additionalContextItems != null) {
                for (Entry<String, Object> entry : additionalContextItems
                        .entrySet()) {
                    vctx.put(entry.getKey(), entry.getValue());
                }
            }

            Template template = getVelocityTemplate();

            // Generate the local file.
            try (SaveableOutputStream sos = locFile.openOutputStream();
                    OutputStreamWriter writer = new OutputStreamWriter(sos)) {
                template.merge(vctx, writer);
                writer.close();
                sos.save();
                // refresh the reference in memory
                locFile = pathManager.getLocalizationFile(context, path);
            } catch (Exception e) {
                throw new GFEException(
                        "Error creating new file " + scriptFName, e);
            }
        }
        return locFile;
    }

    /**
     * Find the named script at the designated localization level, or at the
     * most localized level at which it appears if level is null.
     * 
     * @param name
     *            the simple name of the script to find.
     * @param level
     *            the localization level of the script. If this parameter is
     *            null, the standard USER/SITE/BASE search levels will be used.
     * @return a reference to the script, or null if the script cannot be found.
     * @throws GFEException
     *             if network or file errors (other than FileNotFoundException)
     *             prevent the file from being found.
     */
    public LocalizationFile find(String name, LocalizationLevel level)
            throws GFEException {
        LocalizationFile result = null;

        String path = getScriptTypePathPrefix() + File.separator + name;
        path = normalize(path);
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
     * Add ".py" to name if it doesn't end with ".py", ".pyc", or ".pyo" and
     * return the result.
     * 
     * @param name
     *            the undecorated script name
     * @return the decorated name.
     */
    public String normalize(String name) {
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
