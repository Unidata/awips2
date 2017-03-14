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

package com.raytheon.viz.gfe.core.script;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
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
 * An abstract base class that provides a default implementation of IScriptUtil.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                     wldougher   Initial creation
 * Sep 09, 2013  #2033     dgilling    Use new templates directory.
 * Jan 18, 2016   4834     njensen     Fix creating files, deleted dead code
 * 
 * </pre>
 * 
 * @author wldougher
 * 
 */
public abstract class AbstractScriptUtil implements PythonFileTemplate {

    protected IPathManager pathManager;

    /**
     * Constructor.
     */
    protected AbstractScriptUtil() {
        pathManager = PathManagerFactory.getPathManager();
    }

    @Override
    public LocalizationFile createNew(String script, LocalizationLevel level,
            Overwrite mode) throws GFEException {
        String scriptFName;
        scriptFName = normalize(script);
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
            // Set up the velocity context and template
            initVelocity();
            VelocityContext vctx = new VelocityContext();
            String author = LocalizationManager.getInstance().getCurrentUser();
            vctx.put("author", author);
            vctx.put("itemName", script);
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

    @Override
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
    @Override
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
