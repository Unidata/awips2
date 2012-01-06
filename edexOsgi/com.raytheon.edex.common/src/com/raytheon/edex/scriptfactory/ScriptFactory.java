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
package com.raytheon.edex.scriptfactory;

import java.io.File;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.app.VelocityEngine;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Provides an interface to the script generation template facility
 * 
 * Implemented as a singleton for performance reasons. Templates and context are
 * both cached in memory.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 17, 2008	#1088        chammack	Initial creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class ScriptFactory {

    /** Cached velocity engine */
    private VelocityEngine velocityEngine;

    /** Cached templates */
    private final Map<String, Template> templateMap;

    /** The singleton instance */
    private static ScriptFactory instance;

    /**
     * Guarded constructor
     */
    private ScriptFactory() {
        templateMap = new HashMap<String, Template>();
    }

    public static synchronized ScriptFactory getInstance() {
        if (instance == null) {
            instance = new ScriptFactory();
        }
        return instance;
    }

    /**
     * Generate a script from a template
     * 
     * 
     * @param scriptTemplate
     *            the path to the script template to use
     * @param globalScriptDir
     *            the path to the global macro script directory
     * @param maxRecords
     *            the maximum number of records to return (if applicable, else
     *            zero)
     * @param mode
     *            the script mode (if applicable)
     * @param scriptLibrary
     *            the script library to use (if applicable)
     * @param metaData
     *            the script's metadata that will be passed into the template
     * @param
     * @return a generated script
     * @throws EdexException
     *             if script generation fails
     */
    public synchronized String createScript(File scriptTemplate,
            File globalScriptDir, int maxRecords, String mode,
            String scriptLibrary, Map<String, RequestConstraint> metaData,
            String logDir) throws Exception {
        StringWriter sw = null;
        try {
            if (velocityEngine == null) {
                java.util.Properties p = new java.util.Properties();
                String scriptDir = globalScriptDir.getPath();
                p.setProperty("file.resource.loader.class",
                        ScriptTemplateLoader.class.getName());
                p.setProperty("file.resource.loader.path", scriptDir);
                p.setProperty("velocimacro.permissions.allowInline", "true");
                p.setProperty(
                        "velocimacro.permissions.allow.inline.to.replace.global",
                        "true");
                p.setProperty("runtime.log",
                        FileUtil.join(logDir, "velocity.log"));
                velocityEngine = new VelocityEngine();
                velocityEngine.init(p);
            }

            VelocityContext context = new VelocityContext();

            context.put("scriptMetadata", metaData);
            context.put("maxRecords", maxRecords);
            context.put("scriptLibrary", scriptLibrary);
            context.put("mode", mode);

            sw = new StringWriter();

            String path = scriptTemplate.getAbsolutePath();
            String key = path + File.pathSeparator + globalScriptDir;  //Win32
            Template template = templateMap.get(key);
            if (template == null) {
                template = velocityEngine.getTemplate(path,
                        Velocity.ENCODING_DEFAULT);
                templateMap.put(key, template);
            }

            template.merge(context, sw);

        } catch (Exception e) {
            throw new Exception("Error generating from template", e);
        }

        return sw.toString();
    }
}
