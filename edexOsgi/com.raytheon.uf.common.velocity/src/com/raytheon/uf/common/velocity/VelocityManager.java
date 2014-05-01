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
package com.raytheon.uf.common.velocity;

import java.io.File;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.app.VelocityEngine;

import com.raytheon.uf.common.util.FileUtil;

/**
 * Apache Velocity manager/executor class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2013 1638       mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VelocityManager {

    private static Map<Object, VelocityEngine> engines = new HashMap<Object, VelocityEngine>();

    /**
     * Execute the velocity template designated by vmFile
     * 
     * @param vmFile
     * @param logDir
     * @param templateObjects
     * @return
     */
    public static String executeTemplate(File vmFile, File logDir,
            Map<String, Object> templateObjects) {
        return executeTemplate(vmFile, vmFile.getParentFile(), logDir,
                templateObjects);
    }

    /**
     * Execute the velocity template designated by vmFile
     * 
     * @param vmFile
     * @param globalTemplateDir
     * @param logDir
     * @param templateObjects
     * @return
     */
    public static String executeTemplate(File vmFile, File globalTemplateDir,
            File logDir, Map<String, Object> templateObjects) {
        Object key = vmFile.getAbsolutePath() + "::"
                + globalTemplateDir.getAbsolutePath();
        VelocityEngine engine = null;
        synchronized (engines) {
            engine = engines.get(key);
            if (engine == null) {
                engine = new VelocityEngine();
                Properties p = new Properties();
                p.setProperty("file.resource.loader.class",
                        VelocityTemplateLoader.class.getName());
                p.setProperty("file.resource.loader.path",
                        globalTemplateDir.getAbsolutePath());
                p.setProperty("velocimacro.permissions.allowInline", "true");
                p.setProperty(
                        "velocimacro.permissions.allow.inline.to.replace.global",
                        "true");
                p.setProperty("runtime.log",
                        FileUtil.join(logDir.getAbsolutePath(), "velocity.log"));
                engine.init(p);
                engines.put(key, engine);
            }
        }
        return executeTemplate(vmFile, templateObjects, engine);
    }

    /**
     * Execute the velocity template designated by vmFile
     * 
     * @param vmFile
     * @param templateObjects
     * @param velocityProperties
     * @return
     */
    public static String executeTemplate(File vmFile,
            Map<String, Object> templateObjects, Properties velocityProperties) {
        VelocityEngine engine = new VelocityEngine();
        engine.init(velocityProperties);
        return executeTemplate(vmFile, templateObjects, engine);
    }

    private static String executeTemplate(File vmFile,
            Map<String, Object> templateObjects, VelocityEngine engine) {
        VelocityContext context = new VelocityContext(templateObjects);
        StringWriter sw = new StringWriter();

        Template t = engine.getTemplate(vmFile.getAbsolutePath(),
                Velocity.ENCODING_DEFAULT);
        t.merge(context, sw);
        return sw.toString();
    }

}
