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
package com.raytheon.uf.viz.core.velocity;

import java.io.File;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.app.VelocityEngine;

import com.raytheon.edex.scriptfactory.ScriptTemplateLoader;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.status.StatusConstants;

/**
 * Manages the execution of velocity templates
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VelocityManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(VelocityManager.class);
    private static VelocityManager instance;

    private Map<String, VelocityEngine> engineMap;

    /** Cached templates */
    private final Map<String, Template> templateMap;

    private VelocityManager() {
        engineMap = new HashMap<String, VelocityEngine>();
        templateMap = new HashMap<String, Template>();
    }

    public static VelocityManager getInstance() {
        synchronized (VelocityManager.class) {
            if (instance == null) {
                instance = new VelocityManager();
            }
        }
        return instance;
    }

    /**
     * Create a script given the vm file and object map, uses default props
     * mimicked from ScriptFactory
     * 
     * @param vmFile
     *            the .vm file to use
     * @param objectMap
     *            the string to object map velocity should use
     * @return the formatted text
     */
    public String createScript(File vmFile, Map<String, Object> objectMap) {
        VelocityEngine engine = engineMap.get(vmFile.getAbsolutePath());
        if (engine == null) {
            java.util.Properties p = new java.util.Properties();
            String scriptDir = vmFile.getParentFile().getAbsolutePath();
            p.setProperty("file.resource.loader.class",
                    ScriptTemplateLoader.class.getName());
            p.setProperty("file.resource.loader.path", scriptDir);
            p.setProperty("velocimacro.permissions.allowInline", "true");
            p.setProperty(
                    "velocimacro.permissions.allow.inline.to.replace.global",
                    "true");
            return createScript(vmFile, objectMap, p);
        } else {
            return createScript(vmFile, objectMap, engine);
        }
    }

    /**
     * Create a script given the vm file and object map, and custom velocity
     * properties
     * 
     * @param vmFile
     * @param objectMap
     * @param customProps
     * @return the formatted text
     */
    public String createScript(File vmFile, Map<String, Object> objectMap,
            Properties customProps) {
        VelocityEngine engine = new VelocityEngine();
        try {
            engine.init(customProps);
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error while initializing velocity engine", e);
        }
        return createScript(vmFile, objectMap, engine);
    }

    private String createScript(File vmFile, Map<String, Object> objectMap,
            VelocityEngine engine) {
        StringWriter sw = null;
        try {
            VelocityContext context = new VelocityContext(objectMap);

            sw = new StringWriter();

            String path = vmFile.getAbsolutePath();
            String key = path + File.pathSeparator + vmFile.getParentFile().getAbsolutePath(); //Win32
            Template template = templateMap.get(key);
            if (template == null) {
                template = engine.getTemplate(path, Velocity.ENCODING_DEFAULT);
                templateMap.put(key, template);
            }

            template.merge(context, sw);
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error while generating velocity text", e);
        }
        engineMap.put(vmFile.getAbsolutePath(), engine);
        return sw.toString();
    }
}
