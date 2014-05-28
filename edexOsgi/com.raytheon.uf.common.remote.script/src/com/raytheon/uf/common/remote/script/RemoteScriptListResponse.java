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
package com.raytheon.uf.common.remote.script;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * This contains Localization Context map that is the result of a remote script
 * list request. The keys are sorted list of the script file names.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2014 2742       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@DynamicSerialize
public class RemoteScriptListResponse {
    /**
     * Map of contexts with sorted list of scripts as keys.
     */
    @DynamicSerializeElement
    private Map<String, List<LocalizationContext>> scripts = new TreeMap<String, List<LocalizationContext>>(
            String.CASE_INSENSITIVE_ORDER);

    /**
     * Default Constructor.
     */
    public RemoteScriptListResponse() {
    }

    /**
     * Constructor.
     * 
     * @param scripts
     */
    public RemoteScriptListResponse(
            Map<String, List<LocalizationContext>> scripts) {
        setScripts(scripts);
    }

    /**
     * Getter map of scripts' context lists the script keys are sorted.
     * 
     * @return scripts
     */
    public Map<String, List<LocalizationContext>> getScripts() {
        return scripts;
    }

    /**
     * Setter.
     * 
     * @param scripts
     *            - when null scripts are cleared.
     */
    public void setScripts(Map<String, List<LocalizationContext>> scripts) {
        this.scripts.clear();
        if (scripts != null) {
            this.scripts.putAll(scripts);
        }
    }

    /**
     * Convince method to a add file's script name and context to scripts.
     * 
     * @param lFile
     * @return true if not already in the set
     */
    public boolean add(LocalizationFile lFile) {
        String script = getName(lFile);
        LocalizationContext context = lFile.getContext();

        List<LocalizationContext> contexts = scripts.get(script);
        if (contexts == null) {
            contexts = new ArrayList<LocalizationContext>();
            scripts.put(script, contexts);
        }

        if (!contexts.contains(context)) {
            contexts.add(context);
            return true;
        }
        return false;
    }

    /**
     * Convince method to remove the file's context from scripts.
     * 
     * @param true - if file's script and context was in scripts.
     */
    public boolean remove(LocalizationFile lFile) {
        String name = getName(lFile);
        List<LocalizationContext> contexts = scripts.get(name);
        if (contexts == null) {
            return false;
        }

        if (contexts.remove(contexts)) {
            if (contexts.size() == 0) {
                scripts.remove(name);
            }
            return true;
        }

        return false;
    }

    /**
     * Get the script name of the localized file.
     * 
     * @param lFile
     * @return name
     */
    private String getName(LocalizationFile lFile) {
        String name = lFile.getName().trim();
        return name.substring(name.lastIndexOf(IPathManager.SEPARATOR) + 1);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return String.format("RemoteScriptListResponse: {scripts: %s}",
                getScripts());
    }
}
