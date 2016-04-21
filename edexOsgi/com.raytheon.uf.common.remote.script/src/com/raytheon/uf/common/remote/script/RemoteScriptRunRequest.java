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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.text.StrMatcher;
import org.apache.commons.lang.text.StrTokenizer;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * This class is a request to run a remote script from desired localization
 * directory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2014 2743       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@DynamicSerialize
public class RemoteScriptRunRequest extends RemoteScriptRequest {
    @DynamicSerializeElement
    String script;

    @DynamicSerializeElement
    LocalizationContext context;

    /**
     * Mapping of resource properties to override. See
     * {@link RemoteScriptConstants}.
     */
    @DynamicSerializeElement
    private Map<String, String> propertyMap = new HashMap<String, String>();

    /**
     * Command line arguments for the remote script.
     */
    @DynamicSerializeElement
    private List<String> scriptArguments = new ArrayList<String>();

    /**
     * Default constructor.
     */
    public RemoteScriptRunRequest() {
    }

    /**
     * Constructor.
     * 
     * @param userId
     *            - user to run the script
     * @param filename
     *            - script's filename located in the script directory. See
     *            {@link RemoteScriptConstants}.
     * @param context
     *            - The localize directory's context that contains the script.
     *            See {@link RemoteScriptListResponse#get(String)}
     */
    public RemoteScriptRunRequest(String userId, String script,
            LocalizationContext context) {
        this();
        setUserId(userId);
        setScript(script);
        setContext(context);
    }

    /**
     * Get the context associated with the script's filename. See
     * {@link RemoteScriptListResponse#get(String)}.
     * 
     * @return context
     */
    public LocalizationContext getContext() {
        return context;
    }

    /**
     * Set the context associated with the script's filename. See
     * {@link RemoteScriptListResponse#get(String)}.
     * 
     * @param context
     */
    public void setContext(LocalizationContext context) {
        this.context = context;
    }

    /**
     * Getter for script property map. Recommend this only be used for
     * serialization, see {@link RemoteScriptConstants}.
     * 
     * @return propertyMap
     */
    public Map<String, String> getPropertyMap() {
        return propertyMap;
    }

    /**
     * Setter for script property map. This is used to override the script
     * property values for handling the script. Recommend this only be used for
     * serialization, see {@link RemoteScriptConstants}.
     * 
     * @param propertyMap
     *            - when null map cleared so default values will be used
     */
    public void setPropertyMap(Map<String, String> propertyMap) {
        this.propertyMap.clear();
        if (propertyMap != null) {
            this.propertyMap.putAll(propertyMap);
        }
    }

    /**
     * Override the default resource property, see {@link RemoteScriptConstants}
     * .
     * 
     * @param key
     * @param value
     * @return oldValue - previous value for key
     */
    public String putProperty(String key, String value) {
        return propertyMap.put(key, value);
    }

    /**
     * Remove property.
     * 
     * @param key
     *            - property to remove
     * @return value - Value removed or null if none
     */
    public String removeProperty(String key) {
        return propertyMap.remove(key);
    }

    /**
     * Get list of command line arguments for the script in the order sent to
     * the process builder. See {@link java.lang.ProcessBuilder#command(List)}.
     * Note the full path name to the script to run will be added by the handler
     * when sending the request.
     * 
     * @return scriptArguments
     */
    public List<String> getScriptArguments() {
        return scriptArguments;
    }

    public String getScript() {
        return script;
    }

    public void setScript(String script) {
        this.script = script;
    }

    /**
     * Set the list of command line arguments for the script in the order they
     * are to be sent to the process builder. See
     * {@link java.lang.ProcessBuilder#command(List)}. Note the full path name
     * to the script to run will be added by the handler when sending the
     * request.
     * 
     * @param scriptArguments
     */
    public void setScriptArguments(List<String> scriptArguments) {
        this.scriptArguments.clear();
        if (scriptArguments != null) {
            this.scriptArguments.addAll(scriptArguments);
        }
    }

    /**
     * Append an argument to the script's argument list.
     * 
     * @param argument
     * @return success - true when added to the list
     */
    public boolean addScriptArgument(String argument) {
        return scriptArguments.add(argument);
    }

    /**
     * This clears the script argument list and breaks arguments into a list
     * which becomes the new script argument list. The arguments is split into
     * tokens delimited by whitespace. A token may be surrounded by single or
     * double quotes. A quote may be escaped within a quoted section by
     * duplicating itself.
     * 
     * <pre>
     * Examples of lists from java strings:
     * "a b c"                  - Three arguments [a, b, c]
     * "1 'a b c'd 3"           - Three arguments [1, a b cd, 3]
     * "1 \"a b c\"d 3"         - Three arguments [1, a b cd, 3]
     * "1 'a b ''c'''d 3"       - Three arguments [1, a b 'c'd, 3]
     * "1 \"a b \"\"c\"\"\"d 3" - Three arguments [1, a b "c"d, 3]
     * </pre>
     */
    public void parseAndSetScriptArguments(String arguments) {
        if (arguments == null || (arguments.trim().length() == 0)) {
            clearScriptArguements();
            return;
        }
        String[] args = (new StrTokenizer(arguments, StrMatcher.spaceMatcher(),
                StrMatcher.quoteMatcher())).getTokenArray();
        setScriptArguments(Arrays.asList(args));
    }

    /**
     * Remove all arguments from the script's argument list.
     */
    public void clearScriptArguements() {
        scriptArguments.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(getClass().getName());
        sb.append("[ userId: ").append(getUserId());
        sb.append(", script: ").append(getScript());
        sb.append(", context: ").append(getContext());
        sb.append(", propertyMap: ").append(propertyMap);
        sb.append(", scriptArguments: ").append(getScriptArguments());
        sb.append("]");
        return sb.toString();
    }
}
