package gov.noaa.nws.ncep.viz.resources.util;

import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * VariableSubstitutorNCEP is a substitute for
 * com.raytheon.uf.common.util.VariableSubstitutor. Both classes have a single
 * method: processVariables. The Raytheon version makes the editing of GPARMS in
 * CAVE > Data > Manage Resources > Attributes > Edit cas sensitive. It isn't
 * supposed to be. Raytheon code can not be changed without permission. That
 * class could also not be extended as the constructor was set to private.
 * 
 * This class does the same thing, with the same function, but does not use the
 * HashMap.get() method. Instead it loops through the keys of HashMap looking
 * for a case insensitive match. See RedMine 4163
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2014            srussell     Initial creation.
 * 
 * </pre>
 * 
 * @author srussell
 * @version 1.0
 */

public class VariableSubstitutorNCEP {

    public static String processVariables(String string, Map<String, String> variables) throws VizException {
        if (variables == null) {
            variables = new HashMap<String, String>();
        }

        int offset = 0;
        int pos = string.indexOf("${", offset);
        StringBuilder substStr = new StringBuilder();

        try {

            while (pos >= 0) {
                substStr.append(string.substring(offset, pos));
                offset = pos + 2;
                pos = string.indexOf('}', offset);
                String var = string.substring(offset, pos);
                String defaultVal = null;
                int idx = var.indexOf(';');
                if (idx > 0 && idx < var.length() - 1) {
                    // There must be room for values for each side, can't be ";",
                    // "a;" or ";a", must be of form "a;b" where b is default value
                    defaultVal = var.substring(idx + 1);
                    var = var.substring(0, idx);
                }

                String value = null;

                // RedMine 4163 retrieve the value for "var" from the HashMap,
                // searching in a case insensitive manner
                for (Map.Entry<String, String> entry : variables.entrySet()) {
                    String key = entry.getKey();
                    String val = entry.getValue();

                    if (key.equalsIgnoreCase(var)) {
                        value = new String(val);
                        break;
                    }

                }

                if (value == null) {
                    value = defaultVal;
                    variables.put(var, value);
                }
                if (value != null) {
                    substStr.append(value);
                } else {
                    throw new ParseException("Undefined variable \"" + var + "\"", offset);
                }
                offset = pos + 1;
                pos = string.indexOf("${", offset);
            }
            if (offset < string.length()) {
                substStr.append(string.substring(offset));
            }
        }// end try
        catch (ParseException e) {
            throw new VizException("Error processing variable substitution", e);
        }

        return substStr.toString();
    }

}// end class VariableSubstitutorNCEP
