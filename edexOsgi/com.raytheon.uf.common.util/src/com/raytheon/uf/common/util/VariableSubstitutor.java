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
package com.raytheon.uf.common.util;

import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

/**
 * Handles basic variable substitutions in strings
 * 
 * Variables should be in ${variable} format with optional specification of
 * default value through ${variable;defaultVal}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class VariableSubstitutor {

    private VariableSubstitutor() {
        // no instantiation
    }

    /**
     * @param string
     * @param variables
     * @return
     * @throws VizException
     */
    public static String processVariables(String string,
            Map<String, String> variables) throws ParseException {
        if (variables == null) {
            variables = new HashMap<String, String>();
        }

        int offset = 0;
        int pos = string.indexOf("${", offset);
        StringBuilder substStr = new StringBuilder();
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
            String value = variables.get(var);
            if (value == null) {
                value = defaultVal;
                variables.put(var, value);
            }
            if (value != null) {
                substStr.append(value);
            } else {
                throw new ParseException("Undefined variable \"" + var + "\"",
                        offset);
            }
            offset = pos + 1;
            pos = string.indexOf("${", offset);
        }
        if (offset < string.length()) {
            substStr.append(string.substring(offset));
        }

        return substStr.toString();
    }
}
