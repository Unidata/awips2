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
package com.raytheon.uf.viz.core;

import java.text.ParseException;
import java.util.Map;

import com.raytheon.uf.common.util.VariableSubstitutor;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Handles basic variable substitutions in strings
 * 
 * Variables should be in ${variable} format.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2009            chammack    Split from Bundle class.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class VariableSubstitutionUtil {

    private VariableSubstitutionUtil() {
        // no instantiation
    }

    /**
     * @param string
     * @param variables
     * @return
     * @throws VizException
     */
    public static String processVariables(String string,
            Map<String, String> variables) throws VizException {
        try {
            return VariableSubstitutor.processVariables(string, variables);
        } catch (ParseException e) {
            throw new VizException("Error processing variable substitution", e);
        }
    }
}
