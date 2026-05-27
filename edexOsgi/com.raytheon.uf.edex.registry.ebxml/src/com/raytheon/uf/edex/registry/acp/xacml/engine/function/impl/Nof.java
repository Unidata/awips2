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
package com.raytheon.uf.edex.registry.acp.xacml.engine.function.impl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.function.XACMLFunction;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;


/**
 * 
 *
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class Nof extends XACMLFunction {

    private static final String FUNCTION_ID = "urn:oasis:names:tc:xacml:1.0:function:n-of";

    @Override
    public String getFunctionId() {
        return FUNCTION_ID;
    }

    protected boolean isVariableLengthArgs() {
        return true;
    }

    public Boolean executeFunction(Object[] args) throws XACMLException {
        if (args.length <= 1) {
            throw new IllegalArgumentException(
                    "Invalid number of arguments submitted to [" + FUNCTION_ID
                            + "]. Expected 2 or more. Received " + args.length);

        }

        int requiredTrueCount = 0;
        if (args[0] instanceof Integer) {
            requiredTrueCount = (Integer) args[0];
            if (requiredTrueCount == 0) {
                return true;
            }
        }

        if (requiredTrueCount > args.length - 1) {
            throw new XACMLProcessingException(
                    "Error processing ["
                            + FUNCTION_ID
                            + "].  Number of required true values is less than the number of provided values");
        }

        int trueCount = 0;
        for (int i = 1; i < args.length; i++) {
            int remaining = args.length - 1;
            if (!(args[i] instanceof Boolean)) {
                throw new IllegalArgumentException(
                        "Illegal arguments submitted to [" + FUNCTION_ID + "]");
            }
            if (((Boolean) args[i]).booleanValue()) {
                trueCount++;
            }

            if (trueCount >= requiredTrueCount) {
                return true;
            }

            if (remaining < (requiredTrueCount - trueCount)) {
                return false;
            }
        }

        return false;
    }
}
