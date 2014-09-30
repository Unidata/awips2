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
package com.raytheon.edex.uengine.runners;

import com.raytheon.edex.uengine.exception.MicroEngineException;

/**
 * A factory class that creates and returns &mu;Engine script runners. This
 * class does not performing any logging.
 * <P>
 * Basic usage:
 * 
 * <pre>
 * <code>
 *    IMicroEngine runner = MicroEngine.getInstance(type);
 * </code>
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 17Nov2008    1709       MW Fegan    Initial creation.
 * Jul 10, 2014 2914       garmendariz Remove EnvProperties
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 * @see com.raytheon.edex.uengine.runners.AMicroEngine
 * @see com.raytheon.edex.uengine.runners.IMicroEngine
 */

public final class MicroEngine {

    /**
     * Constructor. This is private to prevent accidental instantiation
     */
    private MicroEngine() {
    }

    /**
     * Creates a &mu;Engine script runner of the specified type.
     * 
     * @param type
     *            the type of script runner to create
     * 
     * @return the script runner
     * 
     * @throws MicroEngineException
     *             if unable to create the script runner
     */
    public static IMicroEngine getInstance(String type)
            throws MicroEngineException {
        String className = null;
        Class<?> aClass = null;
        IMicroEngine retVal = null;
        String runner = type.toLowerCase() + "_runner";

        /* get the class name for the uEngine script runner */
        className = System.getProperty(runner);
        if (className == null) {
            throw new MicroEngineException(
                    "Unable to find class name for MicroEngine, type = " + type);
        }

        /* attempt to create the script runner class */
        try {
            aClass = Class.forName(className);
            retVal = (IMicroEngine) aClass.newInstance();
        } catch (Exception e) {
            throw new MicroEngineException(
                    "Unable to create MicroEngine script runner, type = "
                            + type, e);
        }
        return retVal;
    }
}
