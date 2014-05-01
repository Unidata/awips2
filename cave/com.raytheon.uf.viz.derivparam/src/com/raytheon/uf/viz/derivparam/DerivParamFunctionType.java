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
package com.raytheon.uf.viz.derivparam;

/**
 * Class that defines a derived parameter function
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DerivParamFunctionType {

    public static class FunctionArgument {
        /** The name of the argument */
        public String name;

        /**
         * The type of the argument, will be type returned from
         * getArgumentTypes() on the adapter
         */
        public String type;
    }

    /** Name of the type (python, openGL shader, CUDA, etc) */
    private String name;

    /** File extension for the function type */
    private String extension;

    private IDerivParamFunctionAdapter adapter;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getExtension() {
        return extension;
    }

    public void setExtension(String extension) {
        this.extension = extension;
    }

    public IDerivParamFunctionAdapter getAdapter() {
        return adapter;
    }

    public void setAdapter(IDerivParamFunctionAdapter adapter) {
        this.adapter = adapter;
    }

}
