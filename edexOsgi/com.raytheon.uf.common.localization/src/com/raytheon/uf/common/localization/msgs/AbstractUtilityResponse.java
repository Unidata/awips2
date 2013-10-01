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

package com.raytheon.uf.common.localization.msgs;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines an abstract response message
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2007            chammack    Initial Creation.	
 * May 19, 2007  #1127     randerso    Implemented error reporting
 * Aug 22, 2008  #1502     bclement    Added JAXB/Serializable annotations
 * Oct 01, 2013   2361     njensen     Removed XML annotations
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@DynamicSerialize
public abstract class AbstractUtilityResponse {

    @DynamicSerializeElement
    protected LocalizationContext context;

    @DynamicSerializeElement
    protected String pathName;

    /**
     * Error message, null = no errors occurred
     */
    @DynamicSerializeElement
    protected String errorText;

    public AbstractUtilityResponse() {
    }

    public AbstractUtilityResponse(LocalizationContext context,
            String pathName, String errorText) {
        this.context = context;
        this.pathName = pathName;
        this.errorText = errorText;
    }

    /**
     * @return the context
     */
    public LocalizationContext getContext() {

        return context;
    }

    /**
     * @param context
     *            the context to set
     */
    public void setContext(LocalizationContext context) {
        this.context = context;
    }

    public abstract String getFormattedErrorMessage();

    public String getErrorText() {
        return errorText;
    }

    public void setErrorText(String errorText) {
        this.errorText = errorText;
    }

    public String getPathName() {
        return pathName;
    }

    public void setPathName(String pathName) {
        this.pathName = pathName;
    }

    public boolean successful() {
        return errorText == null;
    }

    protected String getContextRelativePath() {
        return context + "/" + pathName;
    }
}
