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
package com.raytheon.viz.avnconfig;

/**
 * A class containing available method argument data. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class MethodArgData
{
    /**
     * Argument name.
     */
    private String argName = "";
    
    /**
     * Argument value.
     */
    private String argValue = "";
    
    /**
     * Constructor.
     */
    public MethodArgData()
    {        
    }
    
    /**
     * Constructor.
     * @param argName Argument name.
     * @param argValue Argument value.
     */
    public MethodArgData(String argName, String argValue)
    {
        this.argName = argName;
        this.argValue = argValue;
    }

    /**
     * Get the argument name.
     * @return The argument name.
     */
    public String getArgName()
    {
        return argName;
    }

    /**
     * Set the argument name.
     * @param argName The argument name.
     */
    public void setArgName(String argName)
    {
        this.argName = argName;
    }

    /**
     * Get the argument value.
     * @return The argument value.
     */
    public String getArgValue()
    {
        return argValue;
    }

    /**
     * Set the argument value.
     * @param argValue The argument value.
     */
    public void setArgValue(String argValue)
    {
        this.argValue = argValue;
    }     
}
