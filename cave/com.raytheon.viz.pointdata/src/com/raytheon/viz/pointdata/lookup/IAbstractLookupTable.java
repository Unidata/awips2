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
package com.raytheon.viz.pointdata.lookup;

/**
 * Interface for SVG plot lookup tables
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                      ???         Initial creation
 * 
 * </pre>
 * 
 * @version 1.0
 */
public interface IAbstractLookupTable {

    /**
     * lookup the key from the table, numerical values should be converted to a
     * string before lookup.
     * 
     * @param key
     * @return
     */
    public abstract String lookup(String key);

    /**
     * set the mode of the table ( the plotMode attribute )
     * 
     * @param mode
     */
    public abstract void setMode(String mode);
}
