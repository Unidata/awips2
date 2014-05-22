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
package com.raytheon.uf.common.dataplugin.annotations;

/**
 * Interface classes can register for converting {@link DataURI} annotated
 * fields to and from {@link String}s
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2013  2081      mschenke    Initial creation
 * May 06, 2014  2060      njensen     Added generics
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface DataURIFieldConverter<T> {

    /**
     * Converts field object to a String for the DataURI
     * 
     * @param field
     * @return
     */
    public String toString(T field);

    /**
     * Converts String returned from {@link #toString()} back into a field
     * object
     * 
     * @param string
     * @return
     */
    public T fromString(String string);

}
