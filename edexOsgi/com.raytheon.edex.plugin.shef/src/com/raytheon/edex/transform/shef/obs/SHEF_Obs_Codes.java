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
package com.raytheon.edex.transform.shef.obs;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * This interface provides a facade through which various clients may encode
 * shef data from various sources. The implementor should then write a specific
 * formatter method called from format that casts the PluginDataObject to the
 * concrete report class to be encoded.
 * 
 * <pre>
 * <code>
 * public StringBuilder format(StringBuilder buffer, PluginDataObject pdo) {
 *     return internalFormat(buffer, (MyRecord) pdo);
 * }
 * 
 * private StringBuilder(StringBuilder buffer, MyRecord pdo) {
 *     // encoding specific to MyRecord.
 *     return buffer;
 * }
 * </code>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 20, 2008       1659 jkorman     Initial creation
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface SHEF_Obs_Codes<T extends PluginDataObject> {

    /**
     * Format to an encoded shef using data within the supplied
     * PluginDataObject.
     * 
     * @param buffer
     *            StringBuilder to receive the encoded data.
     * @param value
     *            The PluginDataObject containing the data to be encoded.
     * @return The StringBuilder with the encoded data.
     */
    StringBuilder format(StringBuilder buffer, T pdo, String reportText, ObsToSHEFOptions options);
    
}
