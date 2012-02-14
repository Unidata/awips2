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
package com.raytheon.uf.edex.esb.camel;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Converts PluginDataObjects or arrays of PluginDataObjects into their dataURIs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 25, 2008            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ToDataURI {

    public String[] toDataURI(PluginDataObject[] pdo) {
        String[] strs = new String[pdo.length];
        for (int i = 0; i < strs.length; i++) {
            strs[i] = pdo[i].getDataURI();
        }

        return strs;
    }

    public String toDataURI(PluginDataObject pdo) {
        return pdo.getDataURI();
    }
}
