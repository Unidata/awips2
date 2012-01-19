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
package com.raytheon.edex.plugin;


import com.raytheon.uf.common.dataplugin.PluginException;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 July 2007        411 jkorman     Initial coding.	
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */
public interface IDecodePluginProxy<T> {

//    /**
//     * 
//     * @param messageData
//     */
//    public void setMessageData(byte [] messageData);
//    
//    /**
//     * 
//     * @param messageFile
//     */
//    public void setMessageData(File messageFile);
    
    /**
     * Determine if there is more decoded data available.
     * @return Is there at least one more decoded record!
     */
    public boolean hasNext();
    
    /**
     * 
     * @param message
     * @return
     * @throws PluginException
     */
    public T next() throws PluginException;
    
    /**
     * Signal the decoder to dispose of any local resources it holds. After
     * a call to dispose, hasNext must return false and next() must throw
     * a "No data available" exception.
     */
    public void dispose();
    
}
