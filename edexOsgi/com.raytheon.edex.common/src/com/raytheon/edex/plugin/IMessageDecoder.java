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

import java.io.File;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.core.props.Properties;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/14/06                garmendariz Initial check-in 
 * 2/14/07		139		   Phillippe   Eliminated next method and replaced with decode
 * 10 Aug 2007         379 jkorman     Added disposal behavior
 * 20080408           1039 jkorman     Added traceId for tracing data.  
 * </pre>
 * 
 * @author garmendariz
 * @version 1.0
 */
public interface IMessageDecoder {

    /**
     * Set the message received from the ESB
     * 
     * @param msgAsBytes
     *            The message as an array of bytes
     */
    public void setMessage(byte[] messageData);

    /**
     * Set the message received from the ESB
     * 
     * @param msgAsBytes
     *            The message as an aray of bytes
     */
    public void setMessage(File messageFile);

    /**
     * Does the decoder have any more decoded records available?
     * 
     * @return
     */
    public boolean hasNext();

    /**
     * Decodes a data record
     * 
     * @return The data record containing decoded fields
     * @throws DecoderException
     */
    public PluginDataObject decode() throws DecoderException;

    /**
     * Instruct the decoder to perform any post processing behavior.
     */
    public void dispose();

    /**
     * Set a trace identifier for the source data.
     * 
     * @param traceId
     *            A unique identifier associated with the input data.
     */
    public void setTraceId(String traceId);

    /**
     * Sets the plugin name
     * 
     * @param pluginName
     *            the name of the plugin
     */
    public void setPluginName(String pluginName);

    /**
     * Sets the plugin properties
     * 
     * @param properties
     *            the plugin's properties
     */
    public void setProperties(Properties properties);
}