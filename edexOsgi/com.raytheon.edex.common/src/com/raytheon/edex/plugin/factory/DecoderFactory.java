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

package com.raytheon.edex.plugin.factory;

import com.raytheon.edex.plugin.IMessageDecoder;
import com.raytheon.uf.common.dataplugin.PluginException;

/**
 * Retrieves data type specific decoders.<br>
 * This factory will instantiate a decoder based on the fully qualified class
 * name provided in the <Decoder> tag in the plugin.xml for the specified plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/24/07		353			bphillip	Initial check-in
 * 20071108            423  jkorman     Add synchronization to getInstance and get.
 *                                      Made constructor private.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class DecoderFactory extends AbstractFactory {

    private static DecoderFactory instance;

    /**
     * Gets the singleton instance of the DecoderFactory
     * 
     * @return The singleton instance of the DecoderFactory
     * @throws PluginException
     *             If the DecoderFactory cannot be instantiated
     */
    public static synchronized DecoderFactory getInstance()
            throws PluginException {
        if (instance == null) {
            instance = new DecoderFactory();
        }
        return instance;
    }

    /**
     * Creates a new instance of the Decoder Factory
     * 
     * @throws PluginException
     *             If environment properties cannot be loaded or proxy settings
     *             cannot be set
     */
    private DecoderFactory() throws PluginException {
        super(FactoryType.DECODER);
    }

    public synchronized IMessageDecoder get(String pluginName)
            throws PluginException {
        // check for valid parameters
        if ((pluginName == null) || pluginName.equals("")) {
            throw new PluginException("Invalid arguments to factory getter: ",
                    new IllegalArgumentException("Plugin name = " + pluginName));
        }

        IMessageDecoder decoder = null;
        decoder = (IMessageDecoder) getInstance(pluginName);
        return decoder;
    }

}
