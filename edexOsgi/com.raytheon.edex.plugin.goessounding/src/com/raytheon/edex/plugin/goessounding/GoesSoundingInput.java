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
package com.raytheon.edex.plugin.goessounding;

import java.util.Properties;

import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.core.IDecoderInput;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class GoesSoundingInput implements IDecoderInput {

    private Properties properties;
    
    private static final long serialVersionUID = 1L;

    private WMOHeader wmoHeader;
    
    private byte [] documentData;

    @Override
    public String getProperty(String key) {
        String value = null;
        if(properties != null) {
            value = properties.getProperty(key);
        }
        return value;
    }

    /**
     * @return Always returns a null String reference.
     */
    @Override
    public String getReport() {
        return null;
    }

    /**
     * 
     * @param header
     */
    public void setWMOHeader(WMOHeader header) {
        wmoHeader = header;
    }
    
    /**
     * 
     * @return
     */
    @Override
    public WMOHeader getWmoHeader() {
        return wmoHeader;
    }

    /**
     * 
     */
    @Override
    public void setProperty(String key, String value) {
        if(properties == null) {
            properties = new Properties();
        }
        properties.put(key,value);
    }

    /**
     * 
     * @param data
     */
    public void setDocumentData(byte [] data) {
        documentData = data;
    }
    
    /**
     * 
     * @return
     */
    public byte [] getDocumentData() {
        return documentData;
    }
}
