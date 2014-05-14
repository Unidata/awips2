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
package com.raytheon.uf.edex.decodertools.core;

import java.util.HashMap;

import com.raytheon.uf.common.wmo.WMOHeader;



/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class DecoderInput implements IDecoderInput {
    
    private static final long serialVersionUID = 1L;

    private HashMap<String,String> properties = new HashMap<String,String>();

    private final String report;
    
    private final WMOHeader wmoHeader;
    
    /**
     * 
     * @param header
     * @param rpt
     */
    public DecoderInput(WMOHeader header, String rpt) {
        report = rpt;
        wmoHeader = header;
    }
    
    /**
     * @see com.raytheon.uf.edex.decodertools.core.IDecoderInput#getProperty(java.lang.String)
     */
    @Override
    public String getProperty(String key) {
        return properties.get(key);
    }

    /**
     * @param key
     * @param value
     */
    @Override
    public void setProperty(String key, String value) {
        properties.put(key,value);
    }
    
    /**
     * @see com.raytheon.uf.edex.decodertools.core.IDecoderInput#getReport()
     */
    @Override
    public String getReport() {
        return report;
    }

    /**
     * @see com.raytheon.uf.edex.decodertools.core.IDecoderInput#getWmoHeader()
     */
    @Override
    public WMOHeader getWmoHeader() {
        return wmoHeader;
    }

}
