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
package com.raytheon.uf.edex.plugin.bufrsigwx.decoder;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.bufrsigwx.SigWxData;
import com.raytheon.uf.common.dataplugin.bufrsigwx.common.SigWxType;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0 
 */

public class SigWxNullData extends SigWxDataAdapter {
    private Log logger = LogFactory.getLog(getClass());

    SigWxType wxType;
    
    /**
     * 
     * @param container
     */
    public SigWxNullData(PointDataDescription pdd, PointDataPluginDao<SigWxData> dao, String pluginName) {
        super(pdd,dao,pluginName);
    }

    /**
     * 
     * @param pointData
     * @param locPoint
     * @param dataPoint
     * @param index
     */
    List<SigWxData> getSigWxData(SigWxData sigWx, List<IBUFRDataPacket> dataList) {
        
        logger.info("Unknown data " + sigWx.getTraceId());
        
        return new ArrayList<SigWxData>();
    }

    /**
     * @see com.raytheon.uf.edex.plugin.bufrsigwx.decoder.SigWxDataAdapter#getType()
     */
    @Override
    SigWxType getType() {
        return wxType;
    }
    
    /**
     * @see com.raytheon.uf.edex.plugin.bufrsigwx.decoder.SigWxDataAdapter#getType()
     */
    @Override
    void setType(SigWxType type) {
        wxType = type;
    }
    
}
