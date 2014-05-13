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
package com.raytheon.edex.plugin.bufrua.decoder;

import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
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
 * Jul 21, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class BUFRUANullLevelAdapter extends AbstractBUFRUAAdapter {
    
    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     */
    public BUFRUANullLevelAdapter(PointDataDescription pdd, PointDataPluginDao<UAObs> dao, String pluginName) {
        super(pdd,dao,pluginName);
    }

    /**
     * 
     */
    @Override
    public UAObs createData(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {
        // Make sure we consume an item from the iterator. 
        iterator.next();
        return null;
    }

    /**
     * Dummy implementation. Not used.
     * 
     */
    @Override
    UAObs getSpecificData(UAObs obsData, PointDataView view,
            List<IBUFRDataPacket> dataList) {
        return null;
    }

}
