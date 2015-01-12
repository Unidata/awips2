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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 21, 2009            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BUFRUAAdapterFactory {

    Log logger = LogFactory.getLog(getClass());

    private PointDataDescription pdd;

    private PointDataPluginDao<UAObs> dao;

    private String pluginName;

    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     */
    public BUFRUAAdapterFactory(PointDataDescription pdd,
            PointDataPluginDao<UAObs> dao, String pluginName) {
        this.pdd = pdd;
        this.dao = dao;
        this.pluginName = pluginName;
    }

    /**
     * 
     * @return
     */
    public AbstractBUFRUAAdapter getAdapter(WMOHeader wmoHeader) {
        
        AbstractBUFRUAAdapter adapter = null;
        
        int ii = wmoHeader.getIi() % 10;;
        
        switch(ii) {
        case 1 :
        case 3 : {
            adapter = new BUFRUAManLevelAdapter(pdd, dao, pluginName);
            break;
        }
        case 2 :
        case 4 :
        case 6 :
        case 8 : {
            adapter = new BUFRUASigLevelAdapter(pdd, dao, pluginName);
            break;
        }
        default : {
            logger.error("Unknown WMOHeader info. Creating null adapter for " + wmoHeader.getWmoHeader());
            adapter = new BUFRUANullLevelAdapter(pdd, dao, pluginName);
        }
        } // switch
        return adapter;
    }
}
