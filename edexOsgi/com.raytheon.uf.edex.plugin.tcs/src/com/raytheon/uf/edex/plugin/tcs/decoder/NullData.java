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
package com.raytheon.uf.edex.plugin.tcs.decoder;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.common.dataplugin.tcs.dao.TropicalCycloneSummaryDao;
import com.raytheon.uf.common.pointdata.PointDataDescription;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2009            jsanchez     Initial creation
 *
 * </pre>
 *
 * @author jsanchez
 * @version 1.0 
 */
public class NullData extends TCSDataAdapter{
    
    public NullData(PointDataDescription pdd, TropicalCycloneSummaryDao dao, String pluginName) {
        super(pdd,dao,pluginName);
    }
    
    public List<TropicalCycloneSummary> findReports(byte [] message) {
        logger.info("Unknown data " + traceId);
        return new ArrayList<TropicalCycloneSummary>();
    }
}
