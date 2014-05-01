package com.raytheon.uf.edex.plugin.obs.ogc.metar;

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

import java.util.Calendar;

import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.edex.ogc.common.db.DefaultPointDataDimension;
import com.raytheon.uf.edex.ogc.common.db.ILayerStore;
import com.raytheon.uf.edex.ogc.common.db.SingleLayerCollector;

/**
 * 
 * Metar Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   753       dhladky      Modified from a class written by Brian Clements
 * 04/01/2013   1746      dhladky      Updated for MADIS impl.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MetarLayerCollector
        extends
        SingleLayerCollector<DefaultPointDataDimension, MetarLayer, MetarRecord> {

    private static final String METAR_LAYER_NAME = "metar";

    public MetarLayerCollector(ILayerStore store) {
        super(MetarLayer.class, MetarRecord.class, METAR_LAYER_NAME, store);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.SingleLayerCollector#getTime(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected Calendar getTime(MetarRecord record) {
        return record.getTimeObs();
    }

}
