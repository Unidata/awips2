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
 /**
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 1, 2011            bclement     Initial creation
 *
 **/

package com.raytheon.uf.edex.ogc.common.db;

import java.util.Arrays;
import java.util.Collection;
import java.util.Date;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Default Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   754       dhladky      initial creation, based on B Clements original
 * 04/01/2013   1746      dhladky      Updated for MADIS configuration
 * 05/27/2013   753       dhladky      Updated for wfs service
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class DefaultLayerCollector<DIMENSION extends SimpleDimension, L extends SimpleLayer<DIMENSION>, R extends PluginDataObject>
        extends LayerCollector<DIMENSION, L> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DefaultLayerCollector.class);

    protected Class<R> recordClass;

    protected Class<L> layerClass;

    public DefaultLayerCollector(LayerTransformer<DIMENSION, L> transformer,
            Class<L> layerClass, Class<R> recordClass) {
        super(transformer);
        this.recordClass = recordClass;
        this.layerClass = layerClass;
    }

    public void add(R... pdos) {
        if (pdos != null && pdos.length > 0) {
            addAll(Arrays.asList(pdos));
        }
    }

    protected abstract void addAll(Collection<R> coll);
    
    @Override
    protected L newLayer() {
        try {
            return layerClass.newInstance();
        } catch (Exception e) {
            statusHandler
                    .error("Unable to instantiate class: " + layerClass, e);
            throw new RuntimeException(e);
        }
    }

    protected void addToTimes(L layer, R rec) {
        Date refTime = rec.getDataTime().getRefTime();
        layer.getTimes().add(refTime);
    }

    protected void addToDims(L layer, R rec) {
        // default is to do nothing
    }

    protected abstract boolean initializeLayer(L layer, R rec);

    protected abstract String getLayerName(R rec);


}
