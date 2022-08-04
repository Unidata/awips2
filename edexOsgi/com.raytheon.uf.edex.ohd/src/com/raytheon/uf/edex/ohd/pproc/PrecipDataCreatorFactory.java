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
package com.raytheon.uf.edex.ohd.pproc;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.edex.ohd.satpre.SatPrecipDataCreator;

/**
 * Used to identify and retrieve the {@link IPrecipDataCreator} based on the
 * {@link PluginDataObject} that is to be processed.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class PrecipDataCreatorFactory {

    private final Map<Class<? extends PluginDataObject>, IPrecipDataCreator<? extends PluginDataObject>> creatorMap = new HashMap<>();

    public PrecipDataCreatorFactory() throws PrecipCreationException {
        creatorMap.put(GridRecord.class, new GridPrecipDataCreator());
        creatorMap.put(SatelliteRecord.class, new SatPrecipDataCreator());
    }

    public IPrecipDataCreator<? extends PluginDataObject> lookupCreator(
            final Class<? extends PluginDataObject> pdoClass) {
        return creatorMap.get(pdoClass);
    }
}