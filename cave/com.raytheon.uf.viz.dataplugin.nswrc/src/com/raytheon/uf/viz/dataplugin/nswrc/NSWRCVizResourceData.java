/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.viz.dataplugin.nswrc;

import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCRadialRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Provides a NetCDF radar implementation for data types that are request
 * able from the EDEX backend and utilize PluginDataObjects.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class NSWRCVizResourceData extends AbstractRequestableResourceData {

    protected HashMap<DataTime, NSWRCRadialRecord> recordMap = new HashMap<DataTime, NSWRCRadialRecord>();

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        if (updateData instanceof PluginDataObject[]) {
            PluginDataObject[] recs = (PluginDataObject[]) updateData;
            for ( PluginDataObject pdo : recs ) {
                NSWRCRadialRecord rec = (NSWRCRadialRecord) pdo;
                recordMap.put(rec.getDataTime(), rec);
            }
        }
        super.update(updateData);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#equals(java.lang.Object
     * )
     */
    @Override
    public boolean equals(Object obj) {
    	return super.equals(obj);
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.rsc.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        AbstractVizResource<?, ?> resource = new NSWRCVizResource(this,
                loadProperties);

        for (PluginDataObject pdo : objects) {
            if (pdo instanceof NSWRCRadialRecord) {
                recordMap.put(pdo.getDataTime(), (NSWRCRadialRecord) pdo);
            }
        }

        return resource;
    }

    /**
     *
     * @param dataTime
     * @return
     */
    public NSWRCRadialRecord getRecord(DataTime dataTime) {
        return recordMap.get(dataTime);
    }

    public void remove(DataTime dataTime) {
        recordMap.remove(dataTime);
    }

}
