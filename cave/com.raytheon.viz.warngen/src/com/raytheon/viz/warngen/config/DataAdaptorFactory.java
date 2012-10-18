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
package com.raytheon.viz.warngen.config;

import javax.measure.converter.UnitConverter;

import com.raytheon.uf.common.dataplugin.warning.config.PathcastConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration.PointType;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Creates data adaptors for PointSource data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2011            bgonzale     Initial creation
 * Oct 17, 2012            jsanchez     Allowed adaptor to be used for pathcast.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class DataAdaptorFactory {

    public static AbstractDbSourceDataAdaptor createDataAdaptor(
            PointSourceConfiguration pointConfig, Geometry searchArea,
            String localizedSite) throws VizException {
        AbstractDbSourceDataAdaptor adaptor = null;
        if (pointConfig.getType() == PointType.AREA) {
            adaptor = new DbAreaSourceDataAdaptor(pointConfig, searchArea,
                    localizedSite);
        } else if (pointConfig.getType() == PointType.POINT) {
            adaptor = new DbPointSourceDataAdaptor(pointConfig, searchArea,
                    localizedSite);
        }
        return adaptor;
    }

    public static AbstractDbSourceDataAdaptor createPathcastDataAdaptor(
            PathcastConfiguration pathcastConfiguration,
            UnitConverter distanceToMeters, Geometry searchArea,
            String localizedSite) throws VizException {
        AbstractDbSourceDataAdaptor adaptor = null;

        if (pathcastConfiguration.getType() == PointType.AREA) {
            adaptor = new DbAreaSourceDataAdaptor(pathcastConfiguration,
                    distanceToMeters, searchArea, localizedSite);
        } else if (pathcastConfiguration.getType() == PointType.POINT) {
            adaptor = new DbPointSourceDataAdaptor(pathcastConfiguration,
                    distanceToMeters, searchArea, localizedSite);
        }

        return adaptor;
    }
}
