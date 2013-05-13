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
package com.raytheon.viz.radar.rsc;

import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.interrogators.RadarDMDInterrogator;
import com.raytheon.viz.radar.interrogators.RadarGFMInterrogator;
import com.raytheon.viz.radar.interrogators.RadarDefaultInterrogator;
import com.raytheon.viz.radar.interrogators.RadarEETInterrogator;
import com.raytheon.viz.radar.interrogators.RadarGraphicInterrogator;
import com.raytheon.viz.radar.interrogators.RadarPrecipInterrogator;
import com.raytheon.viz.radar.interrogators.RadarRadialInterrogator;
import com.raytheon.viz.radar.interrogators.RadarRasterInterrogator;
import com.raytheon.viz.radar.interrogators.RadarVelocityInterrogator;
import com.raytheon.viz.radar.interrogators.RadarXsectInterrogator;
import com.raytheon.viz.radar.rsc.graphic.RadarGraphicsResource;
import com.raytheon.viz.radar.rsc.graphic.RadarMLResource;
import com.raytheon.viz.radar.rsc.image.RadarRadialResource;
import com.raytheon.viz.radar.rsc.image.RadarRasterResource;
import com.raytheon.viz.radar.rsc.image.RadarSRMResource;
import com.raytheon.viz.radar.ui.xy.RadarGSMResource;
import com.raytheon.viz.radar.ui.xy.RadarGraphResource;
import com.raytheon.viz.radar.ui.xy.RadarXYResource;
import com.raytheon.viz.radar.ui.xy.RadarXsectXYResource;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2010            mnash     Initial creation
 * 03/04/2013   DCS51     zwang     Handle GFM product
 * 05/02/2013   DR 14587  D. Friedman  Add isVelocityProductCode
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarProductFactory {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarProductFactory.class);

    private static final List<Integer> velocities = Arrays.asList(183, 182,
            154, 99, 56, 55, 27, 26, 25, 24, 23, 22);

    private static final List<Integer> precips = Arrays.asList(138, 80, 79, 78);

    private static final List<Integer> xSects = Arrays.asList(50, 51, 85, 86);

    public static IRadarInterrogator buildInterrogator(int productCode,
            String format) {
        IRadarInterrogator interrogator = null;
        if (velocities.contains(productCode)) {
            interrogator = new RadarVelocityInterrogator();
        } else if (xSects.contains(productCode)) {
            interrogator = new RadarXsectInterrogator();
        } else if (precips.contains(productCode)) {
            interrogator = new RadarPrecipInterrogator();
        } else if (productCode == 135) {
            interrogator = new RadarEETInterrogator();
        } else if ("Radial".equals(format)) {
            interrogator = new RadarRadialInterrogator();
        } else if ("Raster".equals(format)) {
            interrogator = new RadarRasterInterrogator();
        } else if ("Graphic".equals(format)) {
            if (productCode == 149) {
                interrogator = new RadarDMDInterrogator();
            } 
            else if (productCode == 140) {
                interrogator = new RadarGFMInterrogator();
            } else {
                interrogator = new RadarGraphicInterrogator();
            }
        } else {
            interrogator = new RadarDefaultInterrogator();
        }
        return interrogator;
    }

    public static AbstractRadarResource<?> buildResource(RadarResourceData rrd,
            LoadProperties loadProps, IRadarInterrogator interrogator,
            int productCode, String format) throws VizException {
        AbstractRadarResource<?> resource = null;
        if ("XY".equals(format)) {
            if (productCode == 2) {
                resource = new RadarGSMResource(rrd, loadProps, interrogator);
            } else if (xSects.contains(productCode)) {
                resource = new RadarXsectXYResource(rrd, loadProps,
                        interrogator);
            } else {
                resource = new RadarXYResource(rrd, loadProps, interrogator);
            }
        } else if ("Graph".equals(format)) {
            resource = new RadarGraphResource(rrd, loadProps);
        } else if ("Raster".equals(format)) {
            if (rrd.mode.equals("CZ-Pg")) {
                // Don't use a text contributer
                resource = new RadarGraphicsResource(rrd, loadProps, null);
            } else {
                resource = new RadarRasterResource(rrd, loadProps, interrogator);
            }
        } else if ("Radial".equals(format)) {
            if (rrd.mode.startsWith("SRM")) {
                resource = new RadarSRMResource(rrd, loadProps, interrogator);
            } else {
                resource = new RadarRadialResource(rrd, loadProps, interrogator);
            }
        } else if ("Graphic".equals(format)) {
            if (productCode == 166) {
                resource = new RadarMLResource(rrd, loadProps, interrogator);
            } else {
                resource = new RadarGraphicsResource(rrd, loadProps,
                        interrogator);
            }
        } else if ("Text".equals(format)) {
            statusHandler.handle(Priority.EVENTA, format + " product #"
                    + productCode);
        } else {
            resource = new AbstractRadarResource<MapDescriptor>(rrd, loadProps,
                    interrogator);
        }
        return resource;
    }

    public static boolean isVelocityProductCode(int productCode) {
        return velocities.contains(productCode);
    }
}
