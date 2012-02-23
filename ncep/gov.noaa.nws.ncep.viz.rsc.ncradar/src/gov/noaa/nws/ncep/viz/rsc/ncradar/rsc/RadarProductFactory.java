/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarProductFactory
 * 
 * 12-07-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc;

import java.util.Arrays;

import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.interrogators.RadarDMDInterrogator;
import com.raytheon.viz.radar.interrogators.RadarDefaultInterrogator;
import com.raytheon.viz.radar.interrogators.RadarEETInterrogator;
import com.raytheon.viz.radar.interrogators.RadarGraphicInterrogator;
import com.raytheon.viz.radar.interrogators.RadarPrecipInterrogator;
import com.raytheon.viz.radar.interrogators.RadarRadialInterrogator;
import com.raytheon.viz.radar.interrogators.RadarRasterInterrogator;
import com.raytheon.viz.radar.interrogators.RadarVILInterrogator;
import com.raytheon.viz.radar.interrogators.RadarVelocityInterrogator;
import com.raytheon.viz.radar.interrogators.RadarXsectInterrogator;

import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.graphic.RadarGraphicsResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.graphic.RadarMLResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image.RadarRadialResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image.RadarRasterResource;

/**
 * TODO Add Description
 * 
 * This class is based on Raytheon's code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/08/2011   #541       S. Gurung     Initial creation
 * 12/16/2011   #541       S. Gurung     Added support for Graphics format
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class RadarProductFactory {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RadarProductFactory.class);

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
        } else if (productCode == 134) {
            interrogator = new RadarVILInterrogator();
        } else if (productCode == 135) {
            interrogator = new RadarEETInterrogator();
        } else if ("Radial".equals(format)) {
            interrogator = new RadarRadialInterrogator();
        } else if ("Raster".equals(format)) {
            interrogator = new RadarRasterInterrogator();
        } else if ("Graphic".equals(format)) {
            if (productCode == 149) {
                interrogator = new RadarDMDInterrogator();
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
                //resource = new RadarGSMResource(rrd, loadProps, interrogator);
            } else if (xSects.contains(productCode)) {
               // resource = new RadarXsectXYResource(rrd, loadProps,
                       // interrogator);
            } else {
                //resource = new RadarXYResource(rrd, loadProps, interrogator);
            }
        } else if ("Graph".equals(format)) {
           // resource = new RadarGraphResource(rrd, loadProps);
        } else if ("Raster".equals(format)) {
            if (rrd.mode.equals("CZ-Pg")) {
                // Don't use a text contributer
                resource = new RadarGraphicsResource(rrd, loadProps, null);
            } else {
                resource = new RadarRasterResource(rrd, loadProps, interrogator);
            }
        } else if ("Radial".equals(format)) {
            if (rrd.mode.startsWith("SRM")) {
              //  resource = new RadarSRMResource(rrd, loadProps, interrogator);
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
            statusHandler.handle(Priority.EVENTA, format
                            + " product #" + productCode);
        } else {
            resource = null;//new AbstractRadarResource<MapDescriptor>(rrd, loadProps,
                    //interrogator);
        }
        return resource;
    }
}
