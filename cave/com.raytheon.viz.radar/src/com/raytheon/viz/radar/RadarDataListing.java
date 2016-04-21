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
package com.raytheon.viz.radar;

import java.io.File;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.datalisting.impl.DefaultDataListing;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * 
 * DataListing which can format product codes and elevation angles nicely.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 09, 2015  4153     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarDataListing extends DefaultDataListing {

    private static final DecimalFormat ELEVATION_ANGLE_FORMAT = new DecimalFormat("#.##");

    private RadarInfoDict infoDict;

    public RadarDataListing() throws ReflectiveOperationException {
        super(RadarRecord.class);
    }

    public RadarDataListing(List<String> keySet) {
        super("radar", keySet);
    }

    public RadarDataListing(Set<String> keySet) {
        super("radar", keySet);
    }

    @Override
    protected Map<String, String> getFormattedValues(String key, Collection<String> values) {
        if (infoDict == null) {
            File radarInfo = PathManagerFactory.getPathManager().getStaticFile("radarInfo.txt");
            if (radarInfo != null) {
                infoDict = RadarInfoDict.getInstance(radarInfo.getParent());
            } else {
                return super.getFormattedValues(key, values);
            }
        }
        try {
            if ("productCode".equals(key)) {
                List<ProductCodeLabel> labels = new ArrayList<>(values.size());
                for (String value : values) {
                    labels.add(new ProductCodeLabel(infoDict, value));
                }
                Collections.sort(labels);
                Map<String, String> formatted = new LinkedHashMap<>();
                for (ProductCodeLabel label : labels) {
                    formatted.put(label.getKey(), label.getLabel());
                }
                return formatted;
            } else if ("primaryElevationAngle".equals(key)) {
                List<Double> angles = new ArrayList<>(values.size());
                for (String value : values) {
                    angles.add(Double.parseDouble(value));
                }
                Collections.sort(angles);
                Map<String, String> formatted = new LinkedHashMap<>();
                for (Double angle : angles) {
                    String value = ELEVATION_ANGLE_FORMAT.format(angle);
                    formatted.put(value, value);
                }
                return formatted;
            }
        } catch (Exception e) {
            return super.getFormattedValues(key, values);
        }
        return super.getFormattedValues(key, values);
    }

    private static class ProductCodeLabel implements Comparable<ProductCodeLabel> {

        private final int productCode;

        private final String name;

        private final int bitDepth;

        private final double resolution;

        public ProductCodeLabel(RadarInfoDict infoDict, String productCode) {
            this.productCode = Integer.parseInt(productCode);
            if (infoDict.getInfo(this.productCode).getResolution() != 0) {
                int numLevels = infoDict.getInfo(this.productCode).getNumLevels();
                bitDepth = (int) (Math.log(numLevels) / Math.log(2));
                int resolutionM = infoDict.getInfo(this.productCode).getResolution();
                resolution = resolutionM / 1000.0;
                name = infoDict.getInfo(this.productCode).getName();
            } else {
                bitDepth = 0;
                resolution = Double.NaN;
                name = infoDict.getInfo(this.productCode).getName();
            }
        }

        public String getKey() {
            return Integer.toString(productCode);
        }

        public String getLabel() {
            if (bitDepth != 0) {
                return bitDepth + "-bit " + resolution + " km " + name + " (" + productCode + ")";
            } else {
                return name + " (" + productCode + ")";
            }
        }

        @Override
        public int compareTo(ProductCodeLabel o) {
            int result = this.name.compareTo(o.name);
            if (result != 0) {
                return result;
            }
            result = Integer.compare(bitDepth, o.bitDepth);
            if (result != 0) {
                return result;
            }
            result = Double.compare(resolution, o.resolution);
            if (result != 0) {
                return result;
            }
            return Integer.compare(productCode, o.productCode);
        }


    }

}
