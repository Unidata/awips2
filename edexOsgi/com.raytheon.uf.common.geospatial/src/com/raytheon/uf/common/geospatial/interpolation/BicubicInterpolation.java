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
package com.raytheon.uf.common.geospatial.interpolation;

import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * bicubic convolusion.
 * 
 * <pre>
 * http://en.wikipedia.org/wiki/Bicubic_interpolation#Bicubic_convolution_algorithm
 * http://docs.oracle.com/cd/E17802_01/products/products/java-media/jai/forDevelopers/jai-apidocs/javax/media/jai/InterpolationBicubic.html
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class BicubicInterpolation implements Interpolation {

    private double a = -0.5;

    public BicubicInterpolation() {
    }

    @Override
    public double getInterpolatedValue(DataSource source, double x, double y) {
        double value = 0.0f;
        double weight = 0.0f;

        double xd = ((int) x) - x;
        double yd = ((int) y) - y;

        for (int xi = -1; xi <= 2; xi += 1) {
            for (int yi = -1; yi <= 2; yi += 1) {
                double xWeight = weight(xd + xi);
                double yWeight = weight(yd + yi);
                double w = xWeight * yWeight;
                double val = source.getDataValue((int) x + xi, (int) y + yi);
                value += w * val;
                weight += w;
            }
        }
        return (float) (value / weight);
    }

    private double weight(double dist) {
        if (dist < 0) {
            dist = -dist;
        }
        if (dist <= 1) {
            return (a + 2) * dist * dist * dist - (a + 3) * dist * dist + 1.0;
        } else if (dist > 1 && dist <= 2) {
            return a * dist * dist * dist - 5 * a * dist * dist + 8 * a * dist
                    - 4 * a;
        }
        return 0.0f;

    }

    /**
     * Should be -0.5, -0.75, or -1.0, or maybe something inbetween?
     * 
     * @param a
     */
    public void setA(double a) {
        this.a = a;
    }

}
