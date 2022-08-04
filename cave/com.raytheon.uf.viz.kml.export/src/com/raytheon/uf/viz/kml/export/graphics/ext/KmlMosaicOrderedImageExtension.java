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
package com.raytheon.uf.viz.kml.export.graphics.ext;

import java.util.Comparator;

import com.raytheon.uf.viz.core.drawables.ext.IMosaicOrderedImageExtension;

/**
 * Extends mosaic extesnion to perform ordered mosaicing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlMosaicOrderedImageExtension extends KmlMosaicImageExtension
        implements IMosaicOrderedImageExtension {

    @Override
    protected Comparator<Double> getMosaicComparator() {
        return new Comparator<Double>() {
            @Override
            public int compare(Double o1, Double o2) {
                if (o2 != 0 && !Double.isNaN(o2)) {
                    return -1;
                }
                return 1;
            }

        };
    }
}
