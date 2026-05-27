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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

import java.util.Comparator;

/**
 * {@link Comparator} implementation used to order {@link HPERadarMosaic} to
 * ensure that base mosaics and mosaics with the fewest number of dependencies
 * will be first in an ordered list of {@link HPERadarMosaic}s.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HPERadarMosaicDependencyComparator
        implements Comparator<HPERadarMosaic> {

    @Override
    public int compare(HPERadarMosaic o1, HPERadarMosaic o2) {
        if (o1.isBase() && !o2.isBase()) {
            return -1;
        } else if (!o1.isBase() && o2.isBase()) {
            return 1;
        }

        /*
         * They are either both base or not base.
         */
        return Integer.compare(o1.getDependencies().length,
                o2.getDependencies().length);
    }
}