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
package com.raytheon.uf.edex.plugin.npp.viirs.dao;

import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * Wraps a {@link DataSource} and checks for an array of missingValues and
 * returns Double.NaN instead
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDataSourceWrapper implements DataSource {

    private DataSource dataSource;

    private float[] missingValues;

    public VIIRSDataSourceWrapper(DataSource dataSource, float[] missingValues) {
        this.dataSource = dataSource;
        this.missingValues = missingValues;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.geospatial.interpolation.data.DataSource#getDataValue
     * (int, int)
     */
    @Override
    public double getDataValue(int x, int y) {
        double value = dataSource.getDataValue(x, y);
        if (Double.isNaN(value) == false) {
            for (int i = 0; i < missingValues.length; ++i) {
                if (value == missingValues[i]) {
                    return Double.NaN;
                }
            }
        }
        return value;
    }

}
