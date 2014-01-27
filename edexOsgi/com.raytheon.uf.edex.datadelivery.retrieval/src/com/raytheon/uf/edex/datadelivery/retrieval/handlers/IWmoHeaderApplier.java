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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import java.util.Date;

/**
 * Apply a WMO compliant header to text data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 04, 2013 1647       djohnson     Initial creation
 * Aug 07, 2013 1822       bgonzale     Added arguments to applyWmoHeader.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IWmoHeaderApplier {

    /**
     * Apply the WMO compliant header.
     * 
     * @param dataProvider
     *            NOMADS, MADIS, or PDA
     * @param dataFormat
     *            Binary Grid(OPENDAP), POINT(MADIS), NetCDF4
     * @param sourceType
     *            Model, Observation, Satellite
     * @param date
     *            data date
     * @param data
     *            the data
     * @return the data with a WMO compliant header
     */
    String applyWmoHeader(final String dataProvider, final String dataFormat,
            final String sourceType, final Date date, final String data);

}
