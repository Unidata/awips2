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
 * Applies a WMO header that is always the same.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 04, 2013 1647       djohnson     Initial creation
 * Aug 09, 2013 1822       bgonzale     Added parameters to IWmoHeaderApplier.applyWmoHeader().
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class AlwaysSameWmoHeader implements IWmoHeaderApplier {

    private final String wmoHeader;

    public AlwaysSameWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String applyWmoHeader(String dataProvider, String dataFormat,
            String sourceType, Date date, String data) {
        return wmoHeader + "\n" + data;
    }

}
