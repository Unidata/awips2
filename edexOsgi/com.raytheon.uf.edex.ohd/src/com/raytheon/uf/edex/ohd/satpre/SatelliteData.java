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
package com.raytheon.uf.edex.ohd.satpre;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;

/**
 * Basic POJO used to hold retrieved satellite data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public final class SatelliteData {

    private final SatelliteRecord dataRecord;

    private final BufferWrapper data;

    public SatelliteData(final SatelliteRecord dataRecord,
            final BufferWrapper data) {
        this.dataRecord = dataRecord;
        this.data = data;
    }

    public SatelliteRecord getDataRecord() {
        return dataRecord;
    }

    public BufferWrapper getData() {
        return data;
    }
}