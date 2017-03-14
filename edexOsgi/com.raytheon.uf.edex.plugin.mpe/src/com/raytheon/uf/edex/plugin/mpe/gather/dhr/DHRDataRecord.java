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
package com.raytheon.uf.edex.plugin.mpe.gather.dhr;

/**
 * POJO representation of a single record/row of DHR data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2016 4625       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class DHRDataRecord {

    private final short radarStartAngle;

    private final short radarDeltaAngle;

    private final short[] data;

    public DHRDataRecord(final short radarStartAngle,
            final short radarDeltaAngle, final short[] data) {
        if (data == null) {
            throw new IllegalArgumentException(
                    "Required argument 'data' cannot be NULL.");
        }
        this.radarStartAngle = radarStartAngle;
        this.radarDeltaAngle = radarDeltaAngle;
        this.data = data;
    }

    public short getRadarStartAngle() {
        return radarStartAngle;
    }

    public short getRadarDeltaAngle() {
        return radarDeltaAngle;
    }

    public short[] getData() {
        return data;
    }
}