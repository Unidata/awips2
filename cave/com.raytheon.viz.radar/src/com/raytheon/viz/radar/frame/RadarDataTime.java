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
package com.raytheon.viz.radar.frame;

import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * A {@link DataTime} that also contains information about the volume scan time
 * and the elevation number for the time it represents. This is used by the
 * {@link SailsFrameCoordinator}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 13, 2015  4461     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarDataTime extends DataTime {

    private static final long serialVersionUID = 1L;

    private Integer elevationNumber;

    private int volumeScanNumber;

    public RadarDataTime(DataTime time) {
        super(time.getRefTime());
    }

    public Integer getElevationNumber() {
        return elevationNumber;
    }

    public void setElevationNumber(Integer elevationNumber) {
        this.elevationNumber = elevationNumber;
    }

    public int getVolumeScanNumber() {
        return volumeScanNumber;
    }

    public void setVolumeScanNumber(int volumeScanNumber) {
        this.volumeScanNumber = volumeScanNumber;
    }

}
