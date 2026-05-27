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

import com.raytheon.uf.common.dataplugin.radar.RadarRecord.ScanType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 *
 * A {@link DataTime} that also contains information about the volume scan and
 * the elevation number for the time it represents. This is used by the
 * {@link SailsFrameCoordinator} and overrides times used in time matching code.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 13, 2015  4461     bsteffen    Initial creation
 * Feb 16, 2018  7032     njensen     Added nullary constructor for JAXB
 * Mar 06, 2025  2038488  mapeters    Add scanType, icao, normalScanMatchRef,
 *                                    getMatchRef() override, isSameScan()
 * Jun 04, 2025  2038858  mapeters    Update copy constructor, override clone()
 *
 * </pre>
 *
 * @author bsteffen
 */
public class RadarDataTime extends DataTime {

    private static final long serialVersionUID = 1L;

    private Integer elevationNumber;

    private int volumeScanNumber;

    private ScanType scanType;

    private String icao;

    /**
     * This should only be set for SAILS/MRLE scans, and is the match reference
     * time of the normal scan that this SAILS/MRLE scan is a part of.
     */
    private Long normalScanMatchRef;

    public RadarDataTime() {
        super();
    }

    /**
     * Copy constructor.
     *
     * @param other
     *            other data time to copy
     */
    public RadarDataTime(DataTime other) {
        super(other.getRefTime());
        this.levelValue = other.getLevelValue();
        this.levelType = other.getLevelType();

        if (other instanceof RadarDataTime rdt) {
            this.elevationNumber = rdt.elevationNumber;
            this.volumeScanNumber = rdt.volumeScanNumber;
            this.scanType = rdt.scanType;
            this.icao = rdt.icao;
            this.normalScanMatchRef = rdt.normalScanMatchRef;
        }
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

    public ScanType getScanType() {
        return scanType;
    }

    public void setScanType(ScanType scanType) {
        this.scanType = scanType;
    }

    public String getIcao() {
        return icao;
    }

    public void setIcao(String icao) {
        this.icao = icao;
    }

    public Long getNormalScanMatchRef() {
        return normalScanMatchRef;
    }

    public void setNormalScanMatchRef(Long normalScanMatchRef) {
        this.normalScanMatchRef = normalScanMatchRef;
    }

    @Override
    public long getMatchRef() {
        // This override also modifies getMatchValid()
        long matchRef = super.getMatchRef();
        if (normalScanMatchRef != null) {
            /*
             * This should only be set for SAILS/MRLE times. Override the match
             * reference time to be halfway between the normal scan time and the
             * SAILS/MRLE time, to ensure that SAILS/MRLE frames time match the
             * normal scan times that they're a part of, even if they are closer
             * in time to the next scan.
             */
            matchRef = (normalScanMatchRef + matchRef) / 2;
        }
        return matchRef;
    }

    /**
     * @param other
     *            other radar time to compare with
     * @return true if this radar time and the given radar time are part of the
     *         same volume scan, false otherwise
     */
    public boolean isSameScan(RadarDataTime other) {
        /*
         * ICAO and volume scan numbers must match, and the times must be
         * reasonably close to each other, since the volume scan numbers
         * eventually repeat.
         */
        return icao != null && icao.equals(other.icao)
                && volumeScanNumber == other.volumeScanNumber
                && Math.abs(getRefTime().getTime() - other.getRefTime()
                        .getTime()) < TimeUtil.MILLIS_PER_HOUR;
    }

    @Override
    public RadarDataTime clone() {
        return new RadarDataTime(this);
    }
}
