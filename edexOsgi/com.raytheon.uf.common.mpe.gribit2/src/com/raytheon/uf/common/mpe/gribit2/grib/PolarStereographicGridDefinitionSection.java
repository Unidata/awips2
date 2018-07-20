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
package com.raytheon.uf.common.mpe.gribit2.grib;

import java.io.IOException;
import java.io.OutputStream;

import com.raytheon.uf.common.mpe.gribit2.grid.PolarStereographicGridDefinition;

/**
 * POJO representing the Polar Stereographic variation of a Grid Definition
 * Section (GDS). Based on: /rary.ohd.pproc.gribit/TEXT/w3fi74.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2016  4619       bkowal      Initial creation
 * Aug 18, 2016 4619       bkowal      Implemented section write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class PolarStereographicGridDefinitionSection extends
        GridDefinitionSection<PolarStereographicGridDefinition> {

    public static final int[] ASSOCIATED_DATA_REPR_TYPE = { 5 };

    /**
     * LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
     */
    private int lonMeridianParallelX;

    /**
     * X-DIRECTION GRID LENGTH (INCREMENT)
     */
    private int gridLengthX;

    /**
     * Y-DIRECTION GRID LENGTH (INCREMENT)
     */
    private int gridLengthY;

    /**
     * PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE, 1=SOUTH POLE ON PLANE)
     */
    private int projectionCenterFlag;

    /**
     * SCANNING MODE FLAGS (CODE TABLE 8)
     */
    private int scanningModeFlag;

    @Override
    protected void readSectionFully(final short[] unsignedSectionBytes) {
        /*
         * Read the remaining, grid-specific attributes. Starts at index: 15.
         */
        final int[] unpackedValue = new int[1];
        /* gds(18), gds(19), gds(20) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(18), 24, true);
        lonMeridianParallelX = unpackedValue[0];

        /* gds(21), gds(22), gds(23) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(21), 24);
        gridLengthX = unpackedValue[0];

        /* gds(24), gds(25), gds(26) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(24), 24);
        gridLengthY = unpackedValue[0];

        /* gds(27) */
        projectionCenterFlag = (int) unsignedSectionBytes[23];

        /* gds(28) */
        scanningModeFlag = (int) unsignedSectionBytes[24];

        /*
         * Remainder of this part of the data array should be all 0s.
         */
    }

    @Override
    protected void generateSectionFully(
            PolarStereographicGridDefinition gridDefinition) {
        lonMeridianParallelX = gridDefinition.getLonMeridianParallelX();
        gridLengthX = gridDefinition.getGridLengthX();
        gridLengthY = gridDefinition.getGridLengthY();
        projectionCenterFlag = gridDefinition.getProjectionCenterFlag();
        scanningModeFlag = gridDefinition.getScanningModeFlag();
    }

    @Override
    protected void writeSectionFully(final OutputStream os) throws IOException {
        short[] packedValues = new short[3];
        final int[] toPack = new int[1];

        /* gds(18), gds(19), gds(20) = lon meridian parallel x */
        toPack[0] = lonMeridianParallelX;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24, true);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(21), gds(22), gds(23) = grid length x */
        toPack[0] = gridLengthX;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(24), gds(25), gds(26) = grid length y */
        toPack[0] = gridLengthY;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(27) = projection center flag */
        os.write(projectionCenterFlag);

        /* gds(28) = scanning mode flag */
        os.write(scanningModeFlag);

        /*
         * gds(29), gds(30), gds(31), gds(32) will all be zero
         */
        os.write(0);
        os.write(0);
        os.write(0);
        os.write(0);
    }

    public int getLonMeridianParallelX() {
        return lonMeridianParallelX;
    }

    public void setLonMeridianParallelX(int lonMeridianParallelX) {
        this.lonMeridianParallelX = lonMeridianParallelX;
    }

    public int getGridLengthX() {
        return gridLengthX;
    }

    public void setGridLengthX(int gridLengthX) {
        this.gridLengthX = gridLengthX;
    }

    public int getGridLengthY() {
        return gridLengthY;
    }

    public void setGridLengthY(int gridLengthY) {
        this.gridLengthY = gridLengthY;
    }

    public int getProjectionCenterFlag() {
        return projectionCenterFlag;
    }

    public void setProjectionCenterFlag(int projectionCenterFlag) {
        this.projectionCenterFlag = projectionCenterFlag;
    }

    public int getScanningModeFlag() {
        return scanningModeFlag;
    }

    public void setScanningModeFlag(int scanningModeFlag) {
        this.scanningModeFlag = scanningModeFlag;
    }
}