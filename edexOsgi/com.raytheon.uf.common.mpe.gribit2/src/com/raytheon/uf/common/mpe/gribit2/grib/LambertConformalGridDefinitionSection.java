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

import com.raytheon.uf.common.mpe.gribit2.grid.LambertConformalGridDefinition;

/**
 * POJO representing the Lambert Conformal variation of a Grid Definition
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

public class LambertConformalGridDefinitionSection extends
        GridDefinitionSection<LambertConformalGridDefinition> {

    public static final int[] ASSOCIATED_DATA_REPR_TYPE = { 3 };

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

    /**
     * FIRST LATITUDE FROM THE POLE AT WHICH THE SECANT CONE CUTS THE SPERICAL
     * EARTH
     */
    private int firstLatConeCut;

    /**
     * SECOND LATITUDE ...
     */
    private int secondLat;

    /**
     * LATITUDE OF SOUTH POLE (MILLIDEGREES)
     */
    private int latSouthPole;

    /**
     * LONGITUDE OF SOUTH POLE (MILLIDEGREES)
     */
    private int lonSouthPole;

    @Override
    protected void readSectionFully(final short[] unsignedSectionBytes) {
        /*
         * Read the remaining, grid-specific attributes. Starts at index: 15.
         */
        final int[] unpackedValue = new int[1];
        /* gds(18), gds(19), gds(20) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(18), 24, 0, 1, true);
        lonMeridianParallelX = unpackedValue[0];

        /* gds(21), gds(22), gds(23) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(21), 24, 0, 1);
        gridLengthX = unpackedValue[0];

        /* gds(24), gds(25), gds(26) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(24), 24, 0, 1);
        gridLengthY = unpackedValue[0];

        /* gds(27) */
        projectionCenterFlag = (int) unsignedSectionBytes[23];

        /* gds(28) */
        scanningModeFlag = (int) unsignedSectionBytes[24];

        /* gds(29), gds(30), gds(31) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(29), 24, 0, 1);
        firstLatConeCut = unpackedValue[0];

        /* gds(32), gds(33), gds(34) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(32), 24, 0, 1);
        secondLat = unpackedValue[0];

        /* gds(35), gds(36), gds(37) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(35), 24, 0, 1);
        latSouthPole = unpackedValue[0];

        /* gds(38), gds(39), gds(40) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(38), 24, 0, 1);
        lonSouthPole = unpackedValue[0];

        /*
         * gds(41), gds(42) will all be zero
         */
    }

    @Override
    protected void generateSectionFully(
            LambertConformalGridDefinition gridDefinition) {
        lonMeridianParallelX = gridDefinition.getLonMeridianParallelX();
        gridLengthX = gridDefinition.getGridLengthX();
        gridLengthY = gridDefinition.getGridLengthY();
        projectionCenterFlag = gridDefinition.getProjectionCenterFlag();
        scanningModeFlag = gridDefinition.getScanningModeFlag();
        firstLatConeCut = gridDefinition.getFirstLatConeCut();
        secondLat = gridDefinition.getSecondLat();
        latSouthPole = gridDefinition.getLatSouthPole();
        lonSouthPole = gridDefinition.getLonSouthPole();
    }

    @Override
    protected void writeSectionFully(final OutputStream os) throws IOException {
        short[] packedValues = new short[3];
        final int[] toPack = new int[1];

        /* gds(18), gds(19), gds(20) = lon meridian parallel x */
        toPack[0] = lonMeridianParallelX;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
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

        /* gds(29), gds(30), gds(31) = first lat cone cut */
        toPack[0] = firstLatConeCut;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(32), gds(33), gds(34) = second lat */
        toPack[0] = secondLat;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(35), gds(36), gds(37) = lat south pole */
        toPack[0] = latSouthPole;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(38), gds(39), gds(40) = lon south pole */
        toPack[0] = lonSouthPole;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /*
         * gds(41), gds(42) will all be zero
         */
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

    public int getFirstLatConeCut() {
        return firstLatConeCut;
    }

    public void setFirstLatConeCut(int firstLatConeCut) {
        this.firstLatConeCut = firstLatConeCut;
    }

    public int getSecondLat() {
        return secondLat;
    }

    public void setSecondLat(int secondLat) {
        this.secondLat = secondLat;
    }

    public int getLatSouthPole() {
        return latSouthPole;
    }

    public void setLatSouthPole(int latSouthPole) {
        this.latSouthPole = latSouthPole;
    }

    public int getLonSouthPole() {
        return lonSouthPole;
    }

    public void setLonSouthPole(int lonSouthPole) {
        this.lonSouthPole = lonSouthPole;
    }
}