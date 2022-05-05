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

import com.raytheon.uf.common.mpe.gribit2.grid.MercatorGridDefinition;

/**
 * POJO representing the Mercator variation of a Grid Definition Section (GDS).
 * Based on: /rary.ohd.pproc.gribit/TEXT/w3fi74.f
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

public class MercatorGridDefinitionSection extends
        GridDefinitionSection<MercatorGridDefinition> {

    public static final int[] ASSOCIATED_DATA_REPR_TYPE = { 1 };

    /**
     * LATITUDE OF EXTREME POINT (SOUTH - IVE)
     */
    private int extremePointLat;

    /**
     * LONGITUDE OF EXTREME POINT (WEST - IVE)
     */
    private int extremePointLon;

    /**
     * LATITUDE INCREMENT
     */
    private int incrementLat;

    /**
     * LONGITUDE INCREMENT
     */
    private int incrementLon;

    /**
     * LATITUDE AT WHICH PROJECTION CYLINDER INTERSECTS EARTH
     */
    private int latProjCylinderIntersect;

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
                XmrgGribPropertyUtils.calculateBitOffset(18), 24, 0, 1, true);
        extremePointLat = unpackedValue[0];

        /* gds(21), gds(22), gds(23) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(21), 24, 0, 1, true);
        extremePointLon = unpackedValue[0];

        /* gds(24), gds(25), gds(26) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(24), 24, 0, 1);
        latProjCylinderIntersect = unpackedValue[0];

        /* gds(27) == 0 */

        /* gds(28) */
        scanningModeFlag = (int) unsignedSectionBytes[23];

        /* gds(29), gds(30), gds(31) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(29), 24, 0, 1);
        incrementLon = unpackedValue[0];

        /* gds(32), gds(33), gds(34) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(32), 24, 0, 1);
        incrementLat = unpackedValue[0];

        /*
         * gds(35), gds(36), gds(37), gds(38), gds(39), gds(40), gds(41),
         * gds(42) will all be zero
         */
    }

    @Override
    protected void generateSectionFully(MercatorGridDefinition gridDefinition) {
        extremePointLat = gridDefinition.getExtremePointLat();
        extremePointLon = gridDefinition.getExtremePointLon();
        incrementLat = gridDefinition.getIncrementLat();
        incrementLon = gridDefinition.getIncrementLon();
        latProjCylinderIntersect = gridDefinition.getLatProjCylinderIntersect();
        scanningModeFlag = gridDefinition.getScanningModeFlag();
    }

    @Override
    protected void writeSectionFully(final OutputStream os) throws IOException {
        short[] packedValues = new short[3];
        final int[] toPack = new int[1];

        /* gds(18), gds(19), gds(20) = extreme point lat */
        toPack[0] = extremePointLat;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24, true);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(21), gds(22), gds(23) = extreme point lon */
        toPack[0] = extremePointLon;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24, true);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(24), gds(25), gds(26) = lat proj cylinder intersect */
        toPack[0] = latProjCylinderIntersect;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* write 0 for gds(27) */
        os.write(0);

        /* gds(28) = scanning mode flag */
        os.write(scanningModeFlag);

        /* gds(29), gds(30), gds(31) - increment lon */
        toPack[0] = incrementLon;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(29), gds(30), gds(31) - increment lat */
        toPack[0] = incrementLat;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /*
         * gds(35), gds(36), gds(37), gds(38), gds(39), gds(40), gds(41),
         * gds(42) will all be zero
         */
        os.write(0);
        os.write(0);
        os.write(0);
        os.write(0);
        os.write(0);
        os.write(0);
        os.write(0);
        os.write(0);
    }

    public int getExtremePointLat() {
        return extremePointLat;
    }

    public void setExtremePointLat(int extremePointLat) {
        this.extremePointLat = extremePointLat;
    }

    public int getExtremePointLon() {
        return extremePointLon;
    }

    public void setExtremePointLon(int extremePointLon) {
        this.extremePointLon = extremePointLon;
    }

    public int getIncrementLat() {
        return incrementLat;
    }

    public void setIncrementLat(int incrementLat) {
        this.incrementLat = incrementLat;
    }

    public int getIncrementLon() {
        return incrementLon;
    }

    public void setIncrementLon(int incrementLon) {
        this.incrementLon = incrementLon;
    }

    public int getLatProjCylinderIntersect() {
        return latProjCylinderIntersect;
    }

    public void setLatProjCylinderIntersect(int latProjCylinderIntersect) {
        this.latProjCylinderIntersect = latProjCylinderIntersect;
    }

    public int getScanningModeFlag() {
        return scanningModeFlag;
    }

    public void setScanningModeFlag(int scanningModeFlag) {
        this.scanningModeFlag = scanningModeFlag;
    }
}