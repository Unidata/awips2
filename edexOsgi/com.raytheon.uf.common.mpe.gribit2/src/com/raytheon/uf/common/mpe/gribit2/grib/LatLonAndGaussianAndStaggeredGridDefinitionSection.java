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

import com.raytheon.uf.common.mpe.gribit2.grid.ExtendedLatLonGridDefinition;

/**
 * POJO representing the Lat/Lon, Gaussian, and Staggered variations of a Grid
 * Definition Section (GDS). Based on: /rary.ohd.pproc.gribit/TEXT/w3fi74.f
 * 
 * The reason that so many definitions were combined into one POJO
 * representation is due to certain values of the data representation type flag
 * being used across multiple grid definitions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2016 4619       bkowal      Initial creation
 * Aug 18, 2016 4619       bkowal      Implemented section write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class LatLonAndGaussianAndStaggeredGridDefinitionSection extends
        GridDefinitionSection<IMultiGridDefinitionMapping> {

    private static final int GRID_CONST_201 = 201;

    private static final int GRID_CONST_202 = 202;

    private static final int GRID_CONST_203 = 203;

    public static final int[] ASSOCIATED_DATA_REPR_TYPE = { 0, 4,
            GRID_CONST_201, GRID_CONST_202, GRID_CONST_203 };

    private static final int BASIC_SECTION_LENGTH = 32 - XmrgGribPropertyUtils.SIZE_BYTES_LEN;

    private int ninthValue;

    private int tenthValue;

    private int eleventhValue;

    private int twelfthValue;

    /**
     * SCANNING MODE FLAGS (CODE TABLE 8)
     */
    private int scanningModeFlag;

    /**
     * IGDS(91) FOR GRIDS 37-44, NUMBER OF POINTS IN EACH OF 73 ROWS.
     */
    private int[] numberPoints;

    @Override
    protected void readSectionFully(short[] unsignedSectionBytes) {
        final int resolutionCompare = this.getResolutionComponentFlag()
                & XmrgGribPropertyUtils.CONST_128;
        final int dataRepresentationType = this.getResolutionComponentFlag();

        /*
         * Read the remaining, grid-specific attributes. Starts at index: 15.
         */
        final int[] unpackedValue = new int[1];
        /* gds(18), gds(19), gds(20) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(18), 24, 0, 1, true);
        ninthValue = unpackedValue[0];

        /* gds(21), gds(22), gds(23) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(21), 24, 0, 1, true);
        tenthValue = unpackedValue[0];

        /*
         * Note when 'resolutionCompare' is 0, (char) 255 is written to gds(24)
         * - gds(27).
         */

        /* gds(24), gds(25) */
        if (resolutionCompare != 0) {
            XmrgGribPropertyUtils.unpackBytes(unpackedValue,
                    unsignedSectionBytes,
                    XmrgGribPropertyUtils.calculateBitOffset(24), 16, 0, 1);
            if (dataRepresentationType == GRID_CONST_201
                    || dataRepresentationType == GRID_CONST_202
                    || dataRepresentationType == GRID_CONST_203) {
                eleventhValue = unpackedValue[0];
            } else {
                twelfthValue = unpackedValue[0];
            }
        }

        /* gds(26), gds(27) */
        if (resolutionCompare != 0) {
            XmrgGribPropertyUtils.unpackBytes(unpackedValue,
                    unsignedSectionBytes,
                    XmrgGribPropertyUtils.calculateBitOffset(26), 16, 0, 1);
            if (dataRepresentationType == GRID_CONST_201
                    || dataRepresentationType == GRID_CONST_202
                    || dataRepresentationType == GRID_CONST_203) {
                twelfthValue = unpackedValue[0];
            } else {
                eleventhValue = unpackedValue[0];
            }
        }

        /* gds(28) */
        scanningModeFlag = unsignedSectionBytes[24];

        /* gds(29), gds(30), gds(31), gds(32) will all be zero */

        /* starts at: gds(33) */
        if (unsignedSectionBytes.length > BASIC_SECTION_LENGTH) {
            final int numValues = unsignedSectionBytes.length
                    - BASIC_SECTION_LENGTH;
            final int numberPointsLength = numValues / 2;
            numberPoints = new int[numberPointsLength];

            int unsignedIndex = XmrgGribPropertyUtils.calculateBitOffset(33);
            /* in groups of two: gds(i), gds(i + 1), extract the values */
            for (int i = 0; i < numberPointsLength; i++) {
                XmrgGribPropertyUtils.unpackBytes(unpackedValue,
                        unsignedSectionBytes, unsignedIndex, 16, 0, 1);
                numberPoints[i] = unpackedValue[0];
                unsignedIndex += 16;
            }
        }
    }

    @Override
    protected void generateSectionFully(
            IMultiGridDefinitionMapping gridDefinition) {
        ninthValue = gridDefinition.get9thValue();
        tenthValue = gridDefinition.get10thValue();
        eleventhValue = gridDefinition.get11thValue();
        twelfthValue = gridDefinition.get12thValue();
        scanningModeFlag = gridDefinition.getScanningModeFlag();
        if (gridDefinition instanceof ExtendedLatLonGridDefinition) {
            numberPoints = ((ExtendedLatLonGridDefinition) gridDefinition)
                    .getNumberPoints();
        }
    }

    @Override
    protected void writeSectionFully(final OutputStream os) throws IOException {
        short[] packedValues = new short[3];
        final int[] toPack = new int[1];

        /* gds(18), gds(19), gds(20) = the ninth value */
        toPack[0] = ninthValue;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24, true);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        /* gds(21), gds(22), gds(23) = the tenth value */
        toPack[0] = tenthValue;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24, true);
        XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

        final int resolutionCompare = this.getResolutionComponentFlag()
                & XmrgGribPropertyUtils.CONST_128;
        if (resolutionCompare == 0) {
            /* gds(24), gds(25), gds(26), gds(27) = char(255) is written for all */
            os.write(XmrgGribPropertyUtils.CONST_255);
            os.write(XmrgGribPropertyUtils.CONST_255);
            os.write(XmrgGribPropertyUtils.CONST_255);
            os.write(XmrgGribPropertyUtils.CONST_255);
        } else {
            /*
             * The question becomes which is written first.
             */
            short[] packedValues11 = new short[2];
            toPack[0] = eleventhValue;
            XmrgGribPropertyUtils.packBytes(packedValues11, toPack, 0, 16);

            short[] packedValues12 = new short[2];
            toPack[0] = twelfthValue;
            XmrgGribPropertyUtils.packBytes(packedValues12, toPack, 0, 16);

            final int dataRepresentationType = this
                    .getResolutionComponentFlag();
            /* gds(24), gds(25) */
            /* gds(26), gds(27) */
            if (dataRepresentationType == GRID_CONST_201
                    || dataRepresentationType == GRID_CONST_202
                    || dataRepresentationType == GRID_CONST_203) {
                XmrgGribPropertyUtils
                        .writeShortArrayAsBytes(os, packedValues11);
                XmrgGribPropertyUtils
                        .writeShortArrayAsBytes(os, packedValues12);
            } else {
                XmrgGribPropertyUtils
                        .writeShortArrayAsBytes(os, packedValues12);
                XmrgGribPropertyUtils
                        .writeShortArrayAsBytes(os, packedValues11);
            }
        }

        /* gds(28) = scanning mode flag */
        os.write(scanningModeFlag);

        /* gds(29), gds(30), gds(31), gds(32) will all be zero */
        os.write(0);
        os.write(0);
        os.write(0);
        os.write(0);

        if (numberPoints != null) {
            /*
             * Write the extended data at the very end.
             */
            packedValues = new short[2];
            for (int value : numberPoints) {
                toPack[0] = value;
                XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 16,
                        true);
                XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);
            }
        }
    }

    public int getNinthValue() {
        return ninthValue;
    }

    public void setNinthValue(int ninthValue) {
        this.ninthValue = ninthValue;
    }

    public int getTenthValue() {
        return tenthValue;
    }

    public void setTenthValue(int tenthValue) {
        this.tenthValue = tenthValue;
    }

    public int getEleventhValue() {
        return eleventhValue;
    }

    public void setEleventhValue(int eleventhValue) {
        this.eleventhValue = eleventhValue;
    }

    public int getTwelfthValue() {
        return twelfthValue;
    }

    public void setTwelfthValue(int twelfthValue) {
        this.twelfthValue = twelfthValue;
    }

    public int getScanningModeFlag() {
        return scanningModeFlag;
    }

    public void setScanningModeFlag(int scanningModeFlag) {
        this.scanningModeFlag = scanningModeFlag;
    }

    public int[] getNumberPoints() {
        return numberPoints;
    }

    public void setNumberPoints(int[] numberPoints) {
        this.numberPoints = numberPoints;
    }
}