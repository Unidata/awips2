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

import com.raytheon.uf.common.mpe.gribit2.XmrgToGribConstants;
import com.raytheon.uf.common.mpe.gribit2.grid.IGridDefinition;

/**
 * POJO representation of a Grid Definition Section (GDS). Based on:
 * /rary.ohd.pproc.gribit/TEXT/engrib.f and
 * /rary.ohd.pproc.gribit/TEXT/w3fi74.f.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2016 4619       bkowal      Initial creation
 * Aug 10, 2016 4619       bkowal      Converted to a true abstract class that can handle
 *                                     reading/writing all common grid definition attributes.
 * Aug 18, 2016 4619       bkowal      Implemented section write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public abstract class GridDefinitionSection<T extends IGridDefinition> {

    /*
     * BEGIN DATA FIELDS - FIELDS WRITTEN TO AND READ FROM THE FILE.
     */
    /**
     * length of the GDS. reference: gds(1), gds(2), gds(3)
     */
    private int numberBytes;

    /**
     * OCTET 4 = NV, NUMBER OF VERTICAL COORDINATE PARAMETERS. reference: gds(4)
     */
    private int numVertCoords;

    /**
     * OCTET 5 = PV, PL OR 255. reference: gds(5)
     */
    private int pvPL255;

    /**
     * OCTET 6 = DATA REPRESENTATION TYPE (TABLE 6). reference: gds(6)
     */
    private int dataRepresentationType;

    /**
     * number of x points. reference: gds(7), gds(8)
     */
    private int nx;

    /**
     * number of y points. reference: gds(9), gds(10)
     */
    private int ny;

    /**
     * origin latitude. reference: gds(11), gds(12), gds(13)
     */
    private int originLat;

    /**
     * origin longitude. reference: gds(14), gds(15), gds(16)
     */
    private int originLon;

    /* BREAK FOR GRID-SPECIFIC FIELDS THAT VARY BY TYPE */

    /**
     * resolution and component flag for bit 5 of gds(17). 0 = earth oriented
     * winds, 1 = grid oriented winds
     */
    private int resolutionComponentFlag = 0;

    /*
     * END DATA FIELDS - FIELDS WRITTEN TO AND READ FROM THE FILE.
     */

    /**
     * igflag control: 0 - make GDS based on 'igrid' value; 1 - make GDS from
     * user supplied info in 'igds' and 'igrid' value for w3fi74 input (but doc
     * in w3fi71)
     */
    private Boolean igflag = Boolean.TRUE;

    /**
     * grid identification: # - Table B. 255 = user defined grid, 'igds' must be
     * supplied and igflag must = 1
     */
    private int grid = XmrgToGribConstants.PDS_NGRID;

    /**
     * Reads and populates common grid definition section attributes based on
     * the specified unsigned numerical array, the specified data representation
     * type, and the specified number of bytes.
     * 
     * @param unsignedSectionBytes
     *            the specified unsigned numerical array
     * @param dataRepresentationType
     *            the specified data representation type; identifies the type of
     *            grid
     * @param numberBytes
     *            the specified number of bytes; identifies the total size of
     *            the grid definition
     */
    protected final void readSection(final short[] unsignedSectionBytes,
            final int dataRepresentationType, final int numberBytes) {
        this.numberBytes = numberBytes;

        /* gds(4) */
        numVertCoords = (int) unsignedSectionBytes[0];

        /* gds(5) */
        pvPL255 = (int) unsignedSectionBytes[1];

        /* gds(6) */
        this.dataRepresentationType = dataRepresentationType;

        /* gds(7), gds(8) */
        nx = (unsignedSectionBytes[3] * XmrgGribPropertyUtils.DIVISOR_6)
                + unsignedSectionBytes[4];
        /* gds(9), gds(10) */
        ny = (unsignedSectionBytes[5] * XmrgGribPropertyUtils.DIVISOR_6)
                + unsignedSectionBytes[6];

        final int[] unpackedValue = new int[1];
        /* gds(11), gds(12), gds(13) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(11), 24, 0, 1, true);
        originLat = unpackedValue[0];

        /* gds(14), gds(15), gds(16) */
        XmrgGribPropertyUtils.unpackBytes(unpackedValue, unsignedSectionBytes,
                XmrgGribPropertyUtils.calculateBitOffset(14), 24, 0, 1, true);
        originLon = unpackedValue[0];

        resolutionComponentFlag = (int) unsignedSectionBytes[13];

        /*
         * gds(17) contains common information; but, it is based on a hard-coded
         * value, the 'resolutionComponentFlag'.
         */
        readSectionFully(unsignedSectionBytes);
    }

    /**
     * Writes the contents of the Grid Definition Section to the specified
     * {@link OutputStream}.
     * 
     * @param os
     *            the specified {@link OutputStream}
     */
    public void writeSection(final OutputStream os) throws IOException {
        short[] packedValues = new short[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        final int[] toPack = new int[1];

        /*
         * the length.
         */
        toPack[0] = numberBytes;
        XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24);
        try {
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /*
             * gds(4) = number vert coords
             */
            os.write(numVertCoords);

            /*
             * gds(5) = pv-pl-255 flag
             */
            os.write(pvPL255);

            /*
             * gds(6) = data representation type
             */
            os.write(dataRepresentationType);

            packedValues = new short[2];
            /*
             * gds(7), gds(8) = number x values
             */
            toPack[0] = nx;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 16);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /* gds(9), gds(10) = number y values */
            toPack[0] = ny;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 16);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            packedValues = new short[3];
            /* gds(11), gds(12), gds(13) = origin lat (care about + vs -) */
            toPack[0] = originLat;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24, true);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /* gds(14), gds(15), gds(16) = origin lon (care about + vs -) */
            toPack[0] = originLon;
            XmrgGribPropertyUtils.packBytes(packedValues, toPack, 0, 24, true);
            XmrgGribPropertyUtils.writeShortArrayAsBytes(os, packedValues);

            /*
             * gds(17) = 'OR' ICOMP-BIT 5 RESOLUTION & COMPONENT FLAG FOR WINDS
             * WITH the resolution flag INFO (REST OF RESOLUTION & COMPONENT
             * FLAG DATA)
             */
            int shiftedResFlag = resolutionComponentFlag << 3;
            os.write((resolutionComponentFlag | shiftedResFlag));

            writeSectionFully(os);
        } catch (IOException e) {
            throw new IOException(
                    "Failed to write the grid definition section.", e);
        }
    }

    /**
     * Initializes the Grid Definition Section based on the specified
     * {@link IGridDefinition}.
     * 
     * @param gridDefinition
     *            the specified {@link IGridDefinition}
     */
    protected final void generateSection(T gridDefinition) {
        this.numVertCoords = gridDefinition.getNumVertCoords();
        this.pvPL255 = gridDefinition.getPvPL255();
        this.dataRepresentationType = gridDefinition
                .getDataRepresentationType();
        this.nx = gridDefinition.getNumberXPoints();
        this.ny = gridDefinition.getNumberYPoints();
        this.originLat = gridDefinition.getOriginLat();
        this.originLon = gridDefinition.getOriginLon();
        generateSectionFully(gridDefinition);
    }

    /**
     * Finishes reading any type-specific grid definition section attributes
     * from the specified unsigned numerical array.
     * 
     * @param unsignedSectionBytes
     *            the specified unsigned numerical array
     */
    protected abstract void readSectionFully(final short[] unsignedSectionBytes);

    /**
     * Finishes initializing any type-specific grid definition section
     * attributes based on the specified grid definition.
     * 
     * @param gridDefinition
     *            the specified grid definition
     */
    protected abstract void generateSectionFully(final T gridDefinition);

    /**
     * Finishes writing any type-specific grid definition section attributes to
     * the specified {@link OutputStream}.
     * 
     * @param os
     *            the specified {@link OutputStream}
     */
    protected abstract void writeSectionFully(final OutputStream os)
            throws IOException;

    public int getNumberBytes() {
        return numberBytes;
    }

    public void setNumberBytes(int numberBytes) {
        this.numberBytes = numberBytes;
    }

    public int getNumVertCoords() {
        return numVertCoords;
    }

    public void setNumVertCoords(int numVertCoords) {
        this.numVertCoords = numVertCoords;
    }

    public int getPvPL255() {
        return pvPL255;
    }

    public void setPvPL255(int pvPL255) {
        this.pvPL255 = pvPL255;
    }

    public int getDataRepresentationType() {
        return dataRepresentationType;
    }

    public void setDataRepresentationType(int dataRepresentationType) {
        this.dataRepresentationType = dataRepresentationType;
    }

    public int getNx() {
        return nx;
    }

    public void setNx(int nx) {
        this.nx = nx;
    }

    public int getNy() {
        return ny;
    }

    public void setNy(int ny) {
        this.ny = ny;
    }

    public int getOriginLat() {
        return originLat;
    }

    public void setOriginLat(int originLat) {
        this.originLat = originLat;
    }

    public int getOriginLon() {
        return originLon;
    }

    public void setOriginLon(int originLon) {
        this.originLon = originLon;
    }

    public Boolean getIgflag() {
        return igflag;
    }

    public void setIgflag(Boolean igflag) {
        this.igflag = igflag;
    }

    public int getGrid() {
        return grid;
    }

    public void setGrid(int grid) {
        this.grid = grid;
    }

    public int getResolutionComponentFlag() {
        return resolutionComponentFlag;
    }

    public void setResolutionComponentFlag(int resolutionComponentFlag) {
        this.resolutionComponentFlag = resolutionComponentFlag;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("GridDefinitionSection [");
        sb.append("igflag=").append(igflag);
        sb.append(", grid=").append(grid);
        sb.append(", resolutionComponentFlag=").append(resolutionComponentFlag);
        sb.append("]");
        return sb.toString();
    }
}