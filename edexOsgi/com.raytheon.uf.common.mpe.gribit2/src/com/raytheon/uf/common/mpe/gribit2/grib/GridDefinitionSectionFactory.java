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

import java.util.Map;
import java.util.HashMap;
import java.nio.ByteBuffer;

import com.raytheon.uf.common.mpe.gribit2.grid.AbstractLatLonCommon10;
import com.raytheon.uf.common.mpe.gribit2.grid.IGridDefinition;

/**
 * Factory used to lookup/retrieve a Grid Definition Section (GDS).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2016  4619       bkowal      Initial creation
 * Aug 18, 2016 4619       bkowal      Determine the size of the GDS during generation.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class GridDefinitionSectionFactory {

    private static final GridDefinitionSectionFactory INSTANCE = new GridDefinitionSectionFactory();

    private static final int DATA_REPR_TYPE_INDEX = 2;

    private static final int SUPPORTED_GDS_SIZE_32 = 32;

    private static final int SUPPORTED_GDS_SIZE_42 = 42;

    private final Map<Integer, Class<? extends GridDefinitionSection<IGridDefinition>>> gridSectionLookupMap = new HashMap<>();

    protected GridDefinitionSectionFactory() {
        addGridDefinitionLookups(
                PolarStereographicGridDefinitionSection.ASSOCIATED_DATA_REPR_TYPE,
                PolarStereographicGridDefinitionSection.class);
        addGridDefinitionLookups(
                LambertConformalGridDefinitionSection.ASSOCIATED_DATA_REPR_TYPE,
                LambertConformalGridDefinitionSection.class);
        addGridDefinitionLookups(
                MercatorGridDefinitionSection.ASSOCIATED_DATA_REPR_TYPE,
                MercatorGridDefinitionSection.class);
        addGridDefinitionLookups(
                LatLonAndGaussianAndStaggeredGridDefinitionSection.ASSOCIATED_DATA_REPR_TYPE,
                LatLonAndGaussianAndStaggeredGridDefinitionSection.class);
    }

    public static GridDefinitionSectionFactory getInstance() {
        return INSTANCE;
    }

    @SuppressWarnings("unchecked")
    private void addGridDefinitionLookups(final int[] lookupKeys,
            final Class<? extends GridDefinitionSection<?>> definitionClass) {
        for (int lookupKey : lookupKeys) {
            gridSectionLookupMap
                    .put(lookupKey,
                            (Class<? extends GridDefinitionSection<IGridDefinition>>) definitionClass);
        }
    }

    private GridDefinitionSection<IGridDefinition> lookupGridDefinition(
            final int lookupKey) throws GridDefinitionLookupException {
        Class<? extends GridDefinitionSection<IGridDefinition>> clazz = gridSectionLookupMap
                .get(lookupKey);
        if (clazz == null) {
            throw new GridDefinitionLookupException(
                    "Unable to find a Grid Definition Section associated with data representation type: "
                            + lookupKey + ".");
        }

        try {
            return clazz.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new GridDefinitionLookupException(
                    "Failed to instantiate Grid Definition Section: "
                            + clazz.getName() + ".", e);
        }
    }

    /**
     * Attempts to read the specified {@link ByteBuffer} into a
     * {@link GridDefinitionSection}.
     * 
     * @param byteBuffer
     *            the specified {@link ByteBuffer}
     * @return the {@link GridDefinitionSection} that was produced
     * @throws GridDefinitionLookupException
     */
    public GridDefinitionSection<?> readDefinitionSection(
            final ByteBuffer byteBuffer) throws GridDefinitionLookupException {
        final byte[] lengthBytes = new byte[XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        byteBuffer.get(lengthBytes);
        final short[] unsignedLengthBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(lengthBytes);
        final int numberBytes = XmrgGribPropertyUtils
                .convertToSize(unsignedLengthBytes);

        final byte[] sectionBytes = new byte[numberBytes
                - XmrgGribPropertyUtils.SIZE_BYTES_LEN];
        byteBuffer.get(sectionBytes);
        final short[] unsignedSectionBytes = XmrgGribPropertyUtils
                .getUnsignedBytes(sectionBytes);

        /*
         * Read the data representation type.
         */
        final int dataRepresentationType = (int) unsignedSectionBytes[DATA_REPR_TYPE_INDEX];
        GridDefinitionSection<?> gds = lookupGridDefinition(dataRepresentationType);
        gds.readSection(unsignedSectionBytes, dataRepresentationType,
                numberBytes);
        return gds;
    }

    /**
     * Generates a GRIB 1 {@link GridDefinitionSection} associated with the
     * specified {@link IGridDefinition}.
     * 
     * @param gridDefinition
     *            the specified {@link IGridDefinition}
     * @return the generated GRIB 1 {@link GridDefinitionSection}
     * @throws GridDefinitionLookupException
     */
    public GridDefinitionSection<?> generateDefinitionSection(
            final IGridDefinition gridDefinition)
            throws GridDefinitionLookupException {
        final int dtReprType = gridDefinition.getDataRepresentationType();
        GridDefinitionSection<IGridDefinition> gds = lookupGridDefinition(dtReprType);
        gds.generateSection(gridDefinition);
        /*
         * Determine the size of the gds.
         * 
         * 1) LENGTH = 32 FOR LAT/LON, GNOMIC, GAUSIAN LAT/LON, POLAR
         * STEREOGRAPHIC, SPHERICAL HARMONICS 2) LENGTH = 42 FOR MERCATOR,
         * LAMBERT, TANGENT CONE 3) LENGTH = 178 FOR MERCATOR, LAMBERT, TANGENT
         * CONE. However, the number of grids that gribit has actually been
         * created to write is presently limited.
         */
        int gdsSize = SUPPORTED_GDS_SIZE_32;
        /*
         * check for extended grids.
         */
        if (dtReprType == 0 && gridDefinition.getNumVertCoords() == 0
                && gridDefinition.getPvPL255() != 255
                && gridDefinition instanceof AbstractLatLonCommon10) {
            gdsSize = ((AbstractLatLonCommon10) gridDefinition)
                    .getNumberLonPoints() * 2 + gdsSize;
        } else if (dtReprType == 1 || dtReprType == 3 || dtReprType == 13) {
            gdsSize = SUPPORTED_GDS_SIZE_42;
        }

        gds.setNumberBytes(gdsSize);
        return gds;
    }
}