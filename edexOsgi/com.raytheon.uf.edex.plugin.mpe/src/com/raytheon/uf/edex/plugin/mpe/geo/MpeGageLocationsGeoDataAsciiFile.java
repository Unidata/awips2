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
package com.raytheon.uf.edex.plugin.mpe.geo;

import java.util.Map;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Scanner;
import java.util.Collections;
import java.util.HashMap;

/**
 * POJO representative of the structure of the gage locations mpe geo data file.
 * This class was written to be specific to a certain ascii geo data file
 * because there is not a common layout across all ascii geo data files. Based
 * on: hpe_fieldgen/TEXT/get_loc_latlon.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class MpeGageLocationsGeoDataAsciiFile {

    public static final String FILE_NAME = "mpe_gage_locations";

    private Map<String, GageLocation> gageLocationMap;

    public static MpeGageLocationsGeoDataAsciiFile loadFile(final Path filePath)
            throws IOException {
        MpeGageLocationsGeoDataAsciiFile gageLocationsGeoData = new MpeGageLocationsGeoDataAsciiFile();
        try (final Scanner scanner = new Scanner(filePath.toFile())) {
            /*
             * First line should contain the number of gage location entries in
             * the file.
             */
            if (!scanner.hasNextInt()) {
                throw new AsciiGeoDataInconsistentException(
                        "Gage locations geo data file: " + filePath.toString()
                                + " does not start with the number of gage location entries.");
            }
            final int numEntries = scanner.nextInt();
            if (numEntries == 0) {
                /*
                 * File does not contain any gage locations. However, it has
                 * been declared in the file.
                 */
                return gageLocationsGeoData;
            }
            gageLocationsGeoData.gageLocationMap = new HashMap<>(numEntries,
                    1.0f);
            for (int i = 0; i < numEntries; i++) {
                /*
                 * Next should be a lid.
                 */
                if (!scanner.hasNext()) {
                    throw new AsciiGeoDataInconsistentException(
                            "Failed to find the expected lid at the beginning of line "
                                    + (i + 2)
                                    + " in the gage locations geo data file: "
                                    + filePath.toString() + ".");
                }
                final String lid = scanner.next();

                /*
                 * Next should be a lat.
                 */
                if (!scanner.hasNextFloat()) {
                    throw new AsciiGeoDataInconsistentException(
                            "Failed to find the expected latitude as the second element of line "
                                    + (i + 2)
                                    + " in the gage locations geo data file: "
                                    + filePath.toString() + ".");
                }
                final float lat = scanner.nextFloat();

                /*
                 * Next should be a lon.
                 */
                if (!scanner.hasNextFloat()) {
                    throw new AsciiGeoDataInconsistentException(
                            "Failed to find the expected longitude as the third element of line "
                                    + (i + 2)
                                    + " in the gage locations geo data file: "
                                    + filePath.toString() + ".");
                }
                final float lon = scanner.nextFloat();

                GageLocation gageLocation = new GageLocation(lid, lon, lat);
                gageLocationsGeoData.gageLocationMap.put(gageLocation.getLid(),
                        gageLocation);
            }
        } catch (AsciiGeoDataInconsistentException e) {
            throw new IOException(
                    "Failed to read ascii gage location geo data file: "
                            + filePath.toString() + ".",
                    e);
        }
        return gageLocationsGeoData;
    }

    public Map<String, GageLocation> getGageLocationMap() {
        if (gageLocationMap == null) {
            return Collections.emptyMap();
        }
        return gageLocationMap;
    }

    public void setGageLocationMap(Map<String, GageLocation> gageLocationMap) {
        this.gageLocationMap = gageLocationMap;
    }
}