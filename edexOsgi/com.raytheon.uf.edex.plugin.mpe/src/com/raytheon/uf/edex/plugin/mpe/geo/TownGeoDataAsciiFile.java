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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.uf.common.xmrg.hrap.HrapUtil;
import com.raytheon.uf.edex.plugin.mpe.HrapGridFactor;
import org.locationtech.jts.geom.Coordinate;

/**
 * POJO representative of the structure of the town ascii mpe geo data file.
 * This class was written to be specific to a certain ascii geo data file
 * because there is not a common layout across all ascii geo data files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class TownGeoDataAsciiFile {

    public static final String FILE_NAME = "town.dat";

    private static final String TOWN_REGEX = "^(.+) +(.+) (.+)$";

    private static final Pattern TOWN_PATTERN = Pattern.compile(TOWN_REGEX);

    private static final int NAME_GROUP = 1;

    private static final int LAT_GROUP = 2;

    private static final int LON_GROUP = 3;

    private Set<Town> towns;

    public static TownGeoDataAsciiFile loadFile(final Path filePath)
            throws IOException, AsciiGeoDataInconsistentException {
        return loadFile(filePath, null);
    }

    public static TownGeoDataAsciiFile loadFile(final Path filePath,
            final HrapGridFactor hrapGridFactor) throws IOException {
        TownGeoDataAsciiFile townGeoData = new TownGeoDataAsciiFile();
        List<String> lines = Files.readAllLines(filePath);
        try {
            if (lines == null || lines.isEmpty()) {
                throw new AsciiGeoDataInconsistentException(
                        "The specified town geo data file: "
                                + filePath.toString() + " is empty.");
            }
            townGeoData.towns = new HashSet<>(lines.size(), 1.0f);
            for (String line : lines) {
                final Matcher matcher = TOWN_PATTERN.matcher(line);
                if (!matcher.matches()) {
                    throw new AsciiGeoDataInconsistentException(
                            "Encountered unparseable line: '" + line
                                    + "' in town geo data file: "
                                    + filePath.toString() + ".");
                }

                final String name = matcher.group(NAME_GROUP).trim();
                final String latText = matcher.group(LAT_GROUP);
                final float lat;
                try {
                    lat = Float.parseFloat(latText);
                } catch (NumberFormatException e) {
                    throw new AsciiGeoDataInconsistentException(
                            "Encountered unparseable latitude: " + latText
                                    + " in line: '" + line
                                    + "' in town geo data file: "
                                    + filePath.toString() + ".");
                }
                final String lonText = matcher.group(LON_GROUP);
                final float lon;
                try {
                    lon = Float.parseFloat(lonText);
                } catch (NumberFormatException e) {
                    throw new AsciiGeoDataInconsistentException(
                            "Encountered unparseable longitude: " + lonText
                                    + " in line: '" + line
                                    + "' in town geo data file: "
                                    + filePath.toString() + ".");
                }

                final Coordinate latLonCoord = new Coordinate(lon, lat);
                final Coordinate hrapCoord;
                try {
                    hrapCoord = HrapUtil.latLonToHrap(latLonCoord);
                } catch (HrapConversionException e) {
                    throw new AsciiGeoDataInconsistentException(
                            "Failed to convert lat/lon coordinate: "
                                    + latLonCoord.toString()
                                    + " to a hrap coordinate.",
                            e);
                }
                if (hrapGridFactor != null) {
                    hrapCoord.x *= hrapGridFactor.getNum();
                    hrapCoord.y *= hrapGridFactor.getNum();
                }
                townGeoData.towns.add(new Town(name, hrapCoord, latLonCoord));
            }
        } catch (AsciiGeoDataInconsistentException e) {
            throw new IOException("Failed to read ascii town geo data file: "
                    + filePath.toString() + ".", e);
        }
        return townGeoData;
    }

    public Set<Town> getTowns() {
        return towns;
    }

    public void setTowns(Set<Town> towns) {
        this.towns = towns;
    }
}