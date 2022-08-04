package com.raytheon.uf.edex.plugin.mpe.createbasbound;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Create Basin Bound ascii file.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2016  5856       skorolev     Initial creation
 * Aug 07, 2017 6334       bkowal       Directories are now created with 770 permissions and files 660.
 *
 * </pre>
 *
 * @author skorolev
 */
public class CreateBasBoundAscii {

    private static final String TOKENSEP = "\t,() \012";

    private static final String BASN = "BASN";

    private static final String ENGL = "ENGL";

    private static final String METR = "METR";

    private static final String AREA = "AREA";

    private static final String ELEV = "ELEV";

    private static final String END = "END";

    /**
     * Creates Ascii File
     * 
     * @param nwsrfsFname
     * @param asciiFname
     * @throws IOException
     * @throws EdexException
     */
    public static void createAsciiFile(String nwsrfsFname, String asciiFname)
            throws IOException, EdexException {

        int numPairs = 0;
        boolean firstLine = true;
        boolean endOfPts = false;
        int order = -1;

        // open file for reading
        try (BufferedReader br = new BufferedReader(
                new FileReader(nwsrfsFname))) {
            if (br.read() == -1) {
                throw new EdexException("Input file \n" + nwsrfsFname
                        + " is empty. \nOutput file " + asciiFname
                        + " won't be created.");
            }

            // open file for writing
            try (BufferedWriter bw = new BufferedWriter(
                    new FileWriter(asciiFname))) {

                String id = null;
                String desc = null;
                String line = null;
                String token = null;
                List<Double> lat = new ArrayList<>();
                List<Double> lon = new ArrayList<>();

                /* reinitialize variables before processing the next basin */
                firstLine = false;
                endOfPts = false;
                // Read line by line loop
                while ((line = br.readLine()) != null) {
                    if (line.isEmpty()) {
                        continue;
                    }
                    StringTokenizer strtok = new StringTokenizer(line,
                            TOKENSEP);
                    /*
                     * Check if the previous basin data is not written to file.
                     * in case there is no "AREA", "ELEV" or "END" at the end
                     */
                    if (strtok.countTokens() > 2) {
                        firstLine = true;

                        if (numPairs > 0) {
                            writeBasin(id, desc, order, numPairs, lat, lon, bw,
                                    asciiFname);
                            numPairs = 0;
                            endOfPts = false;
                        }
                        numPairs = 0;
                        endOfPts = false;
                    }
                    // read first token
                    token = strtok.nextToken();
                    if (firstLine) {
                        // skip BASN, ENGL and METR
                        if (BASN.equals(token) || ENGL.equals(token)
                                || METR.equals(token)) {
                            if (strtok.hasMoreTokens()) {
                                token = strtok.nextToken();
                            }
                        }
                        // get basin id
                        id = token;
                        // get descriptor
                        if (strtok.hasMoreTokens()) {
                            desc = strtok.nextToken();
                        }
                        // end of fist line, go to next line
                        if (firstLine) {
                            firstLine = false;
                            continue;
                        }
                    }

                    if (AREA.equals(token) || ELEV.equals(token)
                            || END.equals(token)) {
                        endOfPts = true;
                    }

                    // Process the lat-lon data for the line .
                    if (!endOfPts) {
                        lat.add(numPairs, Double.valueOf(token));
                        token = strtok.nextToken();
                        lon.add(numPairs, Double.valueOf(token));
                        numPairs++;
                    }
                }

                // write file when finished
                writeBasin(id, desc, order, numPairs, lat, lon, bw, asciiFname);
                /* reinitialize variables before processing the next basin */
                firstLine = false;
                numPairs = 0;
                endOfPts = false;
                /* end of while loop - finished with file */
            }
        } catch (FileNotFoundException e) {
            throw new EdexException("Error opening file " + asciiFname, e);
        }

        IOPermissionsHelper.applyFilePermissions(Paths.get(nwsrfsFname),
                FilePermissionConstants.POSIX_FILE_SET);
    }

    /**
     * Write Basin data.
     * 
     * @param id
     * @param name
     * @param order
     * @param numPairs
     * @param lat
     * @param lon
     * @param bw
     * @throws EdexException
     */
    public static void writeBasin(String id, String name, int order,
            int numPairs, List<Double> lat, List<Double> lon, BufferedWriter bw,
            String file) throws EdexException {
        /*
         * "name" not currently used by subsequent programs - replace with
         * standard holder.
         */
        name = "XXX";
        // pair %s %s %d %d\n
        StringBuilder pair = new StringBuilder(id);
        pair.append(" ").append(name);
        pair.append(" ").append(order);
        pair.append(" ").append(numPairs);
        pair.append("\n");
        for (int i = 0; i < numPairs; i++) {
            // %.4f %.4f\n , lat, lon "%.2f", floatValue
            pair.append(String.format("%.1f", lat.get(i))).append(" ");
            pair.append(String.format("%.1f", lon.get(i))).append("\n");
        }
        // write info for one basin to the file
        try {
            bw.write(pair.toString());
        } catch (IOException e) {
            throw new EdexException("Error writing file " + file
                    + " in CreateBasBoundAscii.java", e);
        }
    }
}
