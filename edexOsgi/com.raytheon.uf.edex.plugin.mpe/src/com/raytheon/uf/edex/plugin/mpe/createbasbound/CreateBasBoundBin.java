package com.raytheon.uf.edex.plugin.mpe.createbasbound;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.file.Paths;

import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.util.file.IOPermissionsHelper;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.plugin.mpe.CommonMPEUtils;
import org.locationtech.jts.geom.Coordinate;

/**
 * Create Basin Bound binary file.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2016  5856       skorolev    Initial creation
 * Aug 07, 2017 6334       bkowal      Directories are now created with 770 permissions and files 660.
 *
 * </pre>
 *
 * @author skorolev
 */
public class CreateBasBoundBin {

    private static final String SPACE = " ";

    /**
     * Creates binary file from ascii file
     * 
     * @param asciifName
     * @param binfName
     * @throws IOException
     * @throws EdexException
     */
    public static void createBinFile(String asciifName, String binfName)
            throws IOException, EdexException {

        try (BufferedReader reader = new BufferedReader(
                new FileReader(asciifName))) {

            int lenId = 9;
            int lenName = 21;

            try (OutputStream os = IOPermissionsHelper.getOutputStream(
                    Paths.get(binfName),
                    FilePermissionConstants.POSIX_FILE_SET);
                    BufferedOutputStream writer = new BufferedOutputStream(
                            os)) {
                String line;
                while ((line = reader.readLine()) != null) {
                    String[] lineItems = line.split(SPACE);

                    if (lineItems.length > 2) {
                        String id = lineItems[0];
                        String name = lineItems[1];
                        Integer order = Integer.valueOf(lineItems[2]);
                        Integer npts = Integer.valueOf(lineItems[3]);

                        // write in the binary file
                        writer.write(stringToBytes(id, lenId));
                        writer.write(stringToBytes(name, lenName));
                        writer.write(intToBytes(order));
                        writer.write(intToBytes(npts));

                        // read coordinates
                        for (int i = 0; i < npts; i++) {
                            line = reader.readLine();

                            String[] mapItems = line.split(SPACE);
                            Float lat = Float.valueOf(mapItems[0]);
                            Float lon = Float.valueOf(mapItems[1]);
                            Coordinate hrap = CommonMPEUtils
                                    .convertLatLonToHrapByReference(lon, lat);
                            writer.write(doubleToBytes(hrap.x));
                            writer.write(doubleToBytes(hrap.y));
                        }
                    } else {
                        continue;
                    }
                }
            }
        } catch (FileNotFoundException e) {
            throw new EdexException("Error opening input file " + asciifName,
                    e);
        }
    }

    /**
     * Converts string to bytes.
     * 
     * @param str
     * @param bufferSize
     * @return
     * @throws IOException
     */
    public static byte[] stringToBytes(String str, int bufferSize)
            throws IOException {
        ByteBuffer buffer = ByteBuffer.allocate(bufferSize)
                .order(ByteOrder.nativeOrder());
        buffer.put(str.getBytes());
        return buffer.array();
    }

    /**
     * Converts double to 4 bytes binary.
     * 
     * @param x
     * @return
     * @throws IOException
     */
    public static byte[] doubleToBytes(double x) throws IOException {
        ByteBuffer buffer = ByteBuffer.allocate(4)
                .order(ByteOrder.nativeOrder());
        int intBytes = Float.floatToRawIntBits((float) x);
        buffer.putInt(intBytes);
        return buffer.array();
    }

    /**
     * Converts integer to 4 bytes.
     * 
     * @param value
     * @return
     * @throws IOException
     */
    public static byte[] intToBytes(int value) throws IOException {
        ByteBuffer buffer = ByteBuffer.allocate(4)
                .order(ByteOrder.nativeOrder());
        buffer.putInt(value);
        return buffer.array();
    }
}
