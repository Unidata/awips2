package com.raytheon.uf.edex.plugin.madis;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.UUID;
import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Madis record separation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 5/18/13       753         dhladky    Initial creation
 * 6/17/13       2113        dhladky    QPID memory usage alleviation
 * 6/21/13       2129        dhladky    add mkdirs to create directory
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class MadisSeparator {

    private static final Pattern regex = Pattern.compile(",");

    public static final String pathPrefix = EDEXUtil.getEdexData()
            + File.separatorChar + "madis" + File.separatorChar;

    private static final String pathSuffix = ".madis";

    private String madisRoute;

    private int timeback;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisSeparator.class);

    public MadisSeparator(String madisRoute, int timeback) {
        this.madisRoute = madisRoute;
        this.timeback = timeback;

        File file = new File(pathPrefix);
        if (!file.isDirectory()) {
            if (!file.mkdirs()) {
                throw new IllegalArgumentException(
                        "Couldn't create madis drop directory, : " + pathPrefix);
            }
        }
    }

    public void separate(byte[] inputData) throws DecoderException {

        InputStream is = null;
        BufferedReader bfReader = null;
        long time = System.currentTimeMillis();

        if (inputData != null) {

            is = new ByteArrayInputStream(inputData);
            bfReader = new BufferedReader(new InputStreamReader(is));
            String line = null;
            String[] headerType = null;
            MadisIngestObject mio = null;
            String headerLine = null;
            int i = 0;

            try {

                long time3 = 0l;
                int j = 1;

                while ((line = bfReader.readLine()) != null) {
                    // Get the file type, D or F
                    if (i == 0) {
                        time3 = System.currentTimeMillis();
                        headerLine = line;
                        headerType = getHeaderType(headerLine);
                        mio = new MadisIngestObject(headerType);
                        statusHandler.handle(Priority.INFO, "Recieved : "
                                + headerType.length + " column file");
                    } else {
                        // breaks data into chunks of 50,000 lines each
                        if (i % 50000 == 0) {
                            // write to queue and filesystem
                            sendFile(mio);
                            long time4 = System.currentTimeMillis();
                            statusHandler.handle(Priority.INFO,
                                    "MADIS separated record wrote: " + j + " "
                                            + (time4 - time3) + " ms");
                            j++;
                            // reset everything
                            time3 = System.currentTimeMillis();
                            mio = new MadisIngestObject(headerType);
                        } else {
                            mio.addLine(line);
                        }
                    }
                    i++;
                }
                // flush the last record out
                if (mio != null && !mio.getLines().isEmpty()) {
                    // write to queue and filesystem
                    sendFile(mio);
                    long time4 = System.currentTimeMillis();
                    statusHandler.handle(Priority.INFO,
                            "MADIS separated record wrote: " + j + " "
                                    + (time4 - time3) + " ms");
                }

            } catch (IOException e) {
                statusHandler.handle(Priority.ERROR,
                        "Could not open MADIS CSV file!", e);
            } finally {
                if (bfReader != null) {
                    try {
                        bfReader.close();
                    } catch (IOException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        }

        long time2 = System.currentTimeMillis();
        statusHandler.handle(Priority.INFO, "MADIS separation total: "
                + (time2 - time) + " ms");
    }

    /**
     * Gets the correct MADIS header type
     * 
     * @param firstLine
     * @return
     */
    private static String[] getHeaderType(String firstLine) {

        String[] commaSepList = regex.split(firstLine);

        if (commaSepList.length == MadisDecoder.typeDHeader.length) {
            return MadisDecoder.typeDHeader;
        }

        else if (commaSepList.length == MadisDecoder.typeFHeader.length) {
            return MadisDecoder.typeFHeader;

        } else {
            throw new UnsupportedOperationException(
                    "Unknown format for MADIS CSV file! " + commaSepList);
        }
    }

    /**
     * Writes the object to the File System
     * 
     * @param mio
     */
    private static void sendObject(MadisIngestObject mio, String path)
            throws Exception {
        FileOutputStream fos = null;

        try {
            File file = new File(path);
            file.createNewFile();
            fos = new FileOutputStream(file);
            SerializationUtil.transformToThriftUsingStream(mio, fos);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "Couldn't create file", e);
            throw new Exception("Unable to write File, FileNotFound!", e);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Serialization exception writing file", e);
            throw new Exception("Unable to write File, Serialization!", e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "IO Exception creating file", e);
            throw new Exception("Unable to write File, IO!", e);
        } finally {
            if (fos != null) {
                try {
                    fos.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problem closing the stream!", e);
                }
            }
        }
    }

    /**
     * Send the path to QPID
     * 
     * @param path
     * @param route
     */
    private static void sendPath(String path, String route) throws Exception {
        try {
            EDEXUtil.getMessageProducer().sendAsyncUri(route, path);
        } catch (EdexException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            throw new Exception("Unable to send Path message, EdexException!",
                    e);
        }
    }

    /**
     * Get the file from the path
     * 
     * @param path
     * @param route
     */
    public static MadisIngestObject getObject(String path) {

        FileInputStream fis = null;
        MadisIngestObject mio = null;

        try {
            fis = new FileInputStream(new File(path));
            mio = SerializationUtil.transformFromThrift(
                    MadisIngestObject.class, fis);

        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "Couldn't find the file", e);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't de-serialize the file", e);
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problem closing the stream!", e);
                }
            }
        }

        return mio;
    }

    /**
     * Gets the filePath and sends to queue and disk
     * 
     * @param mio
     */
    private void sendFile(MadisIngestObject mio) {

        StringBuilder filePath = new StringBuilder();
        filePath.append(pathPrefix).append(UUID.randomUUID().toString())
                .append(pathSuffix);
        String path = filePath.toString();
        try {
            sendObject(mio, path);
            sendPath(path, madisRoute);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Could not write file or place message on queue!", e);
        }
    }

    /**
     * Cleans up any orphaned files that might be hanging around
     */
    public void fileCleaner() {

        statusHandler.handle(Priority.INFO, "Checking for orphaned files.");
        // Checking for orphaned files in the madis drop directory
        try {
            long currentTimeMinusOne = System.currentTimeMillis()
                    - (timeback * (TimeUtil.MILLIS_PER_HOUR));
            File madisDir = new File(pathPrefix);
            if (madisDir.isDirectory()) {
                File[] files = madisDir.listFiles();
                for (File file : files) {
                    if (file.lastModified() < currentTimeMinusOne) {
                        file.delete();
                        statusHandler.handle(Priority.WARN,
                                "Deleting orphaned file! " + file.getName());
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Coudn't check for orphaned files." + e);
        }
    }

}
