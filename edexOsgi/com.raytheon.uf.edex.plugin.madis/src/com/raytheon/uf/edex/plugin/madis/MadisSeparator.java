package com.raytheon.uf.edex.plugin.madis;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Madis record separation. 
 * <pre>
 *                     
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 5/18/13       753         dhladky    Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */


public class MadisSeparator {
  
    private static final Pattern regex = Pattern.compile(",");
    
    private String madisRoute;
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisSeparator.class);
    
    public MadisSeparator(String madisRoute) {
        this.madisRoute = madisRoute;
    }
    
    public void separate(byte[] inputData)
            throws DecoderException {
        
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
                
                while((line = bfReader.readLine()) != null) {
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
                            if (i != 0) {
                                // write to queue
                                sendObject(mio, madisRoute);
                                long time4 = System.currentTimeMillis();
                                statusHandler.handle(Priority.INFO,
                                        "MADIS separated record wrote: "+j+" "
                                                + (time4 - time3) + " ms");
                                j++;
                            }
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
                    sendObject(mio, madisRoute);
                    long time4 = System.currentTimeMillis();
                    statusHandler.handle(Priority.INFO,
                            "MADIS separated record wrote: "+j+" "
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
                        statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
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
     * Send the object
     * @param mio
     */
    private static void sendObject(MadisIngestObject mio, String route) {
        try {
            byte[] bytes = SerializationUtil.transformToThrift(mio);
            EDEXUtil.getMessageProducer().sendAsyncUri(
                    route, bytes);
        } catch (EdexException e) {
            statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

}
