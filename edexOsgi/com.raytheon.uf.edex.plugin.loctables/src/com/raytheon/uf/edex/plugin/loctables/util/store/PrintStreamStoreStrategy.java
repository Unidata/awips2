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
package com.raytheon.uf.edex.plugin.loctables.util.store;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class PrintStreamStoreStrategy  extends AbstractStoreStrategy {

    private static final String SUFFIX = "%s%04d.%s";
    
    private Log logger = LogFactory.getLog(getClass());

    private File file = null;
    
    private PrintStream pStream = null;
    
    private File path = null;
    
    private String name = null;
    
    private String ext = null;
    
    private int breakFile = -1;
    
    private int lineCount = 0;
    
    private int partCount = 0;
    
    /**
     * 
     * @param file
     */
    public PrintStreamStoreStrategy(File file) throws IOException {
        pStream = new PrintStream(file);
    }

    /**
     * 
     * @param file
     */
    public PrintStreamStoreStrategy(PrintStream stream) {
        pStream = stream;
    }
    
    /**
     * 
     * @param file
     */
    public PrintStreamStoreStrategy(File path, String name, String ext, int breakFile) {
        this.path = path;
        this.name = name;
        this.ext = ext;
        this.breakFile = breakFile;
    }
    
    /**
     * 
     * @param row
     * @return Was the store successful.
     * @see com.raytheon.uf.edex.plugin.loctables.util.store.RowStoreStrategy#store(com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow)
     */
    @Override
    public boolean store(ObStationRow row) {
        boolean stored = false;
        PrintStream stream = getStream();
        if((stream != null)&&(row != null)) {
            stream.println(row.toSQLInsertString());
            stored = true;
            lineCount++;
        }
        return stored;
    }
    
    private void closeStream() throws IOException {
        if(pStream != null) {
            pStream.close();
        }
    }
    
    /**
     * Closes the currently open PrintStream. If the PrintStream is not
     * open, no action occurs.
     * @see java.io.Closeable#close()
     */
    @Override
    public void close() throws IOException {
        closeStream();
    }

    /**
     * 
     * @return
     */
    private PrintStream getStream() {
        PrintStream stream = null;
        if(breakFile > 0) {
            if (lineCount > breakFile) {
                if (pStream != null) {
                    try {
                        closeStream();
                    } catch(IOException ioe) {
                        logger.error("Could not close stream", ioe);
                    }
                    pStream = null;
                }

                String s = String.format(SUFFIX, name, partCount++, ext);
                file = new File(path, s);
                try {
                    pStream = new PrintStream(file);
                } catch (IOException ioe) {
                    logger.error("Could not create stream for " + file, ioe);
                    pStream = null;
                }
                lineCount = 0;
                stream = pStream;
            } else {
                // create the stream if it hasn't been; first write
                if (pStream == null) {
                    String s = String.format(SUFFIX, name, partCount++,
                            ext);
                    file = new File(path, s);
                    try {
                        pStream = new PrintStream(file);
                    } catch (IOException ioe) {
                        logger
                                .error("Could not create stream for " + file,
                                        ioe);
                        pStream = null;
                    }
                }
                stream = pStream;
            }
        } else {
            stream = pStream;
        }
        return stream;
    }
    
}
