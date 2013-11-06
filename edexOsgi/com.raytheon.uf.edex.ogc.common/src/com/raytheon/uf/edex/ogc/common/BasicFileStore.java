/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;

import org.apache.cxf.helpers.IOUtils;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Basic file system storage. Overlaps functionality with AbstractFsStore.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2013            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      Moved to configured
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BasicFileStore {

    protected static final char ESCAPE = '%';

    protected static final String SPECIAL_CHARS = ESCAPE + "./\\?*:|\"<>~";

    private final Object fileMutex = new Object();

    private final File storeLocation;

    /**
     * @param storeName
     */
    public BasicFileStore(String storeName) {
        this(findStore(storeName));
    }

    /**
     * @param storeLocation
     */
    public BasicFileStore(File storeLocation) {
        this.storeLocation = storeLocation;
        ensureStorage();
    }

    /**
     * Create storage location if necessary
     * 
     * @throws IllegalArgumentException
     *             if unable to create storage directory or if provided location
     *             is not a directory
     */
    private void ensureStorage() throws IllegalArgumentException {
        if (!storeLocation.exists()) {
            if (!storeLocation.mkdirs()) {
                throw new IllegalArgumentException(
                        "Unable to create store directory: " + storeLocation);
            }
        }
        if (!storeLocation.isDirectory()) {
            throw new IllegalArgumentException(
                    "Provided storage location is not a directory: "
                            + storeLocation);
        }
        if (!storeLocation.canWrite()) {
            throw new IllegalArgumentException(
                    "Unable to write to storage directory: " + storeLocation);
        }
    }

    /**
     * @param directoryName
     * @return
     */
    protected static File findStore(String directoryName) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticCONFIGURED = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.CONFIGURED);
        return pathMgr.getFile(edexStaticCONFIGURED, directoryName);
    }

    /**
     * @param id
     * @return
     */
    private File createPath(String id) {
        return new File(storeLocation, encode(id));
    }

    /**
     * Get file from store
     * 
     * @param id
     * @return null if file isn't in store
     */
    public File getFile(String id) {
        File rval = createPath(id);
        if (!rval.exists()) {
            return null;
        }
        return rval;
    }

    /**
     * Creates an empty file in store. This is used when data is written to the
     * file outside of the store. This cannot be used with
     * {@link #store(String, InputStream)}
     * 
     * @param id
     * @return
     * @throws IOException
     *             if file already exists in store
     */
    public File reserveFile(String id) throws IOException {
        File f = createPath(id);
        synchronized (fileMutex) {
            if (!f.createNewFile()){
                throw new IOException("File already exists in store with id: "
                        + id);
            }
        }
        return f;
    }

    /**
     * Delete file from store
     * 
     * @param id
     */
    public void remove(String id) {
        File f = createPath(id);
        synchronized (fileMutex) {
            if (f.exists()) {
                f.delete();
            }
        }
    }

    /**
     * Put data in store. This should not be used with
     * {@link #reserveFile(String)}
     * 
     * @param id
     * @param in
     * @throws IOException
     */
    public void store(String id, InputStream in) throws IOException {
        File f = reserveFile(id);
        FileOutputStream out = new FileOutputStream(f);
        try {
            IOUtils.copy(in, out);
        } finally {
            out.close();
        }
    }

    /**
     * Sanitize id for file system
     * 
     * @param id
     * @return
     */
    protected static String encode(String id) {
        StringBuilder sb = new StringBuilder();
        CharacterIterator iter = new StringCharacterIterator(id);
        for (char ch = iter.first(); ch != CharacterIterator.DONE; ch = iter
                .next()) {
            if (SPECIAL_CHARS.indexOf(ch) != -1) {
                sb.append(ESCAPE);
                sb.append(String.format("%2h", ch));
            } else {
                sb.append(ch);
            }
        }
        return sb.toString();
    }

    /**
     * de-sanitize file system name
     * 
     * @param encoded
     * @return
     */
    protected static String decode(String encoded) {
        StringBuilder sb = new StringBuilder();
        int index = 0;
        int prev;
        while (index < encoded.length()) {
            prev = index;
            index = encoded.indexOf(ESCAPE, prev);
            if (index < 0) {
                sb.append(encoded.substring(prev));
                break;
            }
            sb.append(encoded.substring(prev, index));
            String hex = encoded.substring(index + 1, index + 3);
            char charNum = (char) Integer.parseInt(hex, 16);
            sb.append(charNum);
            index = index + 3;
        }
        return sb.toString();
    }
}
