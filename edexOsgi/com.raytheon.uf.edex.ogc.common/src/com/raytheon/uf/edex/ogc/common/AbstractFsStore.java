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
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Abstract File system storage. Provides basic storage location and id <-> file
 * name encoding methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2013            bclement     Initial creation
 * Aug 18, 2013  #2097    dhladky      Changed to configured
 * October, 2012  #2472   dhladky/bclement     Changed back to base
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AbstractFsStore {

    protected static final char ESCAPE = '%';

    protected static final String SPECIAL_CHARS = ESCAPE + "./\\?*:|\"<>~";

    protected final File storeLocation;

    /**
     * @param storeLocation
     */
    public AbstractFsStore(File storeLocation) {
        this.storeLocation = storeLocation;
        ensureStorage();
    }

    protected static File findStore(String directoryName) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        return pathMgr.getFile(context, directoryName);
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
