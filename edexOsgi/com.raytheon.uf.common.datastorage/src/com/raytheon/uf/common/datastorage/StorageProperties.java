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

package com.raytheon.uf.common.datastorage;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Options used that determine how data is stored in the datastore
 * 
 * <pre>
 * 
 *   SOFTWARE HISTORY
 *  
 *   Date         Ticket#     Engineer    Description
 *   ------------ ----------  ----------- --------------------------
 *   Feb 8, 2007              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@DynamicSerialize
public class StorageProperties implements Cloneable, ISerializableObject {

    /**
     * Compression types:
     * <UL>
     * <LI>NONE - No compression
     * <LI>ZLIB - standard libz compression (supported by h5dump, etc.)
     * <LI>LZF - LZF compression (much faster but less effective, not supported
     * by h5dump)
     * </UL>
     */
    public static enum Compression {
        NONE, ZLIB, LZF
    };

    /** The compression flag */
    @DynamicSerializeElement
    private Compression compression;

    /** Is the data chunked (required for compression) */
    @DynamicSerializeElement
    private boolean chunked;

    /** Is the data progressively downsampled */
    @DynamicSerializeElement
    private boolean downscaled;

    /**
     * Construct a default storage properties. Default is all features disabled.
     */
    public StorageProperties() {
        compression = Compression.NONE;
    }

    /**
     * @return the isChunked
     */
    public boolean isChunked() {
        return chunked;
    }

    /**
     * @param isChunked
     *            the isChunked to set
     */
    public void setChunked(boolean isChunked) {
        this.chunked = isChunked;
    }

    /**
     * @return the compression
     */
    public Compression getCompression() {
        return compression;
    }

    /**
     * @param compression
     *            the compression to set
     * 
     *            NOTE: chunking will also be turned on
     */
    public void setCompression(Compression compression) {
        Validate.notNull(compression, "Compression must not be null");

        this.compression = compression;
        if (compression != Compression.NONE) {
            setChunked(true);
        }
    }

    /**
     * @return the isDownscaled
     */
    public boolean isDownscaled() {
        return downscaled;
    }

    /**
     * @param isDownscaled
     *            the isDownscaled to set
     */
    public void setDownscaled(boolean isDownscaled) {
        this.downscaled = isDownscaled;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public StorageProperties clone() {
        StorageProperties sp = new StorageProperties();
        sp.chunked = chunked;
        sp.downscaled = downscaled;
        sp.compression = compression;
        return sp;
    }

}
