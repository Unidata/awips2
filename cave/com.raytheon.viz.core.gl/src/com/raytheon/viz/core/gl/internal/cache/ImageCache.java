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

package com.raytheon.viz.core.gl.internal.cache;

import javax.media.opengl.GL;

import com.raytheon.uf.common.util.cache.LRUCache;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.preferences.PreferenceConstants;
import com.raytheon.viz.core.gl.GLDisposalManager.GLDisposer;

/**
 * Cache for GLImages, one for memory and one fore texture
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2007            chammack    Initial Creation.
 * Jan  9, 2013 2680       mschenke    Changed size calculation to longs to
 *                                     avoid int overflow when using cache 
 *                                     size > 1GB
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ImageCache extends LRUCache<Object, IImageCacheable> implements
        java.io.Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * The defined cache types
     */
    public static enum CacheType {
        TEXTURE, MEMORY
    }

    /** Texture cache size */
    private static final int TEXTURE_CACHE_SIZE;
    static {
        int sz = Activator.getDefault().getPreferenceStore()
                .getInt(PreferenceConstants.P_TEXTURES_CARD);
        if (sz == 0)
            TEXTURE_CACHE_SIZE = 512;
        else
            TEXTURE_CACHE_SIZE = sz;
    }

    /** Memory cache size */
    private static final int MEMORY_CACHE_SIZE;
    static {
        MEMORY_CACHE_SIZE = TEXTURE_CACHE_SIZE / 2;
    }

    /** The instance of the texture cache */
    private static ImageCache textureCache;

    /** The instance of the memory cache */
    private static ImageCache memoryCache;

    /**
     * Get Singletons
     * 
     * Get the instance of the LRU cache the corresponds to the level of caching
     * desired
     * 
     * @param type
     *            the cache type of the LRU cache
     * @return the LRU cache
     */
    public static ImageCache getInstance(CacheType type) {
        if (type == CacheType.MEMORY) {
            if (memoryCache == null) {
                memoryCache = new ImageCache(1024L * 1024L * MEMORY_CACHE_SIZE);
            }
            return memoryCache;
        }

        if (type == CacheType.TEXTURE) {
            if (textureCache == null) {
                textureCache = new ImageCache(
                        1024L * 1024L * TEXTURE_CACHE_SIZE);
            }
            return textureCache;
        }

        return null;

    }

    /**
     * Constructor
     * 
     * @param maxSz
     * @param type
     */
    private ImageCache(long maxSz) {
        super(maxSz);
    }

    public void put(IImageCacheable image) {
        put(image, image);
    }

    @Override
    protected void removeItem(Item item) {
        super.removeItem(item);
        final IImageCacheable i = item.value;
        if (this == memoryCache) {
            i.disposeTextureData();
        } else if (this == textureCache) {
            new GLDisposer() {
                @Override
                protected void dispose(GL gl) {
                    i.disposeTexture();
                }
            }.dispose();
        }
    }
}
