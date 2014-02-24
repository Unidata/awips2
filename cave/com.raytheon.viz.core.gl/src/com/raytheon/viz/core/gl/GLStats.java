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
package com.raytheon.viz.core.gl;

import java.nio.IntBuffer;
import java.util.Date;

import javax.media.opengl.GL;

import com.raytheon.viz.core.gl.internal.cache.ImageCache;
import com.raytheon.viz.core.gl.internal.cache.ImageCache.CacheType;

/**
 * 
 * Provides method for tracking graphics memory useage and logging if it becomes
 * low.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLStats {

    // how many seconds to wait before checking for high memory usage.
    private static final int CHECK_FREQ_SECONDS = 15;

    // How many seconds to wait between printings
    private static final int PRINT_FREQ_SECONDS = 600;

    // The minimum percentage of memory that must be used before printing.
    private static final int MEM_PRINT_THRESHOLD_PERCENT = 90;

    // NVidia extension constants.
    // http://developer.download.nvidia.com/opengl/specs/GL_NVX_gpu_memory_info.txt
    private static final String NVX_EXT_ID = "GL_NVX_gpu_memory_info";

    private static final int GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX = 0x9047;

    private static final int GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX = 0x9048;

    private static final int GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX = 0x9049;

    private static final int GPU_MEMORY_INFO_EVICTION_COUNT_NVX = 0x904A;

    private static final int GPU_MEMORY_INFO_EVICTED_MEMORY_NVX = 0x904B;

    // ATI extension constants
    // http://www.opengl.org/registry/specs/ATI/meminfo.txt
    private static final String ATI_EXT_ID = "GL_ATI_meminfo";

    private static final int VBO_FREE_MEMORY_ATI = 0x87FB;

    private static final int TEXTURE_FREE_MEMORY_ATI = 0x87FC;

    private static final int RENDERBUFFER_FREE_MEMORY_ATI = 0x87FD;

    private static long lastPrintTime;

    private static long lastCheckTime;

    private static int lastNvxEvictionCount = 0;

    public static void printStats(GL gl) {
        // test both check freq and print freq, the check freq should be fairly
        // low so as soon as low memory conditions are reached we will
        // report it so if it is a precursor to a crash it will be in the logs,
        // the print time will be significantly higher to avoid
        // spamming the logs if the user is operating normally with high memory
        // useage.
        long curTime = System.currentTimeMillis();
        if (curTime - lastCheckTime < CHECK_FREQ_SECONDS * 1000) {
            // don't check if it hasn't been very long
            return;
        }
        lastCheckTime = curTime;
        if (curTime - lastPrintTime < PRINT_FREQ_SECONDS * 1000) {
            // don't check if we printed to recently
            return;
        }
        boolean lowMem = false;
        StringBuilder output = new StringBuilder(1024);
        output.append("-High Graphics Memory usage has been detected.\n");
        output.append("-Here are some statisitics that might help with that.\n");
        lowMem |= getSystemStats(output);
        lowMem |= getImageCacheStats(output);
        lowMem |= getNvidiaStats(gl, output);
        lowMem |= getAtiStats(gl, output);

        if (lowMem) {
            lastPrintTime = curTime;
            System.out.println(output.toString());
            System.out.println();
        }
    }

    protected static boolean getSystemStats(StringBuilder output) {
        output.append(String.format(" * Current time = %s\n",
                new Date().toString()));
        Runtime runtime = Runtime.getRuntime();
        output.append(String.format(" * JVM max memory = %dMB\n",
                runtime.maxMemory() / 1024 / 1024));
        output.append(String.format(" * JVM total memory = %dMB\n",
                runtime.totalMemory() / 1024 / 1024));
        output.append(String.format(" * JVM free memory = %dMB\n",
                runtime.freeMemory() / 1024 / 1024));
        return false;
    }

    protected static boolean getImageCacheStats(StringBuilder output) {
        ImageCache memCache = ImageCache.getInstance(CacheType.MEMORY);
        ImageCache texCache = ImageCache.getInstance(CacheType.TEXTURE);

        long memTotal = memCache.maxSize();
        long memUsed = memCache.size();
        long memPercent = memUsed * 100 / memTotal;

        long texTotal = texCache.maxSize();
        long texUsed = texCache.size();
        long texPercent = texUsed * 100 / texTotal;

        output.append(String.format(" * Memory Image Cache size = %dMB\n",
                memTotal / 1024 / 1024));
        output.append(String.format(" * Memory Image Cache used = %dMB\n",
                memUsed / 1024 / 1024));
        output.append(String.format(" * Memory Image Cache percent = %d%%\n",
                memPercent));
        output.append(String.format(" * Texture Image Cache size = %dMB\n",
                texTotal / 1024 / 1024));
        output.append(String.format(" * Texture Image Cache used = %dMB\n",
                texUsed / 1024 / 1024));
        output.append(String.format(" * Texture Image Cache percent = %d%%\n",
                texPercent));

        return texPercent > MEM_PRINT_THRESHOLD_PERCENT;
    }

    protected static boolean getNvidiaStats(GL gl, StringBuilder output) {
        if (gl.isExtensionAvailable(NVX_EXT_ID)) {
            IntBuffer tmp = IntBuffer.allocate(1);

            tmp.rewind();
            gl.glGetIntegerv(GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX, tmp);
            tmp.rewind();
            int nvxTotalAvailableMem = tmp.get();

            tmp.rewind();
            gl.glGetIntegerv(GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX, tmp);
            tmp.rewind();
            int nvxCurrentAvailableMem = tmp.get();

            tmp.rewind();
            gl.glGetIntegerv(GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX, tmp);
            tmp.rewind();
            int nvxDedicatedMem = tmp.get();

            tmp.rewind();
            gl.glGetIntegerv(GPU_MEMORY_INFO_EVICTION_COUNT_NVX, tmp);
            tmp.rewind();
            int nvxEvictionCount = tmp.get();

            tmp.rewind();
            gl.glGetIntegerv(GPU_MEMORY_INFO_EVICTED_MEMORY_NVX, tmp);
            tmp.rewind();
            int nvxEvictionMem = tmp.get();

            int nvxPercent = (nvxTotalAvailableMem - nvxCurrentAvailableMem)
                    * 100 / nvxTotalAvailableMem;

            output.append(String.format(
                    " * GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX = %dMB\n",
                    nvxDedicatedMem / 1024));
            output.append(String.format(
                    " * GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX = %dMB\n",
                    nvxTotalAvailableMem / 1024));
            output.append(String.format(
                    " * GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX = %dMB\n",
                    nvxCurrentAvailableMem / 1024));
            output.append(String.format(
                    " * GPU_MEMORY_INFO_EVICTION_COUNT_NVX = %d\n",
                    nvxEvictionCount));
            output.append(String.format(
                    " * GPU_MEMORY_INFO_EVICTED_MEMORY_NVX = %dMB\n",
                    nvxEvictionMem / 1024));
            output.append(String.format(" * NVX percent = %d%%\n", nvxPercent));

            int evictions = nvxEvictionCount - lastNvxEvictionCount;
            lastNvxEvictionCount = nvxEvictionCount;
            return nvxPercent > MEM_PRINT_THRESHOLD_PERCENT || evictions > 0;
        }
        return false;
    }

    // The ATI version is untested as I don't have an ATI GPU.
    protected static boolean getAtiStats(GL gl, StringBuilder output) {
        if (gl.isExtensionAvailable(ATI_EXT_ID)) {
            IntBuffer tmp = IntBuffer.allocate(4);
            gl.glGetIntegerv(VBO_FREE_MEMORY_ATI, tmp);
            tmp.rewind();
            int vboTotal = tmp.get();
            int vboLargest = tmp.get();
            int vboTotalAux = tmp.get();
            int vboLargestAux = tmp.get();

            tmp.rewind();
            gl.glGetIntegerv(TEXTURE_FREE_MEMORY_ATI, tmp);
            tmp.rewind();
            int texTotal = tmp.get();
            int texLargest = tmp.get();
            int texTotalAux = tmp.get();
            int texLargestAux = tmp.get();

            tmp.rewind();
            gl.glGetIntegerv(RENDERBUFFER_FREE_MEMORY_ATI, tmp);
            tmp.rewind();
            int rbTotal = tmp.get();
            int rbLargest = tmp.get();
            int rbTotalAux = tmp.get();
            int rbLargestAux = tmp.get();

            output.append(String.format(" * VBO_FREE_MEMORY total: %dMB\n",
                    vboTotal / 1024));
            output.append(String.format(" * VBO_FREE_MEMORY largest: %dMB\n",
                    vboLargest / 1024));
            output.append(String.format(" * VBO_FREE_MEMORY total aux: %dMB\n",
                    vboTotalAux / 1024));
            output.append(String.format(
                    " * VBO_FREE_MEMORY largest aux: %dMB\n",
                    vboLargestAux / 1024));
            output.append(String.format(" * TEXTURE_FREE_MEMORY total: %dMB\n",
                    texTotal / 1024));
            output.append(String
                    .format(" * TEXTURE_FREE_MEMORY largest: %dMB\n",
                            texLargest / 1024));
            output.append(String.format(
                    " * TEXTURE_FREE_MEMORY total aux: %dMB\n",
                    texTotalAux / 1024));
            output.append(String.format(
                    " * TEXTURE_FREE_MEMORY largest aux: %dMB\n",
                    texLargestAux / 1024));
            output.append(String
                    .format(" * RENDERBUFFER_FREE_MEMORY total: %dMB\n",
                            rbTotal / 1024));
            output.append(String.format(
                    " * RENDERBUFFER_FREE_MEMORY largest: %dMB\n",
                    rbLargest / 1024));
            output.append(String.format(
                    " * RENDERBUFFER_FREE_MEMORY total aux: %dMB\n",
                    rbTotalAux / 1024));
            output.append(String.format(
                    " * RENDERBUFFER_FREE_MEMORY largest aux: %dMB\n",
                    rbLargestAux / 1024));

            // I think this will print output if we have less that 10MB free for
            // any chunk of memory
            return vboTotal < 10240 || texTotal < 10240 || rbTotal < 10240;
        }
        return false;
    }

}
