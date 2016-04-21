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
package com.raytheon.viz.grid.rsc.general;

import java.lang.management.ManagementFactory;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.numeric.DataUtilities;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.sparse.SparseArray;
import com.raytheon.uf.common.numeric.sparse.SparseFloatArray;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;

/**
 * Manages grid memory. Parses command line options to determine how much direct
 * memory to use.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 25, 2014  2791     bsteffen    Initial creation
 * Aug 06, 2014  3499     bclement    added sparse array optimization
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridMemoryManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridMemoryManager.class);

    private static final String MAX_GRID_PROPERTY_NAME = "MaxDirectGridMemorySize";

    private static final String DATA_SIZE_PATTERN = "([0-9]+)([GgMmKk])";

    private static final String DATA_SIZE_ONLY_PATTERN = "^"
            + DATA_SIZE_PATTERN + "$";

    private static final String MAX_DIRECT_PATTERN = "^-XX:MaxDirectMemorySize="
            + DATA_SIZE_PATTERN + "$";

    /**
     * If only the Max Direct memory option is set then use this percentage of
     * the available direct memory.
     */
    private static final double DIRECT_MEMORY_PERCENTAGE = 0.75f;
    
    private static final int SPARSE_CHECK_THRESHOLD = Integer.getInteger(
            "grid.sparse.check.threshold", 100 * 1024 * 1024); // 100 MB

    private static final int SPARSE_ARRAY_BLOCK_SIZE = Integer.getInteger(
            "grid.sparse.block.size", SparseArray.DEFAULT_BLOCK_SIZE);

    private static final float SPARSE_FILL_VALUE = Float.NaN;

    private static GridMemoryManager instance;

    private final ReferenceQueue<ByteBuffer> refQueue = new ReferenceQueue<ByteBuffer>();

    private final Set<DirectBufferTrackingReference> livingRefs;

    private final AtomicLong remainingDirectMemory;

    public static synchronized GridMemoryManager getInstance() {
        if (instance == null) {
            instance = new GridMemoryManager();
        }
        return instance;
    }

    private GridMemoryManager() {
        livingRefs = Collections
                .synchronizedSet(new HashSet<DirectBufferTrackingReference>());
        long maxDirect = -1;
        long maxGrid = -1;
        Pattern p = Pattern.compile(MAX_DIRECT_PATTERN);
        for (String arg : ManagementFactory.getRuntimeMXBean()
                .getInputArguments()) {
            Matcher m = p.matcher(arg);
            if (m.matches()) {
                maxDirect = parseMemory(m);
                break;
            }
        }
        String maxGridString = System.getProperty(MAX_GRID_PROPERTY_NAME);
        if (maxGridString != null) {
            p = Pattern.compile(DATA_SIZE_ONLY_PATTERN);
            Matcher m = p.matcher(maxGridString);
            if (m.matches()) {
                maxGrid = parseMemory(m);
            } else {
                statusHandler.warn("Cannot parse MaxDirectGridMemorySize: "
                        + maxGridString);
            }
        }
        if ((maxGrid == -1 || maxGrid > maxDirect) && maxDirect != -1) {
            /* Use only some of direct memory, leave a little for anyone else. */
            maxGrid = (long) (maxDirect * DIRECT_MEMORY_PERCENTAGE);
        }
        this.remainingDirectMemory = new AtomicLong(maxGrid);
        statusHandler.debug("Using maximum of " + maxGrid
                + " bytes of direct memory for grids.");
    }

    /**
     * Attempt to make memory optimizations if applicable. Otherwise just return
     * data as is.
     * 
     * @param data
     * @return
     */
    public GeneralGridData manage(GeneralGridData data) {
        free();
        GridGeometry2D gridGeometry = data.getGridGeometry();
        GridEnvelope2D gridRange = gridGeometry.getGridRange2D();
        int numGridPoints = gridRange.width * gridRange.height;
        int sizeInBytes = numGridPoints * 4;
        if (data.isVector()) {
            sizeInBytes *= 2;
        }

        /*
         * TODO checking for memory optimizations could be moved to an external
         * class so things like GFE could take advantage of it
         */
        if (sizeInBytes > SPARSE_CHECK_THRESHOLD && isSparse(data)) {
            data = convertToSparse(data);
        } else if (hasAvailableDirectMemory(sizeInBytes)) {
            data = convertToDirect(data, sizeInBytes, numGridPoints);
        }

        return data;
    }

    /**
     * Loads grid data into direct memory buffers
     * 
     * @param data
     * @param sizeInBytes
     * @param numGridPoints
     * @return a new grid data object with direct memory data sources
     */
    private GeneralGridData convertToDirect(GeneralGridData data,
            int sizeInBytes, int numGridPoints) {
        GridGeometry2D gridGeometry = data.getGridGeometry();
        GridEnvelope2D gridRange = gridGeometry.getGridRange2D();
        try {
            ByteBuffer directBuffer = ByteBuffer.allocateDirect(sizeInBytes);
            livingRefs.add(new DirectBufferTrackingReference(directBuffer,
                    refQueue));
            FloatBuffer dataBuffer = directBuffer.asFloatBuffer();
            if (data.isVector()) {
                dataBuffer.position(0);
                dataBuffer.limit(numGridPoints);
                FloatBufferWrapper uWrapper = new FloatBufferWrapper(
                        dataBuffer.slice(), gridRange);
                dataBuffer.position(numGridPoints);
                dataBuffer.limit(numGridPoints * 2);
                FloatBufferWrapper vWrapper = new FloatBufferWrapper(
                        dataBuffer.slice(), gridRange);
                DataUtilities.copy(data.getUComponent(), uWrapper,
                        gridRange.width, gridRange.height);
                DataUtilities.copy(data.getVComponent(), vWrapper,
                        gridRange.width, gridRange.height);
                data = GeneralGridData.createVectorDataUV(gridGeometry,
                        uWrapper, vWrapper, data.getDataUnit());
            } else {
                FloatBufferWrapper wrapper = new FloatBufferWrapper(dataBuffer,
                        gridRange);
                DataUtilities.copy(data.getScalarData(), wrapper,
                        gridRange.width, gridRange.height);
                data = GeneralGridData.createScalarData(gridGeometry, wrapper,
                        data.getDataUnit());
            }
        } catch (Throwable e) {
            statusHandler.handle(Priority.DEBUG,
                    "Error using direct memory, falling back to heap.", e);
            remainingDirectMemory.addAndGet(sizeInBytes);
        }
        return data;
    }

    /**
     * @param sizeInBytes
     * @return true if there is enough direct memory for the provided grid size
     */
    private boolean hasAvailableDirectMemory(int sizeInBytes) {
        long r = remainingDirectMemory.get();
        boolean makeDirect = false;
        while (r > sizeInBytes && makeDirect == false) {
            if (remainingDirectMemory.compareAndSet(r, r - sizeInBytes)) {
                makeDirect = true;
            }
            r = remainingDirectMemory.get();
        }
        return makeDirect;
    }

    /**
     * @param data
     * @param gridRange
     * @return true if the data is mostly comprised of NaNs
     */
    private boolean isSparse(GeneralGridData data) {
        GridGeometry2D gridGeometry = data.getGridGeometry();
        GridEnvelope2D gridRange = gridGeometry.getGridRange2D();
        DataSource source;
        if (data.isVector()) {
            source = data.getUComponent();
        } else {
            source = data.getScalarData();
        }
        int nx = gridRange.width;
        int ny = gridRange.height;
        long nandCount = 0;
        long largestNandRun = Long.MIN_VALUE;
        long currentNandRun = 0;
        boolean inRun = false;
        for (int y = 0; y < ny; y += 1) {
            for (int x = 0; x < nx; x += 1) {
                if (Double.isNaN(source.getDataValue(x, y))) {
                    if (!inRun){
                        inRun = true;
                    }
                    nandCount += 1;
                    currentNandRun += 1;
                } else {
                    if (inRun){
                        inRun = false;
                        largestNandRun = Math.max(largestNandRun, currentNandRun);
                        currentNandRun = 0;
                    }
                }
            }
        }
        return largestNandRun > SPARSE_ARRAY_BLOCK_SIZE
                && nandCount > ((nx * ny) / 4);
    }

    /**
     * Loads data into a sparse arrays.
     * 
     * @param data
     * @return a new grid data object with sparse array data sources
     */
    private GeneralGridData convertToSparse(GeneralGridData data) {
        GridGeometry2D gridGeometry = data.getGridGeometry();
        GridEnvelope2D gridRange = gridGeometry.getGridRange2D();
        int nx = gridRange.width;
        int ny = gridRange.height;
        if (data.isVector()) {
            DataSource uSource = data.getUComponent();
            DataSource vSource = data.getVComponent();
            SparseFloatArray uDest = new SparseFloatArray(nx, ny,
                    SPARSE_FILL_VALUE, SPARSE_ARRAY_BLOCK_SIZE);
            SparseFloatArray vDest = new SparseFloatArray(nx, ny,
                    SPARSE_FILL_VALUE, SPARSE_ARRAY_BLOCK_SIZE);
            DataUtilities.copy(uSource, uDest, nx, ny);
            DataUtilities.copy(vSource, vDest, nx, ny);
            data = GeneralGridData.createVectorDataUV(gridGeometry, uDest,
                    vDest, data.getDataUnit());
        } else {
            DataSource source = data.getScalarData();
            SparseFloatArray dest = new SparseFloatArray(nx, ny,
                    SPARSE_FILL_VALUE, SPARSE_ARRAY_BLOCK_SIZE);
            DataUtilities.copy(source, dest, nx, ny);
            data = GeneralGridData.createScalarData(gridGeometry, dest,
                    data.getDataUnit());
        }
        return data;
    }

    private void free() {
        DirectBufferTrackingReference ref = (DirectBufferTrackingReference) refQueue
                .poll();
        while (ref != null) {
            remainingDirectMemory.addAndGet(ref.getSize());
            livingRefs.remove(ref);
            ref = (DirectBufferTrackingReference) refQueue.poll();
        }
    }

    private static long parseMemory(Matcher m) {
        long size = Integer.parseInt(m.group(1));
        char unit = m.group(2).charAt(0);
        switch (unit) {
        case 'G':
        case 'g':
            size *= 1024;
        case 'M':
        case 'm':
            size *= 1024;
        case 'K':
        case 'k':
            size *= 1024;
        }
        return size;
    }

    private static final class DirectBufferTrackingReference extends
            WeakReference<ByteBuffer> {

        private final int size;

        public DirectBufferTrackingReference(ByteBuffer referent,
                ReferenceQueue<ByteBuffer> q) {
            super(referent, q);
            this.size = referent.capacity();
        }

        public int getSize() {
            return size;
        }
    }

}
