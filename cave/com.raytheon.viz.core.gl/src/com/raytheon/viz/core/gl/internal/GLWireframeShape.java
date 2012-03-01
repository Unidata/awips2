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

package com.raytheon.viz.core.gl.internal;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.core.gl.GrowableBuffer;
import com.raytheon.viz.core.gl.IGLTarget;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * High performance shape. Implements a shape as a series of line segments
 * 
 * <P>
 * Segments should be appended using addLineSegment() If shape is immutable,
 * compile() can be called to flush out temporary memory.
 * 
 * <P>
 * This implementation is useful because it provides a seamless interface to
 * using native memory. The native memory is allocated outside of the JVM and
 * provides fast memory access to JNI-enabled APIs such as OpenGL.
 * 
 * <pre>
 * 
 *                  SOFTWARE HISTORY
 *                 
 *                  Date          Ticket#     Engineer    Description
 *                  ------------    ----------  ----------- --------------------------
 *                  7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class GLWireframeShape implements IWireframeShape {

    /** Intermediate storage for line start position */
    private List<Integer> theLineStarts;

    /** Intermediate storage for line length */
    private List<Integer> theLineLengths;

    /** The number of vertices */
    private int theVertexCount;

    /** The vertex buffer as a FloatBuffer */
    private GrowableBuffer<FloatBuffer> theVertexBuffer;

    /** The base level */
    private LODLevel baseLevel;

    /** Does shape need compile? */
    private boolean needsCompile;

    /** Is shape mutable? */
    private boolean isMutable;

    /** The levels of disclosure */
    private float[] theLevels;

    /** The level of disclosure threshold */
    private float theLevelThreshold;

    /** Is the shape ready to be drawn? */
    private boolean isDrawable;

    /** The map descriptor */
    private IDescriptor theDescriptor;

    private GeneralGridGeometry theGeometry;

    /** The width of the world */
    private int theWorldWidth;

    /** The height of the world */
    private int worldHeight;

    private int vboID;

    private IGLTarget vboAssociatedTarget;

    private boolean isSpatiallyChopped;

    private IExtent coverage;

    private int initialSize = 50000;

    /** Map of labels to draw */
    private Map<double[], String> labelMap;

    private class LODLevel {

        /** The line length buffer as an IntBuffer */
        public GrowableBuffer<IntBuffer> lineLengthBuffer;

        /** The Level of Disclosure index buffer as an IntBuffer */
        public GrowableBuffer<IntBuffer> lodIndexBuffers;

        public IExtent coverage;

        public LODLevel[] children;

        private int lodCount;

        private float lastX;

        private float lastY;

        private int numLines;

        private int lastPointOutsideLODSemaphore;

        private boolean lineStartedFlag;

        private int pts;

    }

    /**
     * Construct an outline shape with a given MapDataSet to project into and a
     * flag specifying whether the shape is mutable
     * 
     * 
     * @param descriptor
     *            the map descriptor
     * @param mutableFlag
     *            whether the wireframe is mutable
     */
    protected GLWireframeShape(IDescriptor descriptor, boolean mutableFlag) {
        this(descriptor, null, mutableFlag, 0.0f, false, null);
    }

    protected GLWireframeShape(GeneralGridGeometry geom, boolean mutableFlag) {
        this(null, geom, mutableFlag, 4.0f, false, null);
    }

    /**
     * Construct an outline shape with a given MapDataSet to project into and a
     * flag specifying whether the shape is mutable
     * 
     * 
     * @param descriptor
     *            the map descriptor
     * @param mutableFlag
     *            whether the wireframe is mutable
     * @param threshold
     */
    protected GLWireframeShape(IDescriptor descriptor, boolean mutableFlag,
            float threshold) {
        this(descriptor, null, mutableFlag, threshold, false, null);
    }

    protected GLWireframeShape(GeneralGridGeometry geom, boolean mutableFlag,
            float threshold) {
        this(null, geom, mutableFlag, threshold, false, null);
    }

    /**
     * Construct an outline shape with a given MapDataSet to project into and a
     * flag specifying whether the shape is mutable
     * 
     * 
     * @param descriptor
     *            the map descriptor
     * @param geom
     *            the geometry
     * @param mutableFlag
     *            whether the wireframe is mutable
     * @param threshold
     *            the threshold for decimation
     * @param spatialChopFlag
     *            whether spatial region chopping should be done
     */
    protected GLWireframeShape(IDescriptor descriptor,
            GeneralGridGeometry geom, boolean mutableFlag, float threshold,
            boolean spatialChopFlag, IExtent extent) {
        theLineStarts = new ArrayList<Integer>(1000);
        theLineLengths = new ArrayList<Integer>(1000);
        theVertexCount = 0;
        isMutable = mutableFlag;
        theLevels = new float[] { 1.0f, 0.05f };
        theLevelThreshold = threshold;
        isDrawable = false;
        theDescriptor = descriptor;
        if (geom == null) {
            theGeometry = descriptor.getGridGeometry();
        } else {
            theGeometry = geom;
        }
        worldHeight = theGeometry.getGridRange().getSpan(1);
        theWorldWidth = theGeometry.getGridRange().getSpan(0);
        isSpatiallyChopped = spatialChopFlag;
        coverage = extent;
    }

    protected GLWireframeShape() {
        needsCompile = false;
    }

    /**
     * @param vertexCount
     *            the vertexCount to set
     */
    protected void setVertexCount(int vertexCount) {
        theVertexCount = vertexCount;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IOutlineShape#addLineSegment(com.raytheon.viz.core
     * .LatitudeLongitude[])
     */
    public void addLineSegment(Coordinate[] latLong) {
        if (latLong.length == 0) {
            return;
        }

        if (theDescriptor == null) {
            throw new UnsupportedOperationException(
                    "Cannot add coordinate line segment to a wireframe shape that does not have a MapDescriptor.");
        }

        int startVertexCount = theVertexCount;

        double[][] points = new double[latLong.length][3];
        for (int i = 0; i < latLong.length; i++) {
            points[i][0] = MapUtil.correctLon(latLong[i].x);
            points[i][1] = MapUtil.correctLat(latLong[i].y);
            if (Double.isNaN(latLong[i].z)) {
                points[i][2] = 0.0;
            } else {
                points[i][2] = latLong[i].z;
            }
        }

        addVertices(points, true, startVertexCount);

        needsCompile = true;
        isDrawable = false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IWireframeShape#addLineSegment(double
     * [][])
     */
    @Override
    public void addLineSegment(double[][] screenCoordinates) {
        int startVertexCount = theVertexCount;

        addVertices(screenCoordinates, false, startVertexCount);

        needsCompile = true;
        isDrawable = false;
    }

    private void addVertices(double[][] input, boolean translate,
            int startVertexCount) {
        int verticesNeeded = input.length * 3;

        if (theVertexBuffer == null) {
            int sz = Math.max(initialSize, verticesNeeded);

            ByteBuffer vertexBufferAsBytes = ByteBuffer.allocateDirect(sz * 4);
            vertexBufferAsBytes.order(ByteOrder.nativeOrder());
            FloatBuffer vertexBufferTmp = vertexBufferAsBytes.asFloatBuffer();
            vertexBufferTmp.position(0);
            theVertexBuffer = new GrowableBuffer<FloatBuffer>(vertexBufferTmp);
        }

        double[] output = new double[3];
        double[] lastOutput = null;
        boolean lastCellInBounds = true;
        boolean currentlyInBounds = false;
        int addedVertices = 0;

        // Adjust pointer
        theVertexBuffer.position(theVertexCount * 3);

        if (input.length > 0) {
            theVertexBuffer.ensureCapacity(input.length * input[0].length);
        }

        for (int i = 0; i < input.length; i++) {
            if (input[i] == null) {
                continue;
            }

            if (translate) {
                if (theDescriptor != null
                        && theDescriptor instanceof IMapDescriptor) {
                    output = ((IMapDescriptor) theDescriptor)
                            .worldToPixel(input[i]);
                }
            } else {
                output = input[i];
            }

            double startX = theGeometry.getGridRange().getLow(0);
            double startY = theGeometry.getGridRange().getLow(1);
            currentlyInBounds = (output != null && output[0] > startX
                    && output[1] > startY && output[0] < startX + theWorldWidth && output[1] < startY
                    + worldHeight);
            if (currentlyInBounds) {
                if (!lastCellInBounds) {
                    if (lastOutput != null) {
                        // add the last cell if the last cell is out of bounds
                        theVertexCount++;
                        addedVertices++;
                        theVertexBuffer.put((float) lastOutput[0]);
                        theVertexBuffer.put((float) lastOutput[1]);
                        if (lastOutput.length == 3) {
                            theVertexBuffer.put((float) lastOutput[2]);
                        } else {
                            theVertexBuffer.put(1.0f);
                        }
                    }
                }
                theVertexCount++;
                addedVertices++;

                theVertexBuffer.put((float) output[0]);
                theVertexBuffer.put((float) output[1]);

                if (output.length == 3) {
                    theVertexBuffer.put((float) output[2]);
                } else {
                    theVertexBuffer.put(1.0f);
                }
            } else if (lastCellInBounds) {

                if (output != null) {
                    theVertexCount++;
                    addedVertices++;

                    theVertexBuffer.put((float) output[0]);
                    theVertexBuffer.put((float) output[1]);

                    if (output.length == 3) {
                        theVertexBuffer.put((float) output[2]);
                    } else {
                        theVertexBuffer.put(1.0f);
                    }
                }

                theLineStarts.add(startVertexCount);
                theLineLengths.add(addedVertices);

                startVertexCount += addedVertices;
                addedVertices = 0;

            }

            lastCellInBounds = currentlyInBounds;
            lastOutput = output;

        }

        if (addedVertices >= 1) {
            theLineStarts.add(startVertexCount);
            theLineLengths.add(addedVertices);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IOutlineShape#compile()
     */
    public void compile() {
        compileInternal();
        // Allow gc to occur
        if (theLineLengths != null) {
            theLineLengths.clear();
        }

        if (theLineStarts != null) {
            theLineStarts.clear();
        }

        theLineLengths = null;
        theLineStarts = null;
    }

    private void recursiveAllocateLOD(LODLevel lod, int lvl) {

        if (lod.lodIndexBuffers != null) {
            lod.lodIndexBuffers.dispose();
            lod.lodIndexBuffers = null;
        }

        if (lod.lineLengthBuffer != null) {
            lod.lineLengthBuffer.dispose();
            lod.lineLengthBuffer = null;
        }

        ByteBuffer lodIndexBuffersAsBytes = ByteBuffer
                .allocateDirect(10000 * 4);
        lodIndexBuffersAsBytes.order(ByteOrder.nativeOrder());
        lodIndexBuffersAsBytes.rewind();
        IntBuffer lodIndexBufferTmp = lodIndexBuffersAsBytes.asIntBuffer();
        lodIndexBufferTmp.rewind();
        lod.lodIndexBuffers = new GrowableBuffer<IntBuffer>(lodIndexBufferTmp);

        ByteBuffer lineLengthBufferAsBytes = ByteBuffer
                .allocateDirect(theLineLengths.size() * 4);
        lineLengthBufferAsBytes.order(ByteOrder.nativeOrder());
        IntBuffer lineLengthBufferTmp = lineLengthBufferAsBytes.asIntBuffer();
        lineLengthBufferTmp.position(0);
        lod.lineLengthBuffer = new GrowableBuffer<IntBuffer>(
                lineLengthBufferTmp);

        lvl++;
        if (lod.children != null) {
            for (LODLevel l : lod.children) {
                recursiveAllocateLOD(l, lvl);
            }
        }
    }

    private void compileInternal() {

        if (theVertexBuffer == null) {
            return;
        }

        int CHOPS = 9;

        this.baseLevel = new LODLevel();
        if (isSpatiallyChopped) {
            baseLevel.children = new LODLevel[CHOPS];
            for (int i = 0; i < CHOPS; i++) {
                baseLevel.children[i] = new LODLevel();
            }
        } else {
            baseLevel.children = new LODLevel[1];
            baseLevel.children[0] = new LODLevel();
        }

        recursiveAllocateLOD(baseLevel, 0);

        if (isSpatiallyChopped) {
            int dim = (int) Math.sqrt(CHOPS);
            int k = 0;
            baseLevel.coverage = this.coverage;

            double[] ul = new double[] { this.coverage.getMinX(),
                    this.coverage.getMinY() };
            double[] lr = new double[] { this.coverage.getMaxX(),
                    this.coverage.getMaxY() };

            double dx = (lr[0] - ul[0]) / dim;
            double dy = (lr[1] - ul[1]) / dim;
            double minX = ul[0];
            double minY = ul[1];

            for (int i = 0; i < dim; i++) {
                for (int j = 0; j < dim; j++) {
                    baseLevel.children[k].coverage = new PixelExtent(minX
                            + (dx * i), minX + (dx * (i + 1)), minY + (dy * j),
                            minY + (dy * (j + 1)));

                    k++;
                }
            }
        }

        theVertexBuffer.position(0);

        float curX;
        float curY;
        float d;

        for (int i = 0; i < theLineStarts.size(); i++) {
            int lineStart = theLineStarts.get(i);
            int lineLength = theLineLengths.get(i);

            curX = theVertexBuffer.getBuffer().get(lineStart * 3);
            curY = theVertexBuffer.getBuffer().get(lineStart * 3 + 1);

            // All first vertices go into base level
            baseLevel.lodIndexBuffers.put(lineStart);
            baseLevel.lodCount = 1;
            baseLevel.lastX = curX;
            baseLevel.lastY = curY;
            baseLevel.pts++;
            baseLevel.numLines++;

            for (LODLevel level : baseLevel.children) {
                level.lodCount = 0;

                if (isSpatiallyChopped) {

                    // TODO: determine intersection point w/
                    // LOD boundary for lines that next vertex enters
                    // LOD
                    if (level.coverage.contains(new double[] { curX, curY })) {
                        addPoint(curX, curY, lineStart, level);
                        level.lodIndexBuffers.put(lineStart);
                        level.lodCount++;
                        level.lastX = curX;
                        level.lastY = curY;
                        level.lineStartedFlag = true;
                    } else {
                        level.lastPointOutsideLODSemaphore++;
                        level.lastX = curX;
                        level.lastY = curY;
                    }
                } else {
                    addPoint(curX, curY, lineStart, level);

                }
            }

            int lastIndex = lineStart + lineLength - 1;

            for (int j = lineStart + 1; j < (lastIndex + 1); j++) {
                curX = theVertexBuffer.getBuffer().get(j * 3);
                curY = theVertexBuffer.getBuffer().get(j * 3 + 1);
                // Calc manhattan distance d = |x1 - x2| + |y1 - y2|

                // euclidean is way too slow
                // d = (float) Math.sqrt(Math.pow((lastX - curX), 2) +
                // Math.pow((lastY - curY), 2));

                // Determine if distance is w/i threshold for inclusion
                // in base level.
                d = Math.abs(baseLevel.lastX - curX)
                        + Math.abs(baseLevel.lastY - curY);
                if (d > theLevelThreshold) {
                    addPoint(curX, curY, j, baseLevel);

                }

                for (LODLevel level : baseLevel.children) {
                    if (isSpatiallyChopped) {
                        if (level.coverage
                                .contains(new double[] { curX, curY })) {
                            addPoint(curX, curY, j, level);
                        } else {
                            if (level.lastPointOutsideLODSemaphore == 0
                                    && level.lineStartedFlag) {
                                // line just crossed into other lod
                                // include anyway for continuity
                                addPoint(curX, curY, j, level);

                                // end the line, and make it start a new one
                                level.lineLengthBuffer.put(level.lodCount);
                                level.numLines++;
                                level.lodCount = 0;
                                level.lineStartedFlag = false;
                            }
                            level.lastPointOutsideLODSemaphore++;
                        }
                    } else {
                        addPoint(curX, curY, j, level);
                    }
                }

            }

            // Handle the last point: Always add it to the decimated level

            baseLevel.lodIndexBuffers.put(lastIndex);
            baseLevel.lodCount++;
            baseLevel.lineLengthBuffer.put(baseLevel.lodCount);
            baseLevel.pts++;

            // Add the length to each LOD buffer
            for (LODLevel level : baseLevel.children) {
                if (level.lodCount > 0) {
                    level.lineLengthBuffer.put(level.lodCount);
                    level.numLines++;
                }
            }
        }

        needsCompile = false;
        isDrawable = true;
    }

    private void addPoint(float curX, float curY, int j, LODLevel level) {
        level.lodIndexBuffers.put(j);
        level.lodCount++;
        level.lastX = curX;
        level.lastY = curY;
        level.lastPointOutsideLODSemaphore = 0;
        level.lineStartedFlag = true;
        level.pts++;
    }

    public int getVertexCount() {
        return theVertexCount;
    }

    public FloatBuffer getVertexBuffer() {
        if (needsCompile) {
            compileInternal();
        }

        if (theVertexBuffer == null) {
            return null;
        }

        return theVertexBuffer.getBuffer();
    }

    public IntBuffer[] getLineLengths(int level) {
        if (needsCompile) {
            compileInternal();
        }

        if (level == 0) {
            return new IntBuffer[] { baseLevel.lineLengthBuffer.getBuffer() };
        } else {
            IntBuffer[] ib = new IntBuffer[baseLevel.children.length];
            for (int i = 0; i < ib.length; i++) {
                ib[i] = baseLevel.children[i].lineLengthBuffer.getBuffer();
            }
            return ib;
        }

    }

    public int getNumLines(int level, int spatialZone) {
        if (level == 0) {
            return baseLevel.numLines;
        }

        return baseLevel.children[spatialZone].numLines;

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IOutlineShape#isMutable()
     */
    public boolean isMutable() {
        return isMutable;
    }

    /**
     * @return the LODIndexBuffers
     */
    public IntBuffer[] getLODIndexBuffers(int level) {
        if (level == 0) {
            return new IntBuffer[] { baseLevel.lodIndexBuffers.getBuffer() };
        } else {
            IntBuffer[] ib = new IntBuffer[baseLevel.children.length];
            for (int i = 0; i < ib.length; i++) {
                ib[i] = baseLevel.children[i].lodIndexBuffers.getBuffer();
            }
            return ib;
        }

    }

    public int[] getSpatialIndices(IExtent pe) {
        if (this.needsCompile) {
            return new int[0];
        }

        if (!isSpatiallyChopped) {
            return new int[] { 0 };
        }

        int[] c = new int[baseLevel.children.length];
        int k = 0;
        for (int i = 0; i < baseLevel.children.length; i++) {
            if (baseLevel.children[i].coverage.intersects(pe)) {
                c[k] = i;
                k++;
            }
        }

        int[] retVal = new int[k];
        for (int i = 0; i < k; i++) {
            retVal[i] = c[i];
        }

        return retVal;

    }

    /**
     * @return the levels
     */
    public float[] getLevels() {
        return theLevels;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IShape#isDrawable()
     */
    public boolean isDrawable() {
        return isDrawable;
    }

    /**
     * @return the vboID
     */
    public int getVboID() {
        return vboID;
    }

    /**
     * @param vboID
     *            the vboID to set
     */
    public void setVboID(int vboID) {
        this.vboID = vboID;
    }

    /**
     * @return the vboAssociatedTarget
     */
    public IGLTarget getVboAssociatedTarget() {
        return vboAssociatedTarget;
    }

    /**
     * @param vboAssociatedTarget
     *            the vboAssociatedTarget to set
     */
    public void setVboAssociatedTarget(IGLTarget vboAssociatedTarget) {
        this.vboAssociatedTarget = vboAssociatedTarget;
    }

    /**
     * Release the large memory buffers (useful for when using VBOs)
     * 
     */
    protected void release() {
        theVertexBuffer.dispose();
        theVertexBuffer = null;

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IShape#dispose()
     */
    public void dispose() {
        if (theLineLengths != null) {
            theLineLengths.clear();
        }
        theLineLengths = null;
        if (theLineStarts != null) {
            theLineStarts.clear();
        }

        theLineStarts = null;
        if (theVertexBuffer != null) {
            theVertexBuffer.dispose();
        }

        theVertexBuffer = null;

        recursiveDispose(baseLevel);

        if (vboAssociatedTarget != null && vboID > 0) {
            vboAssociatedTarget.getGl().glDeleteBuffers(1, new int[] { vboID },
                    0);
            vboID = 0;
            vboAssociatedTarget = null;
        }

    }

    private void recursiveDispose(LODLevel l) {
        if (l == null) {
            return;
        }

        if (l.lineLengthBuffer != null) {
            l.lineLengthBuffer.dispose();
        }
        l.lineLengthBuffer = null;

        if (l.lodIndexBuffers != null) {
            l.lodIndexBuffers.dispose();
        }
        l.lodIndexBuffers = null;
        if (l.children != null) {
            for (LODLevel i : l.children) {
                recursiveDispose(i);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IShape#reset()
     */
    public void reset() {
        this.isDrawable = false;

        if (this.theVertexBuffer != null) {
            this.theVertexBuffer.clear();
        }

        recursiveReset(baseLevel);

        this.theVertexCount = 0;

        if (this.theLineLengths != null) {
            this.theLineLengths.clear();
        }

        if (this.theLineStarts != null) {
            this.theLineStarts.clear();
        }

        if (this.theLineStarts == null) {
            theLineStarts = new ArrayList<Integer>(1000);
        }

        if (this.theLineLengths == null) {
            theLineLengths = new ArrayList<Integer>(1000);
        }

    }

    private void recursiveReset(LODLevel l) {
        if (l == null) {
            return;
        }

        if (l.lineLengthBuffer != null) {
            l.lineLengthBuffer.clear();
        }

        if (l.lodIndexBuffers != null) {
            l.lodIndexBuffers.clear();
        }

        l.numLines = 0;

        if (l.children != null) {
            for (LODLevel i : l.children) {
                recursiveReset(i);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IShape#setMapDescriptor(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    public void setMapDescriptor(IDescriptor descriptor) {
        this.theDescriptor = descriptor;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IWireframeShape#addLabel(java.lang.String
     * , double[])
     */
    @Override
    public void addLabel(String label, double[] screenCoordinate) {
        synchronized (this) {
            if (this.labelMap == null) {
                this.labelMap = new HashMap<double[], String>();
            }
        }

        this.labelMap.put(screenCoordinate, label);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IWireframeShape#clearLabels()
     */
    @Override
    public void clearLabels() {
        if (this.labelMap != null) {
            this.labelMap.clear();
        }
    }

    public Map<double[], String> getLabelMap() {
        return this.labelMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#finalize()
     */
    @Override
    protected void finalize() throws Throwable {
        this.dispose();
    }

    @Override
    public void allocate(int size) {
        if (theVertexBuffer == null) {
            initialSize = size;
        } else {
            theVertexBuffer.allocate(size);
        }
    }

}