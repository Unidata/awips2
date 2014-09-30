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

package com.raytheon.uf.common.dataplugin.gfe.grid;

import java.awt.Point;
import java.nio.ByteBuffer;

/**
 * BEGIN LEGACY DOCUMENTATION
 * 
 * The two-dimensional bit grid is commonly used to store flags for gridded
 * data. The rows are organized as left-right (west-east) and the columns are
 * organized as down -up (south-north). The size of each dimension is specified
 * upon creation and then remains fixed during the lifetime of the grid (except
 * if another grid is assigned to it). The C language does not support
 * two-dimensional arrays nor does it support array bounds checking. This class
 * overcomes these shortcomings.
 * 
 * The two-dimensional bit grid can be initialized in one step from an array of
 * byte data. Individual elements, specified by x (column number) and y (row)
 * location, may be retrieved and stored.
 * 
 * An invalid Grid2DBit object represents an empty (i.e., a grid without data or
 * dimensions) grid.
 * 
 * Bounds checking is performed on all indexing operations. Attempting to access
 * an element outside the bounds results in a call to ExecutionLog which will
 * abort the program.
 * 
 * Initialization of the grid (through the constructor) is accomplished by
 * passing a pointer to a block of contiguous memory. The memory should be large
 * enough to hold nrows*ncolumns elements and should contain data in row-by-row
 * ordering (column first). This class cannot check for valid initialization
 * data or pointer to memory that is of sufficient size. However, a function
 * dataSize() returns the size of the memory needed in bytes.
 * 
 * In the event that no pointer is passed to the initialization constructor,
 * then the two-dimension grid is filled with bit values of zero.
 * 
 * The coordinate system proceeds from the lower-left.
 * 
 * Additional function includes the ability to obtain the extrema of all of the
 * set bits and getting a grid that contains a contiguous map. Other functions
 * permit translation of the grid and a list of all contiguous areas. --
 * implementation ----------------------------------------------------------
 * Two-dimensional bit grids are implemented as a one-dimensional contiguous
 * block of memory. Indexing into the grid is accomplished by determining the
 * bit number in the grid, accessing the byte, and extracting/inserting the bit.
 * 
 * END LEGACY DOCUMENTATION
 * 
 * Implementation of the Bit version of Grid2D. We use bytes to hold the bit
 * data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2008 879        rbell       Initial Creation.
 * Oct 22, 2008 1624       wdougherty  Speed up translate method
 * Sep 01, 2014 3572       randerso    Added clear(x,y) method
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */
public class Grid2DBit extends Grid2DByte implements Cloneable {

    public static Grid2DBit createBitGrid(int xDim, int yDim, byte[] data) {
        return new Grid2DBit(xDim, yDim, data);
    }

    /**
     * default constructor
     */
    public Grid2DBit() {

    }

    /**
     * Constructor for creating a two-dimension grid containing bits. xDim and
     * yDim specify the size of the grid.
     * 
     * @param xDim
     * @param yDim
     */
    public Grid2DBit(int xDim, int yDim) {
        super(xDim, yDim);
    }

    public Grid2DBit(int xDim, int yDim, boolean value) {
        super(xDim, yDim, (byte) (value ? 1 : 0));
    }

    /**
     * Constructor for creating a two-dimension grid containing bits. xDim and
     * yDim specify the size of the grid. data is an array of initialization
     * data.
     * 
     * @param xDim
     * @param yDim
     * @param data
     *            array of initialization data
     */
    public Grid2DBit(int xDim, int yDim, byte[] data) {
        super(xDim, yDim, data);
        if (!checkBits(data)) {
            throw new IllegalArgumentException(
                    "Buffer contains values other than 0 and 1");
        }
    }

    /**
     * Constructor for creating a two-dimension grid containing bits. xDim and
     * yDim specify the size of the grid. data is a ByteBuffer containing
     * initialization data.
     * 
     * @param xDim
     * @param yDim
     * @param data
     *            ByteBuffer of initialization data
     */
    public Grid2DBit(int xDim, int yDim, ByteBuffer data) {
        super(xDim, yDim, data);
        if (!checkBits(data.array())) {
            throw new IllegalArgumentException(
                    "Buffer conaints values other than 0 and 1");
        }
    }

    /**
     * Constructor for creating a two-dimension grid containing bits. xDim and
     * yDim specify the size of the grid. data is an array of initialization
     * data.
     * 
     * @param xDim
     * @param yDim
     * @param data
     *            array of initialization data
     */
    public Grid2DBit(int xDim, int yDim, boolean[] data) {
        super(xDim, yDim);
        if (data.length != xDim * yDim) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.length);
        }

        for (int i = 0; i < data.length; i++) {
            this.buffer.put((byte) (data[i] ? 1 : 0));
        }
    }

    /**
     * 
     * Copy constructor
     * 
     * @param aGrid2DBit
     *            Grid2DBit to copy
     */
    public Grid2DBit(Grid2DBit aGrid2DBit) {
        super(aGrid2DBit);
        if (!checkBits(aGrid2DBit.buffer.array())) {
            throw new IllegalArgumentException(
                    "Buffer conaints values other than 0 and 1");
        }
    }

    /**
     * @param xDim
     *            x coordinate of bit to set
     * @param yDim
     *            y coordinate of bit to set
     * @param aValue
     *            value of bit to set
     */
    @Override
    public void set(int xDim, int yDim, byte aValue) {
        if (!isBit(aValue)) {
            throw new IllegalArgumentException(
                    "The supplied value is not 0 or 1.");
        }
        super.set(xDim, yDim, aValue);
    }

    /**
     * 
     * Sets a bit to 1.
     * 
     * @param xDim
     *            xDim x coordinate of bit to set
     * @param yDim
     *            yDim y coordinate of bit to set
     */
    public void set(int xDim, int yDim) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException("Dimensions are invalid");
        }
        buffer.put(yDim * this.xdim + xDim, (byte) 1);
    }

    /**
     * 
     * Sets a bit to 0.
     * 
     * @param xDim
     *            xDim x coordinate of bit to set
     * @param yDim
     *            yDim y coordinate of bit to set
     */
    @Override
    public void clear(int xDim, int yDim) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException("Dimensions are invalid");
        }
        buffer.put(yDim * this.xdim + xDim, (byte) 0);
    }

    /**
     * 
     * Sets all bits to the given value.
     * 
     * @param aValue
     *            value to set all bits to.
     */
    @Override
    public void setAllValues(byte aValue) {
        if (!isBit(aValue)) {
            throw new IllegalArgumentException(
                    "The supplied value is not 0 or 1.");
        }
        super.setAllValues(aValue);
    }

    /**
     * 
     * Logical 'OR' operator for Grid2DBit.
     * 
     * Sizes must be identical for this operation to work. Each byte of the
     * internal data structure is logical "OR"'d to the passed in data
     * structure.
     * 
     * @param rhs
     *            Grid2DBit to OR this grid with
     * @return this grid, after the logical OR operation
     */
    public Grid2DBit orEquals(Grid2DBit rhs) {
        if (this == rhs) {
            return this; // same grid - do nothing
        }

        // sizes must be identical
        if (rhs.xdim != this.xdim || rhs.ydim != this.ydim) {
            throw new IllegalArgumentException(
                    "Grids are different dimensions.");
        }

        byte[] data = this.buffer.array();
        byte[] rhsData = rhs.buffer.array();
        for (int i = 0; i < data.length; i++) {
            data[i] = (byte) (data[i] | rhsData[i]);
        }

        return this;
    }

    /**
     * Logical '!' operator for Grid2DBit.
     * 
     * Each bit in this Grid2DBit is negated.
     */
    public void negate() {
        byte[] data = buffer.array();
        for (int i = 0; i < data.length; i++) {
            data[i] = (data[i] == 0 ? (byte) 1 : (byte) 0);
        }
    }

    /**
     * 
     * Logical 'AND' operator for Grid2DBit.
     * 
     * Sizes must be identical for this operation to work. Each byte of the
     * internal data structure is logical "AND"'d to the passed in data
     * structure.
     * 
     * @param rhs
     *            Grid2DBit to AND this grid with
     * @return this grid, after the logical AND operation
     */
    public Grid2DBit andEquals(Grid2DBit rhs) {
        if (this == rhs) {
            return this; // same grid - do nothing
        }

        // sizes must be identical
        if (rhs.xdim != this.xdim || rhs.ydim != this.ydim) {
            throw new IllegalArgumentException(
                    "Grids are different dimensions.");
        }

        byte[] data = this.buffer.array();
        byte[] rhsData = rhs.buffer.array();
        for (int i = 0; i < data.length; i++) {
            data[i] = (byte) (data[i] & rhsData[i]);
        }

        return this;
    }

    /**
     * 
     * Logical 'XOR' operator for Grid2DBit. -- implementation
     * ---------------------------------------------------------- Sizes must be
     * identical for this operation to work. Each byte of the internal data
     * structure is logical "XOR"'d to the passed in data structure.
     * 
     * @param rhs
     *            Grid2DBit to XOR this grid with
     * @return this grid, after the logical XOR operation
     */
    public Grid2DBit xorEquals(Grid2DBit rhs) {
        // sizes must be identical
        if (rhs.xdim != this.xdim || rhs.ydim != this.ydim) {
            throw new IllegalArgumentException(
                    "Grids are different dimensions.");
        }
        byte[] data = this.buffer.array();
        byte[] rhsData = rhs.buffer.array();
        for (int i = 0; i < data.length; i++) {
            data[i] = (byte) (data[i] ^ rhsData[i]);
        }

        return this;
    }

    /**
     * Logical 'XOR' operator for Grid2DBit. Returns new Grid2DBit that is the
     * XOR from this and the one specified. The original Grid2DBits are not
     * modified.
     * 
     * Sizes must be identical for this operation to work. Each byte of the
     * output data structure is set to the logical "XOR"'d of the passed in data
     * structure and this object.
     * 
     * @param rhs
     *            given Grid2DBit to XOR with
     * @return the result of the XOR
     */
    public Grid2DBit xor(Grid2DBit rhs) {
        Grid2DBit rVal = new Grid2DBit(this);
        rVal.xorEquals(rhs);
        return rVal;
    }

    /**
     * Logical 'OR' operator for Grid2DBit. Returns new Grid2DBit that is the OR
     * from this and the one specified. The original Grid2DBits are not
     * modified.
     * 
     * Sizes must be identical for this operation to work. Each byte of the
     * output data structure is set to the logical "OR"'d of the passed in data
     * structure and this object.
     * 
     * @param rhs
     *            given Grid2DBit to OR with
     * @return the result of the OR
     */
    public Grid2DBit or(Grid2DBit rhs) {
        Grid2DBit rVal = new Grid2DBit(this);
        rVal.orEquals(rhs);
        return rVal;
    }

    /**
     * Logical 'AND' operator for Grid2DBit. Returns new Grid2DBit that is the
     * AND from this and the one specified. The original Grid2DBits are not
     * modified.
     * 
     * Sizes must be identical for this operation to work. Each byte of the
     * output data structure is set to the logical "AND"'d of the passed in data
     * structure and this object.
     * 
     * @param rhs
     *            given Grid2DBit to AND with
     * @return the result of the AND
     */
    public Grid2DBit and(Grid2DBit rhs) {
        Grid2DBit rVal = new Grid2DBit(this);
        rVal.andEquals(rhs);
        return rVal;
    }

    /**
     * Returns true if any bits are set.
     * 
     * @return whether or not any bit is set in the data.
     */
    public boolean isAnyBitsSet() {
        for (byte b : this.buffer.array()) {
            if (b != 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns the number of bits that are set.
     * 
     * @return number of bits set.
     */
    public int numberOfBitsSet() {
        int rVal = 0;
        for (byte b : this.buffer.array()) {
            if (b != 0) {
                rVal++;
            }
        }
        return rVal;
    }

    /**
     * Returns a new Grid2DBit whose set bits identify the contiguous area as
     * defined by Point. A contiguous area is defined as the Point plus any
     * adjacent bits that are set plus any adjacent to those. Bits that are next
     * to each other as well as diagonal from each other are considered
     * adjacent.
     * 
     * Create a new empty bit array, pass it to the makeContiguousArray
     * function, and return the bitArray.
     * 
     * @param aPoint
     *            the point from which the contiguous bit array starts
     * @return a Grid2DBit holding the contiguous bit array
     */
    public Grid2DBit contiguousBitArray(Point aPoint) {
        Grid2DBit contigArray = new Grid2DBit(this.xdim, this.ydim);
        makeContiguousArray(contigArray, aPoint.x, aPoint.y, 8);
        return contigArray;
    }

    /**
     * 
     * Returns a new Grid2DBit whose set bits identify the contiguous
     * 4-direction area as defined by CartCoord2D<int>. A contiguous area is
     * defined as the CartCoord2D<int> plus any adjacent bits that are set plus
     * any adjacent to those. Bits that are next to each other in four
     * directions only are considered adjacent.
     * 
     * Create a new empty bit array, pass it to the makeBoundedRegion function,
     * and return the bitArray.
     * 
     * @param aPoint
     *            the point to start the bounded region from
     * @return the resulting bounded region
     */
    public Grid2DBit boundedRegion(Point aPoint)

    {
        Grid2DBit rVal = new Grid2DBit(this.xdim, this.ydim);
        makeBoundedRegion(rVal, aPoint.x, aPoint.y);
        return rVal;
    }

    /**
     * 
     * Figures out the extrema of all of the set bits and returns the
     * coordinates of them through the calling arguments. Returns true if there
     * are any set bits.
     * <p>
     * This was modified from a much simpler routine. The earlier version was
     * much more readable, but it suffered from two performance problems: it
     * iterated over every point, and it used the single-point grid.get(int,
     * int) method. The first wastes effort by looping over points that can't be
     * of interest; the latter performs y*xDim once for each point, and reduces
     * performance in the FilterGrid2DBit subclass because of repetitive subkey
     * searching.
     * <p>
     * The newer implementation uses getBuffer().array() to get the flat version
     * of the grid, calculating its own array offsets, and uses several
     * strategies to reduce the number of points examined. Using the flat array
     * works well with FilterGrid2DBit, allows us to calculate y * xDim once per
     * row, and saves the overhead of a function call to get().
     * 
     * @param lowerLeft
     *            Point to hold the returned lower left extreme
     * @param upperRight
     *            Point to hold the returned upper right extreme
     * @return whether or not extremes were found
     */
    public boolean extremaOfSetBits(Point lowerLeft, Point upperRight) {
        boolean any = false;
        lowerLeft.x = this.xdim - 1;
        lowerLeft.y = this.ydim - 1;
        upperRight.x = 0;
        upperRight.y = 0;
        byte[] arry = this.buffer.array();
        int offset;
        int offset2;

        // Hunt for the first '1' in the array.
        // It will be on the lowest row, lowestLeft.y.
        // Its column is an upper limit on the value of lowerLeft.x.
        for (offset = 0; offset < xdim * ydim; offset++) {
            if (arry[offset] == (byte) 1) {
                any = true;
                lowerLeft.y = offset / xdim;
                lowerLeft.x = offset % xdim; // first approximation
                break;
            }
        }
        // If there were no '1's, we're done.
        if (any) {
            // Hunt for the last '1' in the array.
            // It gives us upperRight.y.
            // It gives us the first approximation of upperRight.x
            for (offset2 = xdim * ydim - 1; offset2 >= offset; offset2--) {
                if (arry[offset2] == (byte) 1) {
                    upperRight.y = offset2 / xdim;
                    upperRight.x = offset2 % xdim; // first approximation
                    break;
                }
            }
            // If only one row had 1s, we're done.
            if (lowerLeft.y != upperRight.y) {
                int row;
                int col;
                // find lowerLeft.x
                for (row = lowerLeft.y + 1; row <= upperRight.y; row++) {
                    offset = row * xdim;
                    for (col = 0; col < lowerLeft.x; col++) {
                        if (arry[offset + col] == (byte) 1) {
                            lowerLeft.x = col;
                            break;
                        }
                    }
                }
                // find upperRight.x
                for (row = upperRight.y - 1; row >= lowerLeft.y; row--) {
                    offset = row * xdim;
                    for (col = xdim - 1; col > upperRight.x; col--) {
                        if (arry[offset + col] == (byte) 1) {
                            upperRight.x = col;
                            break;
                        }
                    }
                }
            }
        }

        return any;
    }

    /**
     * This is a function that sets each bit of the contigArray one bit at a
     * time. Ultimately it sets all of the bits that are set in this object and
     * that are also touching the point x,y.
     * 
     * The initial (x,y) point is checked to be sure it is within the bounds of
     * the grid and set to 1. If it is, its coordinates are pushed on a stack.
     * Then we process the stack. We pop a coordinate pair off the stack and
     * check each of its &lt;directions&gt; neighbors to see if they are in
     * bounds and set to 1. If a neighbor passes that check, we look to see if
     * the corresponding point in contigArray is 0. If it is not, we know we
     * have not visited it before. We set the point in contigArray to 1 and add
     * the point to the stack. When the stack is empty, we know contigArray has
     * all of the points that are 1 with a path to (x,y) that is all 1s.
     * 
     * @param contigArray
     *            the Grid2DBit object that will hold the contiguous array
     * @param x
     *            x coordinate of the bit starting the contiguous array
     * @param y
     *            y coordinate of the bit starting the contiguous array
     * @param directions
     *            directions to check for set bits. Use 4 for up, down, left,
     *            and right, 8 to include diagonals.
     */
    protected void makeContiguousArray(Grid2DBit contigArray, int x, int y,
            int directions) {

        // First check for out of bounds
        if (!isValid(x, y)) {
            return;
        }

        int ssize = xdim * ydim * 2;
        int stack[] = new int[ssize];
        int index = 0; // points to the current stack "top".
        if (setCheck(x, y)) {
            stack[index] = x;
            stack[index + 1] = y;
            contigArray.set(x, y);
        }

        while (index >= 0) {
            x = stack[index];
            y = stack[index + 1];
            index -= 2;
            for (int i = 0; i < directions; i++) {
                int xn = x + IGrid2D.makeContiguousArrayXComponent[i];
                int yn = y + IGrid2D.makeContiguousArrayYComponent[i];
                if (setCheck(xn, yn)) {
                    if (contigArray.get(xn, yn) == (byte) 0) {
                        contigArray.set(xn, yn);
                        index += 2;
                        stack[index] = xn;
                        stack[index + 1] = yn;
                    }
                }
            }
        }
    }

    /**
     * 
     * This is a recursive function that sets each bit of the regionArray one
     * bit at a time. Ultimately it sets all of the bits that are set in this
     * object and that are also touching the point x,y in four directions (up,
     * down, left, and right).
     * 
     * First check to see if we are out of bounds, if so return. Then check to
     * see if we have set this same point before, if so return. Then check to
     * see if the point in private data is set, if so set the corresponding bit
     * in contigArray. If not, return. Finally loop through all four directions
     * and call this same function recursively for each.
     * 
     * @param regionArray
     *            the Grid2DBit to store the bounded region in
     * @param x
     *            x coordinate of where to start the bounded region
     * @param y
     *            y coordinate of where to start the bounded region
     */
    private void makeBoundedRegion(Grid2DBit regionArray, int x, int y) {
        makeContiguousArray(regionArray, x, y, 4);
    }

    /**
     * 
     * Utility function which clears all the bits that are contiguous with the
     * specified location.
     * 
     * @param array
     *            array to clear the contiguous area of
     * @param x
     *            x coordinate of the start of the contiguous area
     * @param y
     *            y coordinate of the start of the contiguous area
     * @param directions
     *            directions that define a contiguous area
     */
    public static void clearContiguous(Grid2DBit array, int x, int y,
            int directions) {
        Grid2DBit contiguousArray = new Grid2DBit(array.xdim, array.ydim);
        array.makeContiguousArray(contiguousArray, x, y, directions);

        byte[] c = contiguousArray.buffer.array();
        byte[] a = array.buffer.array();
        for (int i = 0; i < c.length; i++) {
            if (c[i] != 0) {
                a[i] = (byte) 0;
            }
        }
    }

    /**
     * 
     * Returns true if a bit has been found, returns location through firstSet.
     * 
     * @param startPoint
     *            point to start search from
     * @param firstSet
     *            point to store result in
     * @return whether or not a point was ever found
     */
    public boolean findNearestSet(Point startPoint, Point firstSet) {
        firstSet.setLocation(startPoint);

        // First check for out of bounds
        if (!isValid(startPoint.x, startPoint.y)) {
            return false;
        }

        // check for match right away
        if (setCheck(startPoint.x, startPoint.y)) {
            return true;
        }

        // any bits set?
        if (!isAnyBitsSet()) {
            return false;
        }

        // circular - bigger and bigger
        int x, y;
        int maxOffset = Math.max(xdim, ydim);
        for (int offset = 1; offset <= maxOffset; offset++) {
            // horiz top
            for (int xoff = -offset; xoff <= offset; xoff++) {
                x = startPoint.x + xoff;
                y = startPoint.y + offset;
                if (setCheck(x, y)) {
                    firstSet.setLocation(new Point(x, y));
                    return true;
                }
            }
            // vert right
            for (int yoff = offset - 1; yoff > -offset; yoff--) {
                x = startPoint.x + offset;
                y = startPoint.y + yoff;
                if (setCheck(x, y)) {
                    firstSet.setLocation(new Point(x, y));
                    return true;
                }
            }
            // horiz bottom
            for (int xoff = offset; xoff >= -offset; xoff--) {
                x = startPoint.x + xoff;
                y = startPoint.y - offset;
                if (setCheck(x, y)) {
                    firstSet.setLocation(new Point(x, y));
                    return true;
                }
            }
            // vert left
            for (int yoff = -offset + 1; yoff < offset; yoff++) {
                x = startPoint.x - offset;
                y = startPoint.y + yoff;
                if (setCheck(x, y)) {
                    firstSet.setLocation(new Point(x, y));
                    return true;
                }
            }

        }

        return false;
    }

    /**
     * 
     * Utility function which checks for valid and returns true if set.
     * 
     * @param x
     *            x coordinate of bit to check
     * @param y
     *            y coordinate of bit to check
     * @return if the bit the check is valid and set
     */
    protected boolean setCheck(int x, int y) {
        // out of bounds check
        if (!isValid(x, y)) {
            return false;
        }

        if (buffer.get(y * xdim + x) == 1) {
            return true;
        }
        return false;
    }

    /**
     * 
     * Returns a sequence of points which each one defines an contiguous bit
     * array. The locations can be used for contiguousBitArray() to get each
     * bitarray.
     * 
     * Sets up a copy of this object and begins to search for set bits using the
     * get() command. Each time one is found, contiguousBitArray() is called to
     * obtain the array of bits that are contiguous to the set point. This array
     * is exclusively-or'd with the copy of this object which results in those
     * contiguous bits being cleared. Processing contines on the copy object
     * until no more bits are found. Each time a bit is found, it is added to
     * the return list.
     * 
     * @return an array containing a point from each contiguous region
     */
    public Point[] getContiguousBitArrayLocations() {
        Point locations[] = new Point[100]; // returned array
        int currentLocations = 0;
        Grid2DBit remainingArray = new Grid2DBit(this); // make a copy of this
        // object

        for (int x = 0; x < xdim; x++) {
            for (int y = 0; y < ydim; y++) {
                if (remainingArray.get(x, y) != 0) {
                    if (currentLocations + 1 == locations.length) {
                        Point temp[] = new Point[locations.length * 2];
                        System.arraycopy(locations, 0, temp, 0,
                                locations.length);
                        locations = temp;
                    }
                    clearContiguous(remainingArray, x, y, 8);
                    locations[currentLocations] = new Point(x, y);
                    currentLocations++;
                }
            }
        }

        Point rVal[] = new Point[currentLocations];
        System.arraycopy(locations, 0, rVal, 0, currentLocations);

        return rVal;
    }

    /**
     * 
     * Returns a sequence of points which each one defines a boundedRegion. The
     * locations can be used for with boundedRegion to get each bitarray.
     * 
     * Sets up a copy of this object and begins to search for set bits using the
     * get() command. Each time one is found, boundedRegion() is called to
     * obtain the array of bits that are contiguous to the set point. This array
     * is exclusively-or'd with the copy of this object which results in those
     * contiguous bits being cleared. Processing contines on the copy object
     * until no more bits are found. Each time a bit is found, it is added to
     * the return list.
     * 
     * @returnan array containing a point from each bounded region
     */
    public Point[] boundedRegionLocations() {
        Point locations[] = new Point[100]; // returned array
        int currentLocations = 0;
        Grid2DBit remainingArray = new Grid2DBit(this); // make a copy of this
        // object

        // process each point in the copy
        for (int i = 0; i < xdim; i++) {
            for (int j = 0; j < ydim; j++) {
                if (remainingArray.setCheck(i, j)) {
                    if (currentLocations + 1 == locations.length) {
                        Point temp[] = new Point[locations.length * 2];
                        System.arraycopy(locations, 0, temp, 0,
                                locations.length);
                        locations = temp;
                    }
                    clearContiguous(remainingArray, i, j, 4);
                    locations[currentLocations] = new Point(i, j);
                    currentLocations++;
                }
            }
        }

        Point rVal[] = new Point[currentLocations];
        System.arraycopy(locations, 0, rVal, 0, currentLocations);

        return rVal;
    }

    public boolean getAsBoolean(int x, int y) {
        return (this.get(x, y) != (byte) 0);
    }

    @Override
    public Grid2DBit clone() {
        Grid2DBit rVal = new Grid2DBit(this);
        return rVal;
    }

    @Override
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid) {
        if (!(sourceGrid instanceof Grid2DBit)) {
            throw new IllegalArgumentException(
                    "The input source grid must be of type Grid2DBit");
        }

        super.copyWithMask(sourceGrid, maskGrid);
    }

    @Override
    public void setAllOfValue(byte oldValue, byte newValue) {
        throw new UnsupportedOperationException(
                "This method is invalid for a Grid2DBit");
    }

    private boolean checkBits(byte[] b) {
        for (byte temp : b) {
            if (!isBit(temp)) {
                return false;
            }
        }
        return true;
    }

    private boolean isBit(Byte b) {
        return b == 0 || b == 1;
    }

    /**
     * 
     * Translates the set bytes in this object by the amount specified in
     * deltaCoord and returns a new Grid2DByte.
     * 
     * @param deltaCoord
     *            coordinate representing the translation from each byte's
     *            origin
     * @return the resulting translation
     */
    public Grid2DBit translate(Point deltaCoord) {
        // make another Grid2DBit
        Grid2DBit rVal = new Grid2DBit(xdim, ydim, false);

        if (Math.abs(deltaCoord.x) < xdim && Math.abs(deltaCoord.y) < ydim) {
            // Find iteration limits for X
            int fromXStart;
            int toXStart;
            int cols;
            if (deltaCoord.x < 0) {
                fromXStart = -deltaCoord.x;
                toXStart = 0;
                cols = xdim + deltaCoord.x; // minus abs(deltaCoord.x)
            } else {
                fromXStart = 0;
                toXStart = deltaCoord.x;
                cols = xdim - deltaCoord.x;
            }

            // Find iteration limits for Y
            int fromYStart;
            int toYStart;
            int rows;
            if (deltaCoord.y < 0) {
                fromYStart = -deltaCoord.y;
                toYStart = 0;
                rows = ydim + deltaCoord.y; // minus abs(deltaCoord.y)
            } else {
                fromYStart = 0;
                toYStart = deltaCoord.y;
                rows = ydim - deltaCoord.y;
            }

            // Get the internal arrays to copy between
            byte[] fromA = buffer.array();
            byte[] toA = rVal.getBuffer().array();

            // Calculate from/to array offsets of the first point.
            int fromOffset = fromYStart * xdim + fromXStart;
            int toOffset = toYStart * xdim + toXStart;

            // For each row, copy cols bytes of data.
            // Then update offsets for next row.
            for (int row = 0; row < rows; row++, fromOffset += xdim, toOffset += xdim) {
                System.arraycopy(fromA, fromOffset, toA, toOffset, cols);
            }
        }

        return rVal;
    }
}
