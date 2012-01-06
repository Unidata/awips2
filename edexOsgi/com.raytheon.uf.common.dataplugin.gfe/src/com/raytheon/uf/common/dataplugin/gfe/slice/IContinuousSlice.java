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

package com.raytheon.uf.common.dataplugin.gfe.slice;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;

/**
 * IContinuousSlice defines an interface to continuous-valued slices (i.e. both
 * Scalar and Vector)
 * 
 * This contains only methods applicable to both Scalar and Vector types.
 * Methods that are only applicable to one of the subclasses, will be defined at
 * that level.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial Creation.
 * 02/21/2008   879        rbell       Added operateEquals(...)
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IContinuousSlice extends IGridSlice {

    /**
     * Constrains the GridSlice to the specified values over the editArea.
     * 
     * @param minValue
     *            the minimum constraint value
     * @param maxValue
     *            the maximum constraint value
     * @param editArea
     *            the edit area to apply the limits to, if null, apply over the
     *            entire area
     */
    public abstract void limitValue(float minValue, float maxValue,
            Grid2DBit editArea);

    /**
     * Constrains the GridSlice to the specified values.
     * 
     * @param minValue
     *            the minimum constraint value
     * @param maxValue
     *            the maximum constraint value
     */
    public abstract void limitValue(float minValue, float maxValue);

    /**
     * Compare the values of the GridSlice to a constant, and return a grid
     * indicating where this comparison operation is true. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> eq(float)
     * <li> notEq(float)
     * <li> gt(float)
     * <li> gtEq(float)
     * <li> lt(float)
     * <li> ltEq(float)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param value
     *            the second operand
     * @return a Grid2DBit containing the value 1 wherever the comparison
     *         succeeded.
     */
    public abstract Grid2DBit comparisonOperate(Op op, float value);

    /**
     * Compare the values of the GridSlice to another continuously-valued
     * GridSlice, and return a grid indicating this comparison operation is
     * true. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> eq(GridSlice)
     * <li> notEq(GridSlice)
     * <li> gt(GridSlice)
     * <li> gtEq(GridSlice)
     * <li> lt(GridSlice)
     * <li> ltEq(GridSlice)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param cs
     *            the second operand
     * @return a Grid2DBit containing the value 1 wherever the comparison
     *         succeeded.
     */
    public abstract Grid2DBit comparisonOperate(Op op, IContinuousSlice cs);

    /**
     * Apply an operation to a specified subset of the GridSlice, with a single
     * real-valued operand. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> add(float, Grid2DBit)
     * <li> subtract(float, Grid2DBit)
     * <li> multiply(float, Grid2DBit)
     * <li> divide(float, Grid2DBit)
     * <li> assign(float, Grid2DBit)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param value
     *            the value to use as an operand
     * @param editArea
     *            the area to apply the operation over. If null, apply over the
     *            entire grid
     */
    public abstract void operateEquals(Op op, float value, Grid2DBit editArea);

    /**
     * Apply an operation to a GridSlice, with a single real-valued operand.
     * <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> add(float)
     * <li> subtract(float)
     * <li> multiply(float)
     * <li> divide(float)
     * <li> assign(float)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param value
     *            the value to use as an operand
     */
    public abstract void operateEquals(Op op, float value);

    /**
     * Apply an operation to a specified subset of the GridSlice, with a slice
     * of values providing the operands. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> add(GridSlice, Grid2DBit)
     * <li> subtract(GridSlice, Grid2DBit)
     * <li> multiply(GridSlice, Grid2DBit)
     * <li> divide(GridSlice, Grid2DBit)
     * <li> assign(GridSlice, Grid2DBit)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param cs
     *            the slice containing values to use as an operand
     * @param editArea
     *            the area to apply the operation over. If null, apply over the
     *            entire grid
     */
    public abstract void operateEquals(Op op, IContinuousSlice cs,
            Grid2DBit editArea);

    /**
     * Apply an operation to a GridSlice, with a slice of values providing the
     * operands. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> add(GridSlice)
     * <li> subtract(GridSlice)
     * <li> multiply(GridSlice)
     * <li> divide(GridSlice)
     * <li> assign(GridSlice)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param cs
     *            the slice containing values to use as an operand
     */
    public abstract void operateEquals(Op op, IContinuousSlice cs);

    /**
     * Compute and return an operation to a specified subset of the GridSlice,
     * with a single real-valued operand. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> multiplyGrid(float, Grid2DBit)
     * <li> divideGrid(float, Grid2DBit)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param value
     *            the value to use as an operand
     * @param editArea
     *            the area to apply the operation over. If null, apply over the
     *            entire grid
     * @return the result of the operation
     */
    public abstract IContinuousSlice operate(Op op, float value,
            Grid2DBit editArea);

    /**
     * Compute and return an operation to a GridSlice, with a single real-valued
     * operand. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> multiplyGrid(float)
     * <li> divideGrid(float)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param value
     *            the value to use as an operand
     * @return the result of the operation
     */
    public abstract IContinuousSlice operate(Op op, float value);

    /**
     * Compute and return an operation to a specified subset of the GridSlice,
     * with a slice of values providing the operands. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> multiplyGrid(GridSlice, Grid2DBit)
     * <li> divideGrid(GridSlice, Grid2DBit)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param cs
     *            the slice containing values to use as an operand
     * @param editArea
     *            the area to apply the operation over. If null, apply over the
     *            entire grid
     * @return the result of the operation
     */
    public abstract IContinuousSlice operate(Op op, IContinuousSlice cs,
            Grid2DBit editArea);

    /**
     * Compute and return an operation to a GridSlice, with a slice of values
     * providing the operands. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li> multiplyGrid(GridSlice)
     * <li> divideGrid(GridSlice)
     * </ul>
     * 
     * @param op
     *            the operation to perform
     * @param cs
     *            the slice containing values to use as an operand
     * @return the result of the operation
     */
    public abstract IContinuousSlice operate(Op op, IContinuousSlice cs);

    /**
     * Calculates the minimum of the specified GridSlice and this GridSlice and
     * returns the result in a GridSlice.
     * 
     * @param gs
     *            the GridSlice to compare to
     * @return the minimum-valued GridSlice
     */
    public abstract IContinuousSlice min(IContinuousSlice gs);

    /**
     * Calculates the maximum of the specified GridSlice and this GridSlice and
     * returns the result in a GridSlice.
     * 
     * @param gs
     *            the GridSlice to compare to
     * @return the maximum-valued GridSlice
     */
    public abstract IContinuousSlice max(IContinuousSlice gs);

    /**
     * Calculates the sum of the specified GridSlice and this GridSlice and
     * returns the result in a GridSlice.
     * 
     * @param gs
     *            the GridSlice to sum with
     * @return the sum of both GridSlice
     */
    public abstract IContinuousSlice sum(IContinuousSlice gs);

}
