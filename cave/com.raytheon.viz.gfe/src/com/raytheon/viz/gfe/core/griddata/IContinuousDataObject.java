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
package com.raytheon.viz.gfe.core.griddata;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;

/**
 * IContinuousDataObject defines an interface to continuous-valued data objects
 * (i.e. both Scalar and Vector)
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2017            randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public interface IContinuousDataObject extends IDataObject {

    @Override
    default void collapse() {
        // do nothing for continuous grids
    }

    /**
     * Constrains the DataObject to the specified values over the editArea.
     *
     * @param minValue
     *            the minimum constraint value
     * @param maxValue
     *            the maximum constraint value
     * @param editArea
     *            the edit area to apply the limits to, if null, apply over the
     *            entire area
     */
    public void limitValue(float minValue, float maxValue, Grid2DBit editArea);

    /**
     * Constrains the DataObject to the specified values.
     *
     * @param minValue
     *            the minimum constraint value
     * @param maxValue
     *            the maximum constraint value
     */
    public void limitValue(float minValue, float maxValue);

    /**
     * Compare the values of the DataObject to a constant, and return a grid
     * indicating where this comparison operation is true. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>eq(float)
     * <li>notEq(float)
     * <li>gt(float)
     * <li>gtEq(float)
     * <li>lt(float)
     * <li>ltEq(float)
     * </ul>
     *
     * @param op
     *            the operation to perform
     * @param value
     *            the second operand
     * @return a Grid2DBit containing the value 1 wherever the comparison
     *         succeeded.
     */
    public Grid2DBit comparisonOperate(Op op, float value);

    /**
     * Compare the values of the DataObject to another continuously-valued
     * DataObject, and return a grid indicating this comparison operation is
     * true. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>eq(DataObject)
     * <li>notEq(DataObject)
     * <li>gt(DataObject)
     * <li>gtEq(DataObject)
     * <li>lt(DataObject)
     * <li>ltEq(DataObject)
     * </ul>
     *
     * @param op
     *            the operation to perform
     * @param dataObject
     *            the second operand
     * @return a Grid2DBit containing the value 1 wherever the comparison
     *         succeeded.
     */
    public Grid2DBit comparisonOperate(Op op, IContinuousDataObject dataObject);

    /**
     * Apply an operation to a specified subset of this DataObject, with a
     * single real-valued operand. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>add(float, Grid2DBit)
     * <li>subtract(float, Grid2DBit)
     * <li>multiply(float, Grid2DBit)
     * <li>divide(float, Grid2DBit)
     * <li>assign(float, Grid2DBit)
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
    public void operateEquals(Op op, float value, Grid2DBit editArea);

    /**
     * Apply an operation to this DataObject, with a single real-valued operand.
     * <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>add(float)
     * <li>subtract(float)
     * <li>multiply(float)
     * <li>divide(float)
     * <li>assign(float)
     * </ul>
     *
     * @param op
     *            the operation to perform
     * @param value
     *            the value to use as an operand
     */
    public void operateEquals(Op op, float value);

    /**
     * Apply an operation to a specified subset of this DataObject, with a
     * DataObject of values providing the operands.<BR>
     * <BR>
     * These operations are the equivalents to +=, -=, *=, /=, and =
     * (assignment) operators. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>add(DataObject, Grid2DBit)
     * <li>subtract(DataObject, Grid2DBit)
     * <li>multiply(DataObject, Grid2DBit)
     * <li>divide(DataObject, Grid2DBit)
     * <li>assign(DataObject, Grid2DBit)
     * </ul>
     *
     * @param op
     *            the operation to perform
     * @param dataObject
     *            the DataObject containing values to use as an operand
     * @param editArea
     *            the area to apply the operation over. If null, apply over the
     *            entire grid
     */
    public void operateEquals(Op op, IContinuousDataObject dataObject,
            Grid2DBit editArea);

    /**
     * Apply an operation to this DataObject, with a DataObject of values
     * providing the operands. <BR>
     * <BR>
     * These operations are the equivalents to +=, -=, *=, /=, and =
     * (assignment) operators. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>add(DataObject)
     * <li>subtract(DataObject)
     * <li>multiply(DataObject)
     * <li>divide(DataObject)
     * <li>assign(DataObject)
     * </ul>
     *
     * @param op
     *            the operation to perform
     * @param dataObject
     *            the DataObject containing values to use as an operand
     */
    public void operateEquals(Op op, IContinuousDataObject dataObject);

    /**
     * Compute and return an operation to a specified subset of the DataObject,
     * with a single real-valued operand. <BR>
     * <BR>
     * These operations are the equivalents to *= and /= operators. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>multiplyGrid(float, Grid2DBit)
     * <li>divideGrid(float, Grid2DBit)
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
    public IContinuousDataObject operate(Op op, float value,
            Grid2DBit editArea);

    /**
     * Compute and return an operation to a DataObject, with a single
     * real-valued operand. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>multiplyGrid(float)
     * <li>divideGrid(float)
     * </ul>
     *
     * @param op
     *            the operation to perform
     * @param value
     *            the value to use as an operand
     * @return the result of the operation
     */
    public IContinuousDataObject operate(Op op, float value);

    /**
     * Compute and return an operation to a specified subset of the DataObject,
     * with a DataObject of values providing the operands. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>multiplyGrid(DataObject, Grid2DBit)
     * <li>divideGrid(DataObject, Grid2DBit)
     * </ul>
     *
     * @param op
     *            the operation to perform
     * @param dataObject
     *            the DataObject containing values to use as an operand
     * @param editArea
     *            the area to apply the operation over. If null, apply over the
     *            entire grid
     * @return the result of the operation
     */
    public IContinuousDataObject operate(Op op,
            IContinuousDataObject dataObject, Grid2DBit editArea);

    /**
     * Compute and return an operation to a DataObject, with a DataObject of
     * values providing the operands. <BR>
     * <BR>
     * <B>NOTE:</B> In AWIPS I GFE, this replaces the following methods:
     * <ul>
     * <li>multiplyGrid(DataObject)
     * <li>divideGrid(DataObject)
     * </ul>
     *
     * @param op
     *            the operation to perform
     * @param dataObject
     *            the DataObject containing values to use as an operand
     * @return the result of the operation
     */
    public IContinuousDataObject operate(Op op,
            IContinuousDataObject dataObject);

    /**
     * Calculates the minimum of the specified DataObject and this DataObject
     * and returns the result in a DataObject.
     *
     * @param gs
     *            the DataObject to compare to
     * @return the minimum-valued DataObject
     */
    public IContinuousDataObject min(IContinuousDataObject gs);

    /**
     * Calculates the maximum of the specified DataObject and this DataObject
     * and returns the result in a DataObject.
     *
     * @param gs
     *            the DataObject to compare to
     * @return the maximum-valued DataObject
     */
    public IContinuousDataObject max(IContinuousDataObject gs);

    /**
     * Calculates the sum of the specified DataObject and this DataObject and
     * returns the result in a DataObject.
     *
     * @param gs
     *            the DataObject to sum with
     * @return the sum of both DataObject
     */
    public IContinuousDataObject sum(IContinuousDataObject gs);

}
