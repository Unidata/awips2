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

import java.nio.FloatBuffer;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

/**
 * 
 * A class which holds data for a grid. Includes FloatBuffers for holding scalar
 * or vector data as well as the dataUnits.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GeneralGridData {

    private FloatBuffer scalarData;

    private FloatBuffer direction = null;

    private FloatBuffer uComponent = null;

    private FloatBuffer vComponent = null;

    private Unit<?> dataUnit;

    /**
     * Create a scalar grid Data object.
     * 
     * @param scalarData
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createScalarData(FloatBuffer scalarData,
            Unit<?> dataUnit) {
        return new GeneralGridData(scalarData, dataUnit);
    }

    /**
     * Create GridData for a vector. Providing (u,v) and (mag,dir) is redundant
     * and it will be assumed that these are equivalent. This should only be
     * used when both these representations are readily available to save time
     * if one or the other is needed later.
     * 
     * @param magnitude
     * @param direction
     * @param uComponent
     * @param vComponent
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorData(FloatBuffer magnitude,
            FloatBuffer direction, FloatBuffer uComponent,
            FloatBuffer vComponent, Unit<?> dataUnit) {
        return new GeneralGridData(magnitude, direction, uComponent,
                vComponent, dataUnit);
    }

    /**
     * Create gridData for a vector by providing the magnitude and direction of
     * the vector.
     * 
     * @param magnitude
     * @param direction
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorData(FloatBuffer magnitude,
            FloatBuffer direction, Unit<?> dataUnit) {
        return new GeneralGridData(magnitude, direction, null, null, dataUnit);
    }

    /**
     * Create gridData for a vector by providing the u and v components of the
     * vector
     * 
     * @param uComponent
     * @param vComponent
     * @param dataUnit
     * @return
     */
    public static GeneralGridData createVectorDataUV(FloatBuffer uComponent,
            FloatBuffer vComponent, Unit<?> dataUnit) {
        return new GeneralGridData(null, null, uComponent, vComponent, dataUnit);
    }

    private GeneralGridData(FloatBuffer scalarData, Unit<?> dataUnit) {
        this.scalarData = scalarData;
        this.dataUnit = dataUnit;
    }

    private GeneralGridData(FloatBuffer magnitude, FloatBuffer direction,
            FloatBuffer uComponent, FloatBuffer vComponent, Unit<?> dataUnit) {
        this.scalarData = magnitude;
        this.direction = direction;
        this.uComponent = uComponent;
        this.vComponent = vComponent;
        this.dataUnit = dataUnit;
    }

    /**
     * Attempt to convert this data to the new unit. If this is successful then
     * the dataUnit and data will be changed.
     * 
     * @param unit
     * @return true if units are compatible, false if data is unchanged.
     */
    public boolean convert(Unit<?> unit) {
        if (dataUnit == null && unit == null) {
            return true;
        } else if (dataUnit == null) {
            return false;
        }
        if (!dataUnit.isCompatible(unit)) {
            return false;
        }
        UnitConverter converter = dataUnit.getConverterTo(unit);
        if (scalarData != null) {
            scalarData.rewind();
            FloatBuffer newData = FloatBuffer.allocate(scalarData.capacity());
            while (scalarData.hasRemaining()) {
                newData.put((float) converter.convert(scalarData.get()));
            }
            newData.rewind();
            scalarData = newData;
        }
        if (uComponent != null) {
            uComponent.rewind();
            FloatBuffer newData = FloatBuffer.allocate(uComponent.capacity());
            while (uComponent.hasRemaining()) {
                newData.put((float) converter.convert(uComponent.get()));
            }
            newData.rewind();
            uComponent = newData;
        }
        if (vComponent != null) {
            vComponent.rewind();
            FloatBuffer newData = FloatBuffer.allocate(vComponent.capacity());
            while (vComponent.hasRemaining()) {
                newData.put((float) converter.convert(vComponent.get()));
            }
            newData.rewind();
            vComponent = newData;
        }
        dataUnit = unit;
        return true;
    }

    public boolean isVector() {
        return (scalarData != null && direction != null)
                || (uComponent != null && vComponent != null);
    }

    public FloatBuffer getMagnitude() {
        if (scalarData == null && uComponent != null && vComponent != null) {
            uComponent.rewind();
            vComponent.rewind();
            scalarData = FloatBuffer.allocate(uComponent.capacity());
            while (vComponent.hasRemaining()) {
                scalarData.put((float) Math.hypot(uComponent.get(),
                        vComponent.get()));
            }
            uComponent.rewind();
            vComponent.rewind();
            scalarData.rewind();
        }
        return scalarData;
    }

    public FloatBuffer getScalarData() {
        return getMagnitude();
    }

    public FloatBuffer getDirection() {
        if (direction == null && uComponent != null && vComponent != null) {
            uComponent.rewind();
            vComponent.rewind();
            direction = FloatBuffer.allocate(uComponent.capacity());
            while (vComponent.hasRemaining()) {
                direction.put((float) Math.toDegrees(Math.atan2(
                        uComponent.get(), vComponent.get())));
            }
            uComponent.rewind();
            vComponent.rewind();
            direction.rewind();
        }
        return direction;
    }

    public FloatBuffer getUComponent() {
        if (uComponent == null && scalarData != null && direction != null) {
            scalarData.rewind();
            direction.rewind();
            uComponent = FloatBuffer.allocate(scalarData.capacity());
            while (scalarData.hasRemaining()) {
                double angle = Math.toRadians(direction.get());
                uComponent.put((float) (Math.sin(angle) * scalarData.get()));
            }
            scalarData.rewind();
            direction.rewind();
            uComponent.rewind();
        }
        return uComponent;
    }

    public FloatBuffer getVComponent() {
        if (vComponent == null && scalarData != null && direction != null) {
            scalarData.rewind();
            direction.rewind();
            vComponent = FloatBuffer.allocate(scalarData.capacity());
            while (scalarData.hasRemaining()) {
                double angle = Math.toRadians(direction.get());
                vComponent.put((float) (Math.cos(angle) * scalarData.get()));
            }
            scalarData.rewind();
            direction.rewind();
            vComponent.rewind();
        }
        return vComponent;
    }

    public Unit<?> getDataUnit() {
        return dataUnit;
    }

}
