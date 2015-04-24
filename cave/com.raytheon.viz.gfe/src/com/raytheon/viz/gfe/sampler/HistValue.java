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

package com.raytheon.viz.gfe.sampler;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;

/**
 * Concept of holding a scalar, vector, weather, or discrete key
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 21, 2008 1167        mnash       Initial creation
 * Aug 6, 2008  1283        njensen     Implemented Comparable
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class HistValue implements Comparable<HistValue> {

    GridType _type;

    private float _scalar;

    private float _direction;

    private DiscreteKey _discreteKey;

    private WeatherKey _weatherKey;

    /**
     * Default constructor, sets type to none and other values to 0
     */
    public HistValue() {
        _type = GridType.NONE;
        _scalar = 0;
        _direction = 0;
    }

    /**
     * Constructor taking a scalar, stores the scalar and sets other data types
     * to 0 - type to SCALAR
     * 
     * @param scalar
     */
    public HistValue(float scalar) {
        _type = GridType.SCALAR;
        _scalar = scalar;
        _direction = 0;
    }

    /**
     * Constructor taking a vector (magnitude and direction), stores those and
     * sets type to VECTOR
     * 
     * @param magnitude
     * @param direction
     */
    public HistValue(float magnitude, float direction) {
        _type = GridType.VECTOR;
        _scalar = magnitude;
        _direction = direction;
    }

    /**
     * Constructor taking a DiscreteKey, stores a DiscreteKey and sets other
     * values to 0 - type to DISCRETE
     * 
     * @param key
     */
    public HistValue(DiscreteKey key) {
        _type = GridType.DISCRETE;
        _scalar = 0;
        _direction = 0;
        _discreteKey = key;
    }

    /**
     * Constructor taking a WeatherKey, stores a WeatherKey and sets other
     * values to 0 - type to WEATHER
     * 
     * @param key
     */
    public HistValue(WeatherKey key) {
        _type = GridType.WEATHER;
        _scalar = 0;
        _direction = 0;
        _weatherKey = key;
    }

    /**
     * @return the scalar value, user should only call for scalar type.
     */
    public float scalar() {
        return _scalar;
    }

    /**
     * @return the magnitude component of the vector. User should only call this
     *         function for a vector type.
     */
    public float magnitude() {
        return _scalar;
    }

    /**
     * @return the direction component of the vector. User should only call this
     *         function for a vector type.
     */
    public float direction() {
        return _direction;
    }

    /**
     * @return the DiscreteKey. User should only call this function for a
     *         discrete type.
     */
    public DiscreteKey discrete() {
        return _discreteKey;
    }

    /**
     * @return the WeatherKey. User should only call this function for a weather
     *         type.
     */
    public WeatherKey weather() {
        return _weatherKey;
    }

    /**
     * @return the type of HistValue
     */
    public GridType dataType() {
        return _type;
    }

    @Override
    public int compareTo(HistValue o) {
        if (_type != o._type) {
            return _type.compareTo(o._type);
        } else {
            switch (_type) {
            case SCALAR:
                if (_scalar < o._scalar) {
                    return -1;
                } else if (_scalar > o._scalar) {
                    return 1;
                } else {
                    return 0;
                }
            case VECTOR:
                if (_scalar < o._scalar) {
                    return -1;
                } else if (_scalar > o._scalar) {
                    return 1;
                } else {
                    if (_direction < o._direction) {
                        return -1;
                    } else if (_direction > o._direction) {
                        return 1;
                    } else {
                        return 0;
                    }
                }
            case WEATHER:
                return _weatherKey.compareTo(o._weatherKey);

            case DISCRETE:
                return _discreteKey.compareTo(o._discreteKey);
            default:
                return 0;
            }
        }
    }

    @Override
    public String toString() {
        switch (_type) {
        case SCALAR:
            return String.valueOf(_scalar);
        case VECTOR:
            return ("(" + _scalar + "." + _direction + ")");
        case WEATHER:
            return _weatherKey.toString();
        case DISCRETE:
            return _discreteKey.toString();
        case NONE:
            return "<NONE>";
        }
        return "<NONE>";
    }
}
