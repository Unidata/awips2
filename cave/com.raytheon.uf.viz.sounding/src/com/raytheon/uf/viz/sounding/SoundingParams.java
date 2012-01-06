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
package com.raytheon.uf.viz.sounding;

import static com.raytheon.uf.common.sounding.SoundingLayer.MISSING;

import java.util.Arrays;
import java.util.Calendar;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.edex.meteoLib.PHT;
import com.raytheon.edex.meteoLib.Tsoar;
import com.raytheon.edex.meteoLib.Velocity;
import com.raytheon.edex.meteoLib.WindComp;
import com.raytheon.uf.common.sounding.ParcelLift.PARCEL_TYPE;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * contains all the function calls to existing FORTRAN routines for computing
 * the convective parameters table; used by the skew-t depictable and the
 * interactive skew-t extension.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SoundingParams {
    private VerticalSounding interleavedData;

    private VerticalSounding analysisData;

    private float[] _pressure;

    private float[] _height;

    private float[] _temp;

    private float[] _dewPoint;

    private double[] _wetbulbs;

    private float[] _uComp;

    private float[] _vComp;

    private int _hour;

    private float[] _virtTemp;

    private float[] _parcelP;

    private float[] _parcelZ;

    private float[] _parcelT;

    private float[] _parcelVirtT;

    private float[] _envVirtTemp;

    private float[] _environTemp;

    private int _numParLvls;

    private int _gustPotential;

    private float _precipWater;

    private float _LIdx;

    private float _KIndex;

    private float _Totals;

    private float _sweatIdx;

    private float _convTemp;

    private float _maxTemp;

    private float _frzgLvlZ;

    private float _frzgLvlP;

    private float _pressWBZ;

    private float _tempWBZ;

    private float _hgtWBZ;

    private float _soarIndex;

    private float _triggerTemp_C;

    private float _pLFC1;

    private float _zLFC1;

    private float _tLFC1;

    private float _pLFC2;

    private float _zLFC2;

    private float _tLFC2;

    private float _pEqLvl;

    private float _zEqLvl;

    private float _tEqLvl;

    private float _maxVVel;

    private float _hailSize;

    private float _hgtCldTop;

    private float _posBuoy;

    private float _negBuoy;

    private float _CINfrmCAPE;

    private float _richNum;

    private float _mdpi;

    private float _windex;

    private WindComp avgWind;

    private WindComp helicity;

    private float _meanMixRatio;

    private float _initParT_C;

    private float _initParTd_C;

    private float _initParP;

    private float _pressLCL;

    private float _hgtLCL;

    private float _tempLCL;

    private float _mixLCL;

    private float _thetaLCL;

    private float _theta_eLCL;

    private float _temp500;

    private float _pressCCL;

    private float _tempCCL;

    private float _hgtCCL;

    private PARCEL_TYPE _liftingMethod = PARCEL_TYPE.SURFACE;

    private boolean _restoreSkewT;

    private boolean _isInitialDisplay = true;

    private boolean _useFcstMax = true;

    private float _userLevel = 0f;

    private boolean _is12ZTime = false;

    private float _ghx = 0;

    private float _ghy = 0;

    private static final UnitConverter kelvinToCelsius = SI.KELVIN
            .getConverterTo(SI.CELSIUS);

    private static final UnitConverter celciusToKelvin = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    private float _hgtHelLyr = 3000.0f; // hgt of the layer for determining

    // helicity

    /**
     * Constructor for the sounding parameters. Used for RaobOA
     */
    public SoundingParams(VerticalSounding vs, float[] levels) {
        interleavedData = vs;
        fillOutHeights(interleavedData, false);

        analysisData = down_interp(interleavedData, levels);
    }

    /**
     * Constructor for the sounding parameters. Used for SkewT
     */
    public SoundingParams(VerticalSounding vs) {
        interleavedData = vs;

        // interpolate the current raob data
        int hgtDiff = 50;
        fillOutHeights(interleavedData, false);
        interleavedData.invalidate();
        analysisData = rebuildDataInHgt(vs, hgtDiff);

        _temp500 = transDataStyle();

        _hour = vs.getDataTime().getValidTime().get(Calendar.HOUR_OF_DAY);

        // _temp500 = interleavedData.getLayerNearest(500).getTemperature();

        clearIndices();

        // calculate the preliminary parcel info
        if (_temp.length > 3) {
            _maxTemp = computeFcstMaxTemp();

            // calculate the convective temperature; essential for
            // determining other parameters
            computeConvTemp();

            // determine the parcel temp/pressures needed for buoyancy
            // calculations
            computeParcelInfo();
        }
    }

    /**
     * @return the _pressure
     */
    public float[] getPressures() {
        return _pressure;
    }

    /**
     * @return the _height
     */
    public float[] getHeights() {
        return _height;
    }

    /**
     * @return the _temp
     */
    public float[] getTemperatures() {
        return _temp;
    }

    /**
     * @return the _wetbulbs in Celcius
     */
    public double[] getWetbulbs() {
        return _wetbulbs;
    }

    /**
     * @return the _dewPoint
     */
    public float[] getDewPoints() {
        return _dewPoint;
    }

    /**
     * @return the _uComp
     */
    public float[] getWindUs() {
        return _uComp;
    }

    /**
     * @return the _vComp
     */
    public float[] getWindVs() {
        return _vComp;
    }

    private void clearIndices() {
        _gustPotential = (int) MISSING;
        _precipWater = MISSING;
        _LIdx = MISSING;
        _KIndex = MISSING;
        _Totals = MISSING;
        _sweatIdx = MISSING;
        _convTemp = MISSING;
        _maxTemp = MISSING;
        _frzgLvlZ = MISSING;
        _frzgLvlP = MISSING;
        _pressWBZ = MISSING;
        _tempWBZ = MISSING;
        _hgtWBZ = MISSING;
        _soarIndex = MISSING;
        _triggerTemp_C = MISSING;
        _pLFC1 = MISSING;
        _zLFC1 = MISSING;
        _tLFC1 = MISSING;
        _pLFC2 = MISSING;
        _zLFC2 = MISSING;
        _tLFC2 = MISSING;
        _pEqLvl = MISSING;
        _zEqLvl = MISSING;
        _tEqLvl = MISSING;
        _maxVVel = MISSING;
        _hailSize = MISSING;
        _hgtCldTop = MISSING;
        _posBuoy = MISSING;
        _negBuoy = MISSING;
        _CINfrmCAPE = MISSING;
        _richNum = MISSING;
        _mdpi = MISSING;
        _windex = MISSING;
        _meanMixRatio = MISSING;
        _initParT_C = MISSING;
        _initParTd_C = MISSING;
        _initParP = MISSING;
        _pressLCL = MISSING;
        _hgtLCL = MISSING;
        _tempLCL = MISSING;
        _mixLCL = MISSING;
        _thetaLCL = MISSING;
        _theta_eLCL = MISSING;
        _pressCCL = MISSING;
        _tempCCL = MISSING;
        _hgtCCL = MISSING;
    }

    private float computeFcstMaxTemp() {
        float elev = analysisData.get(0).getGeoHeight();
        float maxTemp;

        // do nothing if no data or not a sounding between 06Z & 17Z
        if (elev >= MISSING || _hour < 6 || _hour >= 18 || _temp[0] >= MISSING) {
            // maxTemp = 999.0f;
            maxTemp = MISSING;
            return maxTemp;
        }

        maxTemp = computeFcstMaxTemp(_pressure, _height, _temp, _dewPoint,
                interleavedData.getStationId(), interleavedData.getDataTime(),
                (float) interleavedData.getLatitude(),
                (float) interleavedData.getLongitude());
        return maxTemp;
    }

    private void computeConvTemp() {
        int n = _temp.length;

        float levelDewPt;
        float e = 0.0f;
        // morning soundings are between 6Z and 18Z
        if (_hour >= 6 && _hour < 18) {
            // calculate the mean mixing ratio in lowest 50 mb of sounding.
            // Used for later parcel and parameter calculations.
            int j = 0;
            while (j < n && _temp[j] >= MISSING) {
                j++;
            }
            int i = j;
            _meanMixRatio = 0.0f;
            while (++i < n && _pressure[0] - _pressure[i] <= 50) {
                levelDewPt = _dewPoint[i];
                e = Controller.esat(levelDewPt);
                _meanMixRatio += (0.622 * e) / (_pressure[i] - e);
            }
            levelDewPt = _dewPoint[i];
            e = Controller.esat(levelDewPt);
            _meanMixRatio += (0.622 * e) / (_pressure[i] - e);
            _meanMixRatio /= (i - j);
        } else // at other times compute the mixing ratio at the surface
        // dewpoint
        {
            levelDewPt = _dewPoint[0];
            e = Controller.esat(levelDewPt);
            _meanMixRatio = (0.622f * e) / (_pressure[0] - e);
        }

        computeConvTemp(_pressure, _height, _temp, _meanMixRatio);
    }

    private void computeParcelInfo() {
        int n = _temp.length;
        // lack of data, so no need to continue
        if (n < 3) {
            return;
        }
        // if 06Z - 17Z sounding, use the calculated max temp as a sfctemp
        if (_hour >= 6 && _hour < 18) {
            _is12ZTime = true;
        }

        if (_liftingMethod != PARCEL_TYPE.USERSELECT) {
            _initParP = MISSING;
            _initParT_C = MISSING;
            _initParTd_C = MISSING;
            _numParLvls = 0;
        }

        if (_restoreSkewT) {
            _liftingMethod = PARCEL_TYPE.SURFACE;
        }

        switch (_liftingMethod) {

        case PMAX: // pmax method of lifting
            computePmaxLifting(_initParP, _initParT_C, _initParTd_C);
            break;

        case MEANTEMP: // mean thermal lifting
            computeMeanTLifting(_initParP, _initParT_C, _initParTd_C);
            break;

        case USERSELECT: // user-selected level
            _initParP = _userLevel;
            computeUserLifting(_initParP, _initParT_C, _initParTd_C);
            break;

        case SURFACE: // lift from the surface
        default:
            computeSurfaceLifting(_initParP, _initParT_C, _initParTd_C);
            break;
        }

        // compute buoyancy and hailsize
        hailSize();
        // compute the Wetbulb profile
        computeWBS(_pressure, _temp, _dewPoint);
    }

    /**
     * Based upon the surface values, compute the associated LCL and parcel
     * track trajectory.
     * 
     * @param parTd_C
     * @param parT_C
     * @param parP
     * 
     */
    private void computeSurfaceLifting(float parP, float parT_C, float parTd_C) {
        float parcelTd_C = 0.0f;
        float newParcelT = 0f;
        float newParcelP = 0f;
        float testNewParcelT = 0f;

        // find first occurrance in the array of valid temps and assign
        // accordingly
        for (int i = 0; i < _temp.length; i++) {
            if (_temp[i] > SoundingLayer.NODATA) {
                continue;
            }
            newParcelP = _pressure[i];
            newParcelT = (float) kelvinToCelsius.convert(_temp[i]);
            parcelTd_C = (float) kelvinToCelsius.convert(_dewPoint[i]);
            testNewParcelT = _temp[i];
            if (testNewParcelT > _maxTemp) {
                _maxTemp = computeFcstMaxTemp();
            }
            break;
        }

        if (!_is12ZTime) {
            _useFcstMax = false;
            _maxTemp = SoundingLayer.NODATA;
        }
        if (_isInitialDisplay) {
            if (_is12ZTime) {
                _maxTemp = computeFcstMaxTemp();
                newParcelT = (float) kelvinToCelsius.convert(_maxTemp);
            }
            _isInitialDisplay = false;
        }

        if (!_restoreSkewT) {
            // use the forecast max temp for parcel temp
            if (_useFcstMax) {
                newParcelT = (float) kelvinToCelsius.convert(_maxTemp);
            }

        }
        _initParT_C = newParcelT;
        _initParP = newParcelP;
        _initParTd_C = parcelTd_C;
        float sfcTemp = MISSING;

        if (_initParT_C != MISSING) {
            sfcTemp = (float) celciusToKelvin.convert(_initParT_C);
        }
        // compute the LCL
        computeLCL(_pressure, _height, _temp, _dewPoint, sfcTemp, _meanMixRatio);
        _meanMixRatio = _mixLCL;

        // compute the Wetbulb profile
        computeWBS(_pressure, _temp, _dewPoint);

        // compute parcel characteristics at each sounding level;
        computeParcelLvls(_pressure, _height, _temp, _dewPoint, newParcelP,
                newParcelT, _pressLCL, _hgtLCL, _tempLCL, _thetaLCL,
                _theta_eLCL, _mixLCL);

    }

    /**
     * 
     */
    private void computeUserLifting(float newParcelP, float parcelT,
            float parcelTd) {
        float parcelT_K = 0f;
        float parcelTd_K = 0f;
        float ParcelP = newParcelP;
        float[] parcelVals = { ParcelP, parcelT_K, parcelTd_K };
        selectLevel(parcelVals);
        ParcelP = parcelVals[0];
        parcelT_K = parcelVals[1];
        parcelTd_K = parcelVals[2];
        parcelT = (float) kelvinToCelsius.convert(parcelT_K);
        parcelTd = (float) kelvinToCelsius.convert(parcelTd_K);
        float newParcelT = parcelT;

        // compute the LCL
        computeLCL(_pressure, _height, _temp, _dewPoint, parcelT_K,
                _meanMixRatio);
        _meanMixRatio = _mixLCL;

        _initParP = ParcelP;
        _initParT_C = parcelT;
        _initParTd_C = parcelTd;

        // compute the Wetbulb profile
        computeWBS(_pressure, _temp, _dewPoint);

        if (_hgtLCL < MISSING) {
            // compute parcel characteristics at each sounding level;
            computeParcelLvls(_pressure, _height, _temp, _dewPoint, newParcelP,
                    newParcelT, _pressLCL, _hgtLCL, _tempLCL, _thetaLCL,
                    _theta_eLCL, _meanMixRatio);
        }

    }

    /**
     * @param parcelVals
     */
    private void selectLevel(float[] parcelVals) {
        float parcelT = MISSING;
        float parcelTd = MISSING;

        float newParcelP = parcelVals[0];
        int index = 0;
        // An invalid pressure level below the observed surface value
        if (newParcelP > _pressure[0]) {
            return;
        }
        // loop thru each level, finding the closest pressure and calculate
        // it's index pointer to that level
        while ((newParcelP <= _pressure[index]) && index < _pressure.length) {
            index++;
        }

        if (_pressure[index - 1] == newParcelP) {
            parcelT = _temp[index - 1];
            parcelTd = _dewPoint[index - 1];
        } else // interpolate new parcel values
        {
            float logNewP = (float) Math.log(newParcelP);
            float logP1 = (float) Math.log(_pressure[index - 1]);
            float logP2 = (float) Math.log(_pressure[index]);
            parcelT = _temp[index]
                    + ((_temp[index - 1] - _temp[index]) * ((logNewP - logP2) / (logP1 - logP2)));
            parcelTd = _dewPoint[index]
                    + ((_dewPoint[index - 1] - _dewPoint[index]) * ((parcelT - _temp[index]) / (_temp[index - 1] - _temp[index])));
        }
        parcelVals[1] = parcelT;
        parcelVals[2] = parcelTd;
    }

    /**
     * 
     */
    private void computeMeanTLifting(float parcelP, float parcelT,
            float parcelTd) {
        float parcelT_K = 0f;
        float parcelTd_K = 0f;
        float ParcelP = parcelP;
        float[] parcelVals = { ParcelP, parcelT_K, parcelTd_K };
        meanThermalParams(parcelVals);
        ParcelP = parcelVals[0];
        parcelT_K = parcelVals[1];
        parcelTd_K = parcelVals[2];
        parcelT = (float) kelvinToCelsius.convert(parcelT_K);
        parcelTd = (float) kelvinToCelsius.convert(parcelTd_K);
        float newParcelT = parcelT;
        float newParcelP = parcelP;
        float[] newP = new float[_pressure.length];
        float[] newT = new float[_pressure.length];
        float[] newTd = new float[_pressure.length];
        float[] newZ = new float[_pressure.length];
        int i = 0;
        int j = 0;
        for (i = 0; i < _pressure.length; i++) {
            if (_pressure[i] > parcelP) {
                continue;
            }
            newP[j] = _pressure[i];
            newZ[j] = _height[i];
            newT[j] = _temp[i];
            newTd[j] = _dewPoint[i];
            j++;
        }
        int newn = j;
        j = 0;
        float levelDewPt;
        float e = 0.0f;
        while (j < newn && newT[j] > 99998.0) {
            j++;
        }
        i = j;
        float meanMixRatio = 0.0f;
        while (++i < newn && newP[0] - newP[i] <= 100) {
            levelDewPt = newTd[i];
            e = Controller.esat(levelDewPt);
            meanMixRatio += (0.622 * e) / (newP[i] - e);
        }
        levelDewPt = newTd[i];
        e = Controller.esat(levelDewPt);
        meanMixRatio /= (i - j);
        _meanMixRatio = meanMixRatio;

        // compute the LCL
        computeLCL(_pressure, _height, _temp, _dewPoint, parcelT_K,
                _meanMixRatio);
        _mixLCL = _meanMixRatio;

        _initParP = ParcelP;
        _initParT_C = parcelT;
        _initParTd_C = parcelTd;

        // compute the Wetbulb profile
        computeWBS(_pressure, _temp, _dewPoint);

        if (_hgtLCL < MISSING) {
            // compute parcel characteristics at each sounding level;
            computeParcelLvls(newP, newZ, newT, newTd, newParcelP, newParcelT,
                    _pressLCL, _hgtLCL, _tempLCL, _thetaLCL, _theta_eLCL,
                    _mixLCL);
        }

    }

    /**
     * @param parcelVals
     */
    private void meanThermalParams(float[] vals) {
        // initialize variables
        float sumTheta = 0.0f; // sum of potential temps
        float meanTP = 0.0f; // mean potential temp
        int index = 0; // count of levels in lowest 100 mb
        boolean foundSurfaceP = false;
        float sfcPressure = MISSING;
        float parcelP = MISSING;
        float parcelT = MISSING;
        float parcelTd = MISSING;

        // loop thru each level, summing up the potential temperatures
        // until we are 100 mb above the surface
        while ((_pressure[0] - _pressure[index + 1] <= 100)
                && index < _pressure.length) {
            if (!foundSurfaceP && _temp[index] > -99999.0) {
                sfcPressure = _pressure[index];
                foundSurfaceP = true;
            }
            // don't want to sum up missing values, so skip if missing
            if (_temp[index] <= -99998.0) {
                continue;
            }

            sumTheta += _temp[index]
                    * Math.pow((1000.0 / _pressure[index]), 0.286);
            index++;
        }

        // compute the mean potential temp in the layer (sfc+100mb)
        float pTop = (float) (sfcPressure - 100.0);
        meanTP = (float) ((sumTheta + (_temp[index] * Math.pow((1000.0 / pTop),
                0.286))) / (index + 1));

        float thetaFirst, thetaSecond;

        // iterate thru the layer and determine mean parcel characteristics
        // by comparing mean pot temp with adjacent sounding levels
        for (int i = 0; i < index; i++) {
            thetaFirst = (float) (_temp[i] * Math.pow((1000.0 / _pressure[i]),
                    0.286));
            thetaSecond = (float) (_temp[i + 1] * Math.pow(
                    (1000.0 / _pressure[i + 1]), 0.286));
            // determine if level representative of the mean pot temp...
            if ((thetaFirst <= meanTP && meanTP < thetaSecond)
                    || (thetaSecond <= meanTP && meanTP < thetaFirst)) {
                // ...it is, so interpolate to find P, T, & Td
                parcelT = _temp[i + 1]
                        + ((_temp[i] - _temp[i + 1]) * ((meanTP - thetaSecond) / (thetaFirst - thetaSecond)));
                parcelTd = _dewPoint[i + 1]
                        + ((_dewPoint[i] - _dewPoint[i + 1]) * ((meanTP - thetaSecond) / (thetaFirst - thetaSecond)));
                float logP1 = (float) Math.log(_pressure[i]);
                float logP2 = (float) Math.log(_pressure[i + 1]);
                parcelP = (float) Math
                        .exp(logP2
                                + ((logP1 - logP2) * ((meanTP - thetaSecond) / (thetaFirst - thetaSecond))));
                break;
            }
        }
        vals[0] = parcelP;
        vals[1] = parcelT;
        vals[2] = parcelTd;
    }

    /**
     * Based upon the pmax values, compute the associated LCL and parcel track.
     * 
     * @param parTd_C
     * @param parT_C
     * @param parP
     * 
     */
    private void computePmaxLifting(float parcelP, float parcelT, float parcelTd) {
        float parcelT_K = 0f;
        float parcelTd_K = 0f;
        float ParcelP = parcelP;
        float[] parcelVals = { ParcelP, parcelT_K, parcelTd_K };
        pmax(parcelVals);
        ParcelP = parcelVals[0];
        parcelT_K = parcelVals[1];
        parcelTd_K = parcelVals[2];
        parcelT = (float) kelvinToCelsius.convert(parcelT_K);
        parcelTd = (float) kelvinToCelsius.convert(parcelTd_K);
        float newParcelT = parcelT;
        float newParcelP = parcelP;

        // compute the LCL
        computeLCL(_pressure, _height, _temp, _dewPoint, parcelT_K,
                _meanMixRatio);
        _meanMixRatio = _mixLCL;

        _initParP = ParcelP;
        _initParT_C = parcelT;
        _initParTd_C = parcelTd;

        // compute the Wetbulb profile
        computeWBS(_pressure, _temp, _dewPoint);

        if (_hgtLCL < MISSING) {
            // compute parcel characteristics at each sounding level;
            computeParcelLvls(_pressure, _height, _temp, _dewPoint, newParcelP,
                    newParcelT, _pressLCL, _hgtLCL, _tempLCL, _thetaLCL,
                    _theta_eLCL, _mixLCL);
        }

    }

    /**
     * @param parcelP
     * @param parcelT_K
     * @param parcelTd_K
     */
    private void pmax(float[] vals) {
        Float theta_e = 0f;
        Float twTest = 0f;
        Float parcelTw = 0f;
        Float temp = 0f;
        Float dewPoint = 0f;
        Float pressure = 0f;
        int i = 0;
        float parcelTd = -MISSING;
        float parcelP = -MISSING;
        float parcelT = -MISSING;
        parcelTw = -MISSING;

        // loop thru each level, testing it's wet bulb potential temp against
        // the max value.
        while (((_pressure[0] - _pressure[++i]) <= 50) && (i < _temp.length)) {
            temp = _temp[i];
            dewPoint = _dewPoint[i];
            pressure = _pressure[i];
            theta_e = Controller.ept(temp, dewPoint, pressure);
            twTest = Controller.tsa(theta_e, pressure);
            if (twTest > parcelTw) {
                parcelTw = twTest;
                parcelP = _pressure[i];
                parcelT = _temp[i];
                parcelTd = _dewPoint[i];
            }
        }
        vals[0] = parcelP;
        vals[1] = parcelT;
        vals[2] = parcelTd;
    }

    public VerticalSounding getAnalysisData() {
        return analysisData;
    }

    public void recomputeSoundingParams(VerticalSounding vs) {
        interleavedData = vs;

        // interpolate the current raob data
        int hgtDiff = 50;
        fillOutHeights(interleavedData, false);
        interleavedData.invalidate();
        analysisData = rebuildDataInHgt(vs, hgtDiff);

        _temp500 = transDataStyle();

        _hour = vs.getDataTime().getValidTime().get(Calendar.HOUR_OF_DAY);

        // _temp500 = interleavedData.getLayerNearest(500).getTemperature();

        clearIndices();

        // calculate the preliminary parcel info
        if (_temp.length > 3) {
            _maxTemp = computeFcstMaxTemp();

            // calculate the convective temperature; essential for
            // determining other parameters
            computeConvTemp();

            // determine the parcel temp/pressures needed for buoyancy
            // calculations
            computeParcelInfo();
        }
    }

    public void recomputeParcelInfo() {
        this.computeParcelInfo();
    }

    public VerticalSounding getInterleavedData() {
        return interleavedData;
    }

    public WindComp avgWind() {
        if (avgWind == null) {
            int n = _uComp.length;
            if (n <= 3) {
                return avgWind;
            }
            float elev = analysisData.get(0).getGeoHeight();

            if (elev >= MISSING
                    || analysisData.get(n - 1).getGeoHeight() < (elev + 6000.0f)) {
                return avgWind;
            }
            if (_height.length > 0 && _pressure.length > 0 && _temp.length > 0
                    && _uComp.length > 0 && _vComp.length > 0) {
                compute0_6kmAvgWind(_height, _pressure, _temp, _uComp, _vComp,
                        elev);
            }
        }
        return avgWind;
    }

    /**
     * @return the _hgtHelLyr
     */
    public float get_hgtHelLyr() {
        return _hgtHelLyr;
    }

    public WindComp helicity() {
        if (helicity == null) {
            int n = _uComp.length;
            float elev = _height[0];
            WindComp avgWind = avgWind();
            // _SRelHelicity = 99999.0;
            // _helicity = 99999.0;

            // don't calculate anything if no data or only data below 6 km AGL
            if (elev >= MISSING || avgWind == null
                    || avgWind.getWindDirection() >= MISSING
                    || (analysisData.get(n - 1).getGeoHeight() < elev + 6000.0)) {
                return helicity;
            }

            computeSHelicity(_pressure, _height, _uComp, _vComp, elev,
                    avgWind.getWindDirection(), avgWind.getWindSpeed());
        }

        return helicity;
    }

    public int gustPotential() {
        if (_gustPotential == (int) MISSING) {
            computeGustPot(_pressure, _temp, _dewPoint);
        }
        return _gustPotential;
    }

    public float precipWater() {
        if (_precipWater == MISSING) {
            computePrecipWater(_pressure, _dewPoint);
        }
        return _precipWater;
    }

    public float liftedI() {
        if (_LIdx == MISSING) {
            int n = _temp.length;
            if (_temp500 >= MISSING
                    || analysisData.get(0).getPressure()
                            - analysisData.get(n - 1).getPressure() < 100) {
                return _LIdx;
            }

            float T0, meanTd, p0;

            if (_hour >= 6 && _hour < 18) {
                // calculate the mean Td of the lowest 50 mb & use the fcst max
                // temp
                int j = 0;
                while (j < n && _temp[j] >= MISSING) {
                    j++;
                }
                p0 = _pressure[j];
                T0 = _maxTemp;
                meanTd = _dewPoint[j] / 2;
                int i = j;
                while (i < n && (_pressure[0] - _pressure[++i] <= 50)) {
                    meanTd += _dewPoint[i];
                }
                meanTd += _dewPoint[i] / 2;
                meanTd /= (i - j);
            } else // use the sfc temp and sfc Td
            {
                int j = 0;
                while (j < n && _temp[j] >= MISSING) {
                    j++;
                }
                T0 = _temp[j];
                meanTd = _dewPoint[j];
                p0 = _pressure[j];
            }

            computeLiftIdx(T0, meanTd, p0, _temp500);
        }
        return _LIdx;
    }

    public float KIndex() {
        if (_KIndex == MISSING) {
            int n = _temp.length;
            if (n < 1) {
                return _KIndex;
            }
            if (analysisData.get(n - 1).getPressure() > 500
                    || analysisData.get(0).getPressure() < 800
                    || _temp[1] >= MISSING) {
                return _KIndex;
            }
            computeKandTotal(_pressure, _temp, _dewPoint, _temp500);
        }
        return _KIndex;
    }

    public float Totals() {
        if (_Totals == MISSING) {
            int n = _temp.length;
            if (n < 1) {
                return _Totals;
            }
            if (analysisData.get(n - 1).getPressure() > 500
                    || analysisData.get(0).getPressure() < 800
                    || _temp[1] >= MISSING) {
                KIndex();
            }
        }
        return _Totals;
    }

    public float sweatIdx() {
        if (_sweatIdx == MISSING) {
            int nTemp = _temp.length;
            int nWind = _uComp.length;
            float elev = analysisData.get(0).getGeoHeight();

            if (nTemp == 0 || nWind == 0 || elev >= MISSING
                    || _temp[1] >= MISSING) {
                return _sweatIdx;
            }

            computeSweatIndex(_pressure, _temp, _dewPoint, _uComp, _vComp);
        }
        return _sweatIdx;
    }

    public float convTemp() {
        return _convTemp;
    }

    public float maxtemp() {
        return _maxTemp;
    }

    public float frzgLvlZ() {
        if (_frzgLvlZ == MISSING) {
            int n = _temp.length;
            float elev = analysisData.get(0).getGeoHeight();

            if (n == 0 || elev >= MISSING || _temp.length < 2
                    || _temp[0] >= MISSING) {
                return _frzgLvlZ;
            }

            computeFrzgLvl(_pressure, _temp, _height);
        }
        return _frzgLvlZ;
    }

    public float frzgLvlP() {
        if (_frzgLvlP == MISSING) {
            frzgLvlZ();
        }
        return _frzgLvlP;
    }

    public float pressWBZ() {
        if (_pressWBZ == MISSING) {
            float elev = analysisData.get(0).getGeoHeight();

            if (elev >= MISSING || _temp.length < 2 || _temp[0] >= MISSING) {
                return _pressWBZ;
            }

            computeWetbulbZero(_pressure, _temp, _height, _dewPoint);
        }
        return _pressWBZ;
    }

    public float tempWBZ() {
        if (_tempWBZ == MISSING) {
            pressWBZ();
        }
        return _tempWBZ;
    }

    public float hgtWBZ() {
        if (_hgtWBZ == MISSING) {
            pressWBZ();
        }
        return _hgtWBZ;
    }

    public float soarIndex() {
        if (_hour != 12) {
            return MISSING;
        }
        if (_soarIndex >= MISSING) {
            triggerTemp_C();
        }
        return _soarIndex;
    }

    public float triggerTemp_C() {
        if (_triggerTemp_C >= MISSING) {
            float elev = analysisData.get(0).getGeoHeight();

            if (elev >= MISSING || _temp.length < 2 || _temp[0] >= MISSING) {
                return _triggerTemp_C;
            }

            computeSoaringIdx(_pressure, _height, _temp, _maxTemp);
        }
        return _triggerTemp_C;
    }

    public float pEqLvl() {
        return _pEqLvl;
    }

    public float zEqLvl() {
        return _zEqLvl;
    }

    public float tEqLvl() {
        return _tEqLvl;
    }

    public float zLFC1() {
        return _zLFC1;
    }

    public float pLFC1() {
        return _pLFC1;
    }

    public float tLFC1() {
        return _tLFC1;
    }

    public float zLFC2() {
        return _zLFC2;
    }

    public float pLFC2() {
        return _pLFC2;
    }

    public float tLFC2() {
        return _tLFC2;
    }

    public float maxVVel() {
        if (_maxVVel >= MISSING) {
            hailSize();
        }
        return _maxVVel;
    }

    public float hailSize() {
        int n = _temp.length;
        if (n <= 3 || _uComp.length <= 3) {
            return _hailSize;
        }
        if (_hailSize >= MISSING) {
            float elev = analysisData.get(0).getGeoHeight();

            if (elev >= MISSING || _initParT_C >= MISSING
                    || _pressLCL >= MISSING || _hgtLCL >= MISSING
                    || _tempLCL >= MISSING || _temp.length < 2
                    || _temp[0] >= MISSING) {
                return _hailSize;
            }
            // if no LFC or equilibrium level return with no values to compute
            if (zLFC1() >= MISSING || pEqLvl() == 99999.0) {
                return _hailSize;
            }

            // compute buoyancies, hailsize, cloud top, and Bulk Richardson
            // Number
            buoyancyProcess(_pressure, _height, _uComp, _vComp, n, _pressLCL,
                    _hgtLCL, _tempLCL, _mixLCL, _thetaLCL, _theta_eLCL);
        }
        return _hailSize;
    }

    public float hgtCldTop() {
        if (_hgtCldTop >= MISSING) {
            hailSize();
        }
        return _hgtCldTop;
    }

    public float posBuoy() {
        if (_posBuoy >= MISSING) {
            hailSize();
        }
        return _posBuoy;
    }

    public float negBuoy() {
        if (_negBuoy >= MISSING) {
            hailSize();
        }
        return _negBuoy;
    }

    public float richNum() {
        if (_richNum >= MISSING) {
            hailSize();
        }
        return _richNum;
    }

    public float mdpi() {
        if (_mdpi >= MISSING) {
            computeMDPIwindex(_pressure, _height, _temp, _dewPoint);
        }
        return _mdpi;
    }

    public float windex() {
        if (_windex >= MISSING) {
            mdpi();
        }
        return _windex;
    }

    public float initParT_C() {
        return _initParT_C;
    }

    public float initParTd_C() {
        return _initParTd_C;
    }

    public float initParP() {
        return _initParP;
    }

    public float pressLCL() {
        return _pressLCL;
    }

    public float hgtLCL() {
        return _hgtLCL;
    }

    public float tempLCL() {
        return _tempLCL;
    }

    public float pressCCL() {
        return _pressCCL;
    }

    public float tempCCL() {
        return _tempCCL;
    }

    public float hgtCCL() {
        return _hgtCCL;
    }

    /**
     * @param method
     *            the _liftingMethod to set
     */
    public void set_liftingMethod(PARCEL_TYPE method) {
        _liftingMethod = method;
        this.computeParcelInfo();
    }

    /**
     * @return the _ghx
     */
    public float get_ghx() {
        return _ghx;
    }

    /**
     * @return the _ghy
     */
    public float get_ghy() {
        return _ghy;
    }

    /**
     * @param level
     *            the _userLevel to set
     */
    public void set_userLevel(float level) {
        _userLevel = level;
    }

    /**
     * @param fcstMax
     *            the _useFcstMax to set
     */
    public void set_useFcstMax(boolean fcstMax) {
        _useFcstMax = fcstMax;
    }

    /**
     * @return the _useFcstMax
     */
    public boolean is_useFcstMax() {
        return _useFcstMax;
    }

    /**
     * calculate the amount of total precipitable water
     * 
     * @param pressure
     *            millibars
     * @param dewPoint
     *            Kelvin
     */
    public void computePrecipWater(float[] pressure, float[] dewPoint) {
        int ni = _temp.length;
        float[] mixRatio = new float[ni];
        if (ni < 1) {
            return;
        }

        mixRatio = Controller.spechum2(pressure, dewPoint);

        _precipWater = 0.0f;

        int i;
        for (i = 1; i < ni; i++) {
            if (mixRatio[i - 1] < SoundingLayer.NODATA
                    && mixRatio[i] < SoundingLayer.NODATA) {
                _precipWater += ((mixRatio[i - 1] + mixRatio[i]) * 0.5 * (pressure[i - 1] - pressure[i]));
            }
        }
        _precipWater = _precipWater / (float) (2.54 * 980.7); // in inch
    }

    /**
     * calculate the lifted index<BR>
     * <BR>
     * Based upon the computed low-level (50 mb) moisture profile, compute the
     * the lifted index via calls to existing DARE FORTRAN library routines.
     * Pass the index back to the calling routine.
     * 
     * @param initTemp
     *            Kelvin
     * @param initTd
     *            Kelvin
     * @param initial_p
     *            millibar
     * @param temp500
     *            Kelvin
     */
    public void computeLiftIdx(float initTemp, float initTd, float initial_p,
            float temp500) {

        // calculate the lifted index
        float p500 = 500.0f;
        int mni, nj, ni;
        mni = nj = ni = 1;
        float[] relaHumi = Controller.calcrh(new float[] { initTemp },
                new float[] { initTd }, mni, ni, nj);
        _LIdx = Controller.calcli(new float[] { initial_p },
                new float[] { initTemp }, relaHumi, new float[] { temp500 },
                p500, mni, ni, nj)[0];
    }

    /**
     * calculate the K index and Total-totals index
     * 
     * @param pressure
     *            millibar
     * @param temp
     *            Kelvin
     * @param dewPoint
     *            Kelvin
     * @param temp500
     *            Kelvin
     */
    public void computeKandTotal(final float[] pressure, final float[] temp,
            final float[] dewPoint, final float temp500) {
        float temp700 = 0;
        float td700 = 0;
        float lowLevel = pressure[0] > 900 ? 850 : 800;
        float temp850 = 0;
        float td850 = 0;
        for (int i = 1; i < temp.length; i++) {
            if (pressure[i] < 700) {
                break;
            }
            temp700 = temp[i];
            td700 = dewPoint[i];
            if (pressure[i] < lowLevel) {
                continue;
            }
            temp850 = temp[i];
            td850 = dewPoint[i];
        }
        if ((temp700 == MISSING || td700 == MISSING)
                || (temp850 == MISSING || td850 == MISSING)) {
            _KIndex = MISSING;
            _Totals = MISSING;
            return;
        }
        _KIndex = (temp850 - 273.15f) + (td850 - temp500) - (temp700 - td700);
        _Totals = temp850 + td850 - 2 * temp500;
    }

    /**
     * calculate the sweat index
     * 
     * @param pressure
     *            millibar
     * @param temp
     *            Kelvin
     * @param dewPoint
     *            Kelvin
     * @param uComp
     *            meters/second
     * @param vComp
     *            meters/second
     */
    public void computeSweatIndex(float[] pressure, float[] temp,
            float[] dewPoint, float[] uComp, float[] vComp) {

        _sweatIdx = 0.0f;

        _sweatIdx = Controller.sweat(pressure, temp, dewPoint, temp.length,
                pressure, uComp, vComp, uComp.length);

    }

    /**
     * calculate the average wind from the sfc to 6 km <BR>
     * <BR>
     * Define the top of the layer to average; based on the u & v components of
     * the wind, calculates the avg direction (deg) and speed (m/s)
     * 
     * @param height
     *            meters
     * @param pressure
     *            millibar
     * @param temp
     *            Kelvin
     * @param uComp
     *            meters/second
     * @param vComp
     *            meters/second
     * @param elev
     *            meters
     */
    public void compute0_6kmAvgWind(float[] height, float[] pressure,
            float[] temp, float[] uComp, float[] vComp, float elev) {
        float top = 6.0f; // km
        float bottom = 0.0f;

        avgWind = Controller.avwind(elev, top, bottom, height, pressure, temp,
                uComp, vComp);
    }

    /**
     * calculate the 0-3 km storm relative helicity and the 0-3 km storm motion<BR>
     * <BR>
     * Based upon the pre-defined layer-depth (3 km), compute the storm relative
     * helicity and storm motion via calls to existing FORTRAN library routines.
     * Pre-process the wind data to throw out bad/no data areas (99999.0).
     * Passes back to the calling class other parameters used in computing the
     * helicity contours and storm motion plot (stormDir, stormSpd, ghx, ghy,
     * uCompStorm, & vCompStorm).
     * 
     * @param pressure
     *            millibar
     * @param height
     *            meters asl
     * @param uComp
     *            meters/second
     * @param vComp
     *            meters/second
     * @param elev
     *            meters
     * @param averageDir
     *            degrees
     * @param averageSpd
     *            meters/second
     */
    public void computeSHelicity(final float[] pressure, final float[] height,
            final float[] uComp, final float[] vComp, final float elev,
            final float averageDir, final float averageSpd) {
        // calculate the helicity
        int numLvls = uComp.length;
        float[] windHeights = new float[numLvls];
        float[] windPressures = new float[numLvls];
        float[] windUComp = new float[numLvls];
        float[] windVComp = new float[numLvls];
        float ghx[] = new float[1];
        float ghy[] = new float[1];
        float average_Dir = averageDir;
        float average_Spd = averageSpd;
        float elevation = elev;

        // preprocess the wind data, excluding any missing data levels
        int index;
        for (index = 0; index < uComp.length; index++) {
            // TODO are the 999s below going to work?
            if (uComp[index] > 999.0 || vComp[index] > 999.0
                    || uComp[index] < -999.0 || vComp[index] < -999.0) {
                continue;
            }
            windHeights[index] = height[index];
            windPressures[index] = pressure[index];
            windUComp[index] = uComp[index];
            windVComp[index] = vComp[index];
        }

        helicity = Controller.calchelicity(windHeights, windPressures,
                windUComp, windVComp, elevation, _hgtHelLyr, ghx, ghy,
                average_Dir, average_Spd);
        _ghx = ghx[0];
        _ghy = ghy[0];
    }

    /**
     * calculate the convective temperature<BR>
     * <BR>
     * Uses FORTRAN routine originally written for DARE to compute the
     * convective temperature. Uses the mean mixing ratio in the lowest 50 mb of
     * sounding as part of the calculation of the CCL, which gives the conv
     * temp. Passes back the meanMixRatio and the CCL info (press, height, and
     * temp).
     * 
     * @param pressure
     *            millibar
     * @param height
     *            meters
     * @param temp
     *            Kelvin
     * @param mixRatio
     */
    public void computeConvTemp(float[] pressure, float[] height, float[] temp,
            float mixRatio) {
        PHT ccl = Controller.cclpar(mixRatio, pressure, height, temp);
        _pressCCL = ccl.getPressure();
        _tempCCL = ccl.getTemperature();
        _hgtCCL = ccl.getHeight();

        // calculate theta at the CCL
        float potTempCCL = (float) (ccl.getTemperature() * Math.pow(
                (1000.0 / ccl.getPressure()), 0.286));
        // calculate the convective temp
        _convTemp = (float) (potTempCCL * Math.pow((pressure[0] / 1000.0),
                0.286));
    }

    /**
     * calculate the forecast max temperature<BR>
     * <BR>
     * Uses FORTRAN routine originally written for DARE to compute the modeled
     * forecast max temperature valid for only the 12Z sounding. Routine is
     * based on a climo model with solar heating added to it to arrive at this
     * temp. The dataTimes object is passed into method for DTG calculations;
     * dataLoc contains the lat/lon location of the sounding; legend contains
     * the RAOB station id (used in model's lookup climo table). maxTemp is used
     * in later methods (initial parcel calculations) to compute buoyancy.
     * 
     * @param pressure
     *            millibar
     * @param height
     *            meters
     * @param temp
     *            Kelvin
     * @param dewPoint
     *            Kelvin
     * @param stnId
     * @param dataTimes
     * @param lat
     *            degrees
     * @param lon
     *            degrees
     * @return forecast max temp in Kelvin
     */
    public float computeFcstMaxTemp(float[] pressure, float[] height,
            float[] temp, float[] dewPoint, final String stnId,
            final DataTime dataTimes, final float lat, final float lon) {
        // initialize boundary conditions to the model
        Calendar cal = null;
        int year = 0;
        int month = 0;
        int day = 0;
        int hour = 0;
        int minute = 0;

        cal = dataTimes.getRefTimeAsCalendar();
        year = cal.get(Calendar.YEAR);
        month = cal.get(Calendar.MONTH) + 1;
        day = cal.get(Calendar.DAY_OF_MONTH);
        hour = cal.get(Calendar.HOUR_OF_DAY);
        minute = cal.get(Calendar.MINUTE);

        int snowDepth = 0;
        int nLvls = temp.length;

        // Default value for the max temp
        float maxTemp;
        if (temp.length > 0) {
            maxTemp = temp[0];
        } else {
            maxTemp = 1e37f;
        }

        while (nLvls > 0 && pressure[nLvls - 1] < 100.0) {
            nLvls--;
        }
        if (nLvls < 5 || pressure[nLvls - 1] > 400.0) {
            return maxTemp;
        }

        // Verify that our inputs are useful
        if (nLvls > pressure.length || nLvls > height.length
                || nLvls > temp.length || nLvls > dewPoint.length || nLvls < 5
                || dewPoint[0] > temp[0] + 5) {
            return maxTemp;
        }

        maxTemp = Controller.forecast(year, month, day, hour, minute, stnId,
                snowDepth, lat, lon, pressure, height, temp, dewPoint);

        return maxTemp;
    }

    /**
     * calculate the freezing level of the sounding<BR>
     * <BR>
     * Uses FORTRAN routine originally written for DARE to compute the freezing
     * level height (meters and mb).
     * 
     * @param pressure
     *            millibar
     * @param temp
     *            Kelvin
     * @param height
     *            meters
     */
    public void computeFrzgLvl(float[] pressure, float[] temp, float[] height) {
        // assume level starts at sfc
        _frzgLvlP = 0.0f;
        _frzgLvlZ = 0.0f;
        float elev = height[0];

        PHT frzlev = Controller.frzlev(elev, pressure, height, temp);
        _frzgLvlP = frzlev.getPressure();
        _frzgLvlZ = frzlev.getHeight();
    }

    /**
     * calculate the height of the wet-bulb zero<BR>
     * <BR>
     * Uses FORTRAN routine originally written for DARE to compute the height of
     * the wet-bulb zero.
     * 
     * @param pressure
     *            millibar
     * @param temp
     *            Kelvin
     * @param height
     *            meters
     * @param dewPoint
     *            Kelvin
     */
    public void computeWetbulbZero(float[] pressure, float[] temp,
            float[] height, float[] dewPoint) {
        // compute the height of the wet-bulb zero from sounding data
        float elev = height[0];

        _hgtWBZ = MISSING;
        _pressWBZ = MISSING;
        _tempWBZ = MISSING;

        PHT wbzero = Controller.wbzero(elev, pressure, height, temp, dewPoint);

        _hgtWBZ = wbzero.getHeight();
        _pressWBZ = wbzero.getPressure();
        _tempWBZ = wbzero.getTemperature();
    }

    /**
     * calculate the Dry Microburst Potential, a.k.a. Convective Gust Potential<BR>
     * <BR>
     * Uses FORTRAN routine originally written for DARE to compute the
     * convective gust potential. Based on a nomogram for the Western US.
     * 
     * @param pressure
     *            millibar
     * @param temp
     *            Kelvin
     * @param dewPoint
     *            Kelvin
     */
    public void computeGustPot(float[] pressure, float[] temp, float[] dewPoint) {
        if (pressure.length > 0 && temp.length > 0 && dewPoint.length > 0) {
            _gustPotential = Controller.gusts(pressure, temp, dewPoint);
        }
    }

    /**
     * calculate a measure of both the wet (mdpi) and dry (windex) microburst
     * potential. If mdpi>1, wet microbursts are likely. The windex value is an
     * actual maximum potential wind speed for dry microburst gusts.<BR>
     * <BR>
     * User should have called computeFrzgLvl first.
     * 
     * @param pressure
     *            millibar
     * @param height
     *            meters
     * @param temp
     *            Kelvin
     * @param dewPoint
     *            Kelvin
     */
    public void computeMDPIwindex(float[] pressure, float[] height,
            float[] temp, float[] dewPoint) {
        // Check whether we have the bare minimum to calculate either.
        _windex = _mdpi = MISSING;
        int numLvls = temp.length;
        if (numLvls < 5 || _frzgLvlZ >= MISSING) {
            return;
        }
        int top = numLvls - 1;
        float zbound = height[0] + 1000;
        if (pressure[top] > 660
                && (height[top] <= zbound || _frzgLvlZ <= zbound
                        || temp[0] < 273.15 || temp[top] > 273.15)) {
            return;
        }
        int nLvls = numLvls;
        int one = 1;
        int i;

        // First calculate max theta E from sfc to 150mb above sfc, min
        // theta E above 660mb, use this to calculate mdpi.
        float[] thetae;
        if (pressure[top] <= 660) {
            thetae = Controller.calcthetae2(pressure, temp, dewPoint, nLvls,
                    nLvls, one);
            float maxLo = 0;
            float pbound = pressure[0] - 150;
            for (i = 0; i < nLvls && pressure[i] >= pbound; i++) {
                if (thetae[i] > maxLo) {
                    maxLo = thetae[i];
                }
            }
            pbound = 660;
            while (i < nLvls && pressure[i] > pbound) {
                i++;
            }
            float minHi = 1e37f;
            for (; i < nLvls; i++) {
                if (thetae[i] < minHi) {
                    minHi = thetae[i];
                }
            }
            if (maxLo > 0 && minHi < 1e36) {
                _mdpi = (maxLo - minHi) / 30;
            }
        }

        // We will reuse thetae for specific humidity and calculate windex.
        if (height[top] <= zbound || _frzgLvlZ <= zbound || temp[0] < 273.15
                || temp[top] > 273.15) {
            return;
        }
        thetae = Controller.spechum2(pressure, dewPoint);// produces g/Kg
        float hm = (_frzgLvlZ - height[0]) / 1000;
        float lapse = (temp[0] - 273.15f) / hm;
        float q1km = 0;
        for (i = 1; i < nLvls && height[i] < zbound; i++) {
            q1km += (height[i] - height[i - 1]) * (thetae[i] + thetae[i - 1])
                    / 2;
        }
        float q2 = thetae[i - 1] + (zbound - height[i - 1])
                * (thetae[i] + thetae[i - 1]) / (height[i] - height[i - 1]);
        q1km += (zbound - height[i - 1]) * (q2 + thetae[i - 1]) / 2;
        q1km /= 1000;
        float rq = q1km > 12 ? 1 : q1km / 12;
        while (height[i] < _frzgLvlZ) {
            i++;
        }
        float qm = thetae[i - 1] + (_frzgLvlZ - height[i - 1])
                * (thetae[i] + thetae[i - 1]) / (height[i] - height[i - 1]);
        _windex = lapse * lapse - 30 + q1km - 2 * qm;
        if (_windex < 0) {
            _windex = 0;
        }
        _windex = (float) (5 * Math.sqrt(hm * rq * _windex));
    }

    /**
     * calculate the soaring parameters trigger temp and soaring index (ft/min)<BR>
     * <BR>
     * Uses FORTRAN routine originally written for DARE to compute the aircraft
     * soaring parameters of trigger temp and soaring index.
     * 
     * @param pressure
     *            millibar
     * @param height
     *            meters
     * @param temp
     *            Kelvin
     * @param maxTemp
     *            Kelvin
     */
    public void computeSoaringIdx(float[] pressure, float[] height,
            float[] temp, float maxTemp) {
        // compute a series of pot temps based on sounding data
        float elev = height[0];
        int numLvls = temp.length;
        float[] potTemp = new float[numLvls];
        for (int i = 0; i < numLvls; i++) {
            potTemp[i] = (float) (temp[i] * Math.pow((1000.0 / pressure[i]),
                    0.286));
        }

        Tsoar tsoar = Controller.tsoar(elev, pressure, height, temp, potTemp,
                maxTemp);
        _soarIndex = tsoar.getSoarIndex();
        _triggerTemp_C = tsoar.getTriggerTemperature();
    }

    /**
     * calculate the lifted condensation level (LCL).<BR>
     * <BR>
     * Uses FORTRAN routine originally written for DARE. Based upon the LCL,
     * calculate potential temperature at the LCL.
     * 
     * @param pressure
     *            millibar
     * @param height
     *            meters
     * @param temp
     *            Kelvin
     * @param dewPoint
     *            Kelvin
     * @param sfcTemp
     *            Kelvin
     * @param meanMixRatio
     */
    public void computeLCL(float[] pressure, float[] height, float[] temp,
            float[] dewPoint, float sfcTemp, float meanMixRatio) {

        PHT lcl = Controller.lclpar(meanMixRatio, sfcTemp, pressure, height,
                temp, dewPoint);

        // compute theta theta-e, and w at LCL
        _pressLCL = lcl.getPressure();
        _hgtLCL = lcl.getHeight();
        _tempLCL = lcl.getTemperature();
        _mixLCL = meanMixRatio;
        _thetaLCL = (float) (lcl.getTemperature() * Math.pow(
                (1000.0 / lcl.getPressure()), 0.286));
        float equivTemp = Controller.adiabatic_te(lcl.getTemperature(),
                lcl.getPressure());
        _theta_eLCL = (float) (equivTemp * Math.pow(
                (1000.0 / lcl.getPressure()), 0.286));
    }

    /**
     * @param _pressure2
     * @param _temp2
     * @param point
     */
    private void computeWBS(float[] _pressure, float[] _temp, float[] _dewpoint) {
        _wetbulbs = new double[_temp.length];
        UnitConverter kelvinToCelsius = SI.KELVIN.getConverterTo(SI.CELSIUS);
        for (int i = 0; i < _temp.length; i++) {
            if ((i % 3) != 0) {
                _wetbulbs[i] = MISSING;
                continue;
            }
            _wetbulbs[i] = WxMath.wetbulb(_pressure[i],
                    kelvinToCelsius.convert(_temp[i]),
                    kelvinToCelsius.convert(_dewpoint[i]));
            // sanity check, wetbulb should never be higher than 200 kelvin
            // anyway
            if (_wetbulbs[i] > 999) {
                _wetbulbs[i] = MISSING;
            }
        }
    }

    /**
     * calculate the initial parcel pressure (mb), temp, and dewpoints (C);
     * compute the parameters needed to calculate the Level of Free Convection
     * (LFC), the lifted parcel arrays, and the equilibrium level.<BR>
     * <BR>
     * Uses FORTRAN routines originally written for DARE. Step 1 of computing
     * the buoyancy parameters. Compute the lifted parcel arrays according to
     * the type of lifting desired. For the depictable, simply choose the 12Z
     * fcst max temp or the sfc temp of sounding. For the interactive skew-t,
     * lifted parcels are computed either from the sfc, the mean temp lowest 50
     * mb, or a user-defined temp/level. These are used to compute subsequent
     * buoyancy parameters. LFC and equilibrium level values are passed back to
     * the calling class. If no LFC can be found, then the value is flagged as
     * 99999.0 (missing) & buoyancy calculations are skipped.
     * 
     * @param pressure
     *            millibar
     * @param height
     *            meters
     * @param temp
     *            Kelvin
     * @param dewPoint
     *            Kelvin
     * @param initParP
     *            millibar
     * @param initParT_C
     *            Celsius
     * @param pressLCL
     *            millibar
     * @param hgtLCL
     *            meters
     * @param tempLCL
     *            Kelvin
     * @param thetaLCL
     *            Kelvin
     * @param theta_eLCL
     *            Kelvin
     * @param mixLCL
     */
    public void computeParcelLvls(float[] pressure, float[] height,
            float[] temp, float[] dewPoint, float initParP, float initParT_C,
            float pressLCL, float hgtLCL, float tempLCL, float thetaLCL,
            float theta_eLCL, float mixLCL) {
        // compute virtual temp and potential temp at sounding levels
        int numLvls = temp.length;
        _virtTemp = Controller.virtualt(temp, dewPoint, pressure, numLvls);

        // initially define the array bounds of the computed parcel arrays
        // as one + the number of levels in the sounding. Used for the
        // call to the FORTRAN routine 'liftedp()'.
        int numPars = numLvls + 1;

        // determine the parcel characteristics at each sounding level

        PHT liftedp = Controller.liftedp(pressure, temp, height, _virtTemp,
                numPars, pressLCL, hgtLCL, tempLCL, mixLCL, thetaLCL,
                theta_eLCL, initParP, initParT_C);
        _parcelP = liftedp.getPressureArray();
        _parcelZ = liftedp.getHeightArray();
        _parcelT = liftedp.getTemperatureArray();
        _parcelVirtT = liftedp.getVirtualTemps();
        _environTemp = liftedp.getSoundingTemps();
        _envVirtTemp = liftedp.getSoundingVirtTemps();
        _numParLvls = liftedp.getNumLevels();

        // copy the contents of parcel temps and parcel pressures for later
        // plotting
        // parcelP.setLength(_numParLvls);
        // parcelT.setLength(_numParLvls);
        // for (int i = 0; i < _numParLvls; i++) {
        // parcelP[i] = _parcelP[i];
        // parcelT[i] = _parcelT[i];
        // }

        // calculate the Level of Free Convection (LFC) via lifting
        PHT lfc = Controller.lfcpar(theta_eLCL, pressLCL, tempLCL, hgtLCL,
                _parcelT, _environTemp, _parcelP, _parcelZ, _numParLvls);
        _pLFC1 = lfc.getPressure();
        _zLFC1 = lfc.getHeight();
        _tLFC1 = lfc.getTemperature();
        _pLFC2 = lfc.getPressure1();
        _zLFC2 = lfc.getHeight1();
        _tLFC2 = lfc.getTemperature1();

        // calculate the equilibrium level, if LFC exists
        _pEqLvl = MISSING;

        if (_pLFC1 < MISSING) {
            PHT eqlev = Controller.eqlev(_parcelP, _parcelZ, _parcelT,
                    _environTemp, _pLFC1, theta_eLCL, _numParLvls);

            _pEqLvl = eqlev.getPressure();
            _zEqLvl = eqlev.getHeight();
            _tEqLvl = eqlev.getTemperature();
        }
    }

    public void buoyancyProcess(float[] pressure, float[] height,
            float[] uComp, float[] vComp, int numLvls, float pressLCL,
            float hgtLCL, float tempLCL, float mixLCL, float thetaLCL,
            float theta_eLCL) {
        // compute the vertical velocity of a lifted parcel

        Velocity vvel = Controller.vvel(pressLCL, _pEqLvl, _parcelP, _parcelZ,
                _parcelT, _envVirtTemp, _parcelVirtT, mixLCL, _numParLvls);
        float[] parcelVVel = vvel.getVerticalVelocity();
        _maxVVel = vvel.getMaxVerticalVelocity();

        // find the max hailsize
        _hailSize = Controller.hailsiz(_maxVVel);

        // calculate the estimated cloud top
        _hgtCldTop = Controller.ctop(_parcelP, _parcelZ, parcelVVel, _pEqLvl,
                _numParLvls);

        // calculate the positive and negative buoyant energies
        _posBuoy = 0.0f;
        _negBuoy = 0.0f;

        PHT posarea = Controller.posarea(_pLFC1, _pEqLvl, _tLFC1, _tEqLvl,
                _zLFC1, _zEqLvl, theta_eLCL, _parcelP, _parcelZ, _environTemp,
                _parcelT, _numParLvls);
        _posBuoy = posarea.getPositiveEnergy();
        _CINfrmCAPE = posarea.getCin();

        _negBuoy = Controller.negarea(pressLCL, tempLCL, hgtLCL, _pLFC1,
                _zLFC1, _tLFC1, thetaLCL, theta_eLCL, _parcelP, _parcelZ,
                _environTemp, _parcelT, _numParLvls, _CINfrmCAPE);

        float[] soundingRho = Controller.density(pressure, _virtTemp);

        // compute the Bulk Richardson Number
        int numWindLvls = uComp.length;
        int tmpLen = numLvls;
        if (numWindLvls > numLvls) {
            numWindLvls = numLvls;
        }
        if (numWindLvls < numLvls) {
            tmpLen = numWindLvls;
        }

        _richNum = Controller.richno(height, height, uComp, vComp, soundingRho,
                tmpLen, numWindLvls, _posBuoy);
    }

    /**
     * This routine creates a set of densely packed levels for which the data
     * set is completely filled out, using interpolation to fill in the missing
     * data.
     * 
     * @param inputData
     *            original data
     * @param hgtDiff
     *            desired height spacing in meters
     * @return interpolated data
     */
    private VerticalSounding rebuildDataInHgt(VerticalSounding inputData,
            final int hgtDiff) {
        int numInterleaveLvls = inputData.size();
        VerticalSounding rebuildData = new VerticalSounding();
        rebuildData.setDataTime(inputData.getDataTime());
        rebuildData.setElevation(inputData.getElevation());
        rebuildData.setName(inputData.getName());
        rebuildData.setObsTime(inputData.getObsTime());
        rebuildData.setSpatialInfo(inputData.getSpatialInfo());

        // check for lack of data and set length appropriately and exit
        if (numInterleaveLvls < 5) {
            return rebuildData;
        }
        // set length of analysisData to 0 & exit if data above 100 mb
        if (inputData.getMaxPressurelayer().getPressure() < 100.0f) {
            return rebuildData;
        }

        // Place the input data into linear arrays.
        int i, k;
        float[] zOrig = new float[inputData.size()];
        float[] pOrig = new float[inputData.size()];
        float[] tOrig = new float[inputData.size()];
        float[] tdOrig = new float[inputData.size()];
        float[] uOrig = new float[inputData.size()];
        float[] vOrig = new float[inputData.size()];

        for (i = 0; i < inputData.size(); i++) {
            SoundingLayer layer = inputData.get(i);
            zOrig[i] = layer.getGeoHeight();
            pOrig[i] = (float) Math.log(layer.getPressure());
            tOrig[i] = layer.getTemperature();
            tdOrig[i] = layer.getDewpoint();
            uOrig[i] = layer.getWindU();
            vOrig[i] = layer.getWindV();
        }

        // Set up an array that contains up to 500 levels every hgtDiff. Make
        // sure that the surface 700, 500 and 850 mb are included..
        float p850 = (float) Math.log(850.0);
        float p700 = (float) Math.log(700.0);
        float p500 = (float) Math.log(500.0);
        float pNext, zNext, r;
        float[] zNew = new float[500];
        float[] pNew = new float[500];
        int k850 = -1;
        int k700 = -1;
        int k500 = -1;
        int i1 = 0;
        k = 0;
        zNew[k] = zOrig[i1];
        pNew[k] = pOrig[i1];
        int zVal = hgtDiff * (int) (zNew[k] / hgtDiff);
        if (zVal <= zNew[k]) {
            zVal += hgtDiff;
        }
        if (pOrig[i1] == p850) {
            k850 = k;
        }
        i = 1;
        while (k < 499 && i < inputData.size()) {
            r = (pOrig[i] - pOrig[i1]) / (zOrig[i] - zOrig[i1]);
            while (k < 499) {
                if (zVal <= zOrig[i]) {
                    zNext = zVal;
                    pNext = pOrig[i1] + r * (zNext - zOrig[i1]);
                } else {
                    zNext = zOrig[i];
                    pNext = pOrig[i];
                }
                if (k850 < 0 && p850 > pNext && p850 < pNew[k]) {
                    k850 = ++k;
                    pNew[k] = p850;
                    zNew[k] = zOrig[i1] + (p850 - pOrig[i1]) / r;
                    continue;
                } else if (k700 < 0 && p700 > pNext && p700 < pNew[k]) {
                    k700 = ++k;
                    pNew[k] = p700;
                    zNew[k] = zOrig[i1] + (p700 - pOrig[i1]) / r;
                    continue;
                } else if (k500 < 0 && p500 > pNext && p500 < pNew[k]) {
                    k500 = ++k;
                    pNew[k] = p500;
                    zNew[k] = zOrig[i1] + (p500 - pOrig[i1]) / r;
                    continue;
                } else if (k850 < 0 && pNext == p850) {
                    k850 = k + 1;
                } else if (k700 < 0 && pNext == p700) {
                    k700 = k + 1;
                } else if (k500 < 0 && pNext == p500) {
                    k500 = k + 1;
                } else if (zVal > zNext) {
                    break;
                }
                zNew[++k] = zNext;
                pNew[k] = pNext;
                zVal += hgtDiff;
                if (zVal > zOrig[i]) {
                    break;
                }
            }
            i1 = (i++);
        }
        if (k < 499) {
            zNew[++k] = zOrig[inputData.size() - 1];
            pNew[k] = pOrig[inputData.size() - 1];
        }
        k++;

        // Do the interpolation among the rest of the items versus height.
        float[] tNew = new float[500];
        float[] tdNew = new float[500];
        float[] uNew = new float[500];
        float[] vNew = new float[500];
        vertInterp(zOrig, tOrig, inputData.size(), zNew, tNew, k, 99999);
        vertInterp(zOrig, tdOrig, inputData.size(), zNew, tdNew, k, 99999);
        vertInterp(zOrig, uOrig, inputData.size(), zNew, uNew, k, 99999);
        vertInterp(zOrig, vOrig, inputData.size(), zNew, vNew, k, 99999);

        // Put this data into the output data structure
        for (i = 0; i < k; i++) {
            SoundingLayer layer = new SoundingLayer();
            layer.setGeoHeight(zNew[i]);
            layer.setPressure((float) Math.exp(pNew[i]));
            layer.setTemperature(tNew[i]);
            layer.setDewpoint(tdNew[i]);
            Coordinate spdDir = WxMath.speedDir(uNew[i], vNew[i]);
            layer.setWindSpeed((float) spdDir.x);
            layer.setWindDirection((float) spdDir.y);

            rebuildData.addLayer(layer);
        }

        // Clean up our memory, make sure that our man levels are set exactly.
        if (k850 >= 0) {
            rebuildData.get(k850).setPressure(850);
        }
        if (k700 >= 0) {
            rebuildData.get(k700).setPressure(700);
        }
        if (k500 >= 0) {
            rebuildData.get(k500).setPressure(500);
        }

        return rebuildData;
    }

    /**
     * Assumes vertical coordinates increase with height, except that the first
     * item in the input arrays might be the earth's surface. Can produce
     * unpredictable results if items in inVert or outVert are undefined. Items
     * in outData which cannot be interpolated for are flagged with a 1e37. The
     * optional parameters gapTest and dVert refer to how large a vertical gap
     * will be considered 'missing' data. The default of gapTest is 2, which
     * means it is possible to interpolate data over a layer with one undefined
     * level in it. A value of 1 for gapTest means interpolation will only be
     * done through layers with no undefined levels, and 0 means no
     * interpolation at all; values will be assigned only when there is an exact
     * match between the input and output vertical coordinate values. The
     * parameter dVert is the largest vertical gap in terms of the vertical
     * coordinate value over which interpolation can occur. When a vertical gap
     * is identified as having undefined data, how it is treated can depend on
     * how many items in the output vertical coordinate array fall within the
     * gap. If there is only one, it will be marked undedefined in all cases. If
     * there is more than one, output levels immediately adjacent to the border
     * of the vertical gap can be assigned meaningful values.
     * 
     * @param inVert
     * @param inData
     * @param nIn
     * @param outVert
     * @param outData
     * @param nOut
     * @param gapTest
     */
    private void vertInterp(final float[] inVert, final float[] inData,
            int nIn, float[] outVert, float[] outData, int nOut, int gapTest) {
        if (nOut == 0) {
            return;
        }

        float dVert = MISSING;

        // get lowest useable input vertical coordinate and useable level
        int endIn = nIn;
        float bottom = inVert[0];
        int index = 0;
        if (nIn == 0 || bottom < MISSING && inData[0] < MISSING) {
            ;
        } else if (bottom < MISSING) {
            do {
                index++;
            } while (index < endIn
                    && (inVert[index] <= bottom || inVert[index] >= MISSING || inData[index] >= MISSING));
        } else {
            do {
                index++;
                if (inVert[index] < bottom) {
                    bottom = inVert[index];
                }
            } while (index < endIn
                    && (inVert[index] >= MISSING || inData[index] >= MISSING));
        }

        // Handle case of no useable input data at all.
        int endOut = nOut;
        int outdex = 0;
        if (index >= endIn) {
            while (outdex < endOut) {
                outData[outdex] = MISSING;
                outdex++;
            }
            return;
        }

        // fill stuff below first level with flags
        float prevVert = MISSING;
        while (outdex < endOut
                && (outVert[outdex] < inVert[index] || outVert[outdex] >= MISSING)) {
            outData[outdex] = MISSING;
            prevVert = outVert[outdex++];
        }

        // See if we put data into the output level immediately below our
        // column.
        if (prevVert >= MISSING || bottom < inVert[index] && prevVert < bottom) {
            prevVert = MISSING;
        } else if (dVert < MISSING) {
            if (gapTest >= 2 && inVert[index] - prevVert < dVert / 2) {
                outData[outdex - 1] = inData[index];
            }
            prevVert = MISSING;
        } else if (gapTest == 2) {
            prevVert = MISSING;
        }
        if (outdex >= endOut && prevVert >= MISSING) {
            return;
        }

        // initialize second input data level
        int endAt = endOut - 1;
        float dprev = 0;
        int next = index + 1;
        while (next < endIn
                && (inVert[next] <= bottom || inData[next] >= MISSING)) {
            dprev = inVert[next] - inVert[next - 1];
            next++;
        }

        // See if we put data into the output level immediately below our
        // column.
        if (prevVert < MISSING && inVert[index] - prevVert < dprev / 2) {
            outData[outdex - 1] = inData[index];
        }

        // Always copy if vert coord exactly the same
        if (outVert[outdex] == inVert[index]) {
            outData[outdex] = inData[index];
            outdex++;
        }
        if (outdex >= endOut) {
            return;
        }

        // Loop through each pair of bracketing input levels
        float wgt;
        float dnow;
        while (next < endIn) {
            dnow = inVert[next] - inVert[index];

            if (dnow == 0 || outVert[outdex] >= inVert[next]) {
                ;
            } else if (next - index <= gapTest && dnow <= dVert) {
                // Usual case of interpolating with no gap
                do {
                    wgt = (outVert[outdex] - inVert[index]) / dnow;
                    outData[outdex] = inData[index] + wgt
                            * (inData[next] - inData[index]);
                    outdex++;
                } while (outdex < endOut && outVert[outdex] < inVert[next]);
            } else if (nOut < 3 || gapTest == 0) {
                // Just mark everything between as undefined
                do {
                    outData[outdex] = MISSING;
                    outdex++;
                } while (outdex < endOut && outVert[outdex] < inVert[next]);
            } else {
                if (dprev <= 0 || dprev > dnow / 3) {
                    dprev = dnow / 3;
                }
                int gapstat = 0;
                do {
                    if (gapstat == 0
                            && outVert[outdex] - inVert[index] <= dprev) {
                        wgt = (outVert[outdex] - inVert[index]) / dnow;
                        outData[outdex] = inData[index] + wgt
                                * (inData[next] - inData[index]);
                        gapstat = 1;
                    } else if (outdex != endAt && outVert[1] <= inVert[next]) {
                        outData[outdex] = MISSING;
                        gapstat |= 2;
                    } else if ((gapstat & 2) != 0
                            && inVert[next] - outVert[outdex] <= dprev) {
                        wgt = (outVert[outdex] - inVert[index])
                                / (inVert[next] - inVert[index]);
                        outData[outdex] = inData[index] + wgt
                                * (inData[next] - inData[index]);
                    } else {
                        if (gapstat == 1) {
                            outData[outdex - 1] = MISSING;
                        }
                        outData[outdex] = MISSING;
                    }
                    outdex++;
                } while (outdex < endOut && outVert[outdex] < inVert[next]);
            }

            // Always copy if vert coord exactly the same
            if (outdex >= endOut) {
                break;
            }
            if (outVert[outdex] == inVert[next]) {
                outData[outdex] = inData[next];
                outdex++;
                if (outdex >= endOut) {
                    break;
                }
            }

            // Advance to next avaliable input level, check if there are still
            // any levels to process yet.
            index = next;
            do {
                next++;
            } while (next < endIn && inData[next] >= MISSING);
            if (next >= endIn) {
                break;
            }
            dprev = inVert[index] - inVert[index - 1];
        }

        // See if we put data into the output level immediately above our
        // column.
        if (outdex >= endOut || index >= endIn || gapTest < 2) {
            ;
        } else if (dVert < MISSING
                && outVert[outdex] - inVert[index] < dVert / 2 || gapTest > 2
                && dVert >= MISSING
                && outVert[outdex] - inVert[index] < dprev / 2) {
            outData[outdex] = inData[index];
            outdex++;
        }

        // flag rest of levels
        while (outdex < endOut) {
            outData[outdex] = MISSING;
            outdex++;
        }
    }

    /**
     * transfer the SeqOf<RaobInterleaveLevel> data to each SeqOf<float> and
     * return the 500mb temperature.
     * 
     * @return
     */
    private float transDataStyle()

    {
        int i;
        float temp500 = MISSING;
        _pressure = new float[analysisData.size()];
        _dewPoint = new float[analysisData.size()];
        _temp = new float[analysisData.size()];
        _height = new float[analysisData.size()];
        _uComp = new float[analysisData.size()];
        _vComp = new float[analysisData.size()];

        int nTemp = -1;
        int nWind = -1;
        for (i = 0; i < analysisData.size(); i++) {
            SoundingLayer layer = analysisData.get(i);
            _pressure[i] = layer.getPressure();
            _height[i] = layer.getGeoHeight();
            _temp[i] = layer.getTemperature();
            _dewPoint[i] = layer.getDewpoint();
            _uComp[i] = layer.getWindU();
            _vComp[i] = layer.getWindV();

            if (_pressure[i] == 500 || (int) _pressure[i] == 500
                    && temp500 == MISSING) {
                temp500 = _temp[i];
            }
            if (_temp[i] < MISSING) {
                nTemp = i;
            }
            if (_uComp[i] < MISSING) {
                nWind = i;
            }
        }

        // resize arrays
        _temp = Arrays.copyOf(_temp, ++nTemp);
        _dewPoint = Arrays.copyOf(_dewPoint, nTemp);
        _uComp = Arrays.copyOf(_uComp, ++nWind);
        _vComp = Arrays.copyOf(_vComp, nWind);

        return temp500;
    }

    private void fillOutHeights(VerticalSounding levels, boolean recursive) {
        if (levels.size() < 3) {
            return;
        }

        SoundingLayer level;
        int k;

        // If not a nested call, remove any levels with no pressure or height.
        float zTop = -100;
        float pTop = 1250;
        if (!recursive) {
            for (k = 0; k < levels.size();) {
                level = levels.get(k);
                if ((level.getGeoHeight() < zTop || level.getGeoHeight() > 88888)
                        && (level.getPressure() > pTop || level.getPressure() < 5)) {
                    levels.removeLayer(level);
                } else {
                    k++;
                }
            }
        }

        int l1, l2;
        l1 = l2 = -1;
        SoundingLayer level1 = null;
        SoundingLayer level2 = null;
        double lp1, lp2, r, r1, r2;
        boolean pNo, zNo;
        int kTop = -1;
        int nOk = 0;

        // Make the surface level with std atmosphere if we can.
        level = levels.get(0);
        if ((level.getGeoHeight() <= zTop || level.getGeoHeight() > 88888)
                && level.getPressure() <= pTop && level.getPressure() > 5) {
            level.setGeoHeight(Controller.ptozsa(level.getPressure()));

        } else if ((level.getPressure() >= pTop || level.getPressure() < 5)
                && level.getGeoHeight() >= zTop && level.getGeoHeight() < 88888) {
            level.setPressure(Controller.ztopsa(level.getGeoHeight()));
        }

        // Do interpolation of height vs log pressure.
        for (k = 0; k < levels.size(); k++) {
            level = levels.get(k);
            if (level.getGeoHeight() <= zTop || level.getGeoHeight() > 88888) {
                continue;
            }
            if (level.getPressure() >= pTop || level.getPressure() < 5) {
                continue;
            }
            nOk++;
            zTop = level.getGeoHeight();
            pTop = level.getPressure();
            kTop = k;
            level1 = level2;
            l1 = l2;
            level2 = level;
            l2 = k;
            if (l1 < 1 || l2 - l1 <= 1) {
                continue;
            }
            lp1 = Math.log(level1.getPressure());
            lp2 = Math.log(level2.getPressure());
            r = (level2.getGeoHeight() - level1.getGeoHeight()) / (lp2 - lp1);
            for (int i = l1 + 1; i < l2; i++) {
                SoundingLayer intLvl = levels.get(i);
                pNo = intLvl.getPressure() >= level1.getPressure()
                        || intLvl.getPressure() <= level2.getPressure();
                zNo = intLvl.getGeoHeight() <= level1.getGeoHeight()
                        || intLvl.getGeoHeight() >= level2.getGeoHeight();
                if (pNo == zNo) {
                    continue;
                }
                nOk++;
                if (zNo) {
                    intLvl.setGeoHeight((float) (level1.getGeoHeight() + r
                            * (Math.log(intLvl.getPressure()) - lp1)));
                } else {
                    intLvl.setPressure((float) Math.exp(lp1
                            + (intLvl.getGeoHeight() - level1.getGeoHeight())
                            / r));
                }
            }
        }

        // If all levels have p and z defined, we are done.
        if (nOk >= levels.size()) {
            return;
        }

        // Perform the hypsometric calculation for remaining levels if possible.
        if (kTop < levels.size()
                && kTop >= 0
                && l2 > 0
                && (l1 > 0 || level2.getTemperature() > 100
                        && level2.getTemperature() < 350)) {
            lp2 = Math.log(level2.getPressure());
            if (l1 > 0) {
                lp1 = Math.log(level1.getPressure());
                r1 = (level2.getGeoHeight() - level1.getGeoHeight())
                        / (lp2 - lp1);
            } else {
                r1 = -Controller.dzdlnp(level2.getPressure(),
                        level2.getTemperature(), level2.getDewpoint());
            }
            level1 = level2;
            l1 = l2;
            lp1 = lp2;
            for (k = kTop + 1; k < levels.size(); k++) {
                SoundingLayer intLvl = levels.get(k);
                pNo = intLvl.getPressure() >= pTop || intLvl.getPressure() < 5;
                zNo = intLvl.getGeoHeight() <= zTop
                        || intLvl.getGeoHeight() > 88888;
                if (pNo == zNo) {
                    continue;
                }
                nOk++;
                if (!pNo && intLvl.getTemperature() > 100
                        && intLvl.getTemperature() < 373) {
                    r2 = -Controller.dzdlnp(intLvl.getPressure(),
                            intLvl.getTemperature(), intLvl.getDewpoint());
                    r = (r1 + r2) / 2;
                    lp2 = Math.log(intLvl.getPressure());
                    intLvl.setGeoHeight((float) (level1.getGeoHeight() + r
                            * (lp2 - lp1)));
                    r1 = r2;
                    level1 = intLvl;
                    l1 = k;
                    lp1 = lp2;
                } else if (zNo) {
                    intLvl.setGeoHeight((float) (level1.getGeoHeight() + r1
                            * (Math.log(intLvl.getPressure()) - lp1)));
                } else {
                    intLvl.setPressure((float) Math.exp(lp1
                            + (intLvl.getGeoHeight() - level1.getGeoHeight())
                            / r1));
                }
            }
        }

        // If all levels have p and z defined, we are done.
        if (nOk >= levels.size()) {
            return;
        }

        // If this is a recursive call, just define all heights by std atm.
        if (recursive) {
            for (k = 0; k < levels.size(); k++) {
                level = levels.get(k);
                if (level.getGeoHeight() < -100
                        || level.getGeoHeight() >= 88888) {
                    level.setGeoHeight(Controller.ptozsa(level.getPressure()));
                }
            }
            return;
        }

        // If there are any undefined pressures, set them by standard atmosphere
        // sort by pressure, and call this method recursively.
        for (k = 0; k < levels.size(); k++) {
            level = levels.get(k);
            if (level.getPressure() < 5 || level.getPressure() > 1250) {
                level.setPressure(Controller.ztopsa(level.getGeoHeight()));
            }
        }
        levels.sortByPressure();
        fillOutHeights(levels, true);
    }

    private VerticalSounding down_interp(VerticalSounding inputData,
            float[] levels) {
        VerticalSounding outputData = new VerticalSounding();
        outputData.setDataTime(inputData.getDataTime());
        outputData.setElevation(inputData.getElevation());
        outputData.setName(inputData.getName());
        outputData.setObsTime(inputData.getObsTime());
        outputData.setSpatialInfo(inputData.getSpatialInfo());

        // Place the input data into linear arrays.
        float[] zOrig = new float[inputData.size()];
        float[] pOrig = new float[inputData.size()];
        float[] tOrig = new float[inputData.size()];
        float[] tdOrig = new float[inputData.size()];
        float[] uOrig = new float[inputData.size()];
        float[] vOrig = new float[inputData.size()];

        for (int i = 0; i < inputData.size(); i++) {
            SoundingLayer layer = inputData.get(i);
            zOrig[i] = layer.getGeoHeight();
            pOrig[i] = (float) -Math.log(layer.getPressure());
            tOrig[i] = layer.getTemperature();
            tdOrig[i] = layer.getDewpoint();
            uOrig[i] = layer.getWindU();
            vOrig[i] = layer.getWindV();
        }

        int ni = inputData.size();
        int nio = levels.length;
        float[] val = new float[nio];
        for (int k = 0; k < nio; k++) {
            if (levels[k] > 1e36) {
                val[k] = 1e37f;
            } else if (levels[k] <= 0) {
                val[k] = -levels[k];
            } else {
                val[k] = (float) -Math.log(levels[k]);
            }
        }
        float[] zNew = new float[nio];
        float[] tNew = new float[nio];
        float[] tdNew = new float[nio];
        float[] uNew = new float[nio];
        float[] vNew = new float[nio];
        vertInterp(pOrig, zOrig, ni, val, zNew, nio, 2);
        vertInterp(pOrig, tOrig, ni, val, tNew, nio, 2);
        vertInterp(pOrig, tdOrig, ni, val, tdNew, nio, 2);
        vertInterp(pOrig, uOrig, ni, val, uNew, nio, 2);
        vertInterp(pOrig, vOrig, ni, val, vNew, nio, 2);

        // Put this data into the output data structure
        for (int i = 0; i < nio; i++) {
            SoundingLayer layer = new SoundingLayer();
            layer.setGeoHeight(zNew[i]);
            layer.setPressure(levels[i]);
            layer.setTemperature(tNew[i]);
            layer.setDewpoint(tdNew[i]);
            Coordinate spdDir = WxMath.speedDir(uNew[i], vNew[i]);
            layer.setWindSpeed((float) spdDir.x);
            layer.setWindDirection((float) spdDir.y);

            outputData.addLayer(layer);
        }

        return outputData;
    }

}
