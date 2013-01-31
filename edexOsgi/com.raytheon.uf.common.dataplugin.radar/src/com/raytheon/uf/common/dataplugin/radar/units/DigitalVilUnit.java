package com.raytheon.uf.common.dataplugin.radar.units;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Quantity;
import javax.measure.unit.DerivedUnit;
import javax.measure.unit.ProductUnit;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

public class DigitalVilUnit extends DerivedUnit<Quantity> {

    private static final long serialVersionUID = 1L;

    private final float linearScale;

    private final float linearOffset;

    private final short logStart;

    private final float logScale;

    private final float logOffset;

    public DigitalVilUnit(short[] thresholds) {
        linearScale = vilShortToFloat(thresholds[0]);
        linearOffset = vilShortToFloat(thresholds[1]);
        logStart = thresholds[2];
        logScale = vilShortToFloat(thresholds[3]);
        logOffset = vilShortToFloat(thresholds[4]);
    }

    @Override
    public Unit<Quantity> getStandardUnit() {
        return new ProductUnit<Quantity>(SI.KILOGRAM.divide(SI.SQUARE_METRE));
    }

    @Override
    public UnitConverter toStandardUnit() {
        return new VilToStdConverter(this);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Float.floatToIntBits(linearOffset);
        result = prime * result + Float.floatToIntBits(linearScale);
        result = prime * result + Float.floatToIntBits(logOffset);
        result = prime * result + Float.floatToIntBits(logScale);
        result = prime * result + logStart;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DigitalVilUnit other = (DigitalVilUnit) obj;
        if (Float.floatToIntBits(linearOffset) != Float
                .floatToIntBits(other.linearOffset))
            return false;
        if (Float.floatToIntBits(linearScale) != Float
                .floatToIntBits(other.linearScale))
            return false;
        if (Float.floatToIntBits(logOffset) != Float
                .floatToIntBits(other.logOffset))
            return false;
        if (Float.floatToIntBits(logScale) != Float
                .floatToIntBits(other.logScale))
            return false;
        if (logStart != other.logStart)
            return false;
        return true;
    }

    public static float vilShortToFloat(short x) {
        int s = (x >> 15) & 0x1;
        int e = (x >> 10) & 0x1f;
        int f = x & 0x3ff;

        float value;
        if (e == 0) {
            value = (float) (Math.pow(-1, s) * 2 * (f / Math.pow(2, 10)));
        } else {
            value = (float) ((Math.pow(-1, s) * Math.pow(2, e - 16) * (1 + f
                    / Math.pow(2, 10))));
        }
        return value;
    }

    private static class VilToStdConverter extends UnitConverter {

        private static final long serialVersionUID = 1L;

        private final DigitalVilUnit vilUnit;

        public VilToStdConverter(DigitalVilUnit vilUnit) {
            this.vilUnit = vilUnit;
        }

        @Override
        public UnitConverter inverse() {
            return new StdToVilConverter(vilUnit);
        }

        @Override
        public double convert(double x) {
            if (x < vilUnit.logStart) {
                return (x - vilUnit.linearOffset) / vilUnit.linearScale;
            } else {
                return Math.exp((x - vilUnit.logOffset) / vilUnit.logScale);
            }
        }

        @Override
        public boolean isLinear() {
            return false;
        }

    }

    private static class StdToVilConverter extends UnitConverter {

        private static final long serialVersionUID = 1L;

        private final DigitalVilUnit vilUnit;

        public StdToVilConverter(DigitalVilUnit vilUnit) {
            this.vilUnit = vilUnit;
        }

        @Override
        public UnitConverter inverse() {
            return new VilToStdConverter(vilUnit);
        }

        @Override
        public double convert(double x) {
            if (x < inverse().convert(vilUnit.logStart)) {
                return Math.round(x * vilUnit.linearScale
                        + vilUnit.linearOffset);
            } else {
                return Math.round(vilUnit.logScale * Math.log(x)
                        + vilUnit.logOffset);
            }
        }

        @Override
        public boolean isLinear() {
            return false;
        }

    }
}
