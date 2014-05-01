package com.raytheon.uf.viz.monitor.ffmp.ffti;

public class FFTIAttribute {
    private double min;

    private double yellowThreshold;

    private double redThreshold;

    private double max;

    private double inc;

    public FFTIAttribute(double inc, double min, double yellowThreshold, double redThreshold, double max) {
        this.min = min;
        this.yellowThreshold = yellowThreshold;
        this.redThreshold = redThreshold;
        this.max = max;
        this.inc = inc;
    }

    public void setMin(double min) {
        this.min = min;
    }

    public double getMin() {
        return min;
    }

    public void setYellowThreshold(double yellowThreshold) {
        this.yellowThreshold = yellowThreshold;
    }

    public double getYellowThreshold() {
        return yellowThreshold;
    }

    public void setRedThreshold(double redThreshold) {
        this.redThreshold = redThreshold;
    }

    public double getRedThreshold() {
        return redThreshold;
    }

    public void setMax(double max) {
        this.max = max;
        if (redThreshold > max)
        {
            redThreshold = max;
        }

        if (yellowThreshold > redThreshold)
        {
            yellowThreshold = redThreshold;
        }
    }

    public double getMax() {
        return max;
    }

    public void setInc(double inc) {
        this.inc = inc;
    }

    public double getInc() {
        return inc;
    }
}
