package com.raytheon.uf.viz.monitor.ffmp.ffti;

public interface DurationInterface {
    public void updateQPEDurHour(double guidDurHour);
    public void updateTotalDurHour(double guidDurHour);
    public void updateGuidDurHour(double durHour);
    public boolean getGuidEnabled() ;
    public void updateAccumAttrib (double totalDurHr) ;
    public double getTotalDurHr();
    public double adjustTotalDurationHr (double durHr) ;
}
