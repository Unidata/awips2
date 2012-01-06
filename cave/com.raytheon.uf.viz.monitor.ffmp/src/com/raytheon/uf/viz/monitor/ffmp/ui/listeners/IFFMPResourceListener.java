package com.raytheon.uf.viz.monitor.ffmp.ui.listeners;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResourceData;

public interface IFFMPResourceListener {

    public void centerChanged(FFMPScreenCenterEvent fsce) throws VizException;

    public void hucChanged();

    public void domainChanged();

    public void refresh();

    public void traceChanged(FFMPStreamTraceEvent fste);

    public void closeDialog();

    public void setQuery(boolean isQuery);

    public void clear();

    public void dirty();

    public void setPaintTime(DataTime paintTime);

    public DataTime getPaintTime();

    public void colorUtilsChange();

    public void clearWorstCase();

    public boolean isAutoRefresh();

    public void setLinkToFrame(boolean isLinkToFrame);

    public void updateDialog();

    public FFMPResourceData getResourceData();

}
