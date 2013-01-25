package com.raytheon.uf.viz.datadelivery.filter;

import com.raytheon.uf.viz.datadelivery.filter.FilterImages.ExpandItemState;

public interface IFilterUpdate {
    void filterUpdate(int index, ExpandItemState state);
}
