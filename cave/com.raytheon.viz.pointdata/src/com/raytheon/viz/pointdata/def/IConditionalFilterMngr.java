package com.raytheon.viz.pointdata.def;

import java.util.ArrayList;
import java.util.HashMap;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 *
 * Interface for the NCP, D2D implementations of Conditional Filter.
 *
 * <pre>
*
* SOFTWARE HISTORY
*
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* 12/10/2019   72281      K Sunil     Initial Creation
 *
 * </pre>
 *
 * @author ksunil
 */

public interface IConditionalFilterMngr {

    public static final String NULL_FILTER_NAME = "NoFilter";

    public ArrayList<String> getPlugins();

    public HashMap<String, ConditionalFilter> getConditionalFiltersByPlugin(
            String plgn);

    public String[] getAllConditionalFiltersByPlugin(String plgn);

    public ConditionalFilter getConditionalFilter(String plgn,
            String ConditionalFilterName);

    public void saveConditionalFilter(ConditionalFilter condFilter)
            throws VizException;

    public void deleteConditionalFilter(String pluginName,
            String condFilterName) throws VizException;

}
