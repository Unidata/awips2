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
package com.raytheon.edex.transform.shef;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractFilterElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractObsFilter;
import com.raytheon.uf.edex.decodertools.core.filterimpl.PluginDataObjectFilter;

/**
 * Use information in metarToShefFilter.xml, MetarToShefFilter filters out the
 * metar messages before send the message to MetarToShefTransformer to encode to
 * a SHEF message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date       Ticket# Engineer Description
 * ---------- ------- -------- --------------------------
 * 1/10/2013  15497   wkwock   Initial creation
 * 2/13/2013   1584   mpduff   Fix creation of "dummy" config.
 * 08/08/2013 16408   wkwock   Use different metar.cfg file and options
 * 09/09/2014  3548   mapeters Replaced SerializationUtil usage with JAXBManager.
 * 10/28/2015  4783   bkowal   Refactored and abstracted into {@link AbstractShefFilter}.
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MetarToShefFilter extends AbstractShefFilter {
    @XmlElement
    @DynamicSerializeElement
    protected List<MetarToShefRun> metarToShefRun;

    private final String metarToShefOptions = AppsDefaults.getInstance()
            .getToken(AbstractShefTransformer.METAR_2_SHEF_OPT);

    /**
     * Empty constructor for {@link DynamicSerialize}.
     */
    public MetarToShefFilter() {
    }

    public MetarToShefFilter(String configFile, String localContext) {
        super(configFile, localContext, MetarToShefFilter.class);

        for (MetarToShefRun mtsr : metarToShefRun) {
            logger.info("Filter name = " + mtsr.getFilterName()
                    + " with config file: " + mtsr.getConfigFileName());
        }
    }

    @Override
    public void init() {
        this.metarToShefRun = new ArrayList<>();
    }

    @Override
    protected void buildRun(final Object obj, final String configFile) {
        if (obj instanceof PluginDataObjectFilter) {
            logger.debug("Found " + configFile + " is PluginDataObjectFilter");
            PluginDataObjectFilter pdof = (PluginDataObjectFilter) obj;
            MetarToShefRun mtsr = new MetarToShefRun();
            mtsr.setConfigFileName(METAR_CFG);
            mtsr.setMetarToShefOptions(metarToShefOptions);
            mtsr.setFilterElements(pdof.getFilterElements());
            mtsr.setFilterName(pdof.getFilterName());
            this.metarToShefRun.add(mtsr);
        } else if (obj instanceof MetarToShefFilter) {
            logger.debug("Found " + configFile + " is MetarToShefFilter");
            MetarToShefFilter filter = (MetarToShefFilter) obj;
            this.metarToShefRun = filter.metarToShefRun;
        }
    }

    /**
     * Apply the list of filters against given input data.
     * 
     */
    @Override
    public PluginDataObject[] filter(PluginDataObject[] reports) {
        Map<Integer, MetarToShefRun> matchList = new HashMap<>();
        List<PluginDataObject> reportList = new ArrayList<PluginDataObject>();
        for (PluginDataObject report : reports) {
            for (MetarToShefRun mtsr : metarToShefRun) {
                PluginDataObject resultRpt = filterARun(report,
                        mtsr.getFilterElements());
                if (resultRpt != null) {
                    reportList.add(resultRpt);
                    matchList.put(resultRpt.getId(), mtsr);
                    break;
                }
            }
        }
        if (matchList.isEmpty() == false) {
            MetarToShefTransformer.setMatchList(matchList);
        }
        return (PluginDataObject[]) reportList.toArray(new PluginDataObject[0]);
    }

    @Override
    protected void createDummyFilter() {
        MetarToShefRun mtsr = new MetarToShefRun();
        mtsr.setConfigFileName(METAR_CFG);
        mtsr.setMetarToShefOptions(metarToShefOptions);

        // Add a dummy element.
        AbstractFilterElement dummy = new AbstractFilterElement() {
            @Override
            public PluginDataObject filter(PluginDataObject report) {
                return report;
            }
        };
        dummy.setFilterType(AbstractObsFilter.INCLUDE_TYPE);
        mtsr.getFilterElements().add(dummy);
        mtsr.setFilterName(DUMMY_FILTER_NAME);
        this.metarToShefRun.add(mtsr);
    }

    public void addMetarToShefRun(MetarToShefRun element) {
        metarToShefRun.add(element);
    }

    /**
     * 
     * @return
     */
    public List<MetarToShefRun> getMetarToShefRun() {
        return metarToShefRun;
    }

    /**
     * 
     * @param elements
     */
    public void setMetarToShefRun(List<MetarToShefRun> elements) {
        metarToShefRun = elements;
    }

}
