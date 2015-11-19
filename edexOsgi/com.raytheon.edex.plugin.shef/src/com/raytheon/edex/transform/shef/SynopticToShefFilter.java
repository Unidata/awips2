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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractFilterElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractObsFilter;
import com.raytheon.uf.edex.decodertools.core.filterimpl.PluginDataObjectFilter;

/**
 * Used to filter synoptic messages before sending them to
 * {@link SMToShefTransformer}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2015 4783       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SynopticToShefFilter extends AbstractShefFilter {
    @XmlElement
    @DynamicSerializeElement
    private List<SynopticToShefRun> synopticToShefRun;

    /**
     * Empty constructor for {@link DynamicSerialize}.
     */
    public SynopticToShefFilter() {
        this.synopticToShefRun = new ArrayList<>();
    }

    public SynopticToShefFilter(String configFile, String localContext) {
        super(configFile, localContext, SynopticToShefFilter.class);

        for (SynopticToShefRun run : synopticToShefRun) {
            logger.info("Filter name = " + run.getFilterName()
                    + " with config file: " + run.getConfigFileName());
        }
    }

    @Override
    public void init() {
        this.synopticToShefRun = new ArrayList<>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.transform.shef.AbstractShefFilter#buildRun(java.lang
     * .Object, java.lang.String)
     */
    @Override
    protected void buildRun(Object obj, String configFile) {
        if (obj instanceof PluginDataObjectFilter) {
            logger.debug("Found " + configFile + " is PluginDataObjectFilter");
            PluginDataObjectFilter pdof = (PluginDataObjectFilter) obj;
            SynopticToShefRun run = new SynopticToShefRun();
            run.setConfigFileName(null);
            run.setFilterElements(pdof.getFilterElements());
            run.setFilterName(pdof.getFilterName());
            this.synopticToShefRun.add(run);
        } else if (obj instanceof SynopticToShefFilter) {
            logger.debug("Found " + configFile + " is SynopticToShefFilter");
            SynopticToShefFilter filter = (SynopticToShefFilter) obj;
            this.synopticToShefRun = filter.synopticToShefRun;
        }
    }

    @Override
    protected void createDummyFilter() {
        SynopticToShefRun run = new SynopticToShefRun();
        run.setConfigFileName(null);

        // Add a dummy element.
        AbstractFilterElement dummy = new AbstractFilterElement() {
            @Override
            public PluginDataObject filter(PluginDataObject report) {
                return report;
            }
        };
        dummy.setFilterType(AbstractObsFilter.INCLUDE_TYPE);
        run.getFilterElements().add(dummy);
        run.setFilterName(DUMMY_FILTER_NAME);
        this.synopticToShefRun.add(run);
    }

    @Override
    public PluginDataObject[] filter(PluginDataObject[] reports) {
        Map<Integer, SynopticToShefRun> matchList = new HashMap<>();
        List<PluginDataObject> reportList = new ArrayList<PluginDataObject>();
        for (PluginDataObject report : reports) {
            for (SynopticToShefRun run : this.synopticToShefRun) {
                PluginDataObject resultRpt = filterARun(report,
                        run.getFilterElements());
                if (resultRpt != null) {
                    reportList.add(resultRpt);
                    matchList.put(resultRpt.getId(), run);
                    break;
                }
            }
        }
        if (matchList.isEmpty() == false) {
            SMToShefTransformer.setMatchMap(matchList);
        }
        return (PluginDataObject[]) reportList.toArray(new PluginDataObject[0]);
    }

    /**
     * @return the synopticToShefRun
     */
    public List<SynopticToShefRun> getSynopticToShefRun() {
        return synopticToShefRun;
    }

    /**
     * @param synopticToShefRun
     *            the synopticToShefRun to set
     */
    public void setSynopticToShefRun(List<SynopticToShefRun> synopticToShefRun) {
        this.synopticToShefRun = synopticToShefRun;
    }
}