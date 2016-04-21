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
package com.raytheon.uf.edex.decodertools.core.filterimpl;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class StationIdFilterElement extends AbstractFilterElement implements ISerializableObject {

    
    @XmlJavaTypeAdapter(JaxBPatternStringAdapter.class)
    private List<Pattern> patterns;
    
    /**
     * @return the patterns
     */
    public List<Pattern> getPatterns() {
        return patterns;
    }

    /**
     * @param patterns the patterns to set
     */
    public void setPatterns(List<Pattern> patterns) {
        this.patterns = patterns;
    }

    /**
     * 
     * @param pattern
     */
    public void addPattern(Pattern pattern) {
        if(patterns == null) {
            patterns = new ArrayList<Pattern>();
        }
        patterns.add(pattern);
    }

    /**
     * 
     * @param pattern
     */
    public void addPattern(String pattern) {
        if(pattern != null) {
            addPattern(Pattern.compile(pattern));
        }
    }

    /**
     * Executes this filter element against the supplied report data. The
     * supplied report is returned if it matches the filter criteria. A null
     * report reference is returned if the report fails.
     * 
     * @param report
     * @return may be null
     */
    @Override
    public PluginDataObject filter(PluginDataObject report) {
        boolean pass = false;
        
        if(report instanceof ISpatialEnabled) {
            
            ISpatialObject loc = ((ISpatialEnabled) report).getSpatialObject();
            
            String stationId = null;
            if(loc instanceof SurfaceObsLocation) {
                stationId = ((SurfaceObsLocation) loc).getStationId();
            } else if (loc instanceof AircraftObsLocation) {
                stationId = ((AircraftObsLocation) loc).getStationId();
            } else if (loc instanceof ObStation) {
                stationId = ((ObStation) loc).getStationId();
            }
            
            // Did we find a stationId in this data?
            if(stationId != null) {
                for(Pattern p : patterns) {
                    if(p != null) {
                        Matcher m = p.matcher(stationId);
                        if(m.matches()) {
                            pass = true;
                            break;
                        }
                    }
                }
            }
        }
        return (pass ? report : null) ;
    }

    /**
     * 
     */
    public String toString() {
        StringBuilder sb = new StringBuilder("StationIdFilterElement:{");
        for(Pattern p : patterns) {
            sb.append(p.pattern());
            sb.append(",");
        }
        sb.append("}");
        return sb.toString();
    }

}
