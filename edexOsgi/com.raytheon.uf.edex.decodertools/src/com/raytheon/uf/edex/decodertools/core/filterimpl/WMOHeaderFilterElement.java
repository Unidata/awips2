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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
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
 * Oct 25, 2011 11312      jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0 
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class WMOHeaderFilterElement extends AbstractFilterElement implements ISerializableObject {
    
    private static final Class<?> [] GETTER_CLASS = new Class [0];

    private static final String WMO_HEADER = "getWmoHeader";

    @XmlElement
    @XmlJavaTypeAdapter(JaxBPatternStringAdapter.class)
    private List<Pattern> patterns;

    public WMOHeaderFilterElement() {
        super();
    }
    
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
     * This execute
     * @see com.raytheon.uf.edex.decodertools.core.IObsFilterElement#filter(com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public PluginDataObject filter(PluginDataObject report) {
        boolean pass = false;
        if(report != null) {
            String wmoHeader = getWMOHeader(report);
            
            if(wmoHeader != null) {
                for(Pattern p : patterns) {
                    if(p != null) {
                        Matcher m = p.matcher(wmoHeader);
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
     * Get the WMO header from this report object. If an error occurs or the
     * WMO header is not defined, a null String reference is returned.
     * @param report
     * @return
     */
    private String getWMOHeader(PluginDataObject report) {
        
        String header = null;
        if(report != null) {
            Method getter = null;
            try {
                getter = report.getClass().getMethod(WMO_HEADER, GETTER_CLASS);
            } catch (Exception e) {
                // Nothing
            }
            if(getter != null) {
                try {
                    header = (String) getter.invoke(report);
                } catch(Exception e) {
                    // Nothing
                }
            }
        }
        return header;
    }
    
    /**
     * 
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("WMOHeaderFilterElement:{");
        for(Pattern p : patterns) {
            sb.append(p.pattern());
            sb.append(",");
        }
        sb.append("}");
        return sb.toString();
    }

    private static class TestObject extends PluginDataObject implements ISpatialEnabled {
        private static final long serialVersionUID = 1L;

        String wmoHeader = null;
        
        SurfaceObsLocation location;
        
        @Override
        public IDecoderGettable getDecoderGettable() {
            return null;
        }
        
        @Override
        public ISpatialObject getSpatialObject() {
            return location;
        }
    
        public String getWmoHeader() {
            return wmoHeader;
        }
    
        public void setWmoHeader(String header) {
            wmoHeader = header;
        }
        
        public String toString() {
            return wmoHeader + " Passed";
        }

    }

    public static final void main(String [] args) {
        
        PluginDataObjectFilter filter = new PluginDataObjectFilter();
        WMOHeaderFilterElement element = new WMOHeaderFilterElement();
        element.setFilterElementName("");
        element.setFilterType(AbstractObsFilter.EXCLUDE_TYPE);
        element.addPattern(Pattern.compile("SAUS70.*"));
        filter.addFilterElement(element);
        
        TestObject t = new TestObject();
        System.out.println("---- Test 1 -----");
        PluginDataObject [] pp = new TestObject[] { t, };
        t.setWmoHeader("SAUS43 KWBC 251300");
        pp = filter.filter(pp);
        for(PluginDataObject p : pp) {
            System.out.println(p);
        }
        
        System.out.println("---- Test 2 -----");
        t.setWmoHeader("SAUS70 KWBC 251410 RRA");
        pp = new TestObject[] { t, };
        pp = filter.filter(pp);
        for(PluginDataObject p : pp) {
            System.out.println(p);
        }
    }
}
