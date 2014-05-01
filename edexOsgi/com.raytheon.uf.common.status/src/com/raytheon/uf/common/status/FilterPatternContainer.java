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
package com.raytheon.uf.common.status;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Container for FilterPattern objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2010            njensen     Initial creation
 * Apr 12, 2011            bgonzale    Refactored from EdexModeContainer
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlRootElement(name = "filterPatterns")
@XmlAccessorType(XmlAccessType.NONE)
public class FilterPatternContainer {

    @XmlElements({ @XmlElement(name = "filterPattern") })
    private ArrayList<FilterPattern> filterPatterns;

    /**
     * A default FilterPatternContainer.
     */
    public FilterPatternContainer() {
    }

    /**
     * Initialize a FilterPatternContainer from a file.
     */
    public FilterPatternContainer(File file) throws JAXBException, IOException {
        FileReader reader = null;

        try {
            JAXBContext context = JAXBContext.newInstance(
                    FilterPatternContainer.class, FilterPattern.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();
            reader = new FileReader(file);
            FilterPatternContainer container = (FilterPatternContainer) unmarshaller
                    .unmarshal(reader);

            container.compile();
            this.filterPatterns = container.getFilterPatterns();
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
    }

    private void compile() {
        for (FilterPattern fp : filterPatterns) {
            fp.compile();
        }
    }

    public ArrayList<FilterPattern> getFilterPatterns() {
        return filterPatterns;
    }

    public void setModes(ArrayList<FilterPattern> filterPatterns) {
        this.filterPatterns = filterPatterns;
    }

    public FilterPattern getFilter(String name) {
        FilterPattern ret = null;
        for (FilterPattern m : filterPatterns) {
            if (m.getName() != null && m.getName().equalsIgnoreCase(name)) {
                ret = m;
                break;
            }
        }
        return ret;
    }

    public FilterPattern findFilter(String str) {
        for (FilterPattern m : filterPatterns) {
            if (m.accept(str)) {
                return m;
            }
        }
        return getFilter("DEFAULT");
    }

    public static FilterPatternContainer createDefault() {
        FilterPatternContainer obj = new FilterPatternContainer();
        obj.filterPatterns = new ArrayList<FilterPattern>();
        FilterPattern fp = new FilterPattern("RADAR");
        fp.addInclude(".*\\.radar.*");
        obj.filterPatterns.add(fp);
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("AVNFPS");
        fp.addInclude(".*\\.aviation.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("ISC");
        fp.addInclude(".*\\.isc.*");
        fp.addInclude(".*Isc.*");
        fp.addInclude(".*ISC.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("GFE");
        fp.addInclude(".*GFE.*");
        fp.addInclude(".*gfe.*");
        fp.addExclude(".*\\.isc.*");
        fp.addExclude(".*Isc.*");
        fp.addExclude(".*ISC.*");
        fp.addExclude(".*\\.smartinit.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("NDFD");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("ANNOUNCER");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("MPE");
        fp.addInclude(".*\\.mpe.*");
        fp.addInclude(".*MPE.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("GHG");
        fp.addInclude(".*\\.ghg.*");
        fp.addInclude(".*GHG.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("WORKSTATION");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("DEFAULT");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("SNOW");
        fp.addInclude(".*\\.snow.*");
        fp.addInclude(".*\\.Snow.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("FOG");
        fp.addInclude(".*\\.fog.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("FFMP");
        fp.addInclude(".*\\.ffmp.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("SCAN");
        fp.addInclude(".*\\.scan.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("SAFESEAS");
        fp.addInclude(".*\\.safeseas.*");
        fp.addInclude(".*\\.Safeseas.*");
        fp.addInclude(".*\\.SafeSeas.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("NWRWAVES");
        fp.addInclude(".*nwr.*");
        fp.addInclude(".*Nwr.*");
        fp.addInclude(".*NWR.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("SMARTINIT");
        fp.addInclude(".*\\.smartinit.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("GDN_ADMIN");
        fp.addInclude(".*\\.alertviz.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("RPGEnvData");
        fp.addInclude(".*\\.rpgenvdata.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("GRIB");
        fp.addInclude(".*\\.grib.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("Localization");
        fp.addInclude(".*\\.localization\\..*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("ActiveTable");
        fp.addInclude(".*\\.activetable\\..*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("PURGE");
        fp.addExclude(".*\\.edex.purgesrv\\..*");
        fp.addExclude(".*\\.database.purge\\..*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("HandleOUP");
        fp.addInclude(".*\\.dissemination\\..*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("Management");
        fp.addInclude(".*\\.management\\..*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("Storage");
        fp.addInclude(".*\\.nwsauth\\..*");
        fp.addInclude(".*\\.persist\\..*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("TOPO");
        fp.addInclude(".*request.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("EDEX");
        fp.addInclude(".*\\.edex.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("D2D");
        fp.addInclude(".*d2d.*");
        obj.filterPatterns.add(fp);
        fp = new FilterPattern("CAVE");
        fp.addInclude(".*\\.viz.*");
        obj.compile();
        return obj;
    }
}
