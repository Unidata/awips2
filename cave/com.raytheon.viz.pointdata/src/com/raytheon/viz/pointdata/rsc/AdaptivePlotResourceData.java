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
package com.raytheon.viz.pointdata.rsc;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Adaptive plot resources, plots static data and samples details for each plot
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2011            mschenke     Initial creation
 * Nov 1, 2017  6271      bsteffen     Remove localization file listener
 * 
 * </pre>
 * 
 * @author mschenke
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AdaptivePlotResourceData extends AbstractResourceData
        implements ILocalizationPathObserver {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AdaptivePlotResourceData.class);

    private static final String ID = "id:";

    private static final String LATITUDE = "latitude";

    private static final String LONGITUDE = "longitude";

    private static final String SEPARATOR = "[|]";

    protected static class PlotObject {
        protected String id;

        protected String name;

        protected String address;

        protected String city;

        protected String phone;

        protected double latitude;

        protected double longitude;

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((id == null) ? 0 : id.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            PlotObject other = (PlotObject) obj;
            if (id == null) {
                if (other.id != null) {
                    return false;
                }
            } else if (!id.equals(other.id)) {
                return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return name + "\n  " + address + "\n  " + city + "\n  " + phone;
        }

    }

    @XmlAttribute
    private String plotName;

    @XmlAttribute
    private String name;

    @XmlAttribute
    private String filePath;

    private Set<PlotObject> lastObjects;

    private String nameKey, addressKey, cityKey, phoneKey;

    public AdaptivePlotResourceData() {
        setNameGenerator(new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return plotName;
            }
        });
    }

    @Override
    public void removeChangeListener(IResourceDataChanged listener) {
        super.removeChangeListener(listener);
        if (super.dataChangedListeners.isEmpty()) {
            /*
             * If no one is listening for changes then it is not necessary to
             * watch the file anymore.
             */
            PathManagerFactory.getPathManager()
                    .removeLocalizationPathObserver(this);
            lastObjects = null;
        }
    }

    @Override
    public synchronized AbstractVizResource<?, ?> construct(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        if (lastObjects == null) {
            if (!loadFromFile()) {
                throw new VizException("Could not load plots from " + filePath);
            }
            PathManagerFactory.getPathManager()
                    .addLocalizationPathObserver(filePath, this);
        }
        AdaptivePlotResource rsc = new AdaptivePlotResource(this,
                loadProperties);
        for (PlotObject obj : lastObjects) {
            rsc.addPlotObject(obj);
        }
        return rsc;
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        /*
         * We aren't sure if the file we got was deleted or perhaps was a lower
         * level we can ignore, don't try to figure it out, just reload.
         */
        loadFromFile();
    }

    /**
     * Load the plot objects from a localization file.
     * 
     * @return true if the file exists and is properly formatted, otherwise
     *         false.
     */
    private boolean loadFromFile() {
        boolean success = false;
        Set<PlotObject> lastObjects = this.lastObjects;
        Set<PlotObject> newObjects = new HashSet<>();
        ILocalizationFile file = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(filePath);
        if (file != null) {
            try {
                readFile(file, newObjects);
                success = true;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reloading " + file, e);
            }
        }

        if (lastObjects != null && !lastObjects.isEmpty()) {
            fireChangeListeners(ChangeType.DATA_REMOVE,
                    lastObjects.toArray(new PlotObject[lastObjects.size()]));
        }
        if (!newObjects.isEmpty()) {
            fireChangeListeners(ChangeType.DATA_UPDATE,
                    newObjects.toArray(new PlotObject[newObjects.size()]));
        }
        this.lastObjects = newObjects;
        return success;
    }

    private void readFile(ILocalizationFile file, Collection<PlotObject> plots)
            throws VizException {
        String prefix = name;
        nameKey = prefix + "Name";
        addressKey = prefix + "Address";
        cityKey = prefix + "City";
        phoneKey = prefix + "Phone";
        try (BufferedReader br = new BufferedReader(
                new InputStreamReader(file.openInputStream()))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.startsWith(ID)) {
                    String id = line.substring(ID.length()).trim();
                    if (id != null) {
                        PlotObject obj = new PlotObject();
                        obj.id = id;
                        if (populatePlotObject(br, obj)) {
                            plots.add(obj);
                        }
                    }
                }
            }
        } catch (Exception e) {
            throw new VizException(
                    "Error reading file contents for " + getName(), e);
        }
    }

    private boolean populatePlotObject(BufferedReader reader, PlotObject object)
            throws IOException {
        String[] keyOrder = new String[] { nameKey, addressKey, cityKey,
                phoneKey, LATITUDE, LONGITUDE };

        for (int i = 0; i < keyOrder.length; ++i) {
            String line = reader.readLine();
            String[] parts = line.split(SEPARATOR);
            if (parts.length != 2) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing plot object (" + object.id + ") "
                                + keyOrder[i] + " value");
                return false;
            }
            String key = parts[0].trim();
            String value = parts[1].trim();

            if (!keyOrder[i].equals(key)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing plot object (" + object.id
                                + "), expected key, " + keyOrder[i]
                                + ", got key, " + key);
                return false;
            }

            switch (i) {
            case 0:
                object.name = value;
                break;
            case 1:
                object.address = value;
                break;
            case 2:
                object.city = value;
                break;
            case 3:
                object.phone = value;
                break;
            case 4:
                try {
                    object.latitude = Double.parseDouble(value);
                } catch (NumberFormatException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error parsing latitude (" + value
                                    + ") for plot object (" + object.id + ")",
                            e);
                    return false;
                }
                break;
            case 5:
                try {
                    object.longitude = Double.parseDouble(value);
                } catch (NumberFormatException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error parsing longitude (" + value
                                    + ") for plot object (" + object.id + ")",
                            e);
                    return false;
                }
                break;
            }
        }

        return true;
    }

    public String getPlotName() {
        return plotName;
    }

    public void setPlotName(String plotName) {
        this.plotName = plotName;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((filePath == null) ? 0 : filePath.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result
                + ((plotName == null) ? 0 : plotName.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AdaptivePlotResourceData other = (AdaptivePlotResourceData) obj;
        if (filePath == null) {
            if (other.filePath != null) {
                return false;
            }
        } else if (!filePath.equals(other.filePath)) {
            return false;
        }
        if (name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!name.equals(other.name)) {
            return false;
        }
        if (plotName == null) {
            if (other.plotName != null) {
                return false;
            }
        } else if (!plotName.equals(other.plotName)) {
            return false;
        }
        return true;
    }

}
