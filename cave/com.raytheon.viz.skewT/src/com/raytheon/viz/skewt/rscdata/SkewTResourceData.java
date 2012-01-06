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
package com.raytheon.viz.skewt.rscdata;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.AbstractVerticalSoundingAdapter;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.skewt.rsc.SkewTResource;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class SkewTResourceData extends AbstractRequestableResourceData {
    private static final String VERTICAL_SOUNDING_ADAPTER_EXTENSION = "com.raytheon.uf.viz.sounding.verticalSoundingAdapter";

    private static Map<String, IConfigurationElement> adapterMap = null;

    private String pointLetter;
    
    protected VerticalSounding[] soundings;

    /**
     * 
     */
    public SkewTResourceData() {
    }

    /**
     * @return the soundings
     */
    public VerticalSounding[] getSoundings() {
        return soundings;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected SkewTResource constructResource(LoadProperties loadProperties,
            PluginDataObject[] objects) throws VizException {

        if (objects != null && objects.length > 0) {
            AbstractVerticalSoundingAdapter adapter = getAdapter(objects[0]);
            adapter.setObjects(objects);
            soundings = adapter.createSoundings();
        } else {
            soundings = new VerticalSounding[0];
        }

        SkewTResource rsc = new SkewTResource(this, loadProperties);
        return rsc;
    }

    protected AbstractVerticalSoundingAdapter getAdapter(PluginDataObject object)
            throws VizException {
        synchronized (SkewTResourceData.class) {
            if (adapterMap == null) {
                adapterMap = new HashMap<String, IConfigurationElement>();
                IExtensionRegistry registry = Platform.getExtensionRegistry();
                if (registry != null) {
                    IExtensionPoint point = registry
                            .getExtensionPoint(VERTICAL_SOUNDING_ADAPTER_EXTENSION);
                    if (point != null) {
                        IExtension[] extensions = point.getExtensions();

                        for (IExtension ext : extensions) {
                            IConfigurationElement[] config = ext
                                    .getConfigurationElements();

                            for (IConfigurationElement cfg : config) {
                                String record = cfg.getAttribute("class");

                                if (adapterMap.put(record, cfg) != null) {
                                    Activator
                                            .getDefault()
                                            .getLog()
                                            .log(
                                                    new Status(
                                                            Status.ERROR,
                                                            Activator.PLUGIN_ID,
                                                            "Duplicate VerticalSoundingAdapter for: \""
                                                                    + record
                                                                    + "\" defined in "
                                                                    + ext
                                                                            .getNamespaceIdentifier()));
                                }
                            }
                        }
                    }
                }
            }
        }

        String className = object.getClass().getName();
        IConfigurationElement cfg = adapterMap.get(className);
        if (cfg == null) {
            throw new VizException("No VerticalSoundingAdapter registered for "
                    + className);
        }
        try {
            AbstractVerticalSoundingAdapter adapter = (AbstractVerticalSoundingAdapter) cfg
                    .createExecutableExtension("adapter");

            return adapter;
        } catch (Throwable e) {
            e.printStackTrace();
            throw new VizException(
                    "Exception while getting VerticalSoundingAdapter for "
                            + className, e);
        }
    }

    /**
     * 
     * @return
     */
    public String getPointLetter() {
        return pointLetter;
    }

    /**
     * 
     * @param pointLetter
     */
    public void setPointLetter(String pointLetter) {
        this.pointLetter = pointLetter;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + Arrays.hashCode(soundings);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        SkewTResourceData other = (SkewTResourceData) obj;
        if (soundings != other.soundings) {
            return false;
        }
        return true;
    }
}
