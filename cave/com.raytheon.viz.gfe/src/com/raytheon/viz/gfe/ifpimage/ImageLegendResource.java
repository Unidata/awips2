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
package com.raytheon.viz.gfe.ifpimage;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.gfe.type.Pair;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.rsc.GFELegendResource;
import com.raytheon.viz.gfe.rsc.GFEResource;

/**
 * Image legend resource used by GFEPainter.py
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2011            mschenke     Initial creation
 * Jun 25, 2012  15080     ryu          Ron's local time fix
 * Jul 10, 2012  15186     ryu          Set legend font
 * Aug 20, 2012  #1078     dgilling     Fix handling of ImageLegend_color
 *                                      setting.
 * Nov 30, 2012  #1328     mschenke     Made GFE use descriptor for time matching
 *                                      and time storage and manipulation
 * Jan 22, 2013  #1518     randerso     Removed use of Map with Parms as keys,
 *                                      really just needed a list anyway.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ImageLegendResource extends GFELegendResource {

    private boolean localTime = false;

    private boolean snapshotTime = false;

    private String descriptiveName;

    private String snapshotFormat;

    private String startFormat;

    private String endFormat;

    private String durationFormat;

    private String language;

    private Map<String, RGB> colorOverrides;

    /**
     * @param dataManager
     * @param resourceData
     * @param loadProps
     */
    public ImageLegendResource(DataManager dataManager) {
        super(dataManager, null, null);
    }

    @Override
    public LegendEntry[] getLegendData(IDescriptor descriptor) {
        List<Pair<Parm, ResourcePair>> parms = getLegendOrderedParms(descriptor);
        LegendData[] data = makeLegend(parms);

        LegendEntry[] entries = new LegendEntry[data.length];
        for (int i = 0; i < entries.length; ++i) {
            entries[i] = new LegendEntry();
            entries[i].font = font;
            entries[i].legendParts = new LegendData[] { data[i] };
        }
        return entries;
    }

    private LegendData[] makeLegend(List<Pair<Parm, ResourcePair>> parms) {
        FramesInfo currInfo = descriptor.getFramesInfo();
        DataTime curTime = currInfo.getCurrentFrame();

        // loop through the grids
        List<LegendData> legendData = new ArrayList<LegendData>();
        for (Pair<Parm, ResourcePair> pair : parms) {
            Parm parm = pair.getFirst();
            ResourcePair rp = pair.getSecond();
            GFEResource rsc = (GFEResource) rp.getResource();
            String parmName = parm.getParmID().getParmName();
            ResourceProperties props = rp.getProperties();
            LegendData data = new LegendData();
            data.resource = rp;

            DataTime curRscTime = currInfo.getTimeForResource(rsc);

            // color for the text
            if ((props.isVisible())
                    && (parm.getDisplayAttributes().getVisMode() == VisMode.IMAGE)) {
                data.color = imageLegendColor;
            } else if (!props.isVisible()) {
                data.color = ColorUtil.GREY;
            } else if ((colorOverrides != null)
                    && (colorOverrides.get(parmName) != null)) {
                // GFEPainter.py populates the colorOverrides map based on the
                // "<parmName>_Legend_color" values from the gfe config file
                data.color = colorOverrides.get(parmName);
            } else {
                data.color = rsc.getCapability(ColorableCapability.class)
                        .getColor();
            }

            String timeString = "";

            // get the units for the time string
            String units = rsc.getParm().getGridInfo().getUnitString();

            Locale locale = Locale.getDefault();
            String lang = getLanguage();
            if (!"".equals(lang)) {
                for (Locale localeX : Locale.getAvailableLocales()) {
                    if (localeX.getDisplayLanguage().equalsIgnoreCase(lang)) {
                        locale = localeX;
                        break;
                    }
                }
            }

            Formatter formatter = new Formatter(locale);

            String tz = "GMT";
            if (localTime) {
                tz = dataManager.getParmManager().compositeGridLocation()
                        .getTimeZone();
            }

            if (snapshotTime) {
                // display the time of the snapshot
                Calendar snap = curTime.getRefTimeAsCalendar();
                snap.setTimeZone(TimeZone.getTimeZone(tz));
                formatter.format(snapshotFormat.replaceAll("\\%", "%1\\$t"),
                        snap);
            } else if (curRscTime != null) {
                TimeRange tr = curRscTime.getValidPeriod();
                if (durationFormat != null) {
                    timeString += durationString(tr);
                }
                if (startFormat != null) {
                    Calendar start = Calendar.getInstance();
                    start.setTimeZone(TimeZone.getTimeZone(tz));
                    start.setTime(tr.getStart());
                    formatter.format(startFormat.replaceAll("\\%", "%1\\$t"),
                            start);
                }
                if (endFormat != null) {
                    Calendar end = Calendar.getInstance();
                    end.setTimeZone(TimeZone.getTimeZone(tz));
                    end.setTime(tr.getEnd());
                    formatter
                            .format(endFormat.replaceAll("\\%", "%1\\$t"), end);
                }

            }

            timeString += formatter.toString();
            timeString = timeString.replaceAll("\\[UNITS\\]", units);

            if (snapshotTime || curRscTime != null) {
                // now prefix the parameter name
                String name = null;

                if (descriptiveName.equals("SHORT")) {
                    name = rsc.getParm().getParmID().compositeNameUI();
                } else if (descriptiveName.equals("ALT")) {
                    name = rsc.getParm().getParmID().compositeNameUI();
                    name = Activator.getDefault().getPreferenceStore()
                            .getString("Png_" + name + "_AltName");
                } else if (descriptiveName.equals("LONG")) {
                    name = rsc.getParm().getGridInfo().getDescriptiveName();
                } else if (descriptiveName.equals("OFF")) {
                    name = "";
                }
                String legend = name + "  " + timeString;
                data.label = legend;
                legendData.add(data);
            }
        }

        return legendData.toArray(new LegendData[legendData.size()]);
    }

    private String durationString(TimeRange tr) {
        String output = durationFormat;
        long duration = tr.getDuration();

        long hours = duration / 3600 / 1000;
        output = output.replaceAll("%H", Long.toString(hours));
        long minutes = (duration % 3600) / 60;
        output = output.replaceAll("%M", Long.toString(minutes));

        return output;
    }

    /**
     * @return the localTime
     */
    public boolean isLocalTime() {
        return localTime;
    }

    /**
     * @param localTime
     *            the localTime to set
     */
    public void setLocalTime(boolean localTime) {
        this.localTime = localTime;
    }

    /**
     * @return the descriptiveName
     */
    public String getDescriptiveName() {
        return descriptiveName;
    }

    /**
     * @param descriptiveName
     *            the descriptiveName to set
     */
    public void setDescriptiveName(String descriptiveName) {
        this.descriptiveName = descriptiveName;
    }

    /**
     * @return the snapshotFormat
     */
    public String getSnapshotFormat() {
        return snapshotFormat;
    }

    /**
     * @param snapshotFormat
     *            the snapshotFormat to set
     */
    public void setSnapshotFormat(String snapshotFormat) {
        this.snapshotFormat = snapshotFormat;
    }

    /**
     * @return the startFormat
     */
    public String getStartFormat() {
        return startFormat;
    }

    /**
     * @param startFormat
     *            the startFormat to set
     */
    public void setStartFormat(String startFormat) {
        this.startFormat = startFormat;
    }

    /**
     * @return the endFormat
     */
    public String getEndFormat() {
        return endFormat;
    }

    /**
     * @param endFormat
     *            the endFormat to set
     */
    public void setEndFormat(String endFormat) {
        this.endFormat = endFormat;
    }

    /**
     * @return the durationFormat
     */
    public String getDurationFormat() {
        return durationFormat;
    }

    /**
     * @param durationFormat
     *            the durationFormat to set
     */
    public void setDurationFormat(String durationFormat) {
        this.durationFormat = durationFormat;
    }

    /**
     * @return the snapshotTime
     */
    public boolean isSnapshotTime() {
        return snapshotTime;
    }

    /**
     * @param snapshotTime
     *            the snapshotTime to set
     */
    public void setSnapshotTime(boolean snapshotTime) {
        this.snapshotTime = snapshotTime;
    }

    /**
     * Specifies the color for a legend entry, overrides the default
     * 
     * @param parmName
     * @param colorName
     */
    public void setColorOverride(String parmName, String colorName) {
        if (colorOverrides == null) {
            colorOverrides = new HashMap<String, RGB>();
        }

        RGB rgb = RGBColors.getRGBColor(colorName);
        colorOverrides.put(parmName, rgb);
    }

    /**
     * @param language
     *            the language to set
     */
    public void setLanguage(String language) {
        this.language = language;
    }

    /**
     * @return the language
     */
    public String getLanguage() {
        return language;
    }
}
