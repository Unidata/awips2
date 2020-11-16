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
package com.raytheon.viz.hydro.pointprecipitation;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.viz.hydrocommon.util.HydroQC;

/**
 * Precipitation Accumulation Dialog Data Object holding the data for an
 * individual gage.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2018   6968     mduff       Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class PrecipAccumulationRowData {
    private static final String PC_FORMAT = "  %8.2f  %s   %s     %-1s     %s\n";

    private static final String PP_FORMAT = "  %8.2f  %s   %-16s %s     %s     %s\n";

    private static final String DUR_FORMAT = "%6.2f";

    private static final String BEGIN_LINE_FORMAT = "%-8s %-20s %-2s %-2s :";

    private final SimpleDateFormat sdf = new SimpleDateFormat("MM/dd HH:mm");

    private String lid;

    private String name;

    private String pe;

    private String ts;

    private double value;

    private double maxValue;

    private int[] durations;

    private boolean details;

    private double[] hrFill;

    private double[] amount;

    // 1 is true, 0 is false
    private int[] summedFlags;

    private List<Rawpc> pcList = null;

    private List<Rawpp> ppList = null;

    public PrecipAccumulationRowData() {
        init();
    }

    private void init() {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        if (name == null) {
            name = "";
        } else if (name.length() > 20) {
            // shorten the name if needed
            name = name.substring(0, 20);
        }

        this.name = name;
    }

    public String getPe() {
        return pe;
    }

    public void setPe(String pe) {
        this.pe = pe;
    }

    public String getTs() {
        return ts;
    }

    public void setTs(String ts) {
        this.ts = ts;
    }

    public double getValue() {
        return value;
    }

    public void setValue(double value) {
        this.value = value;
    }

    public double getMaxValue() {
        return maxValue;
    }

    public void setMaxValue(double max) {
        if (max == PointPrecipConstants.MISSING_PRECIP) {
            maxValue = -1;
        } else {
            maxValue = max;
        }
    }

    public int[] getDurations() {
        return durations;
    }

    public void setDurations(int[] durations) {
        this.durations = durations;
    }

    public boolean isDetails() {
        return details;
    }

    public void setDetails(boolean details) {
        this.details = details;
    }

    public int[] getSummedFlags() {
        return summedFlags;
    }

    public void setSummedFlags(int[] summedFlags) {
        this.summedFlags = summedFlags;
    }

    public double[] getHrFill() {
        return hrFill;
    }

    public void setHrFill(double[] hrFill) {
        this.hrFill = hrFill;
    }

    public double[] getAmount() {
        return amount;
    }

    public void setAmount(double[] amount) {
        this.amount = amount;
    }

    public List<Rawpc> getPcList() {
        return pcList;
    }

    public void setPcList(List<Rawpc> pcList) {
        this.pcList = pcList;
    }

    public List<Rawpp> getPpList() {
        return ppList;
    }

    public void setPpList(List<Rawpp> ppList) {
        this.ppList = ppList;
    }

    public String getDataLine() {
        StringBuilder buffer = new StringBuilder();
        buffer.append(String.format(BEGIN_LINE_FORMAT, lid, name, pe, ts));

        for (int i = 0; i < durations.length; i++) {
            if (amount[i] == PointPrecipConstants.MISSING_PRECIP) {
                buffer.append(" ").append(PointPrecipConstants.MISSING_STRING);
            } else {
                buffer.append(String.format(DUR_FORMAT, amount[i]));
            }
        }
        if (details) {
            boolean first = true;
            buffer.append(" (");
            for (int i = 0; i < durations.length; i++) {
                if (first) {
                    first = false;
                } else {
                    buffer.append("/");
                }

                String derivedStr = "";
                if ((summedFlags[0] == 1)
                        && PointPrecipConstants.PP.equalsIgnoreCase(pe)) {
                    derivedStr = "s";
                }

                buffer.append(
                        String.format("%.1f%s", this.hrFill[i], derivedStr));
            }
            buffer.append(")");
        }
        buffer.append(StringUtil.NEWLINE);

        return buffer.toString();
    }

    public String getTimeSeriesLines() {
        String qcStr = null;
        String timeStr = null;
        short dur;

        List<String> detailLines = new ArrayList<>();
        if (PointPrecipConstants.PC.equalsIgnoreCase(pe)) {
            detailLines.add("\n     Value     Time      Ext Qualif  QC\n");
            if (pcList != null) {
                for (Rawpc pc : pcList) {
                    if (pc.getTs().equalsIgnoreCase(ts)) {
                        int qualCode = 0;
                        String extremum = pc.getExtremum();
                        if (pc.getQualityCode() != null) {
                            qualCode = pc.getQualityCode();
                        }
                        qcStr = HydroQC.buildQcSymbol(qualCode);
                        timeStr = sdf.format(pc.getObstime());
                        detailLines.add(
                                String.format(PC_FORMAT, pc.getValue(), timeStr,
                                        extremum, pc.getShefQualCode(), qcStr));
                    }
                }
            }

            /* Same as AWIPS I PC data is displayed in descending order */
            Collections.reverse(detailLines);
        } else {
            detailLines.add(
                    "     Value     Time      Duration         Ext Qualif  QC\n");
            if (ppList != null) {
                for (Rawpp pp : ppList) {
                    int qualCode = 0;
                    String extremum = pp.getExtremum();
                    if (pp.getQualityCode() != null) {
                        qualCode = pp.getQualityCode();
                    }
                    qcStr = HydroQC.buildQcSymbol(qualCode);
                    timeStr = sdf.format(pp.getObstime());
                    dur = pp.getDur();
                    String durStr = null;

                    /*
                     * build a presentable string for the duration code value
                     */
                    if (dur != 0) {
                        durStr = PointPrecipDataManager.getInstance()
                                .getDur(dur);
                        if (durStr == null) {
                            durStr = dur + " ";
                        }
                    }

                    detailLines.add(String.format(PP_FORMAT, pp.getValue(),
                            timeStr, durStr, extremum, pp.getShefQualCode(),
                            qcStr));
                }
            }
        }

        StringBuilder buffer = new StringBuilder();
        for (String line : detailLines) {
            buffer.append(line);
        }

        return buffer.toString();
    }

    @Override
    public String toString() {
        return "PrecipAccumulationRowData [lid=" + lid + ", value=" + value
                + "]";
    }
}
