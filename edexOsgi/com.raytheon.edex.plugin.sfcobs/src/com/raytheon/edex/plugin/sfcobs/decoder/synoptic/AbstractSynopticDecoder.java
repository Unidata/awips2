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
package com.raytheon.edex.plugin.sfcobs.decoder.synoptic;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.AbstractSfcObsDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.ITimeService;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Base class for the synoptic data decoders. Contains code for common decoder
 * processes and the base the observation time. The section decoders for
 * specific decoders are called indirectly. This allows each specific decoder to
 * replace a section decoder with a strategy specific to it's data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928            391 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class AbstractSynopticDecoder extends AbstractSfcObsDecoder {

    private static final int LINE_CUT_LENGTH = 58;

    private static final ArrayList<String> wxAbrev = new ArrayList<String>();
    static {
        wxAbrev.add("");
        wxAbrev.add("");
        wxAbrev.add("");
        wxAbrev.add("");
        wxAbrev.add("FU");
        wxAbrev.add("HZ");
        wxAbrev.add("DU");
        wxAbrev.add("BLSA");
        wxAbrev.add("PO");
        wxAbrev.add("VCSS");
        wxAbrev.add("BR");
        wxAbrev.add("BCFG");
        wxAbrev.add("MIFG");
        wxAbrev.add("VCTS");
        wxAbrev.add("VCSH");
        wxAbrev.add("VCSH");
        wxAbrev.add("VCSH");
        wxAbrev.add("");
        wxAbrev.add("SQ");
        wxAbrev.add("+FC");
        wxAbrev.add("DZ");
        wxAbrev.add("RA");
        wxAbrev.add("SN");
        wxAbrev.add("RA SN");
        wxAbrev.add("FZRA");
        wxAbrev.add("SHRA");
        wxAbrev.add("SHRA SHSN");
        wxAbrev.add("SHGR");
        wxAbrev.add("FG,FZFG");
        wxAbrev.add("TS");
        wxAbrev.add("SS");
        wxAbrev.add("SS");
        wxAbrev.add("SS");
        wxAbrev.add("+SS");
        wxAbrev.add("+SS");
        wxAbrev.add("+SS");
        wxAbrev.add("DRSN");
        wxAbrev.add("");
        wxAbrev.add("BLSN");
        wxAbrev.add("+BLSN");
        wxAbrev.add("VCFG");
        wxAbrev.add("BCFG");
        wxAbrev.add("FG,FZFG");
        wxAbrev.add("FG,FZFG");
        wxAbrev.add("FG,FZFG");
        wxAbrev.add("FG,FZFG");
        wxAbrev.add("FG,FZFG");
        wxAbrev.add("FG,FZFG");
        wxAbrev.add("FZFG");
        wxAbrev.add("FZFG");
        wxAbrev.add("-DZ");
        wxAbrev.add("-DZ");
        wxAbrev.add("DZ");
        wxAbrev.add("DZ");
        wxAbrev.add("+DZ");
        wxAbrev.add("+DZ");
        wxAbrev.add("-FZDZ");
        wxAbrev.add("FZDZ");
        wxAbrev.add("-DZ -RA");
        wxAbrev.add("DZ RA");
        wxAbrev.add("-RA");
        wxAbrev.add("-RA");
        wxAbrev.add("RA");
        wxAbrev.add("RA");
        wxAbrev.add("+RA");
        wxAbrev.add("+RA");
        wxAbrev.add("-FZRA");
        wxAbrev.add("FZRA");
        wxAbrev.add("-RA -SN");
        wxAbrev.add("RA SN");
        wxAbrev.add("-SN");
        wxAbrev.add("-SN");
        wxAbrev.add("SN");
        wxAbrev.add("SN");
        wxAbrev.add("+SN");
        wxAbrev.add("+SN");
        wxAbrev.add("IC");
        wxAbrev.add("SG");
        wxAbrev.add("IC");
        wxAbrev.add("PE");
        wxAbrev.add("-SHRA");
        wxAbrev.add("SHRA");
        wxAbrev.add("+SHRA");
        wxAbrev.add("-SHSN -SHRA");
        wxAbrev.add("SHSN SHRA");
        wxAbrev.add("-SNSN");
        wxAbrev.add("SHSN");
        wxAbrev.add("-SHPE");
        wxAbrev.add("SHPE");
        wxAbrev.add("");
        wxAbrev.add("SHGR");
        wxAbrev.add("-RA");
        wxAbrev.add("+RA");
        wxAbrev.add("-RA -SN -GR");
        wxAbrev.add("+RA +SN +GR");
        wxAbrev.add("TSRA");
        wxAbrev.add("TSPE");
        wxAbrev.add("+TSRA");
        wxAbrev.add("");
        wxAbrev.add("+TSPE");
    };

    // Synoptic wind indicator. Reference WMO 306 Table 1855
    // 0 estimated m/sec
    // 1 measured m/sec
    // 3 estimated knots
    // 4 measured knots
    private int iSubw = -1;

    private Integer wmoRegion = null;

    private Integer obsYear = null;

    private Integer obsMonth = null;

    private Integer obsDay = null;

    private Integer obsHour = null;

    // private Integer obsMinutes = null;

    private AbstractSectionDecoder[] secDecoders = new AbstractSectionDecoder[6];

    /**
     * Add a section decoder to the internal decoder list. Decoders will be
     * executed in order of entry.
     * 
     * @param dec
     * @param sectNumber
     */
    protected void addSectionDecoder(AbstractSectionDecoder dec, int sectNumber) {
        if ((sectNumber > 0) && (sectNumber < secDecoders.length)) {
            secDecoders[sectNumber] = dec;
        }
    }

    /**
     * Clear all section decoders. This method may be used to abort all further
     * processing.
     */
    protected void clearSectionDecoders() {
        for (int i = 0; i < secDecoders.length; i++) {
            secDecoders[i] = null;
        }
    }

    /**
     * @return the iSubw
     */
    public int getISubw() {
        return iSubw;
    }

    /**
     * Set the iSubw indicator value.
     * 
     * @param subw
     *            the iSubw to set
     */
    public void setISubw(Integer iSubw) {
        if (iSubw != null) {
            this.iSubw = iSubw;
        } else {
            this.iSubw = VAL_ERROR;
        }
    }

    /**
     * @return the wmoRegion
     */
    public Integer getWmoRegion() {
        return wmoRegion;
    }

    /**
     * @param wmoRegion
     *            the wmoRegion to set
     */
    public void setWmoRegion(Integer wmoRegion) {
        this.wmoRegion = wmoRegion;
    }

    /**
     * @return the obsYear
     */
    public Integer getObsYear() {
        return obsYear;
    }

    /**
     * @param obsYear
     *            the obsYear to set
     */
    public void setObsYear(Integer obsYear) {
        if (obsYear != null) {
            this.obsYear = obsYear;
        } else {
            this.obsYear = VAL_ERROR;
        }
    }

    /**
     * @return the obsMonth
     */
    public Integer getObsMonth() {
        return obsMonth;
    }

    /**
     * @param obsMonth
     *            the obsMonth to set
     */
    public void setObsMonth(Integer obsMonth) {
        if (obsMonth != null) {
            this.obsMonth = obsMonth;
        } else {
            this.obsMonth = VAL_ERROR;
        }
    }

    /**
     * @return the obsDay
     */
    public Integer getObsDay() {
        return obsDay;
    }

    /**
     * @param obsDay
     *            the obsDay to set
     */
    public void setObsDay(Integer obsDay) {
        if (obsDay != null) {
            this.obsDay = obsDay;
        } else {
            this.obsDay = VAL_ERROR;
        }
    }

    /**
     * @return the obsHour
     */
    public Integer getObsHour() {
        return obsHour;
    }

    /**
     * @param obsHour
     *            the obsHour to set
     */
    public void setObsHour(Integer obsHour) {
        if (obsHour != null) {
            this.obsHour = obsHour;
        } else {
            this.obsHour = VAL_ERROR;
        }
    }

    /**
     * Decode the observation sections with in the current report.
     */
    @Override
    public PluginDataObject decode() throws DecoderException {
        decodeSection0();
        decodeSection1();
        decodeSection2();
        decodeSection3();
        decodeSection4();
        decodeSection5();
        return consolidateReport();
    }

    /**
     * Perform the section 0 decode. This section is usually different for each
     * type of synoptic data so the implementation is deferred to the subclass.
     * 
     * @throws DecoderException
     */
    protected abstract void decodeSection0() throws DecoderException;

    /**
     * Decode section 1 data if it is defined.
     */
    protected void decodeSection1() throws DecoderException {
        if (secDecoders[1] != null) {
            secDecoders[1].decode(reportParser);
        }
    }

    /**
     * Decode section 2 data if it is defined.
     */
    protected void decodeSection2() throws DecoderException {
        if (secDecoders[2] != null) {
            secDecoders[2].decode(reportParser);
        }
    }

    /**
     * Decode section 3 data if it is defined.
     */
    protected void decodeSection3() throws DecoderException {
        if (secDecoders[3] != null) {
            secDecoders[3].decode(reportParser);
        }
    }

    /**
     * Decode section 4 data if it is defined.
     */
    protected void decodeSection4() throws DecoderException {
        if (secDecoders[4] != null) {
            secDecoders[4].decode(reportParser);
        }
    }

    /**
     * Decode section 5 data if it is defined.
     */
    protected void decodeSection5() throws DecoderException {
        if (secDecoders[5] != null) {
            secDecoders[5].decode(reportParser);
        }
    }

    /**
     * Consolidate report gathers together all of the data decoded in the
     * decoder and any sub-decoders used. Any subclass overriding this method
     * must be sure to call back to this method first.
     * 
     * @return The decoded data.
     */
    protected PluginDataObject consolidateReport() {
        ObsCommon report = null;

        Calendar oTime = calculateObsDateTime(
                TimeTools.getSystemCalendar(obsYear, obsMonth, obsDay), obsDay,
                obsHour, obsYear, obsMonth);
        if (oTime != null) {
            report = new ObsCommon();

            report.setMessageData(getReportData());

            report.setObsText(reformatObs(getReportData()));
            report.setWmoHeader(getHeader().getWmoHeader());

            // Determine if this is a COR'd observation. Value must be in the
            // range of "CC[A..Z]"
            String cor = getHeader().getBBBIndicator();
            // must have 3 characters.
            if ((cor != null) && (cor.length() == 2)) {
                if ("CC".equals(cor.substring(0, 2))) {
                    char c = cor.charAt(2);
                    if ((c >= 'A') && (c <= 'Z')) {
                        report.setCorIndicator(cor);
                    }
                }
            }
            report.setTimeObs(oTime);
            report.setRefHour(TimeTools.copyToNearestHour(oTime));

            DataTime dataTime = new DataTime(oTime);
            report.setDataTime(dataTime);

            for (int i = 0; i < secDecoders.length; i++) {
                if (secDecoders[i] != null) {
                    secDecoders[i].getDecodedData(report);
                }
            }
            // Fixup the present weather string - This has to
            // occur after secDecoders have been visited!
            Integer pWx = report.getWx_present();
            String wx = null;
            if (pWx != null) {
                if ((pWx >= 0) && (pWx < 100)) {
                    wx = wxAbrev.get(pWx);
                    Double t = report.getTemp();
                    if ((pWx == 28) || ((pWx > 41) && (pWx < 48))) {
                        if ((t != null) && (t != -9999.0)) {
                            wx = (t > 273.15) ? "FG" : "FZFG";
                        } else {
                            wx = "FG";
                        }
                    }
                } else {
                    wx = "";
                }
            } else {
                wx = "";
            }
            report.setPresWeather(wx);
        }
        return report;
    }

    /**
     * The observation time algorithm is taken from the NCEP synoptic decoders.
     * 
     * @return
     */
    public static Calendar calculateObsDateTime(Calendar currentClock,
            Integer obsDay, Integer obsHour, Integer obsYear, Integer obsMonth) {
        Calendar obsTime = null;
        Calendar tTime = TimeTools.copyToNearestHour(currentClock);
        TimeTools.rollByDays(tTime, 1);

        if ((obsDay != null) && (obsHour != null)) {
            if (obsDay == currentClock.get(Calendar.DAY_OF_MONTH)) {
                obsTime = TimeTools.copyToNearestHour(currentClock);
                obsTime.set(Calendar.HOUR_OF_DAY, obsHour);
            } else if (obsDay == tTime.get(Calendar.DAY_OF_MONTH)) {
                // Observation time is in the next day
                obsTime = TimeTools.copyToNearestHour(tTime);
                obsTime.set(Calendar.HOUR_OF_DAY, obsHour);
            } else {
                tTime = TimeTools.copyToNearestHour(currentClock);
                int i = 0;
                while (i++ < 25) {
                    // Go back a day
                    TimeTools.rollByDays(tTime, -1);
                    if (obsDay == tTime.get(Calendar.DAY_OF_MONTH)) {
                        // Day values are equal, so this is it.
                        obsTime = TimeTools.copyToNearestHour(tTime);
                        obsTime.set(Calendar.HOUR_OF_DAY, obsHour);
                        break;
                    }
                }
            }
        }
//        if ((obsYear != null) && (obsMonth != null)) {
//            obsTime.set(Calendar.YEAR, obsYear);
//            obsTime.set(Calendar.MONTH, obsMonth - 1);
//        }

        return obsTime;
    }

    /**
     * Reformat an observation so that lines are approx. LINE_CUT_LENGTH
     * characters in length.
     * 
     * @param observation
     *            An observation to reformat.
     * @return The formatted observation.
     */
    private static final String reformatObs(String observation) {

        StringBuilder builder = new StringBuilder();
        if (observation != null) {
            String[] parts = observation.split(" ");
            int lineLen = 0;
            for (int i = 0; i < parts.length; i++) {
                builder.append(parts[i]);
                lineLen += parts[i].length();
                if (lineLen > LINE_CUT_LENGTH) {
                    lineLen = 0;
                    builder.append("\n");
                } else {
                    builder.append(" ");
                    lineLen++;
                }
            }
        }
        return builder.toString();
    }


    public static final void main(String [] args) {
        
        ITimeService service = new ITimeService() {
            @Override
            public Calendar getCalendar() {
                final Calendar c = Calendar.getInstance();
                c.setTimeZone(TimeZone.getTimeZone("GMT"));
                c.set(Calendar.YEAR, 2012);
                c.set(Calendar.MONTH, Calendar.JANUARY);
                c.set(Calendar.DAY_OF_MONTH,1);
                c.set(Calendar.HOUR_OF_DAY, 2);
                c.set(Calendar.MINUTE, 15);
                c.set(Calendar.SECOND, 32);
                c.set(Calendar.MILLISECOND, 268);

                return c;
            }
        };
        TimeTools.setTimeService(service);
        
        
        SimpleDateFormat TMFMT = new SimpleDateFormat(
        "yyyy-MM-dd HH:mm:ss");
        TMFMT.setTimeZone(TimeZone.getTimeZone("GMT"));
        
        Integer obsYear = 2012;
        Integer obsMonth = 1; 
        Integer obsDay = 31;
        Integer obsHour = 21;
        
        Calendar currentClock = TimeTools.getSystemCalendar(obsYear, obsMonth, obsDay);
        System.out.println(TMFMT.format(currentClock.getTime()));
        
        Calendar c = calculateObsDateTime(currentClock,obsDay,obsHour,obsYear,obsMonth); 
        System.out.println(TMFMT.format(c.getTime()));
        
        
        
        
        
        
    }



}
