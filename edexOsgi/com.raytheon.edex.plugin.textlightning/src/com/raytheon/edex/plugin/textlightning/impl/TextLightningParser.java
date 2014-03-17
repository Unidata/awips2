/*****************************************************************************************
 * COPYRIGHT (c), 2006-2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.edex.plugin.textlightning.impl;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgMsgType;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2009       3983 jsanchez     Initial creation
 * Feb 27, 2013    DCS 152 jgerth/elau	Support for WWLLN
 * Jan 27, 2014  DR 16080  M.Porricelli Changed LIGHTNING_PTRN_A
 *                                      to accommodate AK BLM
 *                                      lgtng intensities -999 to
 *                                      999
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class TextLightningParser {
    
    /** The logger */
    private Log logger = LogFactory.getLog(getClass());
    
    int currentReport = -1;
    
    private List<LightningStrikePoint> reports;
    
    // 03/23/2010 13:35:01 72.00 -157.00 -14  1
    // 03/23/2010 13:35:01 72.00 -157.00 14  1
    // 03/23/2010 13:35:01 72.00 -157.00 -142  1
    // 03/23/2010 13:35:01 72.00 -157.00 142  1
    private static final String LIGHTNING_PTRN_A = "(\\d{2,2}/\\d{2,2}/\\d{4,4}) (\\d{2,2}:\\d{2,2}:\\d{2,2})\\s{1,}(\\d{1,2}.\\d{2,2})\\s{1,}( |-\\d{1,3}.\\d{2,2})\\s{1,}(-?\\d{1,3})\\s{1,}(\\d{1,2})";
    private static final Pattern LTG_PTRN_A = Pattern.compile(LIGHTNING_PTRN_A);

    // 10:03:24:13:35:00.68 72.000 157.000   -14.2  1
    private static final String LIGHTNING_PTRN_B = "(\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}\\.\\d{2,2})\\s{1,}(\\d{1,2}\\.\\d{2,})\\s{1,}(-?\\d{1,3}\\.\\d{2,})\\s{1,}(-?\\d{1,3}\\.\\d{1,})\\s{1,}(\\d{1,2}).*";
    private static final Pattern LTG_PTRN_B = Pattern.compile(LIGHTNING_PTRN_B);

    // 2012-03-14T18:58:00,-5.5021,-45.9669,0.0,1
    private static final String LIGHTNING_PTRN_C = "(\\d{4,4}-\\d{2,2}-\\d{2,2})T(\\d{2,2}:\\d{2,2}:\\d{2,2}),(-?\\d{1,2}.\\d{1,4}),(-?\\d{1,3}.\\d{1,4}),(0.0),(1)";
    private static final Pattern LTG_PTRN_C = Pattern.compile(LIGHTNING_PTRN_C);
    
    /**
     * default constructor.
     */
    public TextLightningParser(){
        //empty
    }
    
    public TextLightningParser(byte [] message){
        setData(message);
    }

    /**
     * Set the message data and decode all message reports.
     * @param message Raw message data.
     */
    private void setData(byte [] message) {
        currentReport = -1;
        reports = findReports(message);
        if((reports != null)&&(reports.size() > 0)) {
            currentReport = 0;
        }
    }
    
    /**
     * Does this parser contain any more reports.
     * @return Does this parser contain any more reports.
     */
    public boolean hasNext() {
        boolean next = (reports != null);
        if(next) {
            next = ((currentReport >= 0)&&(currentReport < reports.size()));
        }
        if(!next) {
            reports = null;
            currentReport = -1;
        }
        return next;
    }
    
    /**
     * Get the next available report. Returns a null reference if no
     * more reports are available.
     * @return The next available report.
     */
    public LightningStrikePoint next() {
        
        LightningStrikePoint report = null;
        if(currentReport < 0) {
            return report;
        }
        if(currentReport >= reports.size()) {
            reports = null;
            currentReport = -1;
        } else {
            report = reports.get(currentReport++);
        }
        return report;
    }

    
    /**
     *  
     * @param start
     * @return
     */
    private List<LightningStrikePoint> findReports(byte [] message) {
        List<LightningStrikePoint> reports = new ArrayList<LightningStrikePoint>();       
        List<String> lines = separateLines(message);       
        if(lines != null) {
            LightningStrikePoint strike;
            for(String line : lines){
                try{
                    Matcher m = LTG_PTRN_A.matcher(line);
                    if(m.matches()){
                        String[] date   = m.group(1).split("/");
                        String[] time   = m.group(2).split(":");
                        String month    = date[0];
                        String day      = date[1];
                        String year     = date[2];
                        String hour     = time[0];
                        String min      = time[1];
                        String sec      = time[2];
                        String latitude = m.group(3);
                        String longitude= m.group(4);
                        String strength = m.group(5);
                        String count    = m.group(6);
                        
                        strike = new LightningStrikePoint(
                                Double.parseDouble(latitude),
                                    Double.parseDouble(longitude));
                        strike.setStrikeStrength(Double.parseDouble(strength));
                        strike.setStrikeCount(Integer.parseInt(count));
                        strike.setMonth(Integer.parseInt(month));
                        strike.setDay(Integer.parseInt(day));
                        strike.setYear(Integer.parseInt(year));              
                        strike.setHour(Integer.parseInt(hour));
                        strike.setMinute(Integer.parseInt(min));
                        strike.setSecond(Integer.parseInt(sec));
                        strike.setMillis(0);
                        strike.setMsgType(LtgMsgType.STRIKE_MSG_FL);
                        strike.setType(LtgStrikeType.STRIKE_CG);
                        strike.setLightSource("UNKN");
                        reports.add(strike);                
                    } else {
                        m = LTG_PTRN_B.matcher(line);
                        if(m.matches()){
                            String[] datetime = m.group(1).split(":");
                            String year     = datetime[0];
                            String month    = datetime[1];
                            String day      = datetime[2];
                            String hour     = datetime[3];
                            String min      = datetime[4];
                            String sec      = datetime[5].substring(0,2);
                            String msec     = datetime[5].substring(3,5);
                            

                            String latitude = m.group(2);
                            String longitude= m.group(3);
                            String strength = m.group(4);
                            String count    = m.group(5);
                            
                            
                            double lon = Double.parseDouble(longitude);
                            if(lon > 0) {
                                lon = -lon;
                            }
                            strike = new LightningStrikePoint(
                                    Double.parseDouble(latitude),lon);
                            strike.setStrikeStrength(Double.parseDouble(strength));
                            strike.setStrikeCount(Integer.parseInt(count));
                            strike.setMonth(Integer.parseInt(month));
                            strike.setDay(Integer.parseInt(day));
                            strike.setYear(Integer.parseInt(year) + 2000);              
                            strike.setHour(Integer.parseInt(hour));
                            strike.setMinute(Integer.parseInt(min));
                            strike.setSecond(Integer.parseInt(sec));
                            strike.setMillis(Integer.parseInt(msec)*10);
                            strike.setMsgType(LtgMsgType.STRIKE_MSG_FL);
                            strike.setType(LtgStrikeType.STRIKE_CG);
                            strike.setLightSource("UNKN");
                            reports.add(strike);                
                        } else {
                        	m = LTG_PTRN_C.matcher(line);
                        	if (m.matches()) {
                        		String[] datec = m.group(1).split("-");
                        		String[] timec = m.group(2).split(":");
                        		String year     = datec[0];
                        		String month    = datec[1];
                        		String day      = datec[2];
                        		String hour     = timec[0];
                        		String min      = timec[1];
                        		String sec      = timec[2];
                        		String msec     = "0";
                        		String sls		= "WWLLN";

                        		String latitude = m.group(3);
                        		String longitude= m.group(4);
                        		String strength = m.group(5);
                        		String count    = m.group(6);

                        		strike = new LightningStrikePoint(
                        				Double.parseDouble(latitude),Double.parseDouble(longitude));
                        		strike.setStrikeStrength(Double.parseDouble(strength));
                        		strike.setStrikeCount(Integer.parseInt(count));
                        		strike.setMonth(Integer.parseInt(month));
                        		strike.setDay(Integer.parseInt(day));
                        		strike.setYear(Integer.parseInt(year));              
                        		strike.setHour(Integer.parseInt(hour));
                        		strike.setMinute(Integer.parseInt(min));
                        		strike.setSecond(Integer.parseInt(sec));
                        		strike.setMillis(Integer.parseInt(msec)*10);
                        		strike.setMsgType(LtgMsgType.STRIKE_MSG_FL);
                        		strike.setType(LtgStrikeType.STRIKE_CG);
                        		strike.setLightSource(sls);
                        		reports.add(strike);
                        	} else {
                        		logger.error("Cannot match lightning input " + line);
                        	}
                        }
                    }
                } catch (NumberFormatException e){
                	logger.debug("Invalid numerical value", e);
                }
            }
        }
        return reports;
    }
    
    /**
     * 
     * @param message
     * @return
     */
    private List<String> separateLines(byte[] message) {
        List<String> reportLines = null;

        if (message != null) {
            BufferedReader reader = null;
            try {
                reader = new BufferedReader(new InputStreamReader(
                        new ByteArrayInputStream(message)));
                String s;
                reportLines = new ArrayList<String>();
                while ((s = reader.readLine()) != null) {
                    if (s.length() > 0) {
                        reportLines.add(s);
                    }
                }
            } catch (Exception e) {
                logger.error("Error reading from reader",e);
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException ioe) {
                        logger.error("Error closing reader", ioe);
                    }
                }
            }
        }
        return reportLines;
    }
    
    
    public static final void main(String [] args) {
        //                                10:      03:      24:      13:      35:        00.68              72.000                    157.000                      -14.2               1
//        String LIGHTNING_PTRN_B = "(\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}\\.\\d{2,2})\\s{1,}(\\d{1,2}.\\d{2,})\\s{1,}( |-\\d{1,3}.\\d{2,})\\s{1,}( |-\\d{1,2}\\.\\d{1,})\\s{1,}(\\d{1,2})";
//        String LIGHTNING_PTRN_B = "(\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}:\\d{2,2}\\.\\d{2,2})\\s{1,}(\\d{1,2}\\.\\d{2,})\\s{1,}(-?\\d{1,3}\\.\\d{2,})\\s{1,}(-?\\d{1,3}\\.\\d{1,})\\s{1,}(\\d{1,2}).*";
//
//        Pattern LTG_PTRN_B = Pattern.compile(LIGHTNING_PTRN_B);
//
//        Matcher m = LTG_PTRN_B.matcher("10:03:24:13:35:00.68 72.000 157.000   -14.2  1");
//        if(m.matches()) {
//            for(int i = 0;i <= m.groupCount();i++) {
//                System.out.println(m.group(i));
//            }
//        }
        
        
        
        TextLightningParser parser = null;
        
        parser = new TextLightningParser("03/23/2010 13:35:01 72.00 -157.00 -14  1\n03/23/2010 13:36:01 72.00 -157.00 -14  1".getBytes());
        while(parser.hasNext()) {
            System.out.println(parser.next());
        }
        
        
        parser = new TextLightningParser("10:03:24:13:35:00.68 72.000 157.000   -14.2  1\n10:03:24:13:36:00.68 72.000 157.000   -14.2  1".getBytes());
        while(parser.hasNext()) {
            System.out.println(parser.next());
        }
        
    }
    
    
    
}
