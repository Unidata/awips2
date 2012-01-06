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
package com.raytheon.edex.plugin.obs.metar.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2009            brockwoo     Initial creation
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1.0	
 */

public class VisibilityParser {

    private String visibility;
    private double prevail_vsbySM;
    private String vsby_Dir;
    private Pattern digitsOnly;
    private Pattern twoDigitDir;
    private Pattern oneDigitDir;
    private Pattern parseNumber;
    private UnitConverter meters2Miles;
    private UnitConverter km2Miles;
    private UnitConverter miles2Meters;
    private UnitConverter miles2Km;
    
    // MAX value taken from AWIPSI code
    private static final double MAX = 2147483647;
    
    public VisibilityParser() {
        this.meters2Miles = SI.METER.getConverterTo(NonSI.MILE);
        this.km2Miles = SI.KILOMETER.getConverterTo(NonSI.MILE);
        this.miles2Meters = NonSI.MILE.getConverterTo(SI.METER);
        this.miles2Km = NonSI.MILE.getConverterTo(SI.KILOMETER);
        this.visibility = new String();
        this.prevail_vsbySM = 0.0;
        this.vsby_Dir = new String();
        this.digitsOnly = Pattern.compile("^\\d+$");
        this.twoDigitDir = Pattern.compile("^[NS][EW]$");
        this.oneDigitDir = Pattern.compile("^[NSEW]$");
        this.parseNumber = Pattern.compile("^(\\d+)\\D*$");
    }
    
    public String getVisibility() {
        return visibility;
    }

    public double getPrevail_vsbySM() {
        return prevail_vsbySM;
    }
    
    public double getPrevail_vsbyKM() {
        return this.miles2Km.convert(prevail_vsbySM);
    }
    
    public double getPrevail_vsbyM() {
        return this.miles2Meters.convert(prevail_vsbySM);
    }

    public String getVsby_Dir() {
        return vsby_Dir;
    }
    
    public boolean decode(String visString) {
        
        int offset = 0;
        int charOffset = 0;
        
        String[] vis = visString.split(" ");
        
        if(vis == null || vis.length == 0) {
            return false;
        }
        
        /****************************************/
        /* CHECK FOR VISIBILITY MEASURED <1/4SM */
        /****************************************/
        if(vis[0].equals("M1/4SM") || vis[0].equals("<1/4SM")) {
            this.prevail_vsbySM = 0.0;
            this.visibility = vis[0];
            return true;
        }
        /***********************************************/
        /* CHECK FOR VISIBILITY MEASURED IN KILOMETERS */
        /***********************************************/
        if((offset = vis[0].indexOf("KM")) != -1) {
            Matcher km = digitsOnly.matcher(vis[0].substring(0, offset));
            if(km.matches()) {
                this.prevail_vsbySM = 
                    this.km2Miles.convert(prevailVSBY(vis[0]));
                this.visibility = vis[0];
                return true;
            }
            else {
                return false;
            }
        }
        /***********************************/
        /* CHECK FOR VISIBILITY MEASURED   */
        /* IN A FRACTION OF A STATUTE MILE */
        /***********************************/
        else if( (charOffset = vis[0].indexOf('/')) != -1 &&
                (offset = vis[0].indexOf("SM")) != -1 ) {
            Matcher num = digitsOnly.matcher(vis[0].substring(0, charOffset));
            Matcher den = 
                digitsOnly.matcher(vis[0].substring(charOffset+1, offset));
            if(num.matches() && den.matches()) {
                this.prevail_vsbySM = prevailVSBY(vis[0]);
                this.visibility = vis[0];
                return true;
            }
            else
                return false;
        }
        /***********************************/
        /* CHECK FOR VISIBILITY MEASURED   */
        /*     IN WHOLE STATUTE MILES      */
        /***********************************/
        else if((offset = vis[0].indexOf("SM")) != -1) {
            Matcher sm = digitsOnly.matcher(vis[0].substring(0, offset));
            if(sm.matches()) {
                prevail_vsbySM = prevailVSBY(vis[0]);
                this.visibility = vis[0];
                return true;
            }
            else {
                return false;
            }
        }
        /***********************************/
        /* CHECK FOR VISIBILITY MEASURED   */
        /* IN WHOLE AND FRACTIONAL STATUTE */
        /*             MILES               */
        /***********************************/
        else if( vis[0].length() < 4 ) {
            Matcher wholeNumber = digitsOnly.matcher(vis[0]);
            if(!wholeNumber.matches()) {
                return false;
            }
            String save_token = vis[0];
            
            if(vis.length <= 1 || vis[1] == null) {
                return false;
            }
            if( (charOffset = vis[1].indexOf('/')) != -1 &&
                    (offset = vis[1].indexOf("SM")) != -1 ) {
                Matcher num = 
                    digitsOnly.matcher(vis[1].substring(0, charOffset));
                Matcher den = 
                    digitsOnly.matcher(vis[1].substring(charOffset+1, offset));
                if( num.matches() && den.matches() )
                {
                    prevail_vsbySM = prevailVSBY(vis[1]);
                    prevail_vsbySM += Double.valueOf(save_token);
                    this.visibility = vis[0] + " " + vis[1];
                    return true;
                }
                else {
                    return false;
                }
            }
            else {
                return false;
            }
        } else if(vis[0].length() >= 4) {
            /***********************************/
            /* CHECK FOR VISIBILITY MEASURED   */
            /* IN METERS WITH OR WITHOUT DI-   */
            /*     RECTION OF OBSERVATION      */
            /***********************************/
            Matcher firstFourChar = digitsOnly.matcher(vis[0].substring(0, 4));
            if(!firstFourChar.matches()) {
                return false;
            }
            
            if(vis[0].length() == 6) {
                String dir = vis[0].substring(4, 6);
                if(this.twoDigitDir.matcher(dir).matches()) {
                    vsby_Dir = dir;
                }
            }
            else if(vis[0].length() == 5) {
                String dir = vis[0].substring(4, 5);
                if(this.oneDigitDir.matcher(dir).matches()) {
                    vsby_Dir = dir;
                }
            }
            double visValue = this.antod(vis[0]);


            if( visValue >= 50f &&
                    visValue <= 500f &&
                    (visValue % 50) == 0 ) {
                this.prevail_vsbySM = this.meters2Miles.convert(visValue);
                this.visibility = vis[0];
                return true;
            }
            else if( visValue >= 500.0f &&
                    visValue <= 3000.0f &&
                    (visValue % 100) == 0 )
            {
                prevail_vsbySM = this.meters2Miles.convert(visValue);
                this.visibility = vis[0];
                return true;
            }
            else if( visValue >= 500.0f &&
                    visValue <= 3000.0f &&
                    (visValue % 100) == 0 )
            {
                prevail_vsbySM = this.meters2Miles.convert(visValue);
                this.visibility = vis[0];
                return true;
            }
            else if( visValue >= 3000.0f &&
                    visValue <= 5000.0f &&
                    (visValue % 500) == 0 )
            {
                this.prevail_vsbySM = this.meters2Miles.convert(visValue);
                this.visibility = vis[0];
                return true;
            }
            else if( visValue >= 5000.0f &&
                    visValue <= 9999.0f &&
                    (visValue % 500) == 0 ||
                    visValue == 9999 )
            {
                this.prevail_vsbySM = this.meters2Miles.convert(visValue);
                this.visibility = vis[0];
                return true;
            }
            else {
                return false;
            }

        }
        return false;
    }
    
    private double antod(String vis) {
        if(vis.length() < 0) {
            return MAX;
        }
        Matcher number = this.parseNumber.matcher(vis);
        if(number.matches()) {
            return Double.valueOf(number.group(1));
        }
        else {
            return MAX;
        }
        
    }
    
    private Double prevailVSBY(String vis) {   
        int Slash_ptr, SM_KM_ptr;
        if(vis == null || vis.length() == 0) {
            return MAX;
        }
        if( (SM_KM_ptr = vis.indexOf("SM")) == -1) {
            SM_KM_ptr = vis.indexOf("KM");
        }
        Slash_ptr = vis.indexOf('/');
        if( Slash_ptr == -1 )
        {
            return Double.valueOf(vis.substring(0, SM_KM_ptr));
        }
        else
        {
            String numerator = vis.substring(0, Slash_ptr);
            String denominator = vis.substring(Slash_ptr+1, SM_KM_ptr);
            return (Double.valueOf(numerator)/
                    Double.valueOf(denominator));
        }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        String[] visibilities = {"4000 RA","10SM", "40KM","1 3/8SM","5000NW","3/16SM","9999NDV", "1 1/2SM", "BLAH", "2 1/2SM", "8SM", "30KM", "M1/4SM"};
        VisibilityParser parser = new VisibilityParser();
        for(String vis : visibilities) {
            if(parser.decode(vis)) {
                System.out.println("Found vis info: " + parser.getVisibility() + " " + parser.getPrevail_vsbySM());
            }
            else {
                System.out.println("The decode didn't find anything");
            }
        }

    }

}
