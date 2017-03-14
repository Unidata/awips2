package com.raytheon.rcm.coll;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.EnumSet;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;

import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.ProductInfo.Selector;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.server.Log;

@XmlAccessorType(XmlAccessType.NONE)
public class CronOTR {
    @XmlAttribute private String cron;
    @XmlAttribute private Integer productCode;
    @XmlAttribute private Integer randomWait; // in seconds
    @XmlAttribute private String wmo;
    @XmlAttribute private String nnn;
    private Integer minutesBack; // XML attribute is "hoursBack", below
    @XmlAttribute @XmlList private Set<RadarType> radarTypes;

    @XmlElement(name="request")
    private Request[] requests;
    
    /** List of parameters that indicate a product must be requested by
     * selecting the "current" version.
     */
    private static final EnumSet<Param> DYNAMIC_PRODUCT_PARAMETERS = 
        EnumSet.of(Param.ALTITUDE, Param.BASELINE, Param.LAYER,
                Param.STORM_SPEED_DIR, Param.TIME_SPAN,
                Param.TIME_SPAN_MINUTES, Param.WINDOW_AZ_RAN);
    
    /**
     * @return the cron
     */
    public String getCron() {
        return cron;
    }
    /**
     * @param cron the cron to set
     */
    public void setCron(String cron) {
        this.cron = cron;
    }
    /**
     * @return the productID
     */
    public Integer getProductCode() {
        return productCode;
    }
    /**
     * @param productID the productID to set
     */
    public void setProductCode(Integer productID) {
        this.productCode = productID;
    }
    /**
     * @return the randomWait
     */
    public Integer getRandomWait() {
        return randomWait;
    }
    /**
     * @param randomWait the randomWait to set
     */
    public void setRandomWait(Integer randomWait) {
        this.randomWait = randomWait;
    }
    /**
     * @return the requests
     */
    public Request[] getRequests() {
        return requests;
    }
    /**
     * @param requests the requests to set
     */
    public void setRequests(Request[] requests) {
        this.requests = requests;
    }
    /**
     * @return the wmo
     */
    public String getWmo() {
        return wmo;
    }
    /**
     * @param wmo the wmo to set
     */
    public void setWmo(String wmo) {
        this.wmo = wmo;
    }
    /**
     * @return the nnn
     */
    public String getNnn() {
        return nnn;
    }
    /**
     * @param nnn the nnn to set
     */
    public void setNnn(String nnn) {
        this.nnn = nnn;
    }
    /**
     * @return the minutesBack
     */
    public Integer getMinutesBack() {
        return minutesBack;
    }
    /**
     * @param minutesBack the minutesBack to set
     */
    public void setMinutesBack(Integer minutesBack) {
        this.minutesBack = minutesBack;
    }
    
    public boolean isSendSpecified() {
        return wmo != null && nnn != null;
    }
    
    public List<Request> createRequests(RadarType radarType, Calendar forTime) {
        ArrayList<Request> result = new ArrayList<Request>(
                (productCode != null ? 1 : 0) +
                (requests != null ? requests.length : 0));
        
        if (radarType != null && radarTypes != null 
                && ! radarTypes.contains(radarType))
            return result;
        
        if (productCode != null) {
            Request r = new Request();
            r.productCode = (short) productCode.intValue();
            result.add(fixRequest(r));
        }
        if (requests != null) {
            for (Request r : requests)
                result.add(fixRequest(r));
        }

        /* Apply product specific behavior including removal from the list
         * if not the product is not available for this radar type. 
         */
        Iterator<Request> ir = result.iterator();
        while (ir.hasNext()) {
            Request r = ir.next();
            Selector sel = new Selector(radarType, null, (int) r.productCode,
                    null);
            RadarProduct prod = ProductInfo.getInstance().selectOne(sel);
            if (prod != null) {
                applyMinutesBack(r, prod, forTime);
                
                /* The RPG does not generate products with parameters like
                 * time range, cross section baselines, etc., until they are
                 * explicitly requested.  Thus, there will be no "latest"
                 * version to request.  Selecting the "current" version
                 * ensures they will be generated.   
                 */
                for (Param param : prod.params) {
                    if (DYNAMIC_PRODUCT_PARAMETERS.contains(param)) {
                        r.selectCurrent();
                        break;
                    }
                }
            } else if (ProductInfo.getInstance().getPoductForCode(r.productCode) != null) {
                /* If product is known, but not for the given radar type, 
                 * remove it from the list.  Otherwise, assume it is a 
                 * product that has not yet been added to radarInfo.txt. 
                 */
                ir.remove();
            }
        }
            
        return result;
    }
    
    private static Request fixRequest(Request r) {
        Request result = (Request) r.clone();
        result.count = 1;
        result.interval = 1;
        result.selectLatest();
        result.highPriority = false;
        return result;
    }

    private void applyMinutesBack(Request r, RadarProduct prod, Calendar forTime) {
        if (minutesBack != null) {
            if (prod.params.contains(Param.TIME_SPAN)) {
                r.setEndHour(forTime.get(GregorianCalendar.HOUR_OF_DAY));
                r.setTimeSpan(minutesBack / 60);
            } else if (prod.params.contains(Param.TIME_SPAN_MINUTES)) {
                /* Even though the parameter has minute resolution, 
                 * the intent of "hoursBack" is to use the current
                 * (top of) the hour as the end time.
                 */
                r.setEndHour(forTime.get(GregorianCalendar.HOUR_OF_DAY) * 60);
                r.setTimeSpan(minutesBack);
            }
        }
    }
    
    /**
     * @return the minutesBack
     */
    @XmlAttribute(name="hoursBack")
    public String getHoursBackAsString() {
        if (minutesBack != null) {
            StringBuilder result = new StringBuilder();
            result.append(minutesBack / 60);
            int minutes = minutesBack % 60;
            if (minutes != 0)
                result.append(':').append(minutes);
            return result.toString();
        } else
            return null;
    }
    /**
     * @param minutesBack the minutesBack to set
     */
    public void setHoursBackAsString(String hoursBack) {
        if (hoursBack != null) {
            try {
                if (hoursBack.indexOf(':') >= 0) {
                    Matcher m = Pattern.compile("^(\\d+):(\\d+)$").matcher(hoursBack);
                    if (m.matches())
                        this.minutesBack = Integer.parseInt(m.group(1)) * 60 +
                                Integer.parseInt(m.group(2));
                } else {
                    this.minutesBack = Integer.parseInt(hoursBack) * 60;
                }
            } catch (RuntimeException e) {
                Log.errorf("Invalid value for \"hoursBack\" attribute: \"%s\"", hoursBack);
                this.minutesBack = null;
            }
        } else
            this.minutesBack = null;
    }
    
    /**
     * @return the radarTypes
     */
    public Set<RadarType> getRadarTypes() {
        return radarTypes;
    }
    /**
     * @param radarTypes the radarTypes to set
     */
    public void setRadarTypes(Set<RadarType> radarTypes) {
        this.radarTypes = radarTypes;
    }
}
