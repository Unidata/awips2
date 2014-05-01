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
package com.raytheon.viz.aviation.monitor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.edex.plugin.taf.common.TafRecord;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.jobs.IRequestCompleteListener;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.aviation.xml.MonitorArgs;
import com.raytheon.viz.aviation.xml.MonitorCfg;

/**
 * 
 * SiteMonitor inspired by legacy python SiteMonitor. Applies legacy python
 * rules against taf data, and color codes labels based on severity of
 * conditions triggered by rules.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2009  2537       lvenable     Initial creation
 * Aug 28, 2009 3027      njensen      Major refactor
 * Sep  3, 2010 4022       rferrel      Get Alert information.
 * Sep 27, 2010 6195       rferrel      Use PythonMonitorJob's static queue
 * Oct  6, 2010 7229       rferrel      Update to Metar/Taf's set time methods.
 * Nov  4, 2010 6866       rferrel      Impact statements no longer malformed.
 * May 13, 2011 8611       rferrel      Added type to help determine blink state.
 * Apr 30, 2012 14717      zhao         Indicators turn gray when Metar is outdated
 * 20JUL2012    14570      gzhang/zhao  Modified for highlighting correct time groups in TAF Viewer
 * 11AUG2012    14570      zhao         Added 'cat' to alert_key_map
 * 02Jan2013    15606      gzhang		Remove GridData widthHint so button/label size change with GUI
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SiteMonitor implements IRequestCompleteListener<Map<?, ?>> {

	/**
	 * For DR14717: 
	 * check if Metar is outdated
	 * and turn indicator labels gray when Metar is outdated
	 */
	//private static long latestMetarTime = -1; 
	private final int GRAY_COLOR_SEVERITY = 1;
	private boolean GRAY_LABEL = false;

	/**
     * False while monitor is running a query otherwise true.
     */
    private final AtomicBoolean requestCompleted = new AtomicBoolean(true);

    protected static Color[] severityColors;

    /**
     * Parent composite.
     */
    protected Composite parent;

    /**
     * String of monitor labels.
     */
    protected String[] monitorLabels;

    /**
     * String of monitor items.
     */
    protected String[] monitorItems;

    protected Map<String, Label> labelMap = new HashMap<String, Label>();

    protected Map<String, MonitorToolTip> tooltipMap = new HashMap<String, MonitorToolTip>();

    protected MonitorCfg cfg;

    protected Map<String, Object> args = new HashMap<String, Object>();

    protected TafSiteComp parentSiteComp;

    protected int previousMaxSeverity;

    /**
     * Mapping of alerts used to determine highlighting in the viewer and metar
     * tabs of the editor.
     */
    private Map<String, String[]> alertMap;

    /**
     * This maps the keys used in the Metar, to determine severity, to the keys
     * in the TAF to locate the correspond values.
     */
    private static final Map<String, String[]> ALERT_KEY_MAP = new HashMap<String, String[]>();
    {
    	ALERT_KEY_MAP.put("cat", new String[] { "vsby", "sky" }); // 14570
    	//ALERT_KEY_MAP.put("tempo", new String[] { "wind", "vsby", "pcp", "obv", "vcnty", "sky" } ); // 14570
        ALERT_KEY_MAP.put("vsby", new String[] { "vsby" });
        ALERT_KEY_MAP.put("wind", new String[] { "wind" });
        ALERT_KEY_MAP.put("wx", new String[] { "pcp", "obv", "vcnty" });
        ALERT_KEY_MAP.put("sky", new String[] { "sky" });
    }

    /**
     * Constructor.
     * 
     * @param parent
     * @param config
     * @param stationName
     */
    public SiteMonitor(Composite parent, TafSiteComp parentSiteComp,
            MonitorCfg config, Map<String, String[]> alertMap, Map<String,String> alertTimeMap/* DR 14570 */,Map<String,String[]> tempoMap) {
        this.parent = parent;
        this.parentSiteComp = parentSiteComp;
        this.cfg = config;
        this.alertMap = alertMap;
        this.tempoMap = tempoMap;//20120711
        this.alertTimeMap = alertTimeMap;// DR 14570
        monitorItems = StringUtil.split(cfg.getMonitorItems(), ",");
        initMonitorLabels(cfg.getMonitorLabels());

        Composite controlComposite = createControlComposite();
        for (int i = 0; i < monitorItems.length; i++) {
            labelMap.put(monitorItems[i],
                    createLabel(controlComposite, monitorLabels[i]));
        }

        List<MonitorArgs> mArgs = cfg.getArgsArray();
        if (mArgs != null && mArgs.size() > 0) {
            for (MonitorArgs a : mArgs) {
                args.put(a.getName(), a.getValue());
            }
        }
    }

    /**
     * Parse the monitor labels.
     * 
     * @param monitorLabelsStr
     *            String of comma separated monitor labels.
     */
    private void initMonitorLabels(String monitorLabelsStr) {
        /*
         * Create the number of monitor labels to match the size of the number
         * of items and fill the array with "Unk" (unknown).
         */
        monitorLabels = new String[monitorItems.length];
        Arrays.fill(monitorLabels, 0, monitorLabels.length, "Unk");

        if (monitorLabelsStr != null) {
            // This trims entries of leading & trailing whitespaces.
            String[] tmpLabels = StringUtil.split(monitorLabelsStr, ",");

            /*
             * Find the smallest array size between the tmpLabels (parsed
             * monitorLabelsStr) and the monitorLabels array. We want the
             * smallest size to fill the monitorLabels array. If there are not
             * enough items to fill the monitorLabels, the "Unk" is already
             * present.
             */
            int minArraySize = (tmpLabels.length < monitorLabels.length) ? tmpLabels.length
                    : monitorLabels.length;

            for (int i = 0; i < minArraySize; i++) {
                monitorLabels[i] = tmpLabels[i];
            }
        } else {
            monitorLabels = new String[] { "Unk" };
        }
    }

    /**
     * Create the control composite based on the number of specified monitor
     * items that are valid.
     * 
     * @param validItems
     *            Valid monitor items.
     * @return Composite.
     */
    protected Composite createControlComposite() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        GridLayout gl = new GridLayout(monitorItems.length, false);
        gl.horizontalSpacing = 0;
        gl.marginWidth = 2;
        Composite controlComp = new Composite(parent, SWT.BORDER);
        controlComp.setLayout(gl);
        configMgr.setDefaultColors(controlComp);

        return controlComp;
    }

    /**
     * Create the monitor control label.
     * 
     * @param parentComp
     *            Parent composite.
     * @param text
     *            Text for the label.
     * @return Label control.
     */
    protected Label createLabel(Composite parentComp, String text) {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        text = " " + text.trim() + " ";
        GridData gd = new GridData(15, SWT.DEFAULT);// GridData(35 // DR 15606
        Label lbl = new Label(parentComp, SWT.CENTER);
        configMgr.setDefaultFontAndColors(lbl, text, gd);
        lbl.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_GRAY));

        return lbl;
    }

    /**
     * When text is contains flight categories get the metar/taf status info and
     * clean up the messages; otherwise just append a linefeed to the text.
     * 
     * @param text
     *            - string to parse
     * @param mtrtafinfo
     *            - Modified to contain the metar/taf information or an empty
     *            string
     * @param newMsg
     *            - Modified to contain the clean up text
     */
    private void cleanupMsg(String text, StringBuilder mtrtafinfo,
            StringBuilder newMsg) {
        boolean fltcat = false;
        mtrtafinfo.setLength(0);
        newMsg.setLength(0);
        if (text != null) {
            if (text.indexOf(":METAR:") > 0) {
                // Have one or more flight categories assume each one contains
                // the
                // same metar/taf info line. Keep one copy of it and strip it
                // off of
                // each category entry.
                for (String lne : text.split("\n")) {
                    int metarPos = lne.indexOf(":METAR:");
                    if (metarPos > 0) {
                        newMsg.append(lne.substring(0, metarPos).trim())
                                .append("\n");
                        if (!fltcat) {
                            fltcat = true;
                            mtrtafinfo.append(
                                    lne.substring(metarPos + 1).trim()).append(
                                    "\n");
                        }
                    } else {
                        newMsg.append(lne.trim()).append("\n");
                    }
                }
            } else {
                newMsg.append(text).append("\n");
            }
        }
    }

    /**
     * This generates the formatted tool tip.
     * 
     * @param taf
     *            - The taf string.
     * @param text
     *            - Additional information to follow the TAF such as a METAR
     * @param msg
     *            - Status message
     * @param impactPlacement
     *            - where to place msg top, split or default top
     * @return tooltip
     */
    private String toolTipFormat(String taf, Object text, String msg,
            String impactPlacement) {
        StringBuilder toolTip = new StringBuilder();
        StringBuilder mtrtafinfo = new StringBuilder();
        StringBuilder newMsg = new StringBuilder();
        cleanupMsg(msg, mtrtafinfo, newMsg);

        StringBuilder bText = new StringBuilder();
        if (text != null) {
            bText.append(text.toString().trim()).append("\n");
        }

        StringBuilder bTaf = new StringBuilder();
        if (taf != null) {
            bTaf.append(taf.trim()).append("\n");
        }

        if ("split".equals(impactPlacement)) {
            toolTip.append(mtrtafinfo.toString()).append(bTaf.toString())
                    .append(bText.toString()).append(newMsg.toString());
        } else if ("bottom".equals(impactPlacement)) {
            toolTip.append(bTaf.toString()).append(bText.toString())
                    .append(mtrtafinfo.toString()).append(newMsg.toString());
        } else {
            // Default is top
            toolTip.append(mtrtafinfo.toString()).append(newMsg.toString())
                    .append(bTaf.toString()).append(bText.toString());
        }

        return toolTip.toString().trim();
    }

    /**
     * 
     * @param report
     * @return
     */
    private static String formatMetar(String report) {

        StringBuilder sb = new StringBuilder();
        char lastChar = 0;
        for (int i = 0; i < report.length(); i++) {
            char c = report.charAt(i);
            switch (c) {
            case '\r':
            case '\n': {
                c = ' ';
            }
            default: {
                if (lastChar == ' ') {
                    if (c != ' ') {
                        sb.append(c);
                    }
                } else {
                    sb.append(c);
                }
                lastChar = c;
            }
            }
        }
        String indent = "     ";
        ArrayList<String> parts = new ArrayList<String>();
        int rmkPos = sb.indexOf("RMK");
        if (rmkPos > 0) {
            parts.add(sb.substring(0, rmkPos).trim());
            parts.add(indent + sb.substring(rmkPos));
        } else {
            parts.add(sb.toString());
        }
        for (int i = 0; i < parts.size();) {
            String s = parts.get(i);
            if (s.indexOf("RMK") > 0) {
                indent += " ";
            }
            if (s.length() < 74) {
                i++;
            } else {
                // Start at position 70
                int spcPos = 70;
                // and work backwards to find the first space character
                // so we can cut the line there.
                while ((s.charAt(spcPos) != ' ') && (spcPos > 0)) {
                    spcPos--;
                }
                if (s.charAt(spcPos) == ' ') {
                    parts.set(i, s.substring(0, spcPos));
                    i++;
                    parts.add(i, indent + s.substring(spcPos + 1));
                }
            }
        }
        sb.setLength(0);
        sb.append(parts.get(0));
        for (int i = 1; i < parts.size(); i++) {
            sb.append('\n');
            sb.append(parts.get(i));
        }
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    @Override
    public void requestComplete(Map<?, ?> result) {
    	/**
    	 * DR14717: Outdated Metar should turn gray
    	 */
    	String thisMonitor = this.getMonitorClassName();
    	
        if (!parent.isDisposed()) {
            Object obj = result.get("fatal");
            if (obj != null) {
                ArrayList<?> list = (ArrayList<?>) obj;
                String fatal = (String) list.get(0);
                parentSiteComp.setTafError(fatal);
            } else {
                parentSiteComp.setTafNoError();
                Map<?, ?> statusMap = (Map<?, ?>) result.get("status");
                Map<?, ?> tafMap = (Map<?, ?>) result.get("taf");
                Map<?, ?> status = (Map<?, ?>) statusMap.get("status");
                String type = result.get("type").toString();
                Set<?> keys = status.keySet();
                Color[] colors = getSeverityColors();
                String impactPlacement = getImpactPlacement();
                int maxSeverity = 0;
                if (alertMap != null) {
                    alertMap.clear();
                }

                /**
                 * DR14717: for checking if Metar is outdated. 
                 */
                if ( thisMonitor.equals("MetarMonitor") ) {
                	Map<?, ?> mtrdcdMap = (Map<?, ?>) statusMap.get("dcd");
                	if ( mtrdcdMap != null ) {
                		Map<?, ?> mtrItimeMap = (Map<?, ?>) mtrdcdMap.get("itime");
                		parentSiteComp.setLatestMtrTime(((Float) mtrItimeMap.get("value")).longValue() * 1000);
                	}
                }
                
                long currentTime = SimulatedTime.getSystemTime().getTime().getTime();
                
                for (Object key : keys) {
                    Label label = labelMap.get(key);
                    if (label != null && !label.isDisposed()) {
                        Map<?, ?> valueMap = (Map<?, ?>) status.get(key);
                        int severity = (Integer) valueMap.get("severity");
                        if (severity > maxSeverity) {
                            maxSeverity = severity;
                        }
//                        if ( severity >= 2 ) {
//                        	System.out.println("0***key/severity: "+key.toString()+" / "+severity);                        
//                        }
                        String msg = (String) valueMap.get("msg");
                        
                        /**
                         * DR14717: Metar monitor indicators should turn gray when Metar is outdated
                         */
                        if ( parentSiteComp.getLatestMtrTime() > 0 ) {
                        	if ( ( currentTime > ( parentSiteComp.getLatestMtrTime() + TafSiteComp.METAR_TIMEOUT_4HR10MIN ) )
                        			&& ( thisMonitor.equals("MetarMonitor") || thisMonitor.equals("PersistMonitor") ) ) {
                        		/**
                        		 * both Current observation monitoring indicators 
                        		 * and persistence indicators should turn gray
                        		 */
                        		GRAY_LABEL = true;
                        		msg = "METAR outdated";
                        	} else if ( ( currentTime > ( parentSiteComp.getLatestMtrTime() + TafSiteComp.METAR_TIMEOUT_2HR ) )  
                        			&& thisMonitor.equals("PersistMonitor") ) {
                        		/**
                        		 *  Persistence indicators should turn gray
                        		 */
                        		GRAY_LABEL = true;
                        		msg = "METAR outdated for persistence monitoring";
                        	}
                        }
                        
                        if ( ( parentSiteComp.getLatestMtrTime() < 0 ) && thisMonitor.equals("PersistMonitor") ) {
                        	parentSiteComp.setPersistMonitorProcessedFirst(true);
                        }
                        
                        if ( GRAY_LABEL ) {
                        	label.setBackground(colors[GRAY_COLOR_SEVERITY]);
                        	GRAY_LABEL = false; 
                        } else {
                        	label.setBackground(colors[severity]);
                        }
                        
                        String toolTip = null;
                        String taf = (String) tafMap.get("text");
                        Object text = statusMap.get("text");
                        if (text instanceof String) {
                            String s = (String) text;
                            if (s.startsWith("METAR")) {
                                text = formatMetar(s);
                            }
                        }

                        toolTip = toolTipFormat(taf, text, msg,
                                impactPlacement.toLowerCase());

                        // No mnemonics so escape any '&' in the tooltip.
                        if (toolTip.contains("&")) {
                            String[] ss = toolTip.split("&");
                            StringBuilder sb = new StringBuilder(ss[0]);
                            for (int i = 1; i < ss.length; ++i) {
                                sb.append("&&").append(ss[i]);
                            }
                            toolTip = sb.toString();
                        }

                        // MonitorToolTip provides the tooltip functionality
                        label.setToolTipText(null);
                        label.setData(MonitorToolTip.tooltipTextKey, toolTip);
                        MonitorToolTip mtt = tooltipMap.get(key);
                        if (mtt != null) {
                            label.redraw();
                        } else {
                            mtt = new MonitorToolTip(label);
                            tooltipMap.put((String) key, mtt);
                            mtt.open();
                        }

                        // Get metar keys and taf values to highlight in the
                        // editor's metar and viewer tab.
                        if (alertMap != null && severity >= 2
                                && ALERT_KEY_MAP.containsKey(key)) {
                            String[] tafKeys = ALERT_KEY_MAP.get(key);
                            Map<?, ?> dcd = (Map<?, ?>) tafMap.get("dcd");
                            ArrayList<Map<?, ?>> group = (ArrayList<Map<?, ?>>) dcd
                                    .get("group");
                            Map<?, ?> oncl = group.get(0);
                            Map<?, ?> obs = (Map<?, ?>) oncl.get("prev"); 
                            ArrayList<String> alertValues = new ArrayList<String>();
                            
                            Map<?,?> tempo=null;//20120711
                            ArrayList<String> tempoAlertValues = new ArrayList<String>();//20120711
                            // DR 14570: based on A1 Python code in TafViewer.highlight()                            
                            long tsys= SimulatedTime.getSystemTime().getTime().getTime();
                            long tfrom= ((Float)((Map<?,?>)((Map<?,?>)oncl.get("prev")).get("time")).get("from")).longValue()*1000;
                            long time = tsys>tfrom ? tsys : tfrom;
                            long tto = 0;

                            for(Map<?,?> map : group){
                            	//for( Object o : map.keySet())System.out.println("^^^^^^^^^^^^ map keys: "+(String)o);
                            	tto = ((Float)((Map<?,?>)((Map<?,?>)map.get("prev")).get("time")).get("to")).longValue()*1000;
                            	//System.out.println("---1---time/tto: "+new java.util.Date(time)+" / "+new java.util.Date(tto)+" key: "+key.toString());
                            	if(time < tto){



                            		//20120711:  see A1 TafViewer.py's highlight(), should be outside the if(severity >= 2) block?

                            		//System.out.println("1+++map.keySet().contains(oncl): "+new Boolean(map.keySet().contains("oncl")));
                            		String[] keyArray = map.keySet().toArray(new String[]{});//for TEMPO highlight 
                            		for(String s : keyArray){
                            			if(s.equals("ocnl")){
                            				long oFrom=((Float)((Map<?,?>)((Map<?,?>)map.get("ocnl")).get("time")).get("from")).longValue()*1000; 
                            				long oTo=((Float)((Map<?,?>)((Map<?,?>)map.get("ocnl")).get("time")).get("to")).longValue()*1000;

                            				//System.out.println("2+++oFrom**time**oTo: "+oFrom+"**"+time+"**"+oTo);

                            				if(oFrom<=time && time<oTo)	
                            					tempo=(Map<?, ?>) map.get("ocnl");
                            			}	
                            		}

                            		obs = (Map<?, ?>) map.get("prev"); //System.out.println("______2___time/tto: "+new java.util.Date(time)+" / "+new java.util.Date(tto)+" key: "+key.toString());
                            		break;
                            	}
                            }

                            Map<?, ?> obsTimeMap = (Map<?, ?>) obs.get("time");//for getting correct line using time

                            for (String tafKey : tafKeys) {	
                            	// DR 14570 20120711                            	
                            	Map<?,?> tempoAlert = (tempo==null) ? null:(Map<?,?>)tempo.get(tafKey);
                            	//System.out.println("tempo==null***tempoAlert != null: "+new Boolean(tempo==null)+"***"+new Boolean(tempoAlert != null));
                            	if(tempoAlert != null){ 
                            		tempoAlertValues.add((String)tempoAlert.get("str"));
                            		//System.out.println("(String)tempoAlert.get(str): "+(String)tempoAlert.get("str"));	
                            	}// END 20120711
                            	Map<?, ?> alert = (Map<?, ?>) obs.get(tafKey);
                            	if (alert != null) {
                            		String value = (String) alert.get("str");
                            		alertValues.add(value);
                            	}
                            }	//System.out.println("________3___obsTimeMap: "+(String)obsTimeMap.get("str"));
                            tempoMap.put((String)key, tempoAlertValues.toArray(new String[tempoAlertValues.size()]));//20120711
                            if(alertTimeMap!=null) alertTimeMap.put((String)key, (String)obsTimeMap.get("str"));// DR 14570
                            String[] s = new String[alertValues.size()];
                            alertMap.put((String) key, alertValues.toArray(s));
                        }
                    }
                }

                if (this.getMonitorClassName().equals("MetarMonitor")) {
                    Map<?, ?> tafdcdMap = (Map<?, ?>) tafMap.get("dcd");
                    Map<?, ?> tafItimeMap = (Map<?, ?>) tafdcdMap.get("itime");
                    long tafTime = ((Float) tafItimeMap.get("value"))
                            .longValue() * 1000;
                    parentSiteComp.setTafTime(tafTime, tafItimeMap.get("str")
                            .toString());

                    Map<?, ?> mtrdcdMap = (Map<?, ?>) statusMap.get("dcd");

                    if (mtrdcdMap != null) {
                        Map<?, ?> mtrItimeMap = (Map<?, ?>) mtrdcdMap
                                .get("itime");
                        long mtrTime = ((Float) mtrItimeMap.get("value"))
                                .longValue() * 1000;
                        parentSiteComp.setMetarTime(mtrTime,
                                mtrItimeMap.get("str").toString());
                    } else {
                        parentSiteComp.setMetarMissing();
                    }
                }

                if (maxSeverity > previousMaxSeverity && maxSeverity > 1) {
                    parentSiteComp.startBlinkingButton();
                } else {
                    if (type.equals("TAF")) {
                        if (maxSeverity <= 1) {
                            parentSiteComp.checkSiteButton();
                        }
                    }
                    if (type.endsWith("MetarMonitor")) {
                        if (maxSeverity <= 1) {
                            parentSiteComp.stopBlinkingButton();
                        }
                    }
                }
                previousMaxSeverity = maxSeverity;
                parentSiteComp.updateSeverity(previousMaxSeverity);
                requestCompleted.set(true);
            }
        }
    }

    /**
     * This allows a thread to wait for the monitor's queued request to
     * complete.
     */
    public void waitForMonitor() {
        try {
            while (requestCompleted.get() == false) {
                Thread.sleep(20L);
            }
        } catch (InterruptedException e) {
            // do nothing
        }
    }

    public void monitor(TafRecord taf, String type) {
        if (taf != null) {
            requestCompleted.set(false);

            MonitorRequest req = new MonitorRequest();
            req.setSiteID(this.parentSiteComp.getStationName());
            req.setArgs(args);
            req.setTaf(TafUtil.safeFormatTaf(taf, false));
            req.setWmoHeader(taf.getWmoHeader());
            req.setCfg(cfg);
            req.setType(type);
            req.setListener(this);
            PythonMonitorJob.offerRequest(req);
        }
    }

    private synchronized String getImpactPlacement() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        return configMgr.getDataAsString(ResourceTag.ImpactPlacement);
    }

    private Color[] getSeverityColors() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        return configMgr.getAlertLevelColors();
    }

    public String getMonitorClassName() {
        return cfg.getClassName();
    }

    /**
     * for DR14717:
     */
    public Map<String, Label> getLabelMap() {
    	return labelMap;
    }
    
    /**
     * for DR14717:
     */
    public Color getGraySeverityColor() {
    	return getSeverityColors()[GRAY_COLOR_SEVERITY];
    }
    
    
    //----------------------DR 14570: 
    
    private Map<String, String> alertTimeMap;// = new HashMap<String, String>();
    public Map<String, String[]> tempoMap;// = new HashMap<String, String[]>();
    public void setAlertTimeMap(Map<String, String> map){
    	alertTimeMap = map;
    }
}
